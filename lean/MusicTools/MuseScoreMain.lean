import Xml
import Counterpoint
import Solver
import ViolationJson
import SecondSpecies
import ThirdSpecies
import FourthSpecies
import FifthSpecies
import SecondSpeciesSoftWeighted
import ThirdSpeciesSoftWeighted
import FourthSpeciesSoftWeighted
import FifthSpeciesSoftWeighted
import Lean.Data.Json

open Lean

private def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let ok ← filename.pathExists
  if not ok then pure none
  else
    let h ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle h))

private partial def readStream (s : IO.FS.Stream) : IO String := do
  let l ← s.getLine
  if l.isEmpty then pure ""
  else
    if l.startsWith "<?" || l.startsWith "<!" then readStream s
    else (l ++ ·) <$> readStream s

private def alignVoices (vs : List (List (Option Pitch.Pitch))) : List (List Pitch.Pitch) :=
  let nonEmpty := vs.filter fun v => v.any Option.isSome
  match nonEmpty with
  | []     => []
  | v :: _ =>
      let slots := List.range v.length
      let keep  := slots.filter fun s =>
        nonEmpty.all fun w => (w[s]?).bind id |>.isSome
      nonEmpty.map fun w => keep.filterMap fun s => (w[s]?).bind id

private def parsePitches (j : Json) : Except String (List (Option Pitch.Pitch)) := do
  let arr ← j.getArr?
  arr.toList.mapM fun n =>
    match n with
    | Json.null => pure none
    | _         => do
        let i ← n.getInt?
        pure (some (i : Pitch.Pitch))

-- Expected shape: {"id": "...", "species": N, "weights": {...},
--                  "parts": [ [ [pitch|null, ...], ... ], ... ]}
private structure CheckRequest where
  id      : String
  species : Nat
  weights : Json
  parts   : List (List (List (Option Pitch.Pitch)))

private def parseCheckJson (s : String) : Except String CheckRequest := do
  let j ← Json.parse s
  let id ← j.getObjValAs? String "id"
  let species : Nat :=
    (j.getObjValAs? Nat "species").toOption.getD 1
  let weights : Json :=
    (j.getObjVal? "weights").toOption.getD Json.null
  let partsJ ← j.getObjVal? "parts"
  let partsArr ← partsJ.getArr?
  let parts ← partsArr.toList.mapM fun part => do
    let voicesArr ← part.getArr?
    voicesArr.toList.mapM parsePitches
  pure { id, species, weights, parts }

private def escapeJsonStr (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
      | '"'  => "\\\""
      | '\\' => "\\\\"
      | '\n' => "\\n"
      | '\r' => "\\r"
      | '\t' => "\\t"
      | _    => String.singleton c) ""

-- ─── Weights parsing ──────────────────────────────────────────────────────

private def fieldInt (j : Json) (key : String) (fallback : Int) : Int :=
  (j.getObjValAs? Int key).toOption.getD fallback

private def parseSecondWeights (j : Json) : SecondSpeciesSoftWeighted.Weights :=
  let d : SecondSpeciesSoftWeighted.Weights := {}
  if j.isNull then d else {
    chromatic        := fieldInt j "chromatic"        d.chromatic,
    imperfect        := fieldInt j "imperfect"        d.imperfect,
    contrary         := fieldInt j "contrary"         d.contrary,
    repeated         := fieldInt j "repeated"         d.repeated,
    startEnd         := fieldInt j "startEnd"         d.startEnd,
    strongConsonant  := fieldInt j "strongConsonant"  d.strongConsonant,
    weakConsonant    := fieldInt j "weakConsonant"    d.weakConsonant,
    passingTone      := fieldInt j "passingTone"      d.passingTone,
    parallelPerfect  := fieldInt j "parallelPerfect"  d.parallelPerfect,
    directPerfect    := fieldInt j "directPerfect"    d.directPerfect,
    midUnison        := fieldInt j "midUnison"        d.midUnison
  }

private def parseThirdWeights (j : Json) : ThirdSpeciesSoftWeighted.Weights :=
  let d : ThirdSpeciesSoftWeighted.Weights := {}
  if j.isNull then d else {
    downbeatConsonant  := fieldInt j "downbeatConsonant"  d.downbeatConsonant,
    passingTone        := fieldInt j "passingTone"        d.passingTone,
    cambiata           := fieldInt j "cambiata"           d.cambiata,
    contraryMotion     := fieldInt j "contraryMotion"     d.contraryMotion,
    repeatedNote       := fieldInt j "repeatedNote"       d.repeatedNote,
    startEnd           := fieldInt j "startEnd"           d.startEnd,
    downbeatDissonant  := fieldInt j "downbeatDissonant"  d.downbeatDissonant,
    offbeatDissonant   := fieldInt j "offbeatDissonant"   d.offbeatDissonant,
    parallelPerfect    := fieldInt j "parallelPerfect"    d.parallelPerfect,
    directPerfect      := fieldInt j "directPerfect"      d.directPerfect
  }

private def parseFourthWeights (j : Json) : FourthSpeciesSoftWeighted.Weights :=
  let d : FourthSpeciesSoftWeighted.Weights := {}
  if j.isNull then d else {
    validSuspension       := fieldInt j "validSuspension"       d.validSuspension,
    syncopation           := fieldInt j "syncopation"           d.syncopation,
    consonance            := fieldInt j "consonance"            d.consonance,
    startEnd              := fieldInt j "startEnd"              d.startEnd,
    penultimateSuspension := fieldInt j "penultimateSuspension" d.penultimateSuspension,
    invalid               := fieldInt j "invalid"               d.invalid
  }

private def parseFifthWeights (j : Json) : FifthSpeciesSoftWeighted.Weights :=
  let d : FifthSpeciesSoftWeighted.Weights := {}
  if j.isNull then d else
    let secondJ := (j.getObjVal? "second").toOption.getD Json.null
    let thirdJ  := (j.getObjVal? "third").toOption.getD Json.null
    let fourthJ := (j.getObjVal? "fourth").toOption.getD Json.null
    { second := parseSecondWeights secondJ,
      third  := parseThirdWeights  thirdJ,
      fourth := parseFourthWeights fourthJ }

-- ─── Per-species check dispatch ───────────────────────────────────────────
-- For species ≥ 2, identify CF (shorter voice) and CP (longer voice) within a
-- part. Violations are remapped from the checker's (0=CF, 1=CP) convention
-- back to the score's actual voice indices.

-- Extract concrete pitches (drop None / rests) from a voice slot list.
private def stripRests (v : List (Option Pitch.Pitch)) : List Pitch.Pitch :=
  v.filterMap id

-- Returns (cfIdx, cpIdx) within a part's voice list. Picks the *shortest*
-- non-empty voice as CF, the *longest* non-empty voice as CP. Falls back to
-- (0, 1) when ambiguous.
private def findCfCp (voices : List (List Pitch.Pitch))
    : Option (Nat × Nat) :=
  let withIdx := voices.zipIdx.filter (·.fst.length > 0)
  match withIdx with
  | [] => none
  | [_] => none
  | _ =>
      let cf := withIdx.foldl
        (fun best cur => if cur.fst.length < best.fst.length then cur else best)
        withIdx.head!
      let cp := withIdx.foldl
        (fun best cur => if cur.fst.length > best.fst.length then cur else best)
        withIdx.head!
      if cf.snd = cp.snd then
        if withIdx.length ≥ 2 then some (withIdx[0]!.snd, withIdx[1]!.snd) else none
      else
        some (cf.snd, cp.snd)

private def remap (cfIdx cpIdx : Nat) (v : Counterpoint.Violation)
    : Counterpoint.Violation :=
  let aIs0 := v.voiceA = 0
  let bIs0 := v.voiceB = 0
  let newA := if aIs0 then cfIdx else cpIdx
  let newB := if bIs0 then cfIdx else cpIdx
  -- preserve voiceA < voiceB
  if newA ≤ newB then { v with voiceA := newA, voiceB := newB }
  else                 { v with voiceA := newB, voiceB := newA }

-- Result of running a check across all parts.
private structure CheckOutcome where
  pairs : List (Nat × Counterpoint.Violation)
  score : Int

private def runCheckSpecies (req : CheckRequest) : CheckOutcome := Id.run do
  let mut pairs : List (Nat × Counterpoint.Violation) := []
  let mut score : Int := 0
  match req.species with
  | 1 =>
      -- First species: aligned multi-voice check, no soft score.
      for (voices, pi) in req.parts.zipIdx do
        let aligned := alignVoices voices
        for v in Counterpoint.checkFirstSpecies aligned do
          pairs := pairs ++ [(pi, v)]
  | n =>
      let secondW := parseSecondWeights req.weights
      let thirdW  := parseThirdWeights  req.weights
      let fourthW := parseFourthWeights req.weights
      let fifthW  := parseFifthWeights  req.weights
      for (voices, pi) in req.parts.zipIdx do
        let stripped := voices.map stripRests
        let some (cfIdx, cpIdx) := findCfCp stripped | continue
        let cf := stripped[cfIdx]!
        let cp := stripped[cpIdx]!
        let cpSlots := voices[cpIdx]!
        let cpAbove : Bool := match cf.head?, cp.head? with
                      | some cfP, some cpP => decide (cpP > cfP)
                      | _, _ => true
        let (vs, sc) := match n with
          | 2 => (SecondSpecies.checkSecondSpeciesSlots cf cpSlots,
                  SecondSpeciesSoftWeighted.scoreSecondSpeciesSlots (w := secondW) cf cpSlots)
          | 3 => (ThirdSpecies.checkThirdSpecies cf cp,
                  ThirdSpeciesSoftWeighted.scoreThirdSpecies (w := thirdW) cf cp)
          | 4 => (FourthSpecies.checkFourthSpecies cpAbove cf cp,
                  FourthSpeciesSoftWeighted.scoreFourthSpecies (w := fourthW) cf cp)
          | 5 =>
              let labels := FifthSpeciesSoftWeighted.inferUniformLabels cf cp
              (FifthSpecies.checkFifthSpecies cpAbove cf cp labels,
               FifthSpeciesSoftWeighted.scoreFifthSpeciesWithLabels fifthW cf cp labels)
          | _ => ([], 0)
        score := score + sc
        for v in vs do
          pairs := pairs ++ [(pi, remap cfIdx cpIdx v)]
  return { pairs, score }

-- ─── Solve mode ───────────────────────────────────────────────────────────
-- Solver remains first-species only; species 2–5 solving is a future-work item.

private def midiToAgda (p : Pitch.Pitch) : Pitch.Pitch := p - 12
private def agdaToMidi (p : Pitch.Pitch) : Pitch.Pitch := p + 12

private def parseSolveVoice (j : Json) : Except String (List (Pitch.Pitch × Bool)) := do
  let arr ← j.getArr?
  arr.toList.mapM fun n => do
    let p ← n.getObjValAs? Int "pitch"
    let f ← n.getObjValAs? Bool "free"
    pure ((p : Pitch.Pitch), f)

private def parseSolveJson (s : String)
    : Except String (String × List (List (List (Pitch.Pitch × Bool)))) := do
  let j ← Json.parse s
  let id ← j.getObjValAs? String "id"
  let partsJ ← j.getObjVal? "parts"
  let partsArr ← partsJ.getArr?
  let parts ← partsArr.toList.mapM fun part => do
    let voicesArr ← part.getArr?
    voicesArr.toList.mapM parseSolveVoice
  pure (id, parts)

private inductive SolveOutcome
  | solved (pitches : List Pitch.Pitch)
  | failed (msg : String)

private structure SolveResultEntry where
  partIdx  : Nat
  voiceIdx : Nat
  outcome  : SolveOutcome

private structure SolveVoiceInfo where
  partIdx  : Nat
  voiceIdx : Nat
  notes    : List (Pitch.Pitch × Bool)

private def runSolve (parts : List (List (List (Pitch.Pitch × Bool))))
    : List SolveResultEntry :=
  let allVoices : List SolveVoiceInfo :=
    parts.zipIdx.flatMap fun (voices, pi) =>
      voices.zipIdx.map fun (notes, vi) => ⟨pi, vi, notes⟩
  let cpVoices := allVoices.filter (·.notes.any (·.snd))
  let cfCandidates := allVoices.filter (fun v => !(v.notes.any (·.snd)))
  match cfCandidates with
  | [] =>
      cpVoices.map fun v =>
        ⟨v.partIdx, v.voiceIdx,
          .failed "no CF voice (need at least one voice with no marked beats)"⟩
  | cfInfo :: _ =>
      let cf : List Pitch.Pitch := cfInfo.notes.map (fun (p, _) => midiToAgda p)
      cpVoices.map fun v =>
        if v.notes.length != cf.length then
          ⟨v.partIdx, v.voiceIdx,
            .failed s!"CP/CF length mismatch ({v.notes.length} vs {cf.length})"⟩
        else
          let cpTemplate : List Solver.MPitch := v.notes.map fun (p, isFree) =>
            if isFree then Solver.MPitch.free else Solver.MPitch.known (midiToAgda p)
          match Solver.solvePitches Solver.cMajor cf cpTemplate with
          | none     => ⟨v.partIdx, v.voiceIdx, .failed "no solution"⟩
          | some sol => ⟨v.partIdx, v.voiceIdx, .solved (sol.map agdaToMidi)⟩

private def solveResultsToJson (rs : List SolveResultEntry) : String :=
  let entryJson (r : SolveResultEntry) : String :=
    let body := match r.outcome with
      | .solved ps =>
          ",\"pitches\":[" ++ String.intercalate "," (ps.map toString) ++ "]"
      | .failed msg =>
          ",\"error\":\"" ++ escapeJsonStr msg ++ "\""
    "{\"part\":" ++ toString r.partIdx ++
    ",\"voice\":" ++ toString r.voiceIdx ++ body ++ "}"
  "[" ++ String.intercalate "," (rs.map entryJson) ++ "]"

private def detectMode (s : String) : String :=
  match Json.parse s with
  | .ok j => match j.getObjValAs? String "mode" with
    | .ok m    => m
    | .error _ => "check"
  | .error _ => "check"

def main (args : List String) : IO Unit := do
  let pathStr : String ← match args with
    | [p] => pure p
    | _   => throw (IO.userError "usage: musescore-check <file.musicxml|file.json>")
  let path : System.FilePath := ⟨pathStr⟩
  let ok ← path.pathExists
  unless ok do throw (IO.userError s!"file not found: {pathStr}")
  if pathStr.endsWith ".json" then
    let txt ← IO.FS.readFile path
    match detectMode txt with
    | "solve" =>
        match parseSolveJson txt with
        | .error e => throw (IO.userError s!"solve json parse error: {e}")
        | .ok (id, parts) =>
            let results := runSolve parts
            let body := solveResultsToJson results
            IO.println ("{\"id\":\"" ++ escapeJsonStr id ++
                        "\",\"mode\":\"solve\",\"results\":" ++ body ++ "}")
    | _ =>
        match parseCheckJson txt with
        | .error e => throw (IO.userError s!"json parse error: {e}")
        | .ok req =>
            let outcome := runCheckSpecies req
            let body := ViolationJson.violationsToJson outcome.pairs
            IO.println ("{\"id\":\"" ++ escapeJsonStr req.id ++
                        "\",\"mode\":\"check\"" ++
                        ",\"species\":" ++ toString req.species ++
                        ",\"score\":" ++ toString outcome.score ++
                        ",\"violations\":" ++ body ++ "}")
  else
    match ← fileStream path with
    | none   => throw (IO.userError s!"file not found: {path}")
    | some s =>
        let txt ← readStream s
        match Xml.parse txt with
        | .error e => throw (IO.userError s!"parse error: {e}")
        | .ok root =>
            let parts := Xml.children "part" root
            let voices := parts.map Xml.extractVoicesByTag
            let pairs := voices.zipIdx.flatMap fun (vs, pi) =>
              let aligned := alignVoices vs
              (Counterpoint.checkFirstSpecies aligned).map (pi, ·)
            IO.println (ViolationJson.violationsToJson pairs)
