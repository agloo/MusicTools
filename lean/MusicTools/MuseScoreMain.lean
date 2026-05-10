import Xml
import Counterpoint
import Solver
import ViolationJson
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

private def runViolations
    (parts : List (List (List (Option Pitch.Pitch))))
    : List (Nat × Counterpoint.Violation) :=
  parts.zipIdx.flatMap fun (voices, pi) =>
    let aligned := alignVoices voices
    (Counterpoint.checkFirstSpecies aligned).map (pi, ·)

private def parsePitches (j : Json) : Except String (List (Option Pitch.Pitch)) := do
  let arr ← j.getArr?
  arr.toList.mapM fun n =>
    match n with
    | Json.null => pure none
    | _         => do
        let i ← n.getInt?
        pure (some (i : Pitch.Pitch))

-- Expected shape: {"id": "...", "parts": [ [ [pitch|null, ...], ... ], ... ]}
private def parseScoreJson (s : String)
    : Except String (String × List (List (List (Option Pitch.Pitch)))) := do
  let j ← Json.parse s
  let id ← j.getObjValAs? String "id"
  let partsJ ← j.getObjVal? "parts"
  let partsArr ← partsJ.getArr?
  let parts ← partsArr.toList.mapM fun part => do
    let voicesArr ← part.getArr?
    voicesArr.toList.mapM parsePitches
  pure (id, parts)

private def escapeJsonStr (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
      | '"'  => "\\\""
      | '\\' => "\\\\"
      | '\n' => "\\n"
      | '\r' => "\\r"
      | '\t' => "\\t"
      | _    => String.singleton c) ""

-- ─── Solve mode ───────────────────────────────────────────────────────────
-- MuseScore uses MIDI pitches (C4 = 60). The Solver uses the Agda
-- convention (C4 = 48). Convert at the boundary.

private def midiToAgda (p : Pitch.Pitch) : Pitch.Pitch := p - 12
private def agdaToMidi (p : Pitch.Pitch) : Pitch.Pitch := p + 12

private def parseSolveVoice (j : Json) : Except String (List (Pitch.Pitch × Bool)) := do
  let arr ← j.getArr?
  arr.toList.mapM fun n => do
    let p ← n.getObjValAs? Int "pitch"
    let f ← n.getObjValAs? Bool "free"
    pure ((p : Pitch.Pitch), f)

-- Expected shape: {"id": "...", "mode": "solve",
--                  "parts": [ [ [{pitch, free}, ...], ... ], ... ]}
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
  | solved (pitches : List Pitch.Pitch)  -- MIDI (C4 = 60)
  | failed (msg : String)

private structure SolveResultEntry where
  partIdx  : Nat
  voiceIdx : Nat
  outcome  : SolveOutcome

private structure SolveVoiceInfo where
  partIdx  : Nat
  voiceIdx : Nat
  notes    : List (Pitch.Pitch × Bool)

-- Pick CP voices (any free=true) and one CF voice (no free notes, any part).
-- Solve each CP independently against the shared CF.
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

-- Returns "solve" or "check" (default).
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
        match parseScoreJson txt with
        | .error e => throw (IO.userError s!"json parse error: {e}")
        | .ok (id, parts) =>
            let pairs := runViolations parts
            let body := ViolationJson.violationsToJson pairs
            IO.println ("{\"id\":\"" ++ escapeJsonStr id ++
                        "\",\"mode\":\"check\",\"violations\":" ++ body ++ "}")
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
            let pairs := runViolations voices
            IO.println (ViolationJson.violationsToJson pairs)
