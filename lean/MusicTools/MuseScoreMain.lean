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
open Pitch
open Interval

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

-- Soft-score explanation events returned to the MuseScore dialog.
private structure ScorePoint where
  part   : Nat
  step   : Nat
  points : Int
  rule   : String
  detail : String

private def point (part step : Nat) (points : Int) (rule detail : String) :
    ScorePoint :=
  { part, step, points, rule, detail }

private def pointSum (ps : List ScorePoint) : Int :=
  ps.foldl (fun acc p => acc + p.points) 0

private def pointToJson (p : ScorePoint) : String :=
  "{\"part\":" ++ toString p.part ++
  ",\"step\":" ++ toString p.step ++
  ",\"points\":" ++ toString p.points ++
  ",\"rule\":\"" ++ escapeJsonStr p.rule ++
  "\",\"detail\":\"" ++ escapeJsonStr p.detail ++ "\"}"

private def pointsToJson (ps : List ScorePoint) : String :=
  "[" ++ String.intercalate "," (ps.map pointToJson) ++ "]"

private def isImperfect (iv : Upi) : Bool :=
  let i := intervalWithinOctave iv
  i = min3 || i = maj3 || i = min6 || i = maj6

private def withScoreAdjustment
    (part : Nat) (expected : Int) (ps : List ScorePoint) :
    List ScorePoint :=
  let explained := pointSum ps
  if explained = expected then ps
  else ps ++ [point part 0 (expected - explained) "score-total"
                "backend total adjustment"]

private def secondSpeciesPoints
    (part : Nat) (w : SecondSpeciesSoftWeighted.Weights)
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List ScorePoint := Id.run do
  let mut ps : List ScorePoint := []
  for (slot, idx) in cp.zipIdx do
    match slot with
    | some p =>
        if !Solver.inScale Solver.cMajor p then
          ps := ps ++ [point part idx w.chromatic "chromatic"
            s!"pitch {p} is outside C major"]
    | none => pure ()
  for m in List.range cf.length do
    let strongStep := m * SecondSpecies.subdivisions
    match SecondSpecies.vSlotAt cf cp m 0 with
    | some iv =>
        if isConsonant iv then
          ps := ps ++ [point part strongStep w.strongConsonant
            "strongConsonant" s!"strong-beat interval {iv} is consonant"]
        else
          ps := ps ++ [point part strongStep (-w.strongConsonant)
            "strongConsonant" s!"strong-beat interval {iv} is dissonant"]
    | none => pure ()
    let weakStep := strongStep + 1
    match SecondSpecies.vSlotAt cf cp m 1 with
    | some iv =>
        if isConsonant iv then
          ps := ps ++ [point part weakStep w.weakConsonant
            "weakConsonant" s!"weak-beat interval {iv} is consonant"]
        else
          let isPassing :=
            match SecondSpecies.cpSlotAt cp m 0,
                  SecondSpecies.cpSlotAt cp m 1,
                  SecondSpecies.cpSlotAt cp (m + 1) 0 with
            | some prev, some weak, some next => isPassingTone prev weak next
            | _, _, _ => false
          if isPassing then
            ps := ps ++ [point part weakStep w.passingTone
              "passingTone" "weak-beat dissonance is a passing tone"]
          else
            ps := ps ++ [point part weakStep (-w.weakConsonant)
              "weakConsonant" s!"weak-beat interval {iv} is dissonant"]
    | none => pure ()
    match SecondSpecies.vSlotAt cf cp m 0 with
    | some iv =>
        if isImperfect iv then
          ps := ps ++ [point part strongStep w.imperfect
            "imperfect" s!"strong-beat interval {iv} is imperfect"]
    | none => pure ()
  for m in List.range cf.length do
    if m = 0 then continue
    let step := m * SecondSpecies.subdivisions
    match SecondSpecies.vSlotAt cf cp (m - 1) 0,
          SecondSpecies.vSlotAt cf cp m 0 with
    | some iv1, some iv2 =>
        if isPerfectConsonance iv1 &&
            intervalWithinOctave iv1 = intervalWithinOctave iv2 then
          ps := ps ++ [point part step w.parallelPerfect
            "parallelPerfect" s!"parallel perfect strong beats {iv1}->{iv2}"]
        else
          ps := ps ++ [point part step 10
            "parallelPerfect" "no parallel perfect on strong beats"]
    | _, _ => pure ()
    match cf[m - 1]?, cf[m]?,
          SecondSpecies.cpSlotAt cp (m - 1) 0,
          SecondSpecies.cpSlotAt cp m 0 with
    | some c1, some c2, some p1, some p2 =>
        let mo := motion c1 c2 p1 p2
        let target := upi c2 p2
        if isDirect mo && isPerfectConsonance target then
          ps := ps ++ [point part step w.directPerfect
            "directPerfect" s!"direct motion into perfect interval {target}"]
        else
          ps := ps ++ [point part step 10
            "directPerfect" "no direct motion into a perfect interval"]
        if mo = Motion.contrary then
          ps := ps ++ [point part step w.contrary
            "contrary" "contrary motion between strong beats"]
    | _, _, _, _ => pure ()
  if cf.length > 0 then
    match SecondSpecies.vSlotAt cf cp 0 0 with
    | some iv =>
        let pts := if SecondSpeciesSoftWeighted.isUnisonOrOctave iv
          then w.startEnd else -w.startEnd
        ps := ps ++ [point part 0 pts "startEnd"
          s!"opening interval {iv}"]
    | none =>
        match SecondSpecies.vSlotAt cf cp 0 1 with
        | some iv =>
            let pts := if SecondSpeciesSoftWeighted.isUnisonOrOctave iv
              then w.startEnd else -w.startEnd
            ps := ps ++ [point part 1 pts "startEnd"
              s!"opening interval {iv}"]
        | none => pure ()
    let last := cf.length - 1
    match SecondSpecies.vSlotAt cf cp last 0 with
    | some iv =>
        let pts := if SecondSpeciesSoftWeighted.isUnisonOrOctave iv
          then w.startEnd else -w.startEnd
        ps := ps ++ [point part (last * SecondSpecies.subdivisions) pts
          "startEnd" s!"closing interval {iv}"]
    | none => pure ()
    for m in List.range cf.length do
      if m = 0 || m = last then continue
      match SecondSpecies.vSlotAt cf cp m 0 with
      | some iv =>
          if iv = per1 then
            ps := ps ++ [point part (m * SecondSpecies.subdivisions)
              w.midUnison "midUnison" "mid-piece unison on a strong beat"]
          else
            ps := ps ++ [point part (m * SecondSpecies.subdivisions)
              10 "midUnison" "no mid-piece unison on this strong beat"]
      | none => pure ()
  for m in List.range cf.length do
    if m = 0 then continue
    match SecondSpecies.cpSlotAt cp (m - 1) 1,
          SecondSpecies.cpSlotAt cp m 0 with
    | some p, some q =>
        if p = q then
          ps := ps ++ [point part (m * SecondSpecies.subdivisions)
            w.repeated "repeated" "repeated note across the barline"]
    | _, _ => pure ()
  return ps

private def thirdSpeciesPoints
    (part : Nat) (w : ThirdSpeciesSoftWeighted.Weights)
    (cf cp : List Pitch.Pitch) : List ScorePoint := Id.run do
  let mut ps : List ScorePoint := []
  for m in List.range cf.length do
    let step := m * ThirdSpecies.subdivisions
    match ThirdSpecies.vAt cf cp m 0 with
    | some iv =>
        if isConsonant iv then
          ps := ps ++ [point part step w.downbeatConsonant
            "downbeatConsonant" s!"downbeat interval {iv} is consonant"]
        else
          ps := ps ++ [point part step w.downbeatDissonant
            "downbeatDissonant" s!"downbeat interval {iv} is dissonant"]
    | none => pure ()
    for b in [1, 2, 3] do
      let beatStep := step + b
      if ThirdSpeciesSoftWeighted.isPassingDissonanceAt cf cp m b then
        ps := ps ++ [point part beatStep w.passingTone
          "passingTone" "offbeat dissonance is a passing tone"]
    if ThirdSpeciesSoftWeighted.isCambiataAtSecondBeat cf cp m then
      ps := ps ++ [point part (step + 1) w.cambiata
        "cambiata" "second-quarter dissonance forms a cambiata"]
    let cambiata := ThirdSpeciesSoftWeighted.isCambiataAtSecondBeat cf cp m
    for b in [1, 2, 3] do
      match ThirdSpeciesSoftWeighted.vAt cf cp m b with
      | some iv =>
          if isDissonant iv then
            let ok := ThirdSpeciesSoftWeighted.isPassingDissonanceAt cf cp m b ||
              (b = 1 && cambiata)
            if !ok then
              ps := ps ++ [point part (step + b) w.offbeatDissonant
                "offbeatDissonant" s!"offbeat interval {iv} is unresolved"]
      | none => pure ()
  for m in List.range cf.length do
    if m = 0 then continue
    let step := m * ThirdSpecies.subdivisions
    match cf[m - 1]?, cf[m]?,
          cp[(m - 1) * ThirdSpecies.subdivisions]?,
          cp[m * ThirdSpecies.subdivisions]? with
    | some c1, some c2, some p1, some p2 =>
        let iv2 := upi c2 p2
        let mo := motion c1 c2 p1 p2
        if isPerfectConsonance iv2 && mo = Motion.contrary then
          ps := ps ++ [point part step w.contraryMotion
            "contraryMotion" "perfect downbeat approached by contrary motion"]
        else if isPerfectConsonance iv2 && isDirect mo then
          ps := ps ++ [point part step w.directPerfect
            "directPerfect" s!"direct motion into perfect interval {iv2}"]
    | _, _, _, _ => pure ()
    match ThirdSpeciesSoftWeighted.vAt cf cp (m - 1) 0,
          ThirdSpeciesSoftWeighted.vAt cf cp m 0 with
    | some iv1, some iv2 =>
        if isPerfectConsonance iv1 &&
            intervalWithinOctave iv1 = intervalWithinOctave iv2 then
          ps := ps ++ [point part step w.parallelPerfect
            "parallelPerfect" s!"parallel perfect downbeats {iv1}->{iv2}"]
    | _, _ => pure ()
  for m in List.range (cp.length / ThirdSpeciesSoftWeighted.subdivisions + 1) do
    if m = 0 then continue
    match ThirdSpeciesSoftWeighted.cpAt cp (m - 1) 3,
          ThirdSpeciesSoftWeighted.cpAt cp m 0 with
    | some p, some q =>
        if p = q then
          ps := ps ++ [point part (m * ThirdSpecies.subdivisions)
            w.repeatedNote "repeatedNote" "repeated note across the barline"]
    | _, _ => pure ()
  if cf.length > 0 then
    match ThirdSpeciesSoftWeighted.vAt cf cp 0 0 with
    | some iv =>
        let pts := if ThirdSpeciesSoftWeighted.isUnisonOrOctave iv
          then w.startEnd else -w.startEnd
        ps := ps ++ [point part 0 pts "startEnd" s!"opening interval {iv}"]
    | none => pure ()
    let last := cf.length - 1
    match ThirdSpeciesSoftWeighted.vAt cf cp last 0 with
    | some iv =>
        let pts := if ThirdSpeciesSoftWeighted.isUnisonOrOctave iv
          then w.startEnd else -w.startEnd
        ps := ps ++ [point part (last * ThirdSpecies.subdivisions) pts
          "startEnd" s!"closing interval {iv}"]
    | none => pure ()
  return ps

private def fourthSpeciesPoints
    (part : Nat) (w : FourthSpeciesSoftWeighted.Weights)
    (cf cp : List Pitch.Pitch) : List ScorePoint := Id.run do
  let mut ps : List ScorePoint := []
  for m in List.range cf.length do
    let step := m * FourthSpecies.subdivisions + 1
    match FourthSpecies.vAt cf cp m 1 with
    | some iv =>
        if isConsonant iv then
          ps := ps ++ [point part step w.consonance
            "consonance" s!"third-beat interval {iv} is consonant"]
        else
          ps := ps ++ [point part step w.invalid
            "invalid" s!"third-beat interval {iv} is dissonant"]
    | none => pure ()
  let measures := cf.length
  for m in List.range measures do
    if m + 1 >= measures then continue
    let step := (m + 1) * FourthSpecies.subdivisions
    match FourthSpecies.cpAt cp m 1, FourthSpecies.cpAt cp (m + 1) 0 with
    | some p1, some p2 =>
        if m + 2 = measures then
          if p1 = p2 then
            ps := ps ++ [point part step (-w.syncopation)
              "syncopation" "unexpected tie into the final measure"]
        else if p1 = p2 then
          ps := ps ++ [point part step w.syncopation
            "syncopation" "tie across the barline"]
        else
          ps := ps ++ [point part step (-w.syncopation)
            "syncopation" "missing tie across the barline"]
    | _, _ => pure ()
  for m in List.range cf.length do
    if m = 0 then continue
    match FourthSpecies.vAt cf cp m 0 with
    | some suspension =>
        if isConsonant suspension then continue
        let step := m * FourthSpecies.subdivisions
        match FourthSpecies.vAt cf cp m 1 with
        | some resolution =>
            if FourthSpecies.isTied cp m &&
               FourthSpecies.resolvesDownByStep cp m &&
               FourthSpeciesSoftWeighted.isUpperSuspensionResolution
                 suspension resolution then
              ps := ps ++ [point part step w.validSuspension
                "validSuspension" s!"suspension {suspension} resolves to {resolution}"]
            else
              ps := ps ++ [point part step w.invalid
                "invalid" s!"suspension {suspension} is not valid"]
        | none => pure ()
    | none => pure ()
  if cf.length >= 2 then
    let m := cf.length - 2
    match FourthSpecies.vAt cf cp m 0, FourthSpecies.vAt cf cp m 1 with
    | some suspension, some resolution =>
        let pts := if FourthSpeciesSoftWeighted.isSevenSix suspension resolution
          then w.penultimateSuspension else -w.penultimateSuspension
        ps := ps ++ [point part (m * FourthSpecies.subdivisions) pts
          "penultimateSuspension"
          s!"penultimate suspension {suspension}->{resolution}"]
    | _, _ => pure ()
  if cf.length > 0 then
    match cf[0]?, FourthSpecies.cpAt cp 0 1 with
    | some c, some p =>
        let iv := upi c p
        let pts := if FourthSpeciesSoftWeighted.isUnisonOrOctave iv
          then w.startEnd else -w.startEnd
        ps := ps ++ [point part 1 pts "startEnd" s!"opening interval {iv}"]
    | _, _ => pure ()
    let last := cf.length - 1
    match FourthSpecies.vAt cf cp last 0 with
    | some iv =>
        let pts := if FourthSpeciesSoftWeighted.isUnisonOrOctave iv
          then w.startEnd else -w.startEnd
        ps := ps ++ [point part (last * FourthSpecies.subdivisions) pts
          "startEnd" s!"closing interval {iv}"]
    | none => pure ()
  return ps

private def fifthMeasureLocalPoints
    (part : Nat) (w : FifthSpeciesSoftWeighted.Weights)
    (label : FifthSpecies.Species) (c : Pitch.Pitch)
    (chunk : List Pitch.Pitch) (off : Nat) : List ScorePoint := Id.run do
  let mut ps : List ScorePoint := []
  match label with
  | .first => pure ()
  | .second =>
      match SecondSpecies.vAt [c] chunk 0 0 with
      | some iv =>
          if isConsonant iv then
            ps := ps ++ [point part off w.second.strongConsonant
              "secondStrongConsonant" s!"strong interval {iv} is consonant"]
          else
            ps := ps ++ [point part off (-w.second.strongConsonant)
              "secondStrongConsonant" s!"strong interval {iv} is dissonant"]
          if isImperfect iv then
            ps := ps ++ [point part off w.second.imperfect
              "secondImperfect" s!"strong interval {iv} is imperfect"]
      | none => pure ()
      match SecondSpecies.vAt [c] chunk 0 1 with
      | some iv =>
          if isConsonant iv then
            ps := ps ++ [point part (off + 1) w.second.weakConsonant
              "secondWeakConsonant" s!"weak interval {iv} is consonant"]
          else
            match SecondSpecies.cpAt chunk 0 0,
                  SecondSpecies.cpAt chunk 0 1,
                  SecondSpecies.cpAt chunk 1 0 with
            | some prev, some weak, some next =>
                if isPassingTone prev weak next then
                  ps := ps ++ [point part (off + 1) w.second.passingTone
                    "secondPassingTone" "weak dissonance is a passing tone"]
                else
                  ps := ps ++ [point part (off + 1) (-w.second.weakConsonant)
                    "secondWeakConsonant" s!"weak interval {iv} is dissonant"]
            | _, _, _ => pure ()
      | none => pure ()
  | .third =>
      match ThirdSpecies.vAt [c] chunk 0 0 with
      | some iv =>
          if isConsonant iv then
            ps := ps ++ [point part off w.third.downbeatConsonant
              "thirdDownbeatConsonant" s!"downbeat interval {iv} is consonant"]
          else
            ps := ps ++ [point part off w.third.downbeatDissonant
              "thirdDownbeatDissonant" s!"downbeat interval {iv} is dissonant"]
      | none => pure ()
      for b in [1, 2, 3] do
        if ThirdSpeciesSoftWeighted.isPassingDissonanceAt [c] chunk 0 b then
          ps := ps ++ [point part (off + b) w.third.passingTone
            "thirdPassingTone" "offbeat dissonance is a passing tone"]
      if ThirdSpeciesSoftWeighted.isCambiataAtSecondBeat [c] chunk 0 then
        ps := ps ++ [point part (off + 1) w.third.cambiata
          "thirdCambiata" "second-quarter dissonance forms a cambiata"]
      let cambiata := ThirdSpeciesSoftWeighted.isCambiataAtSecondBeat [c] chunk 0
      for b in [1, 2, 3] do
        match ThirdSpeciesSoftWeighted.vAt [c] chunk 0 b with
        | some iv =>
            if isDissonant iv then
              let ok := ThirdSpeciesSoftWeighted.isPassingDissonanceAt
                [c] chunk 0 b || (b = 1 && cambiata)
              if !ok then
                ps := ps ++ [point part (off + b) w.third.offbeatDissonant
                  "thirdOffbeatDissonant" s!"offbeat interval {iv} is unresolved"]
        | none => pure ()
  | .fourth =>
      match FourthSpecies.vAt [c] chunk 0 1 with
      | some iv =>
          if isConsonant iv then
            ps := ps ++ [point part (off + 1) w.fourth.consonance
              "fourthConsonance" s!"third-beat interval {iv} is consonant"]
          else
            ps := ps ++ [point part (off + 1) w.fourth.invalid
              "fourthInvalid" s!"third-beat interval {iv} is dissonant"]
      | none => pure ()
  return ps

private def fifthSpeciesPoints
    (part : Nat) (w : FifthSpeciesSoftWeighted.Weights)
    (cf cp : List Pitch.Pitch) (labels : List FifthSpecies.Species) :
    List ScorePoint := Id.run do
  let chunks := FifthSpecies.chunkCp labels cp
  let mut ps : List ScorePoint := []
  for m in List.range cf.length do
    match cf[m]?, chunks[m]?, labels[m]? with
    | some c, some chunk, some label =>
        ps := ps ++ fifthMeasureLocalPoints part w label c chunk
          (FifthSpecies.measureOffset labels m)
    | _, _, _ => pure ()
  for m in List.range cf.length do
    if m = 0 then continue
    let step := FifthSpecies.measureOffset labels m
    match cf[m - 1]?, cf[m]?,
          FifthSpecies.downbeat chunks (m - 1),
          FifthSpecies.downbeat chunks m with
    | some c1, some c2, some p1, some p2 =>
        let iv2 := upi c2 p2
        let mo := motion c1 c2 p1 p2
        if isPerfectConsonance iv2 && mo = Motion.contrary then
          ps := ps ++ [point part step w.third.contraryMotion
            "thirdContraryMotion" "perfect downbeat approached by contrary motion"]
        else if isPerfectConsonance iv2 && isDirect mo then
          ps := ps ++ [point part step w.third.directPerfect
            "thirdDirectPerfect" s!"direct motion into perfect interval {iv2}"]
        let currentIsFourth := match labels[m]? with | some .fourth => true | _ => false
        if !currentIsFourth then
          match chunks[m - 1]? with
          | some prevChunk =>
              match FifthSpeciesSoftWeighted.lastPitch prevChunk with
              | some prevLast =>
                  if prevLast = p2 then
                    ps := ps ++ [point part step w.third.repeatedNote
                      "thirdRepeatedNote" "repeated note across the barline"]
              | none => pure ()
          | none => pure ()
    | _, _, _, _ => pure ()
  for m in List.range cf.length do
    match labels[m]? with
    | some .fourth =>
        if m = 0 then continue
        let step := FifthSpecies.measureOffset labels m
        match cf[m]?, chunks[m]?, chunks[m - 1]? with
        | some c, some chunk, some prevChunk =>
            match chunk[0]?, chunk[1]?,
                  FifthSpeciesSoftWeighted.lastPitch prevChunk with
            | some strong, some weak, some prevLast =>
                if prevLast = strong then
                  ps := ps ++ [point part step w.fourth.syncopation
                    "fourthSyncopation" "tie across the barline"]
                else
                  ps := ps ++ [point part step (-w.fourth.syncopation)
                    "fourthSyncopation" "missing tie across the barline"]
                let suspension := upi c strong
                let resolution := upi c weak
                if isDissonant suspension then
                  let s := FourthSpeciesSoftWeighted.scoreValidSuspension
                    w.fourth suspension resolution
                  if s = 0 then
                    ps := ps ++ [point part step w.fourth.invalid
                      "fourthInvalid" s!"suspension {suspension} is not valid"]
                  else
                    ps := ps ++ [point part step s
                      "fourthValidSuspension"
                      s!"suspension {suspension} resolves to {resolution}"]
            | _, _, _ => pure ()
        | _, _, _ => pure ()
    | _ => pure ()
  return ps

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
  pairs  : List (Nat × Counterpoint.Violation)
  score  : Int
  points : List ScorePoint

private def runCheckSpecies (req : CheckRequest) : CheckOutcome := Id.run do
  let mut pairs  : List (Nat × Counterpoint.Violation) := []
  let mut score  : Int := 0
  let mut points : List ScorePoint := []
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
        let (vs, sc, pts) := match n with
          | 2 =>
              let sc := SecondSpeciesSoftWeighted.scoreSecondSpeciesSlots
                (w := secondW) cf cpSlots
              let pts := withScoreAdjustment pi sc
                (secondSpeciesPoints pi secondW cf cpSlots)
              (SecondSpecies.checkSecondSpeciesSlots cf cpSlots, sc, pts)
          | 3 =>
              let sc := ThirdSpeciesSoftWeighted.scoreThirdSpecies
                (w := thirdW) cf cp
              let pts := withScoreAdjustment pi sc
                (thirdSpeciesPoints pi thirdW cf cp)
              (ThirdSpecies.checkThirdSpecies cf cp, sc, pts)
          | 4 =>
              let sc := FourthSpeciesSoftWeighted.scoreFourthSpecies
                (w := fourthW) cf cp
              let pts := withScoreAdjustment pi sc
                (fourthSpeciesPoints pi fourthW cf cp)
              (FourthSpecies.checkFourthSpecies cpAbove cf cp, sc, pts)
          | 5 =>
              let labels := FifthSpeciesSoftWeighted.inferUniformLabels cf cp
              let sc := FifthSpeciesSoftWeighted.scoreFifthSpeciesWithLabels
                fifthW cf cp labels
              let pts := withScoreAdjustment pi sc
                (fifthSpeciesPoints pi fifthW cf cp labels)
              (FifthSpecies.checkFifthSpecies cpAbove cf cp labels, sc, pts)
          | _ => ([], 0, [])
        score := score + sc
        points := points ++ pts
        for v in vs do
          pairs := pairs ++ [(pi, remap cfIdx cpIdx v)]
  return { pairs, score, points }

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
                        ",\"points\":" ++ pointsToJson outcome.points ++
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
