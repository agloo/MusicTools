import Pitch
import Interval
import SecondSpecies
import Solver
open Pitch
open Interval
open SecondSpecies
open Solver

namespace SecondSpeciesSoftWeighted

-- Soft weight values inspired by agda/Weight.agda. Defaults preserved as
-- struct field defaults so `(SecondSpeciesSoftWeighted.Weights).{}` reproduces
-- the original behavior. Per-call overrides come from the plugin UI.

structure Weights where
  chromatic        : Int := -39
  imperfect        : Int := 40
  contrary         : Int := 50
  repeated         : Int := -29
  startEnd         : Int := 80
  strongConsonant  : Int := 40
  weakConsonant    : Int := 20
  passingTone      : Int := 10
  parallelPerfect  : Int := -60
  directPerfect    : Int := -40
  midUnison        : Int := -70
  deriving Repr

def defaultWeights : Weights := {}

-- Legacy constants — kept so existing callers and downstream species keep
-- compiling. They read the default-record fields.
def chromaticWeight       : Int := defaultWeights.chromatic
def imperfectWeight       : Int := defaultWeights.imperfect
def contraryWeight        : Int := defaultWeights.contrary
def repeatedWeight        : Int := defaultWeights.repeated
def startEndWeight        : Int := defaultWeights.startEnd
def strongConsonantWeight : Int := defaultWeights.strongConsonant
def weakConsonantWeight   : Int := defaultWeights.weakConsonant
def passingToneWeight     : Int := defaultWeights.passingTone
def parallelPerfectWeight : Int := defaultWeights.parallelPerfect
def directPerfectWeight   : Int := defaultWeights.directPerfect
def midUnisonWeight       : Int := defaultWeights.midUnison

def isUnisonOrOctave (iv : Upi) : Bool :=
  intervalWithinOctave iv = per1

-- Scale membership penalty/reward

def scoreScaleMembership (w : Weights) (s : Scale) (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for p in cp do
    if inScale s p then
      score := score + 0
    else
      score := score + w.chromatic
  return score

def scoreScaleMembershipSlots
    (w : Weights) (s : Scale) (cp : List (Option Pitch.Pitch)) : Int :=
  scoreScaleMembership w s (cp.filterMap id)

-- Strong beats consonant scoring

def scoreStrongConsonant (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vAt cf cp m 0 | continue
    if isConsonant iv then
      score := score + w.strongConsonant
    else
      score := score - w.strongConsonant
  return score

-- Weak-beat scoring, with passing-tone allowance

def scoreWeakBeats (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vAt cf cp m 1 | continue
    if isConsonant iv then
      score := score + w.weakConsonant
    else
      let some prev := cpAt cp m 0     | continue
      let some weak := cpAt cp m 1     | continue
      let some next := cpAt cp (m+1) 0 | continue
      if isPassingTone prev weak next then
        score := score + w.passingTone
      else
        score := score - w.weakConsonant
  return score

-- Parallel perfect strong-beat penalty

def scoreParallelPerfectStrong (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vAt cf cp (m-1) 0 | continue
    let some iv2 := vAt cf cp m 0     | continue
    if isPerfectConsonance iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      score := score + w.parallelPerfect
    else
      score := score + 10
  return score

-- Direct motion into perfect strong-beat penalty

def scoreDirectIntoPerfectStrong (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m-1]?       | continue
    let some c2 := cf[m]?          | continue
    let some p1 := cpAt cp (m-1) 0 | continue
    let some p2 := cpAt cp m 0     | continue
    let mo := motion c1 c2 p1 p2
    let target := upi c2 p2
    if isDirect mo && isPerfectConsonance target then
      score := score + w.directPerfect
    else
      score := score + 10
  return score

-- Puget Sound second species begins/ends on octave or unison, not fifth.

def scoreStartEnd (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  if cf.length = 0 then return score
  match vAt cf cp 0 0 with
  | some iv =>
      if isUnisonOrOctave iv then
        score := score + w.startEnd
      else
        score := score - w.startEnd
  | none => pure ()
  let last := cf.length - 1
  match vAt cf cp last 0 with
  | some iv =>
      if isUnisonOrOctave iv then
        score := score + w.startEnd
      else
        score := score - w.startEnd
  | none => pure ()
  return score

-- No mid unison scoring

def scoreNoMidUnison (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  if cf.length = 0 then return score
  let last := cf.length - 1
  for m in List.range cf.length do
    if m = 0 || m = last then continue
    let some iv := vAt cf cp m 0 | continue
    if iv = per1 then
      score := score + w.midUnison
    else
      score := score + 10
  return score

-- Imperfect consonance on strong beats

def isImperfectConsonance (iv : Upi) : Bool :=
  let i := intervalWithinOctave iv
  i = min3 || i = maj3 || i = min6 || i = maj6


def scoreImperfectStrongBeats (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vAt cf cp m 0 | continue
    if isImperfectConsonance iv then
      score := score + w.imperfect
  return score

-- Contrary motion between strong-beat pairs

def scoreContraryMotionStrong (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m-1]?       | continue
    let some c2 := cf[m]?          | continue
    let some p1 := cpAt cp (m-1) 0 | continue
    let some p2 := cpAt cp m 0     | continue
    if motion c1 c2 p1 p2 = Motion.contrary then
      score := score + w.contrary
  return score

-- Penalty for repeated notes across barlines.

def scoreRepeatedNotes (w : Weights) (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range (cp.length / subdivisions + 1) do
    if m = 0 then continue
    let some p := cpAt cp (m - 1) 1 | continue
    let some q := cpAt cp m 0 | continue
    if p = q then
      score := score + w.repeated
  return score

-- Slot-aware scoring for MuseScore input. These mirror the hard slot-aware
-- checker in `SecondSpecies` so an opening rest does not shift weak beats.

def scoreStrongConsonantSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vSlotAt cf cp m 0 | continue
    if isConsonant iv then
      score := score + w.strongConsonant
    else
      score := score - w.strongConsonant
  return score

def scoreWeakBeatsSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vSlotAt cf cp m 1 | continue
    if isConsonant iv then
      score := score + w.weakConsonant
    else
      let isPassing :=
        match cpSlotAt cp m 0, cpSlotAt cp m 1, cpSlotAt cp (m + 1) 0 with
        | some prev, some weak, some next => isPassingTone prev weak next
        | _, _, _ => false
      if isPassing then
        score := score + w.passingTone
      else
        score := score - w.weakConsonant
  return score

def scoreParallelPerfectStrongSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vSlotAt cf cp (m-1) 0 | continue
    let some iv2 := vSlotAt cf cp m 0     | continue
    if isPerfectConsonance iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      score := score + w.parallelPerfect
    else
      score := score + 10
  return score

def scoreDirectIntoPerfectStrongSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m-1]?            | continue
    let some c2 := cf[m]?              | continue
    let some p1 := cpSlotAt cp (m-1) 0 | continue
    let some p2 := cpSlotAt cp m 0     | continue
    let mo := motion c1 c2 p1 p2
    let target := upi c2 p2
    if isDirect mo && isPerfectConsonance target then
      score := score + w.directPerfect
    else
      score := score + 10
  return score

def scoreStartEndSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  if cf.length = 0 then return score
  match vSlotAt cf cp 0 0 with
  | some iv =>
      if isUnisonOrOctave iv then
        score := score + w.startEnd
      else
        score := score - w.startEnd
  | none =>
      match vSlotAt cf cp 0 1 with
      | some iv =>
          if isUnisonOrOctave iv then
            score := score + w.startEnd
          else
            score := score - w.startEnd
      | none => pure ()
  let last := cf.length - 1
  match vSlotAt cf cp last 0 with
  | some iv =>
      if isUnisonOrOctave iv then
        score := score + w.startEnd
      else
        score := score - w.startEnd
  | none => pure ()
  return score

def scoreNoMidUnisonSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  if cf.length = 0 then return score
  let last := cf.length - 1
  for m in List.range cf.length do
    if m = 0 || m = last then continue
    let some iv := vSlotAt cf cp m 0 | continue
    if iv = per1 then
      score := score + w.midUnison
    else
      score := score + 10
  return score

def scoreImperfectStrongBeatsSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vSlotAt cf cp m 0 | continue
    if isImperfectConsonance iv then
      score := score + w.imperfect
  return score

def scoreContraryMotionStrongSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m-1]?            | continue
    let some c2 := cf[m]?              | continue
    let some p1 := cpSlotAt cp (m-1) 0 | continue
    let some p2 := cpSlotAt cp m 0     | continue
    if motion c1 c2 p1 p2 = Motion.contrary then
      score := score + w.contrary
  return score

def scoreRepeatedNotesSlots
    (w : Weights) (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some p := cpSlotAt cp (m - 1) 1 | continue
    let some q := cpSlotAt cp m 0       | continue
    if p = q then
      score := score + w.repeated
  return score

-- Total weighted score for second-species counterpoint

def scoreSecondSpeciesWithScale (w : Weights) (s : Scale) (cf cp : List Pitch.Pitch) : Int :=
  scoreScaleMembership w s cp +
  scoreStrongConsonant w cf cp +
  scoreWeakBeats w cf cp +
  scoreImperfectStrongBeats w cf cp +
  scoreParallelPerfectStrong w cf cp +
  scoreDirectIntoPerfectStrong w cf cp +
  scoreContraryMotionStrong w cf cp +
  scoreStartEnd w cf cp +
  scoreNoMidUnison w cf cp +
  scoreRepeatedNotes w cp


def scoreSecondSpecies (w : Weights := {}) (cf cp : List Pitch.Pitch) : Int :=
  scoreSecondSpeciesWithScale w cMajor cf cp

def scoreSecondSpeciesSlotsWithScale
    (w : Weights) (s : Scale)
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) : Int :=
  scoreScaleMembershipSlots w s cp +
  scoreStrongConsonantSlots w cf cp +
  scoreWeakBeatsSlots w cf cp +
  scoreImperfectStrongBeatsSlots w cf cp +
  scoreParallelPerfectStrongSlots w cf cp +
  scoreDirectIntoPerfectStrongSlots w cf cp +
  scoreContraryMotionStrongSlots w cf cp +
  scoreStartEndSlots w cf cp +
  scoreNoMidUnisonSlots w cf cp +
  scoreRepeatedNotesSlots w cf cp

def scoreSecondSpeciesSlots
    (w : Weights := {}) (cf : List Pitch.Pitch)
    (cp : List (Option Pitch.Pitch)) : Int :=
  scoreSecondSpeciesSlotsWithScale w cMajor cf cp

-- Candidate ranking and selection

def scoreCandidate (cf cp : List Pitch.Pitch) : Int :=
  scoreSecondSpecies (w := defaultWeights) cf cp


def scoreCandidates (cf : List Pitch.Pitch) (cps : List (List Pitch.Pitch)) :
    List ((List Pitch.Pitch) × Int) :=
  cps.map fun cp => (cp, scoreCandidate cf cp)


def bestScore (entries : List ((List Pitch.Pitch) × Int)) : Int :=
  entries.foldl (fun max (_, score) => if score > max then score else max) (-1000000000)


def selectTopSecondSpeciesCandidates
    (cf : List Pitch.Pitch)
    (cps : List (List Pitch.Pitch))
    (threshold : Int := 10) : List (List Pitch.Pitch) :=
  let scored := scoreCandidates cf cps
  let best := bestScore scored
  scored.filterMap fun (cp, score) =>
    if score >= best - threshold then
      some cp
    else
      none

end SecondSpeciesSoftWeighted
