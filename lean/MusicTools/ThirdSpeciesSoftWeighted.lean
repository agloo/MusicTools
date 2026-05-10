import Pitch
import Interval
import ThirdSpecies
open Pitch
open Interval

namespace ThirdSpeciesSoftWeighted

-- Soft weights for third species criteria (4 quarter notes per cantus-firmus whole note).

def downbeatConsonantWeight : Int := 50
def passingToneWeight : Int := 30
def cambiataWeight : Int := 40
def contraryMotionWeight : Int := 50
def repeatedNoteWeight : Int := -25
def startEndWeight : Int := 80
def downbeatDissonantWeight : Int := -50
def offbeatDissonantWeight : Int := -30
def parallelPerfectWeight : Int := -60
def directPerfectWeight : Int := -40

def subdivisions : Nat := 4

def cpAt (cp : List Pitch.Pitch) (m b : Nat) : Option Pitch.Pitch :=
  cp[m * subdivisions + b]?

def vAt (cf cp : List Pitch.Pitch) (m b : Nat) : Option Upi :=
  match cf[m]?, cpAt cp m b with
  | some c, some p => some (upi c p)
  | _, _ => none

def isUnisonOrOctave (iv : Upi) : Bool :=
  intervalWithinOctave iv = per1

def isPassingDissonanceAt (cf cp : List Pitch.Pitch) (m b : Nat) : Bool :=
  if b = 0 then false else
    let idx := m * subdivisions + b
    match vAt cf cp m b, cp[idx - 1]?, cp[idx]?, cp[idx + 1]? with
    | some iv, some prev, some curr, some next =>
        isDissonant iv && isPassingTone prev curr next
    | _, _, _, _ => false

def isCambiataAtSecondBeat (cf cp : List Pitch.Pitch) (m : Nat) : Bool :=
  match vAt cf cp m 0, vAt cf cp m 1, vAt cf cp m 2, vAt cf cp m 3,
        cpAt cp m 0, cpAt cp m 1, cpAt cp m 2, cpAt cp m 3 with
  | some iv0, some iv1, some iv2, some iv3, some p, some q, some r, some s =>
      isConsonant iv0 &&
      isDissonant iv1 &&
      isConsonant iv2 &&
      isConsonant iv3 &&
      ThirdSpecies.isCambiata p q r s
  | _, _, _, _, _, _, _, _ => false

-- Reward consonance on downbeats (first quarter note of each measure).
def scoreDownbeatConsonant (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  -- Assuming cp is 4x the length of cf (4 quarter notes per measure).
  for m in List.range cf.length do
    let downbeatIdx := m * 4
    if downbeatIdx >= cp.length then continue
    let some p := cp[downbeatIdx]? | continue
    let some c := cf[m]? | continue
    let iv := upi c p
    if isConsonant iv then
      score := score + downbeatConsonantWeight
    else
      score := score + downbeatDissonantWeight
  return score

-- Reward passing tones only when they explain an offbeat dissonance.
def scorePassingTones (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    for b in [1, 2, 3] do
      if isPassingDissonanceAt cf cp m b then
        score := score + passingToneWeight
  return score

-- Reward cambiata only when the second quarter is the sole vertical dissonance.
def scoresCambiata (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if isCambiataAtSecondBeat cf cp m then
      score := score + cambiataWeight
  return score

-- Penalize offbeat dissonances that are neither passing tones nor a cambiata.
def scoreOffbeatDissonances (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let cambiata := isCambiataAtSecondBeat cf cp m
    for b in [1, 2, 3] do
      let some iv := vAt cf cp m b | continue
      if isDissonant iv then
        let ok := isPassingDissonanceAt cf cp m b || (b = 1 && cambiata)
        if !ok then
          score := score + offbeatDissonantWeight
  return score

-- Perfect consonances on downbeats should be approached by contrary motion.
def scorePerfectDownbeatApproach (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let downbeatIdxCurr := m * 4
    let downbeatIdxPrev := (m - 1) * 4
    if downbeatIdxCurr >= cp.length || downbeatIdxPrev >= cp.length then continue
    let some c1 := cf[m - 1]? | continue
    let some c2 := cf[m]? | continue
    let some p1 := cp[downbeatIdxPrev]? | continue
    let some p2 := cp[downbeatIdxCurr]? | continue
    let iv2 := upi c2 p2
    let mo := motion c1 c2 p1 p2
    if isPerfectConsonance iv2 && mo = Motion.contrary then
      score := score + contraryMotionWeight
    else if isPerfectConsonance iv2 && isDirect mo then
      score := score + directPerfectWeight
  return score

-- Penalize consecutive perfect consonances on downbeats.
def scoreParallelPerfectDownbeats (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vAt cf cp (m - 1) 0 | continue
    let some iv2 := vAt cf cp m 0 | continue
    if isPerfectConsonance iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      score := score + parallelPerfectWeight
  return score

-- Penalize repeated notes across measure boundaries.
def scoreRepeatedNotes (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range (cp.length / subdivisions + 1) do
    if m = 0 then continue
    let some p := cpAt cp (m - 1) 3 | continue
    let some q := cpAt cp m 0 | continue
    if p = q then
      score := score + repeatedNoteWeight
  return score

-- Third species begins and ends on octave or unison.
def scoreStartEnd (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  if cf.length = 0 then return score
  match vAt cf cp 0 0 with
  | some iv =>
      if isUnisonOrOctave iv then
        score := score + startEndWeight
      else
        score := score - startEndWeight
  | none => pure ()
  let last := cf.length - 1
  match vAt cf cp last 0 with
  | some iv =>
      if isUnisonOrOctave iv then
        score := score + startEndWeight
      else
        score := score - startEndWeight
  | none => pure ()
  return score

-- Total soft-weighted score for third-species counterpoint.
def scoreThirdSpecies (cf cp : List Pitch.Pitch) : Int :=
  scoreDownbeatConsonant cf cp +
  scorePassingTones cf cp +
  scoresCambiata cf cp +
  scoreOffbeatDissonances cf cp +
  scorePerfectDownbeatApproach cf cp +
  scoreParallelPerfectDownbeats cf cp +
  scoreRepeatedNotes cp +
  scoreStartEnd cf cp

end ThirdSpeciesSoftWeighted
