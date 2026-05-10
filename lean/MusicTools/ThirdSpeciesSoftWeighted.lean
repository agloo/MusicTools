import Pitch
import Interval
import ThirdSpecies
open Pitch
open Interval

namespace ThirdSpeciesSoftWeighted

-- Soft weights for third species criteria (4 quarter notes per cantus-firmus
-- whole note). Defaults preserved as struct field defaults.

structure Weights where
  downbeatConsonant  : Int := 50
  passingTone        : Int := 30
  cambiata           : Int := 40
  contraryMotion     : Int := 50
  repeatedNote       : Int := -25
  startEnd           : Int := 80
  downbeatDissonant  : Int := -50
  offbeatDissonant   : Int := -30
  parallelPerfect    : Int := -60
  directPerfect      : Int := -40
  deriving Repr

def defaultWeights : Weights := {}

-- Legacy constants — preserved so downstream callers (e.g. FifthSpecies)
-- referencing the names continue to work.
def downbeatConsonantWeight : Int := defaultWeights.downbeatConsonant
def passingToneWeight       : Int := defaultWeights.passingTone
def cambiataWeight          : Int := defaultWeights.cambiata
def contraryMotionWeight    : Int := defaultWeights.contraryMotion
def repeatedNoteWeight      : Int := defaultWeights.repeatedNote
def startEndWeight          : Int := defaultWeights.startEnd
def downbeatDissonantWeight : Int := defaultWeights.downbeatDissonant
def offbeatDissonantWeight  : Int := defaultWeights.offbeatDissonant
def parallelPerfectWeight   : Int := defaultWeights.parallelPerfect
def directPerfectWeight     : Int := defaultWeights.directPerfect

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
def scoreDownbeatConsonant (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let downbeatIdx := m * 4
    if downbeatIdx >= cp.length then continue
    let some p := cp[downbeatIdx]? | continue
    let some c := cf[m]? | continue
    let iv := upi c p
    if isConsonant iv then
      score := score + w.downbeatConsonant
    else
      score := score + w.downbeatDissonant
  return score

-- Reward passing tones only when they explain an offbeat dissonance.
def scorePassingTones (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    for b in [1, 2, 3] do
      if isPassingDissonanceAt cf cp m b then
        score := score + w.passingTone
  return score

-- Reward cambiata only when the second quarter is the sole vertical dissonance.
def scoresCambiata (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if isCambiataAtSecondBeat cf cp m then
      score := score + w.cambiata
  return score

-- Penalize offbeat dissonances that are neither passing tones nor a cambiata.
def scoreOffbeatDissonances (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let cambiata := isCambiataAtSecondBeat cf cp m
    for b in [1, 2, 3] do
      let some iv := vAt cf cp m b | continue
      if isDissonant iv then
        let ok := isPassingDissonanceAt cf cp m b || (b = 1 && cambiata)
        if !ok then
          score := score + w.offbeatDissonant
  return score

-- Perfect consonances on downbeats should be approached by contrary motion.
def scorePerfectDownbeatApproach (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
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
      score := score + w.contraryMotion
    else if isPerfectConsonance iv2 && isDirect mo then
      score := score + w.directPerfect
  return score

-- Penalize consecutive perfect consonances on downbeats.
def scoreParallelPerfectDownbeats (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vAt cf cp (m - 1) 0 | continue
    let some iv2 := vAt cf cp m 0 | continue
    if isPerfectConsonance iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      score := score + w.parallelPerfect
  return score

-- Penalize repeated notes across measure boundaries.
def scoreRepeatedNotes (w : Weights) (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range (cp.length / subdivisions + 1) do
    if m = 0 then continue
    let some p := cpAt cp (m - 1) 3 | continue
    let some q := cpAt cp m 0 | continue
    if p = q then
      score := score + w.repeatedNote
  return score

-- Third species begins and ends on octave or unison.
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

-- Total soft-weighted score for third-species counterpoint.
def scoreThirdSpecies (w : Weights := {}) (cf cp : List Pitch.Pitch) : Int :=
  scoreDownbeatConsonant w cf cp +
  scorePassingTones w cf cp +
  scoresCambiata w cf cp +
  scoreOffbeatDissonances w cf cp +
  scorePerfectDownbeatApproach w cf cp +
  scoreParallelPerfectDownbeats w cf cp +
  scoreRepeatedNotes w cp +
  scoreStartEnd w cf cp

end ThirdSpeciesSoftWeighted
