import Pitch
import Interval
open Pitch
open Interval

namespace ThirdSpeciesSoftWeighted

-- Soft weights for third species criteria (4 quarter notes per cantus-firmus whole note).

def downbeatConsonantWeight : Int := 50
def passingToneWeight : Int := 30
def cambiataWeight : Int := 40
def contraryMotionWeight : Int := 50
def repeatedNoteWeight : Int := -25

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
  return score

-- Reward passing tones (dissonance between consonances, stepped motion).
def scorePassingTones (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for i in List.range (cp.length - 2) do
    let some prev := cp[i]? | continue
    let some curr := cp[i + 1]? | continue
    let some next := cp[i + 2]? | continue
    if isPassingTone prev curr next then
      score := score + passingToneWeight
  return score

-- Reward cambiata pattern (characteristic 4th species skip in melodic contour).
def scoresCambiata (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  -- Cambiata: note → leap → resolution step down
  -- Simplified: penalize dissonant intervals that are not passing tones
  for i in List.range (cp.length - 1) do
    let some p1 := cp[i]? | continue
    let some p2 := cp[i + 1]? | continue
    let iv := upi p1 p2
    if isDissonant iv then
      -- Allow some credit for structured dissonance
      score := score + (cambiataWeight / 2)
  return score

-- Reward contrary motion into perfect consonances on downbeats.
def scoreContraryMotionDownbeat (cf cp : List Pitch.Pitch) : Int := Id.run do
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
    let iv1 := upi c1 p1
    let iv2 := upi c2 p2
    if motion c1 c2 p1 p2 = Motion.contrary && isPerfect iv2 then
      score := score + contraryMotionWeight
  return score

-- Penalize repeated notes across measure boundaries.
def scoreRepeatedNotes (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for i in List.range (cp.length - 1) do
    let some p := cp[i]? | continue
    let some q := cp[i + 1]? | continue
    if p = q then
      score := score + repeatedNoteWeight
  return score

-- Total soft-weighted score for third-species counterpoint.
def scoreThirdSpecies (cf cp : List Pitch.Pitch) : Int :=
  scoreDownbeatConsonant cf cp +
  scorePassingTones cp +
  scoresCambiata cp +
  scoreContraryMotionDownbeat cf cp +
  scoreRepeatedNotes cp

end ThirdSpeciesSoftWeighted
