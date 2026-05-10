import Pitch
import Interval
open Pitch
open Interval

namespace FourthSpeciesSoftWeighted

-- Soft weights for fourth species criteria (suspensions and syncopations).

def suspensionWeight : Int := 60
def suspensionResolutionWeight : Int := 50
def syncopationWeight : Int := 40
def consonanceWeight : Int := 30

-- Reward well-formed suspensions (4-3 or 7-6).
def scoreValidSuspension (suspension : Upi) (resolution : Upi) : Int :=
  let s := intervalWithinOctave suspension
  let r := intervalWithinOctave resolution
  if (s = per4 && r = min3) || (s = min7 && r = maj6) then
    suspensionWeight
  else
    0

-- Reward proper resolution of suspensions (down by step).
def scoreSuspensionResolution (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for i in List.range (cp.length - 1) do
    let some p1 := cp[i]? | continue
    let some p2 := cp[i + 1]? | continue
    -- Check if p1 > p2 by exactly one semitone (or diatonic step equivalent)
    if p1 > p2 && p1 - p2 <= 2 then
      score := score + suspensionResolutionWeight
  return score

-- Reward consonances on 3rd beat (anchor points).
def scoreThirdBeatConsonant (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  -- Assuming cp is 2x the length of cf (two half notes per measure).
  for m in List.range cf.length do
    let thirdBeatIdx := m * 2 + 1
    if thirdBeatIdx >= cp.length then continue
    let some p := cp[thirdBeatIdx]? | continue
    let some c := cf[m]? | continue
    let iv := upi c p
    if isConsonant iv then
      score := score + consonanceWeight
  return score

-- Reward syncopations (consonances that tie across barlines).
def scoreSyncopation (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  -- Syncopation: tied note that lands on downbeat after being on 3rd beat.
  -- Simplified: reward length of tied notes as continuity.
  for i in List.range (cp.length - 1) do
    let some p1 := cp[i]? | continue
    let some p2 := cp[i + 1]? | continue
    if p1 = p2 then
      score := score + syncopationWeight
  return score

-- Total soft-weighted score for fourth-species counterpoint.
def scoreFourthSpecies (cf cp : List Pitch.Pitch) : Int :=
  scoreSuspensionResolution cp +
  scoreThirdBeatConsonant cf cp +
  scoreSyncopation cp

end FourthSpeciesSoftWeighted
