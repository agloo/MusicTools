import Pitch
import Interval
import SecondSpeciesSoftWeighted
import ThirdSpeciesSoftWeighted
import FourthSpeciesSoftWeighted
import FifthSpecies
open Pitch
open Interval

namespace FifthSpeciesSoftWeighted

/-!
  Fifth species (florid counterpoint) is a free mixture of first through fourth
  species. Soft-weight scoring combines criteria from species 2, 3, and 4.

  Weights are bundled per sub-species in `Weights`.
-/

structure Weights where
  second : SecondSpeciesSoftWeighted.Weights := {}
  third  : ThirdSpeciesSoftWeighted.Weights  := {}
  fourth : FourthSpeciesSoftWeighted.Weights := {}
  deriving Repr

def defaultWeights : Weights := {}

def lastPitch (xs : List Pitch.Pitch) : Option Pitch.Pitch :=
  xs[xs.length - 1]?

def scoreMeasureLocal
    (w : Weights) (label : FifthSpecies.Species)
    (c : Pitch.Pitch) (chunk : List Pitch.Pitch) : Int :=
  match label with
  | .first => 0
  | .second =>
      SecondSpeciesSoftWeighted.scoreStrongConsonant w.second [c] chunk +
      SecondSpeciesSoftWeighted.scoreWeakBeats w.second [c] chunk +
      SecondSpeciesSoftWeighted.scoreImperfectStrongBeats w.second [c] chunk
  | .third =>
      ThirdSpeciesSoftWeighted.scoreDownbeatConsonant w.third [c] chunk +
      ThirdSpeciesSoftWeighted.scorePassingTones w.third [c] chunk +
      ThirdSpeciesSoftWeighted.scoresCambiata w.third [c] chunk +
      ThirdSpeciesSoftWeighted.scoreOffbeatDissonances w.third [c] chunk
  | .fourth =>
      FourthSpeciesSoftWeighted.scoreThirdBeatConsonant w.fourth [c] chunk

def scoreBarlineMotion
    (w : Weights)
    (cf : List Pitch.Pitch)
    (chunks : List (List Pitch.Pitch))
    (labels : List FifthSpecies.Species) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m - 1]? | continue
    let some c2 := cf[m]? | continue
    let some p1 := FifthSpecies.downbeat chunks (m - 1) | continue
    let some p2 := FifthSpecies.downbeat chunks m | continue
    let iv2 := upi c2 p2
    let mo := motion c1 c2 p1 p2
    if isPerfectConsonance iv2 && mo = Motion.contrary then
      score := score + w.third.contraryMotion
    else if isPerfectConsonance iv2 && isDirect mo then
      score := score + w.third.directPerfect
    let currentIsFourth := match labels[m]? with | some .fourth => true | _ => false
    if !currentIsFourth then
      let some prevChunk := chunks[m - 1]? | continue
      let some prevLast := lastPitch prevChunk | continue
      if prevLast = p2 then
        score := score + w.third.repeatedNote
  return score

def scoreFourthSegments
    (w : Weights)
    (cf : List Pitch.Pitch)
    (chunks : List (List Pitch.Pitch))
    (labels : List FifthSpecies.Species) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some .fourth := labels[m]? | continue
    if m = 0 then continue
    let some c := cf[m]? | continue
    let some chunk := chunks[m]? | continue
    let some strong := chunk[0]? | continue
    let some weak := chunk[1]? | continue
    let some prevChunk := chunks[m - 1]? | continue
    let some prevLast := lastPitch prevChunk | continue
    if prevLast = strong then
      score := score + w.fourth.syncopation
    else
      score := score - w.fourth.syncopation
    let suspension := upi c strong
    let resolution := upi c weak
    if isDissonant suspension then
      let s := FourthSpeciesSoftWeighted.scoreValidSuspension w.fourth suspension resolution
      if s = 0 then
        score := score + w.fourth.invalid
      else
        score := score + s
  return score

def scoreFifthSpeciesWithLabels
    (w : Weights)
    (cf cp : List Pitch.Pitch)
    (labels : List FifthSpecies.Species) : Int := Id.run do
  let chunks := FifthSpecies.chunkCp labels cp
  let mut score : Int := 0
  for m in List.range cf.length do
    let some c := cf[m]? | continue
    let some chunk := chunks[m]? | continue
    let some label := labels[m]? | continue
    score := score + scoreMeasureLocal w label c chunk
  score := score + scoreBarlineMotion w cf chunks labels
  score := score + scoreFourthSegments w cf chunks labels
  return score

def inferUniformLabels (cf cp : List Pitch.Pitch) : List FifthSpecies.Species :=
  if cp.length = cf.length * 4 then
    List.replicate cf.length FifthSpecies.Species.third
  else if cp.length = cf.length * 2 then
    List.replicate cf.length FifthSpecies.Species.second
  else
    List.replicate cf.length FifthSpecies.Species.first

-- Backwards-compatible fallback for uniform-rhythm inputs. Mixed fifth species
-- should call `scoreFifthSpeciesWithLabels`.
def scoreFifthSpecies (w : Weights := {}) (cf cp : List Pitch.Pitch) : Int :=
  scoreFifthSpeciesWithLabels w cf cp (inferUniformLabels cf cp)

end FifthSpeciesSoftWeighted
