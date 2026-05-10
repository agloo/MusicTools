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

  First species has no soft weights (only hard constraints). A real fifth
  species score needs per-measure rhythm labels, because the same flat pitch
  list cannot tell whether a two-note measure is second or fourth species.
-/

def lastPitch (xs : List Pitch.Pitch) : Option Pitch.Pitch :=
  xs[xs.length - 1]?

def scoreMeasureLocal
    (label : FifthSpecies.Species) (c : Pitch.Pitch) (chunk : List Pitch.Pitch) : Int :=
  match label with
  | .first => 0
  | .second =>
      SecondSpeciesSoftWeighted.scoreStrongConsonant [c] chunk +
      SecondSpeciesSoftWeighted.scoreWeakBeats [c] chunk +
      SecondSpeciesSoftWeighted.scoreImperfectStrongBeats [c] chunk
  | .third =>
      ThirdSpeciesSoftWeighted.scoreDownbeatConsonant [c] chunk +
      ThirdSpeciesSoftWeighted.scorePassingTones [c] chunk +
      ThirdSpeciesSoftWeighted.scoresCambiata [c] chunk +
      ThirdSpeciesSoftWeighted.scoreOffbeatDissonances [c] chunk
  | .fourth =>
      FourthSpeciesSoftWeighted.scoreThirdBeatConsonant [c] chunk

def scoreBarlineMotion
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
      score := score + ThirdSpeciesSoftWeighted.contraryMotionWeight
    else if isPerfectConsonance iv2 && isDirect mo then
      score := score + ThirdSpeciesSoftWeighted.directPerfectWeight
    let currentIsFourth := match labels[m]? with | some .fourth => true | _ => false
    if !currentIsFourth then
      let some prevChunk := chunks[m - 1]? | continue
      let some prevLast := lastPitch prevChunk | continue
      if prevLast = p2 then
        score := score + ThirdSpeciesSoftWeighted.repeatedNoteWeight
  return score

def scoreFourthSegments
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
      score := score + FourthSpeciesSoftWeighted.syncopationWeight
    else
      score := score - FourthSpeciesSoftWeighted.syncopationWeight
    let suspension := upi c strong
    let resolution := upi c weak
    if isDissonant suspension then
      let s := FourthSpeciesSoftWeighted.scoreValidSuspension suspension resolution
      if s = 0 then
        score := score + FourthSpeciesSoftWeighted.invalidWeight
      else
        score := score + s
  return score

def scoreFifthSpeciesWithLabels
    (cf cp : List Pitch.Pitch)
    (labels : List FifthSpecies.Species) : Int := Id.run do
  let chunks := FifthSpecies.chunkCp labels cp
  let mut score : Int := 0
  for m in List.range cf.length do
    let some c := cf[m]? | continue
    let some chunk := chunks[m]? | continue
    let some label := labels[m]? | continue
    score := score + scoreMeasureLocal label c chunk
  score := score + scoreBarlineMotion cf chunks labels
  score := score + scoreFourthSegments cf chunks labels
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
def scoreFifthSpecies (cf cp : List Pitch.Pitch) : Int :=
  scoreFifthSpeciesWithLabels cf cp (inferUniformLabels cf cp)

end FifthSpeciesSoftWeighted
