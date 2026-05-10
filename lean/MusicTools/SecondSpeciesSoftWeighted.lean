import Pitch
import Interval
import SecondSpecies
import Solver
open Pitch
open Interval
open SecondSpecies
open Solver

namespace SecondSpeciesSoftWeighted

-- Soft weight constants inspired by agda/Weight.agda

def chromaticWeight : Int := -39
def imperfectWeight : Int := 40
def contraryWeight : Int := 50
def repeatedWeight : Int := -29
def startEndWeight : Int := 80
def strongConsonantWeight : Int := 40
def weakConsonantWeight : Int := 20
def passingToneWeight : Int := 10
def parallelPerfectWeight : Int := -60
def directPerfectWeight : Int := -40
def midUnisonWeight : Int := -70

def isUnisonOrOctave (iv : Upi) : Bool :=
  intervalWithinOctave iv = per1

-- Scale membership penalty/reward

def scoreScaleMembership (s : Scale) (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for p in cp do
    if inScale s p then
      score := score + 0
    else
      score := score + chromaticWeight
  return score

-- Strong beats consonant scoring

def scoreStrongConsonant (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vAt cf cp m 0 | continue
    if isConsonant iv then
      score := score + strongConsonantWeight
    else
      score := score - strongConsonantWeight
  return score

-- Weak-beat scoring, with passing-tone allowance

def scoreWeakBeats (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vAt cf cp m 1 | continue
    if isConsonant iv then
      score := score + weakConsonantWeight
    else
      let some prev := cpAt cp m 0     | continue
      let some weak := cpAt cp m 1     | continue
      let some next := cpAt cp (m+1) 0 | continue
      if isPassingTone prev weak next then
        score := score + passingToneWeight
      else
        score := score - weakConsonantWeight
  return score

-- Parallel perfect strong-beat penalty

def scoreParallelPerfectStrong (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vAt cf cp (m-1) 0 | continue
    let some iv2 := vAt cf cp m 0     | continue
    if isPerfectConsonance iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      score := score + parallelPerfectWeight
    else
      score := score + 10
  return score

-- Direct motion into perfect strong-beat penalty

def scoreDirectIntoPerfectStrong (cf cp : List Pitch.Pitch) : Int := Id.run do
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
      score := score + directPerfectWeight
    else
      score := score + 10
  return score

-- Puget Sound second species begins/ends on octave or unison, not fifth.

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

-- No mid unison scoring

def scoreNoMidUnison (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  if cf.length = 0 then return score
  let last := cf.length - 1
  for m in List.range cf.length do
    if m = 0 || m = last then continue
    let some iv := vAt cf cp m 0 | continue
    if iv = per1 then
      score := score + midUnisonWeight
    else
      score := score + 10
  return score

-- Imperfect consonance on strong beats

def isImperfectConsonance (iv : Upi) : Bool :=
  let i := intervalWithinOctave iv
  i = min3 || i = maj3 || i = min6 || i = maj6


def scoreImperfectStrongBeats (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let some iv := vAt cf cp m 0 | continue
    if isImperfectConsonance iv then
      score := score + imperfectWeight
  return score

-- Contrary motion between strong-beat pairs

def scoreContraryMotionStrong (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m-1]?       | continue
    let some c2 := cf[m]?          | continue
    let some p1 := cpAt cp (m-1) 0 | continue
    let some p2 := cpAt cp m 0     | continue
    if motion c1 c2 p1 p2 = Motion.contrary then
      score := score + contraryWeight
  return score

-- Penalty for repeated notes across barlines.

def scoreRepeatedNotes (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range (cp.length / subdivisions + 1) do
    if m = 0 then continue
    let some p := cpAt cp (m - 1) 1 | continue
    let some q := cpAt cp m 0 | continue
    if p = q then
      score := score + repeatedWeight
  return score

-- Total weighted score for second-species counterpoint

def scoreSecondSpeciesWithScale (s : Scale) (cf cp : List Pitch.Pitch) : Int :=
  scoreScaleMembership s cp +
  scoreStrongConsonant cf cp +
  scoreWeakBeats cf cp +
  scoreImperfectStrongBeats cf cp +
  scoreParallelPerfectStrong cf cp +
  scoreDirectIntoPerfectStrong cf cp +
  scoreContraryMotionStrong cf cp +
  scoreStartEnd cf cp +
  scoreNoMidUnison cf cp +
  scoreRepeatedNotes cp


def scoreSecondSpecies (cf cp : List Pitch.Pitch) : Int :=
  scoreSecondSpeciesWithScale cMajor cf cp

-- Candidate ranking and selection

def scoreCandidate (cf cp : List Pitch.Pitch) : Int :=
  scoreSecondSpecies cf cp


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
