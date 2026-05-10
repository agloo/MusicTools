import Pitch
import Interval
import Counterpoint
import Solver
open Pitch
open Interval
open Counterpoint
open Solver

namespace SoftWeighted

-- Soft weight constants inspired by agda/Weight.agda

def chromaticWeight : Int := -39
def imperfectWeight : Int := 40
def contraryWeight : Int := 50
def repeatedWeight : Int := -29

-- Scale membership penalty/reward

def scoreScaleMembership (s : Scale) (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for p in cp do
    if inScale s p then
      score := score + 0
    else
      score := score + chromaticWeight
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

-- Penalty for repeated notes in candidate counterpoint

def scoreRepeatedNotes (cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for i in List.range (cp.length - 1) do
    let some p := cp[i]? | continue
    let some q := cp[i+1]? | continue
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

end SoftWeighted
