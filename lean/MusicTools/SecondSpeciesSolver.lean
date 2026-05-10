import Solver
import SecondSpecies

open Pitch

namespace SecondSpeciesSolver

open Solver

private def candidateDomain (s : Scale) (p : MPitch) : List Pitch :=
  match p with
  | .known pitch => if inScale s pitch then [pitch] else []
  | .free        => Solver.searchDomain.filter (inScale s)

private def validSecondSpecies (cf cp : List Pitch) : Bool :=
  (SecondSpecies.checkSecondSpecies cf cp).isEmpty

private def go (s : Scale) (cf : List Pitch)
    : List MPitch -> List Pitch -> Option (List Pitch)
  | [], soFar =>
      if validSecondSpecies cf soFar then some soFar else none
  | slot :: rest, soFar =>
      (candidateDomain s slot).findSome? fun p =>
        go s cf rest (soFar ++ [p])

private def violationCount (cf cp : List Pitch) : Nat :=
  (SecondSpecies.checkSecondSpecies cf cp).length

private def betterCandidate
    (current next : Option (List Pitch × Nat)) : Option (List Pitch × Nat) :=
  match current, next with
  | none, n => n
  | c, none => c
  | some (cpA, nA), some (cpB, nB) =>
      if nB < nA then some (cpB, nB) else some (cpA, nA)

private def goBest (s : Scale) (cf : List Pitch)
    : List MPitch -> List Pitch -> Option (List Pitch × Nat)
  | [], soFar => some (soFar, violationCount cf soFar)
  | slot :: rest, soFar =>
      (candidateDomain s slot).foldl
        (fun best p => betterCandidate best (goBest s cf rest (soFar ++ [p])))
        none

/-- Solve a second-species counterpoint template.

The cantus firmus has one pitch per measure. The counterpoint template has two
pitches per measure, using the same `MPitch` convention as the first-species
solver: fixed `known` pitches are preserved, and `free` slots are searched. -/
def solvePitches (s : Scale) (cf : List Pitch) (cp : List MPitch) :
    Option (List Pitch) :=
  go s cf cp []

/-- Return the candidate with the fewest hard-rule violations. This is intended
as an interactive repair fallback when fixed surrounding notes already make a
fully legal second-species line impossible. -/
def solveBestEffort (s : Scale) (cf : List Pitch) (cp : List MPitch) :
    Option (List Pitch) :=
  (goBest s cf cp []).map Prod.fst

end SecondSpeciesSolver
