import Pitch
import Interval
import Counterpoint
import SecondSpecies
import ThirdSpecies
import FourthSpecies
open Pitch
open Interval
open Counterpoint

namespace FifthSpecies

-- Fifth species (florid): each measure of cp may follow first-, second-,
-- third-, or fourth-species rules. The user supplies a per-measure label
-- (`labels[m]`) and a flat `cp` list; we partition by the labels and dispatch
-- the measure-local rules to the species-specific checkers.
--
-- Cross-measure rules (parallel perfect consonance between adjacent
-- downbeats, start/end perfect) are checked uniformly here regardless of
-- which species each measure is in.

inductive Species where
  | first
  | second
  | third
  | fourth
deriving Repr, DecidableEq

def notesPerMeasure : Species → Nat
  | .first  => 1
  | .second => 2
  | .third  => 4
  | .fourth => 2

-- Slice cp into one chunk per measure, sized by `labels`.
def chunkCp : List Species → List Pitch.Pitch → List (List Pitch.Pitch)
  | [],      _   => []
  | s :: ls, cp =>
    let n := notesPerMeasure s
    cp.take n :: chunkCp ls (cp.drop n)

-- cp note offset (flat-list index) where measure m starts.
def measureOffset (labels : List Species) (m : Nat) : Nat :=
  ((labels.take m).map notesPerMeasure).foldl (· + ·) 0

def downbeat (chunks : List (List Pitch.Pitch)) (m : Nat) : Option Pitch.Pitch :=
  match chunks[m]? with
  | some xs => xs[0]?
  | none    => none

def vDown (cf : List Pitch.Pitch) (chunks : List (List Pitch.Pitch)) (m : Nat) : Option Upi :=
  match cf[m]?, downbeat chunks m with
  | some c, some p => some (upi c p)
  | _, _ => none

-- Translate per-measure-local violation `step`s into global cp-offset steps.
def shiftSteps (off : Nat) (vs : List Violation) : List Violation :=
  vs.map fun v => { v with step := off + v.step }

-- Run the appropriate species checker on a one-measure slice. The slice
-- always has cf-length 1, so we feed it `[c]` and the measure's chunk.
def measureCheck (cf : List Pitch.Pitch) (chunks : List (List Pitch.Pitch))
    (labels : List Species) (cpAbove : Bool) (m : Nat) : List Violation :=
  match cf[m]?, chunks[m]?, labels[m]? with
  | some c, some chunk, some s =>
      let mini_cf := [c]
      let mini_cp := chunk
      let off := measureOffset labels m
      shiftSteps off <| match s with
      | .first  => []  -- 1:1, downbeat consonance handled globally below
      | .second => SecondSpecies.checkStrongConsonant mini_cf mini_cp ++
                   SecondSpecies.checkWeakBeats        mini_cf mini_cp
      | .third  => ThirdSpecies.checkDownbeatConsonant  mini_cf mini_cp ++
                   ThirdSpecies.checkOffbeatDissonance  mini_cf mini_cp
      | .fourth => FourthSpecies.checkStrongHalf cpAbove mini_cf mini_cp ++
                   FourthSpecies.checkWeakHalf           mini_cf mini_cp
  | _, _, _ => []

-- Each downbeat must be consonant — except a fourth-species downbeat, which
-- may be a (prepared, resolving) suspension; the dissonance is checked by
-- the per-measure dispatch above.
def checkAllDownbeatsConsonant
    (cf : List Pitch.Pitch) (chunks : List (List Pitch.Pitch))
    (labels : List Species) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vDown cf chunks m | continue
    let isFourth := match labels[m]? with | some .fourth => true | _ => false
    if !isConsonant iv && !isFourth then
      vs := vs ++ [⟨"downbeat-dissonant", 0, 1, measureOffset labels m,
                    s!"downbeat interval {iv} not consonant"⟩]
  return vs

def checkParallelPerfectDownbeats
    (cf : List Pitch.Pitch) (chunks : List (List Pitch.Pitch))
    (labels : List Species) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vDown cf chunks (m-1) | continue
    let some iv2 := vDown cf chunks m     | continue
    if isPerfect iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      vs := vs ++ [⟨"parallel-perfect-downbeat", 0, 1, measureOffset labels m,
                    s!"parallel {iv1}→{iv2} between downbeats"⟩]
  return vs

def checkStartEnd
    (cf : List Pitch.Pitch) (chunks : List (List Pitch.Pitch))
    (labels : List Species) : List Violation := Id.run do
  let mut vs : List Violation := []
  if cf.length = 0 then return vs
  let last := cf.length - 1
  match vDown cf chunks 0 with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"start-perfect", 0, 1, 0,
                      s!"start interval {iv} not perfect consonance"⟩]
  | none => pure ()
  match vDown cf chunks last with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"end-perfect", 0, 1, measureOffset labels last,
                      s!"end interval {iv} not perfect consonance"⟩]
  | none => pure ()
  return vs

def checkFifthSpecies (cpAbove : Bool) (cf cp : List Pitch.Pitch)
    (labels : List Species) : List Violation := Id.run do
  let chunks := chunkCp labels cp
  let mut vs : List Violation := []
  vs := vs ++ checkAllDownbeatsConsonant   cf chunks labels
  vs := vs ++ checkParallelPerfectDownbeats cf chunks labels
  vs := vs ++ checkStartEnd                 cf chunks labels
  for m in List.range cf.length do
    vs := vs ++ measureCheck cf chunks labels cpAbove m
  return vs

end FifthSpecies
