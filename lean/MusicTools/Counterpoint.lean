import Pitch
import Interval
open Pitch
open Interval

namespace Counterpoint

-- A single rule violation.
-- `voiceA`/`voiceB` are voice indices into the input list (voiceA < voiceB).
-- `step` is the time-step index. For point-in-time rules, `step` is the slot;
-- for motion rules, it's the index of the *target* slot (i.e. step ≥ 1).
structure Violation where
  rule    : String
  voiceA  : Nat
  voiceB  : Nat
  step    : Nat
  detail  : String
deriving Repr

-- Maximum allowed vertical interval (semitones). Matches Agda Int 28.
def maxInterval : Nat := 28

-- All unordered pairs (i, j) with i < j over voice indices [0, n).
def voicePairs (n : Nat) : List (Nat × Nat) :=
  (List.range n).flatMap fun i =>
    (List.range n).filterMap fun j =>
      if i < j then some (i, j) else none

-- Vertical interval between voices a and b at step s, given equal-length streams.
-- Returns none if either voice has fewer than s+1 pitches.
def verticalUpi (a b : List Pitch.Pitch) (s : Nat) : Option Upi :=
  match a[s]?, b[s]? with
  | some p, some q => some (upi p q)
  | _,      _      => none

-- Rule (a): every vertical interval is in firstSpeciesIntervals4.
def checkVerticalIntervals (voices : List (List Pitch.Pitch)) : List Violation := Id.run do
  let n := voices.length
  let len := (voices.map List.length).foldl Nat.min (voices.head?.map List.length |>.getD 0)
  let mut vs : List Violation := []
  for (i, j) in voicePairs n do
    let some a := voices[i]? | continue
    let some b := voices[j]? | continue
    for s in List.range len do
      let some iv := verticalUpi a b s | continue
      if !isFirstSpeciesAllowed iv then
        vs := vs ++ [⟨"vertical-interval", i, j, s,
                      s!"interval {iv} semitones not in first-species set"⟩]
  return vs

-- Rule (b): no vertical interval exceeds maxInterval.
def checkMaxInterval (voices : List (List Pitch.Pitch)) : List Violation := Id.run do
  let n := voices.length
  let len := (voices.map List.length).foldl Nat.min (voices.head?.map List.length |>.getD 0)
  let mut vs : List Violation := []
  for (i, j) in voicePairs n do
    let some a := voices[i]? | continue
    let some b := voices[j]? | continue
    for s in List.range len do
      let some iv := verticalUpi a b s | continue
      if iv > maxInterval then
        vs := vs ++ [⟨"max-interval", i, j, s,
                      s!"interval {iv} > {maxInterval}"⟩]
  return vs

-- Rule (c): no direct (parallel or similar) motion *into* a perfect interval.
def checkDirectIntoPerfect (voices : List (List Pitch.Pitch)) : List Violation := Id.run do
  let n := voices.length
  let len := (voices.map List.length).foldl Nat.min (voices.head?.map List.length |>.getD 0)
  let mut vs : List Violation := []
  for (i, j) in voicePairs n do
    let some a := voices[i]? | continue
    let some b := voices[j]? | continue
    for s in List.range len do
      if s = 0 then continue
      let some a1 := a[s-1]? | continue
      let some a2 := a[s]?   | continue
      let some b1 := b[s-1]? | continue
      let some b2 := b[s]?   | continue
      let m := motion a1 a2 b1 b2
      let target := upi a2 b2
      if isDirect m && isPerfect target then
        vs := vs ++ [⟨"direct-into-perfect", i, j, s,
                      s!"{repr m} motion into perfect interval ({target} st)"⟩]
  return vs

-- Rule (d): start and end vertical interval must be perfect consonance (P1/P5/P8).
def checkStartEndPerfect (voices : List (List Pitch.Pitch)) : List Violation := Id.run do
  let n := voices.length
  let len := (voices.map List.length).foldl Nat.min (voices.head?.map List.length |>.getD 0)
  let mut vs : List Violation := []
  if len = 0 then return vs
  let last := len - 1
  for (i, j) in voicePairs n do
    let some a := voices[i]? | continue
    let some b := voices[j]? | continue
    let some sIv := verticalUpi a b 0    | continue
    let some eIv := verticalUpi a b last | continue
    if !isPerfectConsonance sIv then
      vs := vs ++ [⟨"start-perfect", i, j, 0,
                    s!"start interval {sIv} not perfect consonance"⟩]
    if !isPerfectConsonance eIv then
      vs := vs ++ [⟨"end-perfect", i, j, last,
                    s!"end interval {eIv} not perfect consonance"⟩]
  return vs

-- Aggregator: run all first-species checks.
def checkFirstSpecies (voices : List (List Pitch.Pitch)) : List Violation :=
  checkVerticalIntervals voices ++
  checkMaxInterval voices ++
  checkDirectIntoPerfect voices ++
  checkStartEndPerfect voices

end Counterpoint
