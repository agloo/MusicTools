import Pitch
import Interval
import Counterpoint
open Pitch
open Interval
open Counterpoint

namespace SecondSpecies

-- Second species: two counterpoint notes against one cantus-firmus note.
-- Inputs:
--   `cf` is one pitch per measure.
--   `cp` is `2 * cf.length` pitches: cp[2m] is the strong beat of measure m,
--   cp[2m+1] is the weak beat.
-- The optional opening rest is modeled by passing `cp` of length `2*cf.length - 1`
-- (i.e. omit the first strong beat); checks that touch index 0 are skipped.

def subdivisions : Nat := 2

-- Pitch of cp at (measure, beat). Beat 0 = strong, beat 1 = weak.
def cpAt (cp : List Pitch.Pitch) (m b : Nat) : Option Pitch.Pitch :=
  cp[m * subdivisions + b]?

-- Vertical interval at (measure m, beat b).
def vAt (cf cp : List Pitch.Pitch) (m b : Nat) : Option Upi :=
  match cf[m]?, cpAt cp m b with
  | some c, some p => some (upi c p)
  | _, _ => none

-- Strong beats (cp[2m]) must be consonant.
def checkStrongConsonant (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vAt cf cp m 0 | continue
    if !isConsonant iv then
      vs := vs ++ [⟨"strong-dissonant", 0, 1, m * subdivisions,
                    s!"strong-beat interval {iv} not consonant"⟩]
  return vs

-- Weak beat (cp[2m+1]) must be consonant OR a passing tone between cp[2m]
-- and cp[2m+2] (i.e. the next strong beat).
def checkWeakBeats (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vAt cf cp m 1 | continue
    if isConsonant iv then continue
    let some prev := cpAt cp m 0     | continue
    let some weak := cpAt cp m 1     | continue
    let some next := cpAt cp (m+1) 0 | continue
    if !isPassingTone prev weak next then
      vs := vs ++ [⟨"weak-dissonant", 0, 1, m * subdivisions + 1,
                    s!"weak-beat dissonance {iv} not a passing tone"⟩]
  return vs

-- No parallel perfect (P5/P8/P1) between consecutive strong beats — even
-- though a weak beat intervenes, this is still considered parallel.
def checkParallelPerfectStrong (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vAt cf cp (m-1) 0 | continue
    let some iv2 := vAt cf cp m 0     | continue
    if isPerfect iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      vs := vs ++ [⟨"parallel-perfect-strong", 0, 1, m * subdivisions,
                    s!"parallel {iv1}→{iv2} between strong beats"⟩]
  return vs

-- No direct (parallel/similar) motion into a perfect interval at the strong
-- beat of measure m, measured strong-beat-to-strong-beat.
def checkDirectIntoPerfectStrong (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m-1]?       | continue
    let some c2 := cf[m]?          | continue
    let some p1 := cpAt cp (m-1) 0 | continue
    let some p2 := cpAt cp m 0     | continue
    let mo := motion c1 c2 p1 p2
    let target := upi c2 p2
    if isDirect mo && isPerfect target then
      vs := vs ++ [⟨"direct-into-perfect-strong", 0, 1, m * subdivisions,
                    s!"{repr mo} motion into perfect ({target} st) at strong beat"⟩]
  return vs

-- Start (first sounding strong beat) and end (last strong beat) must be
-- perfect consonance. If `cp` is short by one (opening rest), use cp[0] as the
-- first weak beat and require it consonant instead.
def checkStartEnd (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  if cf.length = 0 then return vs
  -- start
  match vAt cf cp 0 0 with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"start-perfect", 0, 1, 0,
                      s!"start interval {iv} not perfect consonance"⟩]
  | none => pure ()
  -- end
  let last := cf.length - 1
  match vAt cf cp last 0 with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"end-perfect", 0, 1, last * subdivisions,
                      s!"end strong-beat interval {iv} not perfect consonance"⟩]
  | none => pure ()
  return vs

-- No unison on strong beats except the first and last measure.
def checkNoMidUnison (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  if cf.length = 0 then return vs
  let last := cf.length - 1
  for m in List.range cf.length do
    if m = 0 || m = last then continue
    let some iv := vAt cf cp m 0 | continue
    if iv = per1 then
      vs := vs ++ [⟨"unison-strong", 0, 1, m * subdivisions,
                    s!"unison on strong beat of measure {m}"⟩]
  return vs

def checkSecondSpecies (cf cp : List Pitch.Pitch) : List Violation :=
  checkStrongConsonant cf cp ++
  checkWeakBeats cf cp ++
  checkParallelPerfectStrong cf cp ++
  checkDirectIntoPerfectStrong cf cp ++
  checkStartEnd cf cp ++
  checkNoMidUnison cf cp

end SecondSpecies
