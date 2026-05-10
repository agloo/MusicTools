import Pitch
import Interval
import Counterpoint
import Solver
open Pitch
open Interval
open Counterpoint
open Solver

namespace SecondSpecies

-- Second species: two counterpoint notes against one cantus-firmus note.
-- Inputs:
--   `cf` is one pitch per measure.
--   `cp` is `2 * cf.length` pitches: cp[2m] is the strong beat of measure m,
--   cp[2m+1] is the weak beat.
-- The MuseScore plugin can also pass an explicit slot stream where rests are
-- `none`; the slot-aware checker below preserves an opening half rest.

def subdivisions : Nat := 2

-- Pitch of cp at (measure, beat). Beat 0 = strong, beat 1 = weak.
def cpAt (cp : List Pitch.Pitch) (m b : Nat) : Option Pitch.Pitch :=
  cp[m * subdivisions + b]?

-- Vertical interval at (measure m, beat b).
def vAt (cf cp : List Pitch.Pitch) (m b : Nat) : Option Upi :=
  match cf[m]?, cpAt cp m b with
  | some c, some p => some (upi c p)
  | _, _ => none

-- Pitch of cp at (measure, beat) for a slot stream where rests are `none`.
def cpSlotAt (cp : List (Option Pitch.Pitch)) (m b : Nat) : Option Pitch.Pitch :=
  (cp[m * subdivisions + b]?).bind id

-- Vertical interval at (measure m, beat b), preserving rests.
def vSlotAt (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch))
    (m b : Nat) : Option Upi :=
  match cf[m]?, cpSlotAt cp m b with
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

-- Slot-aware variants used by the MuseScore plugin. These keep an opening
-- half rest and a final whole note aligned with the cantus instead of stripping
-- rests and shifting weak beats onto strong-beat positions.

def checkStrongConsonantSlots
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vSlotAt cf cp m 0 | continue
    if !isConsonant iv then
      vs := vs ++ [⟨"strong-dissonant", 0, 1, m * subdivisions,
                    s!"strong-beat interval {iv} not consonant"⟩]
  return vs

def checkWeakBeatsSlots
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vSlotAt cf cp m 1 | continue
    if isConsonant iv then continue
    let isPassing :=
      match cpSlotAt cp m 0, cpSlotAt cp m 1, cpSlotAt cp (m + 1) 0 with
      | some prev, some weak, some next => isPassingTone prev weak next
      | _, _, _ => false
    if !isPassing then
      vs := vs ++ [⟨"weak-dissonant", 0, 1, m * subdivisions + 1,
                    s!"weak-beat dissonance {iv} not a passing tone"⟩]
  return vs

def checkParallelPerfectStrongSlots
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vSlotAt cf cp (m - 1) 0 | continue
    let some iv2 := vSlotAt cf cp m 0       | continue
    if isPerfect iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      vs := vs ++ [⟨"parallel-perfect-strong", 0, 1, m * subdivisions,
                    s!"parallel {iv1}→{iv2} between strong beats"⟩]
  return vs

def checkDirectIntoPerfectStrongSlots
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    if m = 0 then continue
    let some c1 := cf[m-1]?            | continue
    let some c2 := cf[m]?              | continue
    let some p1 := cpSlotAt cp (m-1) 0 | continue
    let some p2 := cpSlotAt cp m 0     | continue
    let mo := motion c1 c2 p1 p2
    let target := upi c2 p2
    if isDirect mo && isPerfect target then
      vs := vs ++ [⟨"direct-into-perfect-strong", 0, 1, m * subdivisions,
                    s!"{repr mo} motion into perfect ({target} st) at strong beat"⟩]
  return vs

def checkStartEndSlots
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List Violation := Id.run do
  let mut vs : List Violation := []
  if cf.length = 0 then return vs
  match vSlotAt cf cp 0 0 with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"start-perfect", 0, 1, 0,
                      s!"start interval {iv} not perfect consonance"⟩]
  | none =>
      match vSlotAt cf cp 0 1 with
      | some iv =>
          if !isPerfectConsonance iv then
            vs := vs ++ [⟨"start-perfect", 0, 1, 1,
                          s!"start interval {iv} not perfect consonance"⟩]
      | none => pure ()
  let last := cf.length - 1
  match vSlotAt cf cp last 0 with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"end-perfect", 0, 1, last * subdivisions,
                      s!"end strong-beat interval {iv} not perfect consonance"⟩]
  | none => pure ()
  return vs

def checkNoMidUnisonSlots
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List Violation := Id.run do
  let mut vs : List Violation := []
  if cf.length = 0 then return vs
  let last := cf.length - 1
  for m in List.range cf.length do
    if m = 0 || m = last then continue
    let some iv := vSlotAt cf cp m 0 | continue
    if iv = per1 then
      vs := vs ++ [⟨"unison-strong", 0, 1, m * subdivisions,
                    s!"unison on strong beat of measure {m}"⟩]
  return vs

def checkSecondSpeciesSlots
    (cf : List Pitch.Pitch) (cp : List (Option Pitch.Pitch)) :
    List Violation :=
  checkStrongConsonantSlots cf cp ++
  checkWeakBeatsSlots cf cp ++
  checkParallelPerfectStrongSlots cf cp ++
  checkDirectIntoPerfectStrongSlots cf cp ++
  checkStartEndSlots cf cp ++
  checkNoMidUnisonSlots cf cp

end SecondSpecies
