import Pitch
import Interval
import Counterpoint
open Pitch
open Interval
open Counterpoint

namespace FourthSpecies

-- Fourth species: suspensions. Rhythmically the same 2:1 grid as second
-- species, but the *weak* half of measure m is tied across the barline to the
-- *strong* half of measure m+1. Concretely:
--   cp[2m]   = strong half of measure m (= cp[2m-1] when tied)
--   cp[2m+1] = weak half of measure m   (= cp[2(m+1)] when tied)
-- A "tie" at measure boundary m+1 means cp[2m+1] = cp[2(m+1)].
--
-- Rules:
--   • Strong half may be dissonant only as a *prepared, resolving suspension*:
--       (a) it is tied from the previous weak half (same pitch),
--       (b) it resolves *down by step* into the following weak half.
--   • Weak half must be consonant.
--   • Allowed suspension intervals (cp above cf): 7-6, 4-3, 9-8.
--     Allowed (cp below cf): 2-3.
--   • Start/end perfect consonance, as in 1st/2nd species.
--
-- For ergonomics we expose a `cpAbove : Bool` flag because the legality of a
-- suspension interval depends on whether cp lies above or below cf.

def subdivisions : Nat := 2

def cpAt (cp : List Pitch.Pitch) (m b : Nat) : Option Pitch.Pitch :=
  cp[m * subdivisions + b]?

def vAt (cf cp : List Pitch.Pitch) (m b : Nat) : Option Upi :=
  match cf[m]?, cpAt cp m b with
  | some c, some p => some (upi c p)
  | _, _ => none

-- Is the strong half of measure m tied from the weak half of measure m-1?
def isTied (cp : List Pitch.Pitch) (m : Nat) : Bool :=
  if m = 0 then false else
    match cpAt cp (m-1) 1, cpAt cp m 0 with
    | some a, some b => decide (a = b)
    | _, _ => false

-- Permissible suspension dissonances above cf: 4 (4-3), 7 (7-6), 9 (≡ 2 mod 12, 9-8).
-- Below cf: 2 (2-3 in the lower voice).
def isAllowedSuspensionInterval (cpAbove : Bool) (n : Upi) : Bool :=
  let i := intervalWithinOctave n
  if cpAbove then i = per4 || i = min7 || i = maj7 || i = maj2
  else i = maj2 || i = min2

-- The suspension at strong half of measure m must resolve down by step to
-- weak half of measure m.
def resolvesDownByStep (cp : List Pitch.Pitch) (m : Nat) : Bool :=
  match cpAt cp m 0, cpAt cp m 1 with
  | some s, some w => stepDown s w
  | _, _ => false

-- Strong half: consonant, OR a properly prepared suspension that resolves.
def checkStrongHalf (cpAbove : Bool) (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vAt cf cp m 0 | continue
    if isConsonant iv then continue
    -- dissonant strong half: must be a valid suspension
    if !isTied cp m then
      vs := vs ++ [⟨"unprepared-suspension", 0, 1, m * subdivisions,
                    s!"dissonant strong half {iv} is not tied (no preparation)"⟩]
    else if !isAllowedSuspensionInterval cpAbove iv then
      vs := vs ++ [⟨"bad-suspension-interval", 0, 1, m * subdivisions,
                    s!"suspension interval {iv} is not an allowed dissonance"⟩]
    else if !resolvesDownByStep cp m then
      vs := vs ++ [⟨"unresolved-suspension", 0, 1, m * subdivisions,
                    s!"suspension {iv} does not resolve down by step"⟩]
  return vs

-- Weak half must be consonant.
def checkWeakHalf (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vAt cf cp m 1 | continue
    if !isConsonant iv then
      vs := vs ++ [⟨"weak-dissonant", 0, 1, m * subdivisions + 1,
                    s!"weak-half interval {iv} not consonant"⟩]
  return vs

-- A suspension also creates parallel-perfect risk between consecutive
-- resolutions (weak-to-weak). Flag if both weak halves form the same perfect.
def checkParallelPerfectWeak (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vAt cf cp (m-1) 1 | continue
    let some iv2 := vAt cf cp m 1     | continue
    if isPerfect iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      vs := vs ++ [⟨"parallel-perfect-weak", 0, 1, m * subdivisions + 1,
                    s!"parallel {iv1}→{iv2} between resolutions"⟩]
  return vs

def checkStartEnd (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  if cf.length = 0 then return vs
  let last := cf.length - 1
  match vAt cf cp 0 0 with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"start-perfect", 0, 1, 0,
                      s!"start interval {iv} not perfect consonance"⟩]
  | none => pure ()
  match vAt cf cp last 0 with
  | some iv =>
      if !isPerfectConsonance iv then
        vs := vs ++ [⟨"end-perfect", 0, 1, last * subdivisions,
                      s!"end interval {iv} not perfect consonance"⟩]
  | none => pure ()
  return vs

def checkFourthSpecies (cpAbove : Bool) (cf cp : List Pitch.Pitch) : List Violation :=
  checkStrongHalf cpAbove cf cp ++
  checkWeakHalf cf cp ++
  checkParallelPerfectWeak cf cp ++
  checkStartEnd cf cp

end FourthSpecies
