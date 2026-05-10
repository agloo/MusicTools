import Std.Tactic.BVDecide

-- Pitches as BitVec 8 (MIDI 0–127 fits unsigned; differences ≤ 127 are safe).
-- `abbrev` (= @[reducible] def) lets bv_decide unfold these during preprocessing.

-- Unsigned semitone distance between two pitches
abbrev bvUpi (p q : BitVec 8) : BitVec 8 :=
  if p ≥ q then p - q else q - p

-- Reduce interval to within one octave (mod 12)
abbrev bvIWO (n : BitVec 8) : BitVec 8 := n % 12

-- First-species allowed vertical intervals mod 12:
-- P1=0, m3=3, M3=4, P4=5, P5=7, m6=8, M6=9
abbrev bvIsFirstSpeciesAllowed (p q : BitVec 8) : Bool :=
  let i := bvIWO (bvUpi p q)
  i == 0 || i == 3 || i == 4 || i == 5 || i == 7 || i == 8 || i == 9

-- Max vertical span: ≤ 28 semitones
abbrev bvMaxOk (p q : BitVec 8) : Bool := bvUpi p q ≤ 28

-- Perfect intervals mod 12: P1=0, P4=5, P5=7
abbrev bvIsPerfect (p q : BitVec 8) : Bool :=
  let i := bvIWO (bvUpi p q)
  i == 0 || i == 5 || i == 7

-- Direct motion: both voices move in the same direction
abbrev bvIsDirect (a1 a2 b1 b2 : BitVec 8) : Bool :=
  (a2 > a1 && b2 > b1) || (a2 < a1 && b2 < b1)

-- No direct motion into a perfect interval (rule c from Counterpoint.lean)
abbrev bvNoDirectIntoPerfect (a1 a2 b1 b2 : BitVec 8) : Bool :=
  !(bvIsDirect a1 a2 b1 b2 && bvIsPerfect a2 b2)

-- Test case:
--   Cantus firmus: C4 D4 E4 D4  = [60, 62, 64, 62]
--   Upper voice:   G4 A4  ?  F4 = [67, 69,  x, 65]  -- beat 2 is the free variable
--
-- First-species constraints that touch x at beat 2.
-- Start/end perfect-consonance checks don't apply: x is not at beat 0 or 3.
abbrev beat2Ok (x : BitVec 8) : Bool :=
  bvIsFirstSpeciesAllowed 64 x         -- vertical interval at beat 2 (cf[2] = E4 = 64)
  && bvMaxOk 64 x                      -- span ≤ 28 semitones
  && bvNoDirectIntoPerfect 62 64 69 x  -- no direct into perfect: step 1→2 (cf[1]=D4, cf[2]=E4, up[1]=A4)
  && bvNoDirectIntoPerfect 64 62 x 65  -- no direct into perfect: step 2→3 (cf[2]=E4, cf[3]=D4, up[3]=F4)

-- bv_decide fails to prove this (solutions exist) and prints a satisfying x.
-- Expected: e.g.  x = 0x43#8  (0x43 = 67 decimal = G4)
-- simp unfolds our abbrevs to raw BitVec ops; bv_decide then drives CaDiCaL.
example : ∀ (x : BitVec 8), beat2Ok x = false := by
  simp only [beat2Ok, bvIsFirstSpeciesAllowed, bvUpi, bvIWO, bvMaxOk,
             bvIsPerfect, bvIsDirect, bvNoDirectIntoPerfect]
  bv_decide

-- All valid MIDI pitches for beat 2 (human-readable decimal list)
#eval (List.range 128).filter fun n => beat2Ok (BitVec.ofNat 8 n)
