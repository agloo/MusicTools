/-
Spec equivalence between the operational `Solver.okStep` and a declarative,
pointwise constraint predicate. This is lemma 1 of the solver-correctness
program: every soundness/completeness theorem about `Solver.go` ultimately
threads through this equivalence at each step.

The other declarative spec (`Counterpoint.checkFirstSpecies`) aggregates
across all steps and voice pairs; we relate `okStep` to a single-step,
single-pair-of-voices version, then later (TODO) chain those facts together
to recover the full aggregate spec.
-/
import Solver
import Counterpoint
import Pitch
import Interval
import Mathlib.Tactic

namespace Proofs.SolverSpec

open Pitch Interval Counterpoint Solver

/-- Pointwise spec for placing pitch `p` at slot `idx` of the CP voice,
given the prior CP prefix `soFar`, the cantus firmus `cf`, and the resolved
CF entry `cfp = cf[idx]`. Mirrors the three constraint families `okStep`
enforces (scale, vertical interval, horizontal motion). Start/end perfect
consonance is intentionally not part of this spec — the solver delegates
that to `MPitch.known` slots in the template.

The horizontal clause is phrased as `(_ && _) = false` (rather than
`¬ (_ ∧ _)`) to mirror `okStep`'s `!(_ && _)` literally, which keeps the
equivalence proof a straight chain of bool rewrites. -/
def StepValidAt (s : Scale) (cf soFar : List Pitch)
    (p : Pitch) (idx : Nat) (cfp : Pitch) : Prop :=
  -- (a) scale membership
  inScale s p = true ∧
  -- (b) vertical interval allowed and bounded
  isFirstSpeciesAllowed (upi p cfp) = true ∧
  upi p cfp ≤ maxInterval ∧
  -- (c) at idx > 0, no direct motion into a perfect interval
  (idx = 0 ∨
    ∀ cfPrev cpPrev,
      cf[idx-1]? = some cfPrev →
      soFar[idx-1]? = some cpPrev →
      (isDirect (motion cfPrev cfp cpPrev p) && isPerfect (upi p cfp)) = false)

/-- `cf.getD idx 0 = cf[idx]` when `idx < cf.length`. Bridges `okStep`'s
operational `getD` lookups to the proof-friendly `[]?` form. -/
private lemma getD_eq_get_of_lt
    {cf : List Pitch} {idx : Nat} (h : idx < cf.length) :
    cf.getD idx 0 = cf[idx]'h := by
  simp [List.getD_eq_getElem?_getD, List.getElem?_eq_getElem h]

/-- Spec equivalence for one step.

`okStep s cf soFar p idx` returns `true` iff the pointwise spec
`StepValidAt` holds for the resolved CF entry at `idx`.

Preconditions:
  · `soFar.length = idx`  — we're placing at the next free slot
  · `idx < cf.length`     — the CF has an entry to compare against

The first precondition lets us identify `soFar[idx-1]?` (used inside
`okStep`) with the actual previous CP pitch. The second avoids the
`getD … 0` fallback in `okStep` masquerading as a real CF entry. -/
theorem okStep_iff_stepValid
    {s : Scale} {cf soFar : List Pitch} {p : Pitch} {idx : Nat}
    (hLen : soFar.length = idx)
    (hIdx : idx < cf.length) :
    Solver.okStep s cf soFar p idx = true ↔
      StepValidAt s cf soFar p idx (cf[idx]'hIdx) := by
  unfold Solver.okStep StepValidAt
  rw [getD_eq_get_of_lt hIdx]
  simp only [Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true',
             decide_eq_true_eq, beq_iff_eq]
  rcases Nat.eq_zero_or_pos idx with hz | hpos
  · -- idx = 0: the horizontal disjunct collapses on both sides
    subst hz
    simp
  · -- idx > 0: resolve `soFar.getD (idx-1) 0` and `cf.getD (idx-1) 0`
    -- to their `getElem` forms via hLen and hIdx.
    have hsoFar : idx - 1 < soFar.length := by omega
    have hcfPrev : idx - 1 < cf.length := by omega
    rw [getD_eq_get_of_lt hsoFar, getD_eq_get_of_lt hcfPrev]
    have hne : idx ≠ 0 := Nat.pos_iff_ne_zero.mp hpos
    constructor
    · rintro ⟨⟨hScale, hVert, hMax⟩, hHoriz⟩
      refine ⟨hScale, hVert, hMax, Or.inr ?_⟩
      intro cfPrev cpPrev hcf hsf
      rw [List.getElem?_eq_getElem hcfPrev] at hcf
      rw [List.getElem?_eq_getElem hsoFar] at hsf
      simp only [Option.some.injEq] at hcf hsf
      subst hcf; subst hsf
      rcases hHoriz with h0 | hH
      · exact absurd h0 hne
      · exact hH
    · rintro ⟨hScale, hVert, hMax, hHoriz⟩
      refine ⟨⟨hScale, hVert, hMax⟩, ?_⟩
      rcases hHoriz with h0 | hH
      · exact absurd h0 hne
      · exact Or.inr (hH _ _ (List.getElem?_eq_getElem hcfPrev)
                              (List.getElem?_eq_getElem hsoFar))

end Proofs.SolverSpec
