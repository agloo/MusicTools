/-
Aggregate spec and soundness for `Solver.go`.

Lemma 1 (`Proofs/SolverSpec.lean`) showed that `okStep` matches a pointwise
spec at one placement. This file lifts that to a multi-step invariant and
proves the corresponding soundness theorem for `go`:

  starting from any already-valid prefix, if `go` succeeds, the output
  extends the prefix and remains step-by-step valid.

`solvePitches_sound` is the practical corollary (`soFar = []`).
-/
import Proofs.SolverSpec

namespace Proofs.SolverSpec

open Pitch Interval Counterpoint Solver

/-- A CP voice is fully step-by-step valid against `cf` if every placement
is locally justified by `StepValidAt` against the corresponding cf entry
and the cp prefix preceding it. -/
def AllStepsValid (s : Scale) (cf cp : List Pitch) : Prop :=
  ∀ idx (h : idx < cp.length) (hcf : idx < cf.length),
    StepValidAt s cf (cp.take idx) (cp[idx]'h) idx (cf[idx]'hcf)

lemma AllStepsValid.nil (s : Scale) (cf : List Pitch) :
    AllStepsValid s cf [] := by
  intro idx h _; simp at h

/-- Extending an already-valid prefix `soFar` with a pitch `p` whose
placement passes `okStep` yields a still-valid prefix. -/
lemma AllStepsValid.snoc
    {s : Scale} {cf soFar : List Pitch} {p : Pitch}
    (hLen : soFar.length < cf.length)
    (hPrev : AllStepsValid s cf soFar)
    (hStep : Solver.okStep s cf soFar p soFar.length = true) :
    AllStepsValid s cf (soFar ++ [p]) := by
  intro idx hidx hcf
  have hidx' : idx < soFar.length + 1 := by
    have := hidx
    simp only [List.length_append, List.length_cons, List.length_nil] at this
    omega
  rcases Nat.lt_or_ge idx soFar.length with hlt | hge
  · -- idx < soFar.length: this slot is unchanged
    have hTake : (soFar ++ [p]).take idx = soFar.take idx :=
      List.take_append_of_le_length hlt.le
    have hGet : ((soFar ++ [p])[idx]'hidx) = soFar[idx]'hlt :=
      List.getElem_append_left hlt
    rw [hTake, hGet]
    exact hPrev idx hlt hcf
  · -- idx = soFar.length: the newly placed step
    have heq : idx = soFar.length := by omega
    subst heq
    have hTake : (soFar ++ [p]).take soFar.length = soFar := by
      rw [List.take_append_of_le_length (Nat.le_refl _), List.take_length]
    have hGet : ((soFar ++ [p])[soFar.length]'hidx) = p := by
      rw [List.getElem_append_right (Nat.le_refl _)]
      simp
    rw [hTake, hGet]
    exact (okStep_iff_stepValid rfl hLen).mp hStep

/-- Extract a witness from a successful `findSome?` lookup. -/
private lemma findSome?_imp_exists {α β : Type*}
    {l : List α} {f : α → Option β} {b : β}
    (h : l.findSome? f = some b) : ∃ a, a ∈ l ∧ f a = some b := by
  induction l with
  | nil => simp [List.findSome?] at h
  | cons x xs ih =>
    simp only [List.findSome?_cons] at h
    cases hfx : f x with
    | none =>
      rw [hfx] at h
      obtain ⟨a, hMem, hfa⟩ := ih h
      exact ⟨a, List.mem_cons_of_mem _ hMem, hfa⟩
    | some b' =>
      rw [hfx] at h
      cases h
      exact ⟨x, List.mem_cons_self, hfx⟩

/-- **Soundness of `go`.** Starting from a valid prefix `soFar`, if `go`
returns `some cp`, then `cp` extends `soFar`, has the expected length,
and remains step-by-step valid against `cf`.

Preconditions:
  · `AllStepsValid s cf soFar` — what we start with is itself sound.
  · `soFar.length + template.length ≤ cf.length` — the template fits in
    the cantus firmus (so every placed pitch has a real CF entry to
    compare against). -/
theorem go_sound :
    ∀ {s : Scale} {cf : List Pitch}
      (template : List MPitch) {soFar cp : List Pitch},
      AllStepsValid s cf soFar →
      soFar.length + template.length ≤ cf.length →
      Solver.go s cf template soFar = some cp →
      cp.length = soFar.length + template.length ∧
      soFar.IsPrefix cp ∧ AllStepsValid s cf cp
  | _, _, [], soFar, cp, hValid, _, h => by
      simp only [Solver.go, Option.some.injEq] at h
      subst h
      refine ⟨by simp, List.prefix_rfl, hValid⟩
  | _, cf, .known p :: rest, soFar, cp, hValid, hLen, h => by
      simp only [Solver.go] at h
      split at h
      · -- okStep s cf soFar p soFar.length = true
        rename_i hStep
        have hLenLt : soFar.length < cf.length := by
          simp only [List.length_cons] at hLen; omega
        have hValid' := hValid.snoc hLenLt hStep
        have hLen' : (soFar ++ [p]).length + rest.length ≤ cf.length := by
          simp only [List.length_append, List.length_cons,
                     List.length_nil] at *; omega
        obtain ⟨hLenC, hPref, hAllValid⟩ := go_sound rest hValid' hLen' h
        refine ⟨?_, ?_, hAllValid⟩
        · simp only [List.length_append, List.length_cons, List.length_nil] at hLenC
          simp only [List.length_cons]; omega
        · exact (List.prefix_append soFar [p]).trans hPref
      · -- okStep false → none ≠ some
        simp at h
  | _, cf, .free :: rest, soFar, cp, hValid, hLen, h => by
      simp only [Solver.go] at h
      obtain ⟨pp, _hMem, hPp⟩ := findSome?_imp_exists h
      -- hPp : (if okStep s cf soFar pp soFar.length then go s cf rest (soFar ++ [pp]) else none) = some cp
      split at hPp
      · rename_i hStep
        have hLenLt : soFar.length < cf.length := by
          simp only [List.length_cons] at hLen; omega
        have hValid' := hValid.snoc hLenLt hStep
        have hLen' : (soFar ++ [pp]).length + rest.length ≤ cf.length := by
          simp only [List.length_append, List.length_cons,
                     List.length_nil] at *; omega
        obtain ⟨hLenC, hPref, hAllValid⟩ := go_sound rest hValid' hLen' hPp
        refine ⟨?_, ?_, hAllValid⟩
        · simp only [List.length_append, List.length_cons, List.length_nil] at hLenC
          simp only [List.length_cons]; omega
        · exact (List.prefix_append soFar [pp]).trans hPref
      · simp at hPp

/-- **Soundness of `solvePitches`.** If the solver returns a CP voice, it
has the same length as the template, fits the cantus firmus, and every
placement satisfies the pointwise spec. -/
theorem solvePitches_sound
    {s : Scale} {cf : List Pitch} {template : List MPitch} {cp : List Pitch}
    (hLen : template.length ≤ cf.length)
    (h : Solver.solvePitches s cf template = some cp) :
    cp.length = template.length ∧ AllStepsValid s cf cp := by
  obtain ⟨hLenC, _, hValid⟩ :=
    go_sound (s := s) (cf := cf) template
      (AllStepsValid.nil s cf)
      (by simpa using hLen)
      h
  exact ⟨by simpa using hLenC, hValid⟩

end Proofs.SolverSpec
