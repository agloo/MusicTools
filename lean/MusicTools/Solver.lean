-- Exhaustive backtracking solver for first-species counterpoint.
-- Replaces the Agda/Haskell SMT stack (solveConstraints / Z3 via SBV).
-- Enforces three constraint families against a fixed cantus firmus (CF):
--
--   (a) scale    — every note belongs to the given key
--   (b) vertical — vertical interval ∈ {P1,m3,M3,P4,P5,m6,M6}, span ≤ 28 st
--   (c) horizontal — no direct (parallel/similar) motion into a perfect interval

import Pitch
import Interval
import Counterpoint
import Xml

open Pitch Interval Counterpoint

namespace Solver

-- ─── Scale ────────────────────────────────────────────────────────────────

abbrev Scale := List Int

-- Diatonic scales as sets of pitch classes mod 12.
def cMajor : Scale := [0, 2, 4, 5, 7, 9, 11]  -- C D E F G A B
def fMajor : Scale := [5, 7, 9, 10, 0, 2, 4]  -- F G A Bb C D E
def gMajor : Scale := [7, 9, 11, 0, 2, 4, 6]  -- G A B C D E F#

-- True iff pitch `p` belongs to scale `s` (handles negative Pitch values).
def inScale (s : Scale) (p : Pitch) : Bool :=
  s.contains ((p % 12 + 12) % 12)

-- ─── MPitch ───────────────────────────────────────────────────────────────

-- A counterpoint note: either a fixed concrete pitch or a free variable.
inductive MPitch where
  | known : Pitch → MPitch
  | free  : MPitch
  deriving Repr

-- ─── Incremental constraint check ─────────────────────────────────────────

-- Returns true iff placing pitch `p` at beat `idx` in the counterpoint voice
-- is consistent with cantus firmus `cf` and the CP notes already in `soFar`.
-- Mirrors the three constraints in the Agda `firstSpeciesConstraints2`:
-- start/end consonances are NOT checked here — the caller controls that by
-- using `known` nodes in the template for the first and last beats.
def okStep (s : Scale) (cf : List Pitch)
           (soFar : List Pitch) (p : Pitch) (idx : Nat) : Bool :=
  let cfp := cf.getD idx 0
  -- (a) scale membership
  inScale s p &&
  -- (b) vertical interval type and maximum span
  (let iv := upi p cfp
   isFirstSpeciesAllowed iv && iv ≤ maxInterval) &&
  -- (c) no direct (similar or parallel) motion into a perfect interval
  (idx == 0 ||
   let cfPrev := cf.getD (idx - 1) 0
   let cpPrev := soFar.getD (idx - 1) 0
   !(isDirect (motion cfPrev cfp cpPrev p) && isPerfect (upi p cfp)))

-- ─── Search domain ────────────────────────────────────────────────────────

-- Pitches tried for free variables.  Agda convention: C4 = 48, C5 = 60.
-- Range: C4 (48) to C7 (84) inclusive — 37 candidates.
-- Starting at C4 avoids the solver picking voice-crossing sub-bass notes
-- when searching from the bottom of the domain.
def searchDomain : List Pitch :=
  (List.range 37).map (fun i => Int.ofNat i + 48)

-- ─── Backtracking search ──────────────────────────────────────────────────

-- Structural backtrack: try each candidate from `searchDomain` for free notes,
-- pruning immediately when any constraint is violated.
-- Terminates on the template list (first arg) shrinking on each call.
def go (s : Scale) (cf : List Pitch)
    : List MPitch → List Pitch → Option (List Pitch)
  | [],               soFar => some soFar
  | .known p :: rest, soFar =>
      if okStep s cf soFar p soFar.length then
        go s cf rest (soFar ++ [p])
      else
        none
  | .free    :: rest, soFar =>
      searchDomain.findSome? fun p =>
        if okStep s cf soFar p soFar.length then
          go s cf rest (soFar ++ [p])
        else
          none

-- ─── Public API ───────────────────────────────────────────────────────────

/-- Solve for free pitches in the counterpoint template `cp` paired with the
    fixed cantus firmus `cf`, restricted to key `s`.  Returns the completed
    CP voice (all constraints satisfied), or `none` if no solution exists.

    Pitches use the Agda octave convention: C4 = 48, C5 = 60.

    Constraints enforced (matching Agda `firstSpeciesConstraints2`):
    · (a) scale    — every note's pitch class is in `s`
    · (b) vertical — interval ∈ {P1,m3,M3,P4,P5,m6,M6}, span ≤ 28 semitones
    · (c) horizontal — no direct (parallel/similar) motion into a perfect
                       interval (P1/P4/P5 mod 12)

    Start/end perfect-consonance is NOT enforced by the solver; fix those
    beats with `MPitch.known` in the template (as in the Agda workflow). -/
def solvePitches (s : Scale) (cf : List Pitch) (cp : List MPitch) : Option (List Pitch) :=
  go s cf cp []

-- ─── MusicXML export ──────────────────────────────────────────────────────

-- Agda convention: C4 = 48.  Standard MusicXML/MIDI: C4 = 60.  Shift = +12.
private def noteXml (agdaP : Pitch) (voiceNum : Nat) : String :=
  let (step, alter, oct) := Xml.midiToMusicXml (agdaP + 12)
  let altXml := if alter.isEmpty then "" else s!"<alter>{alter}</alter>"
  s!"<note><pitch><step>{step}</step>{altXml}<octave>{oct}</octave></pitch>" ++
  s!"<duration>1</duration><voice>{voiceNum}</voice><type>quarter</type></note>"

private def backupXml (n : Nat) : String :=
  s!"<backup><duration>{n}</duration></backup>"

-- Split `xs` into chunks of at most `n` elements.
private def chunked (n : Nat) (xs : List α) : List (List α) :=
  (List.range ((xs.length + n - 1) / n)).map fun i => xs.drop (i * n) |>.take n

/-- Render two solved voices as a MusicXML string.
    `cp` is the counterpoint (voice 1, stem up); `cf` the cantus firmus (voice 2).
    Uses 4/4 time with quarter notes, C major key signature. -/
def toMusicXml (title : String) (cp cf : List Pitch) : String :=
  let bpm := 4
  let cpCs := chunked bpm cp
  let cfCs := chunked bpm cf
  let nM   := max cpCs.length cfCs.length
  let firstAttrs :=
    "<attributes><divisions>1</divisions>" ++
    "<key><fifths>0</fifths></key>" ++
    "<time><beats>4</beats><beat-type>4</beat-type></time>" ++
    "<clef><sign>G</sign><line>2</line></clef></attributes>"
  let measures := (List.range nM).map fun i =>
    let cpN   := cpCs.getD i []
    let cfN   := cfCs.getD i []
    let beats := max cpN.length cfN.length
    s!"<measure number=\"{i + 1}\">" ++
    (if i == 0 then firstAttrs else "") ++
    cpN.foldl (fun acc p => acc ++ noteXml p 1) "" ++
    backupXml beats ++
    cfN.foldl (fun acc p => acc ++ noteXml p 2) "" ++
    "</measure>"
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  "<score-partwise version=\"4.0\">" ++
  s!"<work><work-title>{title}</work-title></work>" ++
  "<part-list><score-part id=\"P1\"><part-name>Score</part-name></score-part></part-list>" ++
  "<part id=\"P1\">" ++
  measures.foldl (·++·) "" ++
  "</part></score-partwise>"

/-- Solve and write a MusicXML file.  Returns `true` on success. -/
def writeSolution (path title : String) (s : Scale) (cf : List Pitch) (cp : List MPitch)
    : IO Bool := do
  match solvePitches s cf cp with
  | none =>
      IO.println s!"[{title}] no solution found"
      return false
  | some cpSolved =>
      let xml := toMusicXml title cpSolved cf
      IO.FS.writeFile path xml
      IO.println s!"[{title}] wrote {path}"
      return true

end Solver

-- ─── Examples ─────────────────────────────────────────────────────────────

section SolverExamples
open Solver Solver.MPitch

-- Tanaka (2022) figure 2: lower-voice CF in C major.
-- Agda convention: C3=36, D3=38, E3=40, F3=41, G3=43, C4=48, C5=60.
private def tanikaCF : List Pitch :=
  [48, 43, 41, 40, 41, 38, 43, 40, 38, 36]

-- CP template: first and last notes fixed at C5=60; 8 middle notes are free.
private def tanikaCP : List MPitch :=
  [known 60] ++ List.replicate 8 free ++ [known 60]

-- Beethoven op. 146 alto voice (CF).
private def bvCF : List Pitch :=
  [55, 60, 59, 57, 55, 57, 57, 60, 59, 55, 55, 55]

-- Soprano voice with beat 5 (the mistake) replaced by a free variable.
private def bvCP : List MPitch :=
  [known 60, known 64, known 67, known 65, known 64,
   free,
   known 69, known 65, known 67, known 64, known 62, known 60]

-- Write both examples to the scores/ directory.
#eval writeSolution "../../scores/tanaka_solved.musicxml"   "Tanaka CF + CP" cMajor tanikaCF tanikaCP
#eval writeSolution "../../scores/beethoven_solved.musicxml" "Beethoven op.146 fix" cMajor bvCF bvCP

end SolverExamples
