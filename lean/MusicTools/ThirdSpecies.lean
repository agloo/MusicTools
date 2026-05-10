import Pitch
import Interval
import Counterpoint
open Pitch
open Interval
open Counterpoint

namespace ThirdSpecies

-- Third species: four counterpoint notes per cantus-firmus note.
-- Inputs:
--   `cf` is one pitch per measure.
--   `cp` is `4 * cf.length` pitches: cp[4m+b] is beat b ∈ {0,1,2,3} of measure m.
-- Beat 0 (strong) must be consonant. Beats 1,2,3 may be dissonant only if
-- they form a passing tone, neighbor tone, or *cambiata* figure.

def subdivisions : Nat := 4

def cpAt (cp : List Pitch.Pitch) (m b : Nat) : Option Pitch.Pitch :=
  cp[m * subdivisions + b]?

def vAt (cf cp : List Pitch.Pitch) (m b : Nat) : Option Upi :=
  match cf[m]?, cpAt cp m b with
  | some c, some p => some (upi c p)
  | _, _ => none

-- Pitch immediately before beat b of measure m (cp[idx-1] where idx = 4m+b).
def cpPrev (cp : List Pitch.Pitch) (m b : Nat) : Option Pitch.Pitch :=
  let idx := m * subdivisions + b
  if idx = 0 then none else cp[idx-1]?

def cpNext (cp : List Pitch.Pitch) (m b : Nat) : Option Pitch.Pitch :=
  cp[m * subdivisions + b + 1]?

-- A *nota cambiata* at q in p→q→r→s: stepwise p→q dissonance, leap q→r by a
-- third in same direction, then step r→s back the other way.
def isCambiata (p q r s : Pitch.Pitch) : Bool :=
  ((stepDown p q && opi q r = -(min3 : Int) || stepDown p q && opi q r = -(maj3 : Int))
    && stepUp r s)
  || ((stepUp p q && opi q r = (min3 : Int) || stepUp p q && opi q r = (maj3 : Int))
    && stepDown r s)

-- Beat 0 must be consonant.
def checkDownbeatConsonant (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    let some iv := vAt cf cp m 0 | continue
    if !isConsonant iv then
      vs := vs ++ [⟨"downbeat-dissonant", 0, 1, m * subdivisions,
                    s!"downbeat interval {iv} not consonant"⟩]
  return vs

-- Beats 1, 2, 3: if dissonant, must be passing, neighbor, or part of cambiata.
def checkOffbeatDissonance (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    for b in [1, 2, 3] do
      let some iv := vAt cf cp m b | continue
      if isConsonant iv then continue
      let some prev := cpPrev cp m b | continue
      let some cur  := cpAt cp m b   | continue
      let some next := cpNext cp m b | continue
      let passing  := isPassingTone prev cur next
      let neighbor := isNeighborTone prev cur next
      -- cambiata: cur is the dissonant second note, look ahead one more
      let cambiata :=
        match cpAt cp m (b+1), cp[m * subdivisions + b + 2]? with
        | some r, some s => isCambiata prev cur r s
        | _, _ => false
      if !(passing || neighbor || cambiata) then
        vs := vs ++ [⟨"offbeat-dissonant", 0, 1, m * subdivisions + b,
                      s!"dissonance {iv} on beat {b} is not passing/neighbor/cambiata"⟩]
  return vs

-- Parallel perfect across barlines (beat 0 of m-1 to beat 0 of m).
def checkParallelPerfectDownbeats (cf cp : List Pitch.Pitch) : List Violation := Id.run do
  let mut vs : List Violation := []
  for m in List.range cf.length do
    if m = 0 then continue
    let some iv1 := vAt cf cp (m-1) 0 | continue
    let some iv2 := vAt cf cp m 0     | continue
    if isPerfect iv1 && intervalWithinOctave iv1 = intervalWithinOctave iv2 then
      vs := vs ++ [⟨"parallel-perfect-downbeat", 0, 1, m * subdivisions,
                    s!"parallel {iv1}→{iv2} between downbeats"⟩]
  return vs

-- Start (downbeat 0) and end (last downbeat) must be perfect consonance.
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
                      s!"end downbeat interval {iv} not perfect consonance"⟩]
  | none => pure ()
  return vs

def checkThirdSpecies (cf cp : List Pitch.Pitch) : List Violation :=
  checkDownbeatConsonant cf cp ++
  checkOffbeatDissonance cf cp ++
  checkParallelPerfectDownbeats cf cp ++
  checkStartEnd cf cp

end ThirdSpecies
