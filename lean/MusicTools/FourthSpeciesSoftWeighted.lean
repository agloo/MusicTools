import Pitch
import Interval
import FourthSpecies
open Pitch
open Interval

namespace FourthSpeciesSoftWeighted

-- Soft weights for fourth species criteria (suspensions and syncopations).
-- NOTE: Suspension resolution is a HARD constraint, not soft-weighted.

structure Weights where
  validSuspension       : Int := 60
  syncopation           : Int := 40
  consonance            : Int := 30
  startEnd              : Int := 80
  penultimateSuspension : Int := 70
  invalid               : Int := -40
  deriving Repr

def defaultWeights : Weights := {}

-- Legacy constants for downstream callers.
def validSuspensionWeight       : Int := defaultWeights.validSuspension
def syncopationWeight           : Int := defaultWeights.syncopation
def consonanceWeight            : Int := defaultWeights.consonance
def startEndWeight              : Int := defaultWeights.startEnd
def penultimateSuspensionWeight : Int := defaultWeights.penultimateSuspension
def invalidWeight               : Int := defaultWeights.invalid

def isUnisonOrOctave (iv : Upi) : Bool :=
  intervalWithinOctave iv = per1

def isUpperSuspensionInterval (iv : Upi) : Bool :=
  let i := intervalWithinOctave iv
  i = per4 || i = min7 || i = maj7

def isUpperSuspensionResolution (suspension resolution : Upi) : Bool :=
  let s := intervalWithinOctave suspension
  let r := intervalWithinOctave resolution
  (s = per4 && (r = min3 || r = maj3)) ||
  ((s = min7 || s = maj7) && (r = min6 || r = maj6))

def isSevenSix (suspension resolution : Upi) : Bool :=
  let s := intervalWithinOctave suspension
  let r := intervalWithinOctave resolution
  (s = min7 || s = maj7) && (r = min6 || r = maj6)

-- Reward well-formed suspensions (4-3 or 7-6).
def scoreValidSuspension (w : Weights) (suspension : Upi) (resolution : Upi) : Int :=
  if isUpperSuspensionResolution suspension resolution then
    w.validSuspension
  else
    0

-- Reward consonances on 3rd beat (anchor points for stability).
def scoreThirdBeatConsonant (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    let thirdBeatIdx := m * 2 + 1
    if thirdBeatIdx >= cp.length then continue
    let some p := cp[thirdBeatIdx]? | continue
    let some c := cf[m]? | continue
    let iv := upi c p
    if isConsonant iv then
      score := score + w.consonance
    else
      score := score + w.invalid
  return score

-- Reward the required fourth-species tie across barlines, not arbitrary repeats.
def scoreSyncopation (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  let measures := cf.length
  for m in List.range measures do
    if m + 1 >= measures then continue
    let some p1 := FourthSpecies.cpAt cp m 1 | continue
    let some p2 := FourthSpecies.cpAt cp (m + 1) 0 | continue
    if m + 2 = measures then
      if p1 = p2 then score := score - w.syncopation
    else if p1 = p2 then
      score := score + w.syncopation
    else
      score := score - w.syncopation
  return score

-- Reward canonical 4-3 and 7-6 suspensions. Resolution itself remains a hard rule.
def scoreValidSuspensions (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  for m in List.range cf.length do
    if m = 0 then continue
    let some suspension := FourthSpecies.vAt cf cp m 0 | continue
    if isConsonant suspension then continue
    let some resolution := FourthSpecies.vAt cf cp m 1 | continue
    if FourthSpecies.isTied cp m &&
       FourthSpecies.resolvesDownByStep cp m &&
       isUpperSuspensionResolution suspension resolution then
      score := score + w.validSuspension
    else
      score := score + w.invalid
  return score

-- Puget Sound fourth species asks for a penultimate 7-6 suspension.
def scorePenultimateSuspension (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  if cf.length < 2 then return score
  let m := cf.length - 2
  let some suspension := FourthSpecies.vAt cf cp m 0 | return score
  let some resolution := FourthSpecies.vAt cf cp m 1 | return score
  if isSevenSix suspension resolution then
    score := score + w.penultimateSuspension
  else
    score := score - w.penultimateSuspension
  return score

-- Start on the third beat at the octave/unison class; end on octave/unison.
def scoreStartEnd (w : Weights) (cf cp : List Pitch.Pitch) : Int := Id.run do
  let mut score : Int := 0
  if cf.length = 0 then return score
  match cf[0]?, FourthSpecies.cpAt cp 0 1 with
  | some c, some p =>
      if isUnisonOrOctave (upi c p) then
        score := score + w.startEnd
      else
        score := score - w.startEnd
  | _, _ => pure ()
  let last := cf.length - 1
  match FourthSpecies.vAt cf cp last 0 with
  | some iv =>
      if isUnisonOrOctave iv then
        score := score + w.startEnd
      else
        score := score - w.startEnd
  | none => pure ()
  return score

-- Total soft-weighted score for fourth-species counterpoint.
def scoreFourthSpecies (w : Weights := {}) (cf cp : List Pitch.Pitch) : Int :=
  scoreThirdBeatConsonant w cf cp +
  scoreSyncopation w cf cp +
  scoreValidSuspensions w cf cp +
  scorePenultimateSuspension w cf cp +
  scoreStartEnd w cf cp

end FourthSpeciesSoftWeighted
