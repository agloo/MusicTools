import Std.Data.TreeMap.Basic
import Pitch
import Music
import Expr

namespace Xml

abbrev Attributes := Std.TreeMap String String

mutual
inductive Element
| Element
    (name : String)
    (attributes : Attributes)
    (content : Array Content)

inductive Content
| Element   (element : Element)
| Comment   (comment : String)
| Character (content : String)
deriving Inhabited
end

instance : ToString Attributes :=
  ⟨fun as => as.foldl (fun s n v => s ++ s!" {n}=\"{v}\"") ""⟩

mutual
partial def eToString : Element → String
| Element.Element n a c =>
    s!"<{n}{a}>{c.map cToString |>.foldl (· ++ ·) ""}</{n}>"

partial def cToString : Content → String
| Content.Element e   => eToString e
| Content.Comment c   => s!"<!--{c}-->"
| Content.Character c => c
end

instance : ToString Element := ⟨eToString⟩
instance : ToString Content := ⟨cToString⟩

namespace Parser

abbrev Tok := List Char

def isNameStart (c : Char) : Bool := c.isAlpha || c == '_' || c == ':'
def isNameChar  (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == ':' || c == '.' || c == '-'

partial def skipWs : Tok → Tok
  | []      => []
  | c :: cs => if c.isWhitespace then skipWs cs else c :: cs

def expect (c : Char) : Tok → Except String Tok
  | []      => .error s!"expected '{c}', got EOF"
  | x :: xs =>
      if x == c then .ok xs
      else .error s!"expected '{c}', got '{x}'"

partial def consumeStr (pref : String) (cs : Tok) : Except String Tok :=
  let rec go : List Char → Tok → Except String Tok
    | [],      cs       => .ok cs
    | _ :: _,  []       => .error s!"expected '{pref}', got EOF"
    | p :: ps, c :: cs  =>
        if p == c then go ps cs
        else .error s!"expected '{pref}'"
  go pref.toList cs

partial def matchPrefix : List Char → Tok → Bool
  | [],      _       => true
  | _ :: _,  []      => false
  | p :: ps, c :: cs => p == c && matchPrefix ps cs

def takeName : Tok → Except String (String × Tok)
  | []      => .error "expected name, got EOF"
  | c :: cs =>
      if !isNameStart c then .error s!"expected name, got '{c}'"
      else
        let rec loop (acc : List Char) : Tok → List Char × Tok
          | []      => (acc.reverse, [])
          | x :: xs =>
              if isNameChar x then loop (x :: acc) xs
              else (acc.reverse, x :: xs)
        let (rest, after) := loop [] cs
        .ok (String.ofList (c :: rest), after)

def decodeEntity (cs : Tok) : Except String (Char × Tok) :=
  if matchPrefix "&amp;".toList cs   then .ok ('&',  cs.drop 5)
  else if matchPrefix "&lt;".toList cs   then .ok ('<',  cs.drop 4)
  else if matchPrefix "&gt;".toList cs   then .ok ('>',  cs.drop 4)
  else if matchPrefix "&quot;".toList cs then .ok ('"',  cs.drop 6)
  else if matchPrefix "&apos;".toList cs then .ok ('\'', cs.drop 6)
  else .error "unsupported XML entity (only &amp; &lt; &gt; &quot; &apos; supported)"

partial def takeAttrValue : Tok → Except String (String × Tok)
  | []      => .error "expected attribute value, got EOF"
  | q :: cs =>
      if q != '"' && q != '\'' then
        .error s!"expected quote for attribute value, got '{q}'"
      else
        let rec loop (acc : List Char) : Tok → Except String (String × Tok)
          | []          => .error "unterminated attribute value"
          | c :: rest   =>
              if c == q then .ok (String.ofList acc.reverse, rest)
              else if c == '&' then
                match decodeEntity (c :: rest) with
                | .ok (ch, rest') => loop (ch :: acc) rest'
                | .error e        => .error e
              else loop (c :: acc) rest
        loop [] cs

partial def takeAttributes : Tok → Except String (Attributes × Tok) := fun cs => do
  let cs := skipWs cs
  match cs with
  | []     => .error "unexpected EOF in attributes"
  | c :: _ =>
      if c == '/' || c == '>' then return (Std.TreeMap.empty, cs)
      else do
        let (name, cs) ← takeName cs
        let cs := skipWs cs
        let cs ← expect '=' cs
        let cs := skipWs cs
        let (val, cs) ← takeAttrValue cs
        let (rest, cs) ← takeAttributes cs
        return (rest.insert name val, cs)

partial def takeComment : Tok → Except String (String × Tok) :=
  let rec go (acc : List Char) : Tok → Except String (String × Tok)
    | []                          => .error "unterminated comment"
    | '-' :: '-' :: '>' :: rest   => .ok (String.ofList acc.reverse, rest)
    | c :: rest                   => go (c :: acc) rest
  go []

partial def takeCharData : Tok → Except String (String × Tok) :=
  let rec loop (acc : List Char) : Tok → Except String (String × Tok)
    | []            => .ok (String.ofList acc.reverse, [])
    | '<' :: rest   => .ok (String.ofList acc.reverse, '<' :: rest)
    | '&' :: rest   =>
        match decodeEntity ('&' :: rest) with
        | .ok (ch, rest') => loop (ch :: acc) rest'
        | .error e        => .error e
    | c :: rest     => loop (c :: acc) rest
  loop []

mutual
partial def parseElement (cs : Tok) : Except String (Element × Tok) := do
  let cs ← expect '<' cs
  let (name, cs) ← takeName cs
  let (attrs, cs) ← takeAttributes cs
  let cs := skipWs cs
  match cs with
  | '/' :: '>' :: rest =>
      return (.Element name attrs #[], rest)
  | '>' :: rest => do
      let (children, rest) ← parseContent rest
      let rest ← consumeStr "</" rest
      let (cname, rest) ← takeName rest
      if cname != name then
        .error s!"mismatched tag: opened <{name}>, closing </{cname}>"
      else do
        let rest := skipWs rest
        let rest ← expect '>' rest
        return (.Element name attrs children.toArray, rest)
  | _ => .error s!"expected '>' or '/>' after attributes for <{name}>"

partial def parseContent : Tok → Except String (List Content × Tok)
  | []                                => .ok ([], [])
  | '<' :: '/' :: rest                => .ok ([], '<' :: '/' :: rest)
  | '<' :: '!' :: '-' :: '-' :: rest  => do
      let (cmt, rest) ← takeComment rest
      let (more, rest) ← parseContent rest
      return (.Comment cmt :: more, rest)
  | '<' :: rest                       => do
      let (e, rest) ← parseElement ('<' :: rest)
      let (more, rest) ← parseContent rest
      return (.Element e :: more, rest)
  | cs                                => do
      let (txt, rest) ← takeCharData cs
      let (more, rest) ← parseContent rest
      return (.Character txt :: more, rest)
end

end Parser

def parse (s : String) : Except String Element := do
  let cs := Parser.skipWs s.toList
  let (e, rest) ← Parser.parseElement cs
  let rest := Parser.skipWs rest
  if rest.isEmpty then .ok e
  else .error s!"trailing content after root element: '{(String.ofList (rest.take 30))}'"

def parseXml (s : String) : Element :=
  match parse s with
    | Except.error err =>
        Element.Element err Std.TreeMap.empty Array.empty
    | Except.ok e => e

abbrev Duration := Int
abbrev Step     := String
abbrev Octave   := String
abbrev Alter    := String
abbrev Voice    := String
abbrev MNum     := String

structure Pitch where
  pStep   : Step
  pAlter  : Alter
  pOctave : Octave
deriving Repr

structure Note where
  nVoice    : Voice
  nDuration : Duration
  nPitch    : Pitch
  nIsRest   : Bool
  nIsChord  : Bool
deriving Repr

structure Measure where
  mNum   : MNum
  mNotes : List Note
deriving Repr

def filterContent : String → Content → List Element
| s, Content.Element e@⟨n, _, _⟩ => if s == n then [e] else []
| _, _                           => []

def children : String → Element → List Element
| s, ⟨_, _, c⟩  => List.flatten (List.map (filterContent s) c.toList)

def child (s : String) (e : Element) : Element :=
  match children s e with
  | (c :: _) => c
  | []       => ⟨ "null", Std.TreeMap.empty, Array.empty ⟩

def attr : String → Element → String
| s, ⟨_, a, _⟩ => Std.TreeMap.getD a s "NOTFOUND"

-- child as a string
def cstr (s : String) (e : Element) : String :=
  match child s e with
  | ⟨_, _, c⟩  => c.map toString |>.foldl (· ++ ·) ""

def pitch (e : Element) : Pitch :=
  ⟨ cstr "step" e, cstr "alter" e, cstr "octave" e ⟩

def hasChild (s : String) (e : Element) : Bool :=
  match children s e with
  | [] => false
  | _  => true

def note (e : Element) : Note :=
  ⟨ cstr "voice" e,
    match String.toInt? (cstr "duration" e) with
    |  some i => i
    |  none   => -1,
    pitch (child "pitch" e),
    hasChild "rest" e,
    hasChild "chord" e ⟩

def measure (e : Element) : Measure :=
  ⟨ attr "number" e, List.map note (children "note" e) ⟩

def measures (e : Element) : List Measure :=
  List.map measure (children "measure" e)

-- Step letter → semitone offset within an octave (C-based).
def stepOffset (s : Step) : Option Int :=
  match s with
  | "C" => some 0
  | "D" => some 2
  | "E" => some 4
  | "F" => some 5
  | "G" => some 7
  | "A" => some 9
  | "B" => some 11
  | _   => none

-- MusicXML pitch → MIDI int. C4 (middle C) = 60.
-- alter is signed integer string ("1" sharp, "-1" flat), defaults to 0 when absent.
def toPitch (p : Pitch) : Option Pitch.Pitch := do
  let off ← stepOffset p.pStep
  let oct ← String.toInt? p.pOctave
  let alt := (String.toInt? p.pAlter).getD 0
  pure ((oct + 1) * 12 + off + alt)

def notePitch (n : Note) : Option Pitch.Pitch :=
  if n.nIsRest then none else toPitch n.nPitch

-- All notes in a part, in document order.
def partNotes (part : Element) : List Note :=
  (measures part).flatMap (·.mNotes)

-- Group consecutive notes into chord-stacks: a non-chord note starts a new group,
-- chord-marked notes append to the current group. Within a group, lower pitches
-- come first per MusicXML convention.
partial def chordGroups : List Note → List (List Note)
  | [] => []
  | n :: rest =>
      let stack := rest.takeWhile (·.nIsChord)
      let tail  := rest.dropWhile (·.nIsChord)
      (n :: stack) :: chordGroups tail

-- Pivot a list of stacks (each of varying height) into N parallel streams,
-- where N = max stack height. Stacks shorter than N pad with `none`.
def stacksToVoices (stacks : List (List Note)) : List (List (Option Note)) :=
  let n := stacks.foldl (fun acc s => Nat.max acc s.length) 0
  (List.range n).map fun i =>
    stacks.map fun s => s[i]?

-- Extract parallel voice streams from a part, splitting both by `<voice>` tag
-- and by chord-stack position within each voice. Output is N streams of
-- Option Pitch — `none` represents a rest or a missing chord member.
def extractVoices (part : Element) : List (List (Option Pitch.Pitch)) :=
  let notes := partNotes part
  -- Stable group-by voice, preserving document order within each group.
  let voices : List (String × List Note) :=
    notes.foldl (fun acc n =>
      let v := n.nVoice
      match acc.find? (·.1 == v) with
      | some _ => acc.map (fun (k, ns) => if k == v then (k, ns ++ [n]) else (k, ns))
      | none   => acc ++ [(v, [n])]) []
  voices.flatMap fun (_, ns) =>
    (stacksToVoices (chordGroups ns)).map (·.map (·.bind notePitch))

-- Group notes by `<voice>` tag only, treating each tag as one melodic line.
-- For chord stacks within a voice, takes the leading (lowest) note and ignores
-- chord-marked stack-mates. Output: one stream per voice, in document order.
def extractVoicesByTag (part : Element) : List (List (Option Pitch.Pitch)) :=
  let notes := (partNotes part).filter (fun n => !n.nIsChord)
  let voices : List (String × List Note) :=
    notes.foldl (fun acc n =>
      let v := n.nVoice
      match acc.find? (·.1 == v) with
      | some _ => acc.map (fun (k, ns) => if k == v then (k, ns ++ [n]) else (k, ns))
      | none   => acc ++ [(v, [n])]) []
  voices.map fun (_, ns) => ns.map notePitch

-- MIDI pitch → (step, alter, octave) using sharps for accidentals.
-- C4 = 60; Int.emod is always non-negative in Lean 4.
def midiToMusicXml (p : Pitch.Pitch) : String × String × String :=
  let pc := p % 12
  let steps : Array (String × String) := #[
    ("C",""),  ("C","1"), ("D",""),  ("D","1"), ("E",""),
    ("F",""),  ("F","1"), ("G",""),  ("G","1"), ("A",""),
    ("A","1"), ("B","")]
  let (step, alter) := steps[pc.toNat]!
  (step, alter, toString ((p - pc) / 12 - 1))

private def mkLeafElem (tag val : String) : Element :=
  Element.Element tag Std.TreeMap.empty #[Content.Character val]

def mkPitchElem (p : Pitch.Pitch) : Element :=
  let (step, alter, oct) := midiToMusicXml p
  let ac := if alter.isEmpty then #[] else #[Content.Element (mkLeafElem "alter" alter)]
  Element.Element "pitch" Std.TreeMap.empty
    (#[Content.Element (mkLeafElem "step" step)] ++ ac ++ #[Content.Element (mkLeafElem "octave" oct)])

private def fillRest (p : Pitch.Pitch) (noteElem : Element) : Element :=
  match noteElem with
  | Element.Element n a cs =>
      Element.Element n a (cs.map fun c =>
        match c with
        | Content.Element (Element.Element "rest" _ _) => Content.Element (mkPitchElem p)
        | _ => c)

private structure SubState where
  voiceOrder : List String
  voiceBeats : List (String × Nat)

private def initSubState (part : Element) : SubState :=
  let notes := (partNotes part).filter (!·.nIsChord)
  let vo := notes.foldl (fun acc n =>
    if acc.any (· == n.nVoice) then acc else acc ++ [n.nVoice]) []
  { voiceOrder := vo, voiceBeats := vo.map (·, 0) }

-- Walk the element tree in document order, replacing rests whose variable
-- name appears in d with the solved pitch.
partial def substituteElem (d : Expr.Dict) : SubState → Element → Element × SubState
  | st, Element.Element name attrs cs =>
      if name != "note" then
        let (cs', st') := cs.foldl (fun ⟨acc, s⟩ c =>
          match c with
          | Content.Element e =>
              let (e', s') := substituteElem d s e
              (acc.push (Content.Element e'), s')
          | _ => (acc.push c, s)) (#[], st)
        (Element.Element name attrs cs', st')
      else
        let noteElem := Element.Element name attrs cs
        let vTag := cstr "voice" noteElem
        if hasChild "chord" noteElem then (noteElem, st)
        else
          let vi := st.voiceOrder.findIdx (· == vTag)
          let bi := (st.voiceBeats.find? (·.1 == vTag)).map (·.2) |>.getD 0
          let st' := { st with voiceBeats := st.voiceBeats.map fun (v, c) =>
                        if v == vTag then (v, c + 1) else (v, c) }
          if !hasChild "rest" noteElem then (noteElem, st')
          else
            match Expr.lookupM d s!"v{vi + 1}b{bi + 1}" with
            | none   => (noteElem, st')
            | some p => (fillRest p noteElem, st')

-- Serialise the score with solved blanks filled in.
-- Only the first part's voice ordering is used for variable name lookup.
def exportScoreStr (d : Expr.Dict) (root : Element) : String :=
  let st := match children "part" root with
    | p :: _ => initSubState p
    | []     => { voiceOrder := [], voiceBeats := [] }
  let (root', _) := substituteElem d st root
  toString root'

def xmlToScore (part : Element) : Music.Score :=
  let voices := extractVoicesByTag part
  voices.mapIdx fun vi beats =>
    beats.mapIdx fun bi mp =>
      match mp with
      | some p => Music.MPitch.known p
      | none   => Music.MPitch.var s!"v{vi + 1}b{bi + 1}"

end Xml
