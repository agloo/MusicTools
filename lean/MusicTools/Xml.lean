import Std.Data.TreeMap.Basic

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

def note (e : Element) : Note :=
  ⟨ cstr "voice" e,
    match String.toInt? (cstr "duration" e) with
    |  some i => i
    |  none   => -1,
    pitch (child "pitch" e) ⟩

def measure (e : Element) : Measure :=
  ⟨ attr "number" e, List.map note (children "note" e) ⟩

def measures (e : Element) : List Measure :=
  List.map measure (children "measure" e)

end Xml
