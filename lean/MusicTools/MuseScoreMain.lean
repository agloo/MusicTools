import Xml
import Counterpoint
import ViolationJson
import Lean.Data.Json

open Lean

private def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let ok ← filename.pathExists
  if not ok then pure none
  else
    let h ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle h))

private partial def readStream (s : IO.FS.Stream) : IO String := do
  let l ← s.getLine
  if l.isEmpty then pure ""
  else
    if l.startsWith "<?" || l.startsWith "<!" then readStream s
    else (l ++ ·) <$> readStream s

private def alignVoices (vs : List (List (Option Pitch.Pitch))) : List (List Pitch.Pitch) :=
  let nonEmpty := vs.filter fun v => v.any Option.isSome
  match nonEmpty with
  | []     => []
  | v :: _ =>
      let slots := List.range v.length
      let keep  := slots.filter fun s =>
        nonEmpty.all fun w => (w[s]?).bind id |>.isSome
      nonEmpty.map fun w => keep.filterMap fun s => (w[s]?).bind id

private def runViolations
    (parts : List (List (List (Option Pitch.Pitch))))
    : List (Nat × Counterpoint.Violation) :=
  parts.zipIdx.flatMap fun (voices, pi) =>
    let aligned := alignVoices voices
    (Counterpoint.checkFirstSpecies aligned).map (pi, ·)

private def parsePitches (j : Json) : Except String (List (Option Pitch.Pitch)) := do
  let arr ← j.getArr?
  arr.toList.mapM fun n =>
    match n with
    | Json.null => pure none
    | _         => do
        let i ← n.getInt?
        pure (some (i : Pitch.Pitch))

-- Expected shape: {"id": "...", "parts": [ [ [pitch|null, ...], ... ], ... ]}
private def parseScoreJson (s : String)
    : Except String (String × List (List (List (Option Pitch.Pitch)))) := do
  let j ← Json.parse s
  let id ← j.getObjValAs? String "id"
  let partsJ ← j.getObjVal? "parts"
  let partsArr ← partsJ.getArr?
  let parts ← partsArr.toList.mapM fun part => do
    let voicesArr ← part.getArr?
    voicesArr.toList.mapM parsePitches
  pure (id, parts)

private def escapeJsonStr (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
      | '"'  => "\\\""
      | '\\' => "\\\\"
      | '\n' => "\\n"
      | '\r' => "\\r"
      | '\t' => "\\t"
      | _    => String.singleton c) ""

def main (args : List String) : IO Unit := do
  let pathStr : String ← match args with
    | [p] => pure p
    | _   => throw (IO.userError "usage: musescore-check <file.musicxml|file.json>")
  let path : System.FilePath := ⟨pathStr⟩
  let ok ← path.pathExists
  unless ok do throw (IO.userError s!"file not found: {pathStr}")
  if pathStr.endsWith ".json" then
    let txt ← IO.FS.readFile path
    match parseScoreJson txt with
    | .error e => throw (IO.userError s!"json parse error: {e}")
    | .ok (id, parts) =>
        let pairs := runViolations parts
        let body := ViolationJson.violationsToJson pairs
        IO.println ("{\"id\":\"" ++ escapeJsonStr id ++ "\",\"violations\":" ++ body ++ "}")
  else
    match ← fileStream path with
    | none   => throw (IO.userError s!"file not found: {path}")
    | some s =>
        let txt ← readStream s
        match Xml.parse txt with
        | .error e => throw (IO.userError s!"parse error: {e}")
        | .ok root =>
            let parts := Xml.children "part" root
            let voices := parts.map Xml.extractVoicesByTag
            let pairs := runViolations voices
            IO.println (ViolationJson.violationsToJson pairs)
