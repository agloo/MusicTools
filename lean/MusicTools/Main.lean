import Xml
open Xml

#eval Lean.versionString

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

partial def readString (stream : IO.FS.Stream) : IO String := do
  let s ← stream.getLine
  if s.isEmpty then pure ""
  else (s ++ ·) <$> readString stream

-- Lean XML parser has bug in parsing !DOCTYPE
-- https://github.com/leanprover/lean4/issues/12109
def discardHeader (stream : IO.FS.Stream) : IO IO.FS.Stream :=  do
  let _ ← stream.getLine -- ?xml
  let _ ← stream.getLine -- !DOCTYPE
  pure stream

-- Resolved relative to the cwd of `lake env lean Main.lean` (this directory).
def filePath : System.FilePath := "../../scores/P5AndP3.musicxml"

def mymain : IO String := do
  let stream ← fileStream filePath
  match stream with
    | none => IO.println s!"file not found: {filePath}"; pure ""
    | some s => discardHeader s >>= readString

-- Smoke checks: file size, first 200 chars, parse status, top-level tag, # of <part>s, # of <measure>s in part 1.
#eval do
  let s ← mymain
  IO.println s!"read {s.length} chars"
  IO.println s!"head: {s.take 200}"
  match parse s with
  | .error e => IO.println s!"parse error: {e}"
  | .ok root =>
      let parts := children "part" root
      IO.println s!"root tag: {match root with | .Element n _ _ => n}"
      IO.println s!"parts: {parts.length}"
      match parts with
      | [] => pure ()
      | p :: _ => IO.println s!"measures in part 1: {(measures p).length}"

#eval (List.map measures ∘ children "part" ∘ parseXml) <$> mymain
