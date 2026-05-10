import Xml
import Counterpoint
open Xml
open Counterpoint

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let ok ← filename.pathExists
  if not ok then pure none
  else
    let h ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle h))

partial def readString (s : IO.FS.Stream) : IO String := do
  let l ← s.getLine
  if l.isEmpty then pure "" else (l ++ ·) <$> readString s

-- Lean's XML parser stumbles on !DOCTYPE; skip the first two lines.
def discardHeader (s : IO.FS.Stream) : IO IO.FS.Stream := do
  let _ ← s.getLine; let _ ← s.getLine; pure s

def filePath : System.FilePath := "../../scores/test1.musicxml"

-- Drop voices that are entirely rests, then keep only time slots where every
-- remaining voice has a pitch. Yields a clean List (List Pitch) for the checker.
def alignVoices (vs : List (List (Option Pitch.Pitch))) : List (List Pitch.Pitch) :=
  let nonEmpty := vs.filter fun v => v.any Option.isSome
  match nonEmpty with
  | []     => []
  | v :: _ =>
      let slots := List.range v.length
      let keep  := slots.filter fun s =>
        nonEmpty.all fun w => (w[s]?).bind id |>.isSome
      nonEmpty.map fun w => keep.filterMap fun s => (w[s]?).bind id

def loadScore (path : System.FilePath) : IO String := do
  match ← fileStream path with
  | none   => IO.println s!"file not found: {path}"; pure ""
  | some s => discardHeader s >>= readString

def main : IO Unit := do
  let txt ← loadScore filePath
  if txt.isEmpty then return
  match parse txt with
  | .error e => IO.println s!"parse error: {e}"
  | .ok root =>
      let parts := children "part" root
      IO.println s!"parts: {parts.length}"
      for (p, pi) in parts.zipIdx do
        let raw := extractVoicesByTag p
        let aligned := alignVoices raw
        IO.println s!"part {pi}: raw={raw.length} voices, aligned={aligned.length}×{(aligned.head?.map List.length).getD 0}"
        for (v, vi) in aligned.zipIdx do
          IO.println s!"  voice {vi}: {v}"
        let violations := checkFirstSpecies aligned
        if violations.isEmpty then
          IO.println "  ✓ no violations"
        else
          IO.println s!"  {violations.length} violation(s):"
          for v in violations do
            IO.println s!"    [{v.rule}] voices {v.voiceA}/{v.voiceB} step {v.step}: {v.detail}"
