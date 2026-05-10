import Xml
import Counterpoint
import ViolationJson

-- Helpers duplicated from Main.lean to keep this executable self-contained.

private def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let ok ← filename.pathExists
  if not ok then pure none
  else
    let h ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle h))

-- Read the full stream, silently dropping XML declaration/DOCTYPE lines.
private partial def readStream (s : IO.FS.Stream) : IO String := do
  let l ← s.getLine
  if l.isEmpty then pure ""
  else
    if l.startsWith "<?" || l.startsWith "<!" then readStream s
    else (l ++ ·) <$> readStream s

-- Drop voices that are entirely rests, then keep only time slots where every
-- remaining voice has a pitch.
private def alignVoices (vs : List (List (Option Pitch.Pitch))) : List (List Pitch.Pitch) :=
  let nonEmpty := vs.filter fun v => v.any Option.isSome
  match nonEmpty with
  | []     => []
  | v :: _ =>
      let slots := List.range v.length
      let keep  := slots.filter fun s =>
        nonEmpty.all fun w => (w[s]?).bind id |>.isSome
      nonEmpty.map fun w => keep.filterMap fun s => (w[s]?).bind id

def main (args : List String) : IO Unit := do
  let path : System.FilePath ← match args with
    | [p] => pure p
    | _   => throw (IO.userError "usage: musescore-check <file.musicxml>")
  match ← fileStream path with
  | none   => throw (IO.userError s!"file not found: {path}")
  | some s =>
      let txt ← readStream s
      match Xml.parse txt with
      | .error e => throw (IO.userError s!"parse error: {e}")
      | .ok root =>
          let parts := Xml.children "part" root
          let pairs : List (Nat × Counterpoint.Violation) :=
            parts.zipIdx.flatMap fun (p, pi) =>
              let aligned := alignVoices (Xml.extractVoicesByTag p)
              (Counterpoint.checkFirstSpecies aligned).map (pi, ·)
          IO.println (ViolationJson.violationsToJson pairs)
