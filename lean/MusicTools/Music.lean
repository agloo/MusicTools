import Pitch

namespace Music

inductive MPitch where
  | known : Pitch.Pitch → MPitch
  | var   : String → MPitch

abbrev Score := List (List MPitch)

end Music
