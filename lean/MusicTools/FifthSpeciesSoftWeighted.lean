import Pitch
import Interval
import SecondSpeciesSoftWeighted
import ThirdSpeciesSoftWeighted
import FourthSpeciesSoftWeighted
open Pitch
open Interval

namespace FifthSpeciesSoftWeighted

/-!
  Fifth species (florid counterpoint) is a free mixture of first through fourth
  species. Soft-weight scoring combines criteria from species 2, 3, and 4.
  
  First species has no soft weights (only hard constraints).
  Fifth species applies the appropriate soft weights depending on the
  rhythmic pattern used at each point.
-/

-- Combined scoring for fifth species: all species 2-4 soft weights.
def scoreFifthSpecies (cf cp : List Pitch.Pitch) : Int :=
  SecondSpeciesSoftWeighted.scoreSecondSpecies cf cp +
  ThirdSpeciesSoftWeighted.scoreThirdSpecies cf cp +
  FourthSpeciesSoftWeighted.scoreFourthSpecies cf cp

end FifthSpeciesSoftWeighted
