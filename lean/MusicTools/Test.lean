import Counterpoint
open Counterpoint

-- Two voices.
-- Bottom: C4 D4 E4 D4 = 60 62 64 62
-- Top:    G4 A4 G4 F4 = 67 69 67 65
-- Vertical Upi: 7 7 3 3 → P5 P5 m3 m3
-- Expected: vertical-interval clean (all in set), max-interval clean,
--          direct-into-perfect: violation at step 1 (parallel P5 → P5),
--          start-end: start P5 (perfect, ok), end m3 (NOT perfect → violation).

def lower : List Pitch.Pitch := [60, 62, 64, 62]
def upper : List Pitch.Pitch := [67, 69, 67, 65]

#eval checkFirstSpecies [lower, upper]

-- Sanity case 2: a clean two-voice 1st species fragment.
-- Bottom: C4 D4 E4 D4 C4 = 60 62 64 62 60
-- Top:    E4 D4 C4 D4 E4 = 64 62 60 62 64  (mirror — produces contrary motion)
-- Verticals: 4 0 4 0 4 → M3 P1 M3 P1 M3
-- Start M3 not perfect → start-perfect violation.
-- End M3 not perfect → end-perfect violation.
-- The parallel-fifth issue gone.
#eval checkFirstSpecies [[60, 62, 64, 62, 60], [64, 62, 60, 62, 64]]

-- Sanity case 3: P1 → P8 is direct motion into perfect (similar motion).
-- Bottom: C4=60 → C5=72  (up 12)
-- Top:    C5=72 → G5=79  (up 7)
-- Both up, different sizes → similar; target upi = 79-72 = 7 = P5.
#eval checkFirstSpecies [[60, 72], [72, 79]]
