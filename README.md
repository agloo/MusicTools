# MusicTools

This is a rough Lean port of John Leo's constraint-based counterpoint checker and solver, originally written in Agda.

It uses a hand-coded brute force solver to check constraints. lean/MusicTools/Proofs contains some proofs that the solver matches the spec.
It's intended to be used as a MuseScore plugin. There, you can:
- Globally check your counterpoint for issues
- Generate counterpoint to fill in your selection
- Specify soft constraints (e.g. "Contrary motion gives you 10 points.", "Big jumps give you -10 points")

