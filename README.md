# MusicTools

This is a rough Lean port of John Leo's constraint-based counterpoint checker and solver, originally written in Agda.

It uses a hand-coded brute force solver to check constraints. lean/MusicTools/Proofs contains some proofs that the solver matches the spec.
It's intended to be used as a MuseScore plugin. There, you can:
- Globally check your counterpoint for issues
- Generate counterpoint to fill in your selection
- Specify soft constraints (e.g. "Contrary motion gives you 10 points.", "Big jumps give you -10 points")

## Setup:
Build the lean file:
cd lean/MusicTools && lake build

To get the plugin running in Musescore, just link it to your MuseScore repo:
ln -s Musescore/musictools.qml ~/Documents/MuseScore4/Plugins

Then open MuseScore. In the top bar run Plugins>Manage plugins. musictools should be on the bottom of the page.

## Usage:
Plugins>musictools will show the main window. You can:
Hit Check to color any wrong notes in red
Select notes and hit Solve to treat them as unconstrained. This will change the notes to maximize the soft constraints within the limits set by the hard constraints.
