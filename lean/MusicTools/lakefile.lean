import Lake
open Lake DSL

package «MusicTools» {
  -- add any package configuration options here
}

lean_lib «Pitch»
lean_lib «Interval»
lean_lib «Music»
lean_lib «Expr»
lean_lib «Xml»
lean_lib «Counterpoint»
lean_lib «SecondSpecies»
lean_lib «ThirdSpecies»
lean_lib «FourthSpecies»
lean_lib «FifthSpecies»
lean_lib «Solver»
lean_lib «ViolationJson»

lean_exe «musescore-check» where
  root := `MuseScoreMain

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

@[default_target]
lean_lib «MusicTools» {
  -- add any library configuration options here
}
