import Counterpoint

namespace ViolationJson

private def escapeJson (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
      | '"'  => "\\\""
      | '\\' => "\\\\"
      | '\n' => "\\n"
      | '\r' => "\\r"
      | '\t' => "\\t"
      | _    => String.singleton c) ""

def violationToJson (partIdx : Nat) (v : Counterpoint.Violation) : String :=
  "{\"part\":"    ++ toString partIdx  ++
  ",\"voiceA\":"  ++ toString v.voiceA ++
  ",\"voiceB\":"  ++ toString v.voiceB ++
  ",\"step\":"    ++ toString v.step   ++
  ",\"rule\":\""  ++ escapeJson v.rule   ++ "\"" ++
  ",\"detail\":\"" ++ escapeJson v.detail ++ "\"}"

def violationsToJson (pairs : List (Nat × Counterpoint.Violation)) : String :=
  "[" ++ String.intercalate "," (pairs.map fun (pi, v) => violationToJson pi v) ++ "]"

end ViolationJson
