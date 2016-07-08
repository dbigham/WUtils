(* Tests for: WUtils`WUtils`EvaluateAndGetMessages

   Author: danielb
*)

Test[
    WUtils`WUtils`EvaluateAndGetMessages[1/0]
    ,
    {ComplexInfinity, {Hold[Power::infy]}}
    ,
    Power::infy
    ,
    TestID -> "EvaluateAndGetMessages-20160613-RXS6UA"
]

Test[
    WUtils`WUtils`EvaluateAndGetMessages[1 + 1]
    ,
    {2, {}}
    ,
    TestID -> "EvaluateAndGetMessages-20160613-WR8TKI"
]