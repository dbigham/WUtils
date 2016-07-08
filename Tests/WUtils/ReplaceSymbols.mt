(* Tests for: WUtils`WUtils`ReplaceSymbols

   Author: danielb
*)

Test[
    WUtils`WUtils`ReplaceSymbols[
        HoldComplete[{Code[1 + 1]}],
        {"Code" -> "CalculateParse`ParseAnalysis`Private`Code"}
    ]
    ,
    HoldComplete[{CalculateParse`ParseAnalysis`Private`Code[1 + 1]}]
    ,
    TestID -> "ReplaceSymbols-20150203-46Y8BC"
]