(* Tests for: WUtils`WUtils`Private`commentPattern

   Author: danielb
*)

Test[
    StringCases[
        "(* just testing *)",
        WUtils`WUtils`Private`commentPattern[]
    ]
    ,
    {"(* just testing *)"}
    ,
    TestID -> "commentPattern-20160706-6N1ZO1"
]

(* Not smart enough to handle nested comments. *)
Test[
    StringCases[
        "(* just (* inner *) testing *)",
        WUtils`WUtils`Private`commentPattern[]
    ]
    ,
    {"(* just (* inner *)"}
    ,
    TestID -> "commentPattern-20160706-6N1ZO2"
]