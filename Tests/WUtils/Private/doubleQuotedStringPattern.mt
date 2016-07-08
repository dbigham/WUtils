(* Tests for: WUtils`WUtils`Private`doubleQuotedStringPattern

   Author: danielb
*)

Test[
    StringCases["Just \"testing\"", WUtils`WUtils`Private`doubleQuotedStringPattern[]]
    ,
    {"\"testing\""}
    ,
    TestID -> "doubleQuotedStringPattern-20160706-JKT59R"
]

Test[
    StringCases["Just \"\\\"testing\\\"\"", WUtils`WUtils`Private`doubleQuotedStringPattern[]]
    ,
    {"\"\\\"testing\\\"\""}
    ,
    TestID -> "doubleQuotedStringPattern-20160706-FW4938"
]

Test[
    StringCases[
        "Just \"testing \\\\ again\"",
        WUtils`WUtils`Private`doubleQuotedStringPattern[]
    ]
    ,
    {"\"testing \\\\ again\""}
    ,
    TestID -> "doubleQuotedStringPattern-20160706-K8H3R1"
]

Test[
    StringCases["Just \"testing\\nagain\"", WUtils`WUtils`Private`doubleQuotedStringPattern[]]
    ,
    {"\"testing\\nagain\""}
    ,
    TestID -> "doubleQuotedStringPattern-20160706-G8DSC1"
]