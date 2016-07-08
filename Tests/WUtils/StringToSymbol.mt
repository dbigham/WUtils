(* Tests for: WUtils`WUtils`StringToSymbol

   Author: danielb
*)

Test[
    Block[
        {$ContextPath = {"System`"}},
        WUtils`WUtils`StringToSymbol["toSingleLine"]
    ]
    ,
    $Failed
    ,
    TestID -> "StringToSymbol-20150131-E1O94G"
]

Test[
    Block[
        {$ContextPath = {"WUtils`WUtils`", "System`"}},
        WUtils`WUtils`StringToSymbol["toSingleLine"]
    ]
    ,
    WUtils`WUtils`Private`toSingleLine
    ,
    TestID -> "StringToSymbol-20150131-09D87A"
]

Test[
    Block[
        {$ContextPath = {"WUtils`WUtils`", "System`"}},
        WUtils`WUtils`StringToSymbol["CreateIssueNotebook"]
    ]
    ,
    WUtils`WUtils`CreateIssueNotebook
    ,
    TestID -> "StringToSymbol-20150131-7VZKZT"
]