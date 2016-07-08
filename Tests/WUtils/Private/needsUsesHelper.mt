(* Tests for: WUtils`WUtils`Private`needsUsesHelper

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`needsUsesHelper[{HoldComplete[Needs["MyPackage`"]]}]
    ,
    {"MyPackage`"}
    ,
    TestID -> "needsUsesHelper-20150304-EI6173"
]

Test[
    WUtils`WUtils`Private`needsUsesHelper[
        {HoldComplete[Needs["MyPackage1`"]], HoldComplete[Needs["MyPackage2`"]]}
    ]
    ,
    {"MyPackage1`", "MyPackage2`"}
    ,
    TestID -> "needsUsesHelper-20150304-546X76"
]

Test[
    WUtils`WUtils`Private`needsUsesHelper[
        {
            HoldComplete[Needs["MyPackage1`"]],
            HoldComplete[Needs["MyPackage2`"]],
            HoldComplete[Needs["MyPackage2`"]]
        }
    ]
    ,
    {"MyPackage1`", "MyPackage2`"}
    ,
    TestID -> "needsUsesHelper-20150304-MFBNYN"
]

Test[
    WUtils`WUtils`Private`needsUsesHelper[{HoldComplete[1 + 1]}]
    ,
    {}
    ,
    TestID -> "needsUsesHelper-20150304-2YKCZ1"
]