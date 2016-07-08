(* Tests for: WUtils`WUtils`EnsureDefined

   Author: danielb
*)

Test[
    WUtils`WUtils`CapturePrint[
        WUtils`WUtils`EnsureDefined[
            myUndefinedVar,
            "Description of variable goes here."
        ]
    ]
    ,
    {
        {
            "The variable ",
            "Global`myUndefinedVar",
            " must be defined. ",
            "Description of variable goes here."
        }
    }
    ,
    TestID -> "EnsureDefined-20150226-IE319E"
]

Test[
    Block[
        {Print = Null},
        WUtils`WUtils`EnsureDefined[
            myUndefinedVar,
            "Description of variable goes here."
        ]
    ]
    ,
    $Failed
    ,
    TestID -> "EnsureDefined-20150226-8W49F8"
]