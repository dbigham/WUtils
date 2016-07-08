(* Tests for: WUtils`WUtils`GetVariablePossiblyFromParentPackage

   Author: danielb
*)

Test[
    WUtils`WUtils`GetVariablePossiblyFromParentPackage[
        "WUtils`WUtils`",
        "$UnitTestDir"
    ]
    ,
    {
        "CalculateParse`Private`",
        FileNameJoin[
            {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Tests", "UnitTests", "CalculateParse"}
        ]
    }
    ,
    TestID -> "GetVariablePossiblyFromParentPackage-20150202-QTOU4E"
]