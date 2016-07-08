(* Tests for: CalculateParse`GeneralLibrary`GetVariablePossiblyFromParentPackage

   Author: danielb
*)

TestExecute[$TestAbortTime = 600]

TestExecute[
    If[TrueQ[Quiet[Get["CalculateTestEnvironment.m"]]===$Failed],
        Get[
        StringCases[$CurrentFile,
        inputfile:(StartOfString~~___~~$PathnameSeparator~~"Tests"~~$PathnameSeparator)~~___
        :> inputfile<>"Utilities"<>$PathnameSeparator<>"CalculateTestEnvironment.m"][[1]]
        ]]
]

TestExecute[$CalculateDataPacletsInit = False;  << "CalculateLoader`"]

TestExecute[$TestAbortTime = $TestAbortTimeInitial]

Test[
    CalculateParse`GeneralLibrary`GetVariablePossiblyFromParentPackage[
        "CalculateParse`GeneralLibrary`",
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