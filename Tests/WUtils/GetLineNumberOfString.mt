(* Tests for: CalculateParse`GeneralLibrary`GetLineNumberOfString

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
    CalculateParse`GeneralLibrary`GetLineNumberOfString["abc", "b"]
    ,
    1
    ,
    TestID -> "GetLineNumberOfString-20150202-VBVSFC"
]

Test[
    CalculateParse`GeneralLibrary`GetLineNumberOfString["abc", "x"]
    ,
    None
    ,
    TestID -> "GetLineNumberOfString-20150202-HWTR17"
]

Test[
    CalculateParse`GeneralLibrary`GetLineNumberOfString["abc\ndef", "e"]
    ,
    2
    ,
    TestID -> "GetLineNumberOfString-20150202-6JF6KP"
]