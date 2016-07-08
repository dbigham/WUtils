(* Tests for: CalculateParse`GeneralLibrary`StringStartsWith

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
    CalculateParse`GeneralLibrary`StringStartsWith["abcdef", {"abc"}]
    ,
    True
    ,
    TestID -> "StringStartsWith-20150304-LBH5N5"
]

Test[
    CalculateParse`GeneralLibrary`StringStartsWith["abcdef", {"xyz"}]
    ,
    False
    ,
    TestID -> "StringStartsWith-20150304-YD57S2"
]

Test[
    CalculateParse`GeneralLibrary`StringStartsWith["abcdef", {"xyz", "abc"}]
    ,
    True
    ,
    TestID -> "StringStartsWith-20150304-D755C7"
]

Test[
    CalculateParse`GeneralLibrary`StringStartsWith["a", {"abc"}]
    ,
    False
    ,
    TestID -> "StringStartsWith-20150304-SLGAOD"
]