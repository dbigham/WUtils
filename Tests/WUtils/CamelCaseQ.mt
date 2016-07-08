(* Tests for: CalculateParse`GeneralLibrary`CamelCaseQ

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
    CalculateParse`GeneralLibrary`CamelCaseQ["JustTesting"]
    ,
    True
    ,
    TestID -> "CamelCaseQ-20160119-1PEPMU"
]

Test[
    CalculateParse`GeneralLibrary`CamelCaseQ["Just Testing"]
    ,
    False
    ,
    TestID -> "CamelCaseQ-20160119-8MWSHW"
]

Test[
    CalculateParse`GeneralLibrary`CamelCaseQ["justtesting"]
    ,
    False
    ,
    TestID -> "CamelCaseQ-20160119-K08SQ6"
]

Test[
    CalculateParse`GeneralLibrary`CamelCaseQ["justTesting"]
    ,
    True
    ,
    TestID -> "CamelCaseQ-20160119-E87GI1"
]