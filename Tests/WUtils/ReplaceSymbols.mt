(* Tests for: CalculateParse`GeneralLibrary`ReplaceSymbols

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
    CalculateParse`GeneralLibrary`ReplaceSymbols[
        HoldComplete[{Code[1 + 1]}],
        {"Code" -> "CalculateParse`ParseAnalysis`Private`Code"}
    ]
    ,
    HoldComplete[{CalculateParse`ParseAnalysis`Private`Code[1 + 1]}]
    ,
    TestID -> "ReplaceSymbols-20150203-46Y8BC"
]