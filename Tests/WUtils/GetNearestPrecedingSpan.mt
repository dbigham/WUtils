(* Tests for: CalculateParse`GeneralLibrary`GetNearestPrecedingSpan

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
    CalculateParse`GeneralLibrary`GetNearestPrecedingSpan[
        {10, 20},
        {{1, 3}, {4, 8}, {12, 14}, {23, 25}, {33, 55}}
    ]
    ,
    {4, 8}
    ,
    TestID -> "GetNearestPrecedingSpan-20160527-IZ2RPP"
]