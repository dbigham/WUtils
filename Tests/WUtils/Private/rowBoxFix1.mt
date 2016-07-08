(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`rowBoxFix1

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
    CalculateParse`GeneralLibrary`Private`rowBoxFix1[RowBox[{"b"}]]
    ,
    "<<RowBox>>"[{"b"}]
    ,
    TestID -> "rowBoxFix1-20150225-F3WIBY"
]

Test[
    CalculateParse`GeneralLibrary`Private`rowBoxFix1["<<RowBox>>"[{"b"}]]
    ,
    "<<<RowBox>>>"[{"b"}]
    ,
    TestID -> "rowBoxFix1-20150225-1LCYRW"
]

Test[
    CalculateParse`GeneralLibrary`Private`rowBoxFix1[
        RowBox[{RowBox[{"1", "2", "3"}]}]
    ]
    ,
    "<<RowBox>>"[{"<<RowBox>>"[{"1", "2", "3"}]}]
    ,
    TestID -> "rowBoxFix1-20150226-WCXNHH"
]