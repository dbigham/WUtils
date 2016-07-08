(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`rowBoxFix2

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/Private/rowBoxFix2.mt"]
   ]
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
    CalculateParse`GeneralLibrary`Private`rowBoxFix2["\"<<RowBox>>\"[{\"b\"}]"]
    ,
    "RowBox[{\"b\"}]"
    ,
    TestID -> "rowBoxFix2-20150225-A5KL34"
]

Test[
    CalculateParse`GeneralLibrary`Private`rowBoxFix2["\"<<<RowBox>>>\"[{\"b\"}]"]
    ,
    "\"<<RowBox>>\"[{\"b\"}]"
    ,
    TestID -> "rowBoxFix2-20150226-M85EFP"
]