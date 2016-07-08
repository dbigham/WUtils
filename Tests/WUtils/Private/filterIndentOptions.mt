(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateUtilities`DebuggingUtilities`Private`filterIndentOptions

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/Private/filterIndentOptions.mt"]
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
    CalculateParse`GeneralLibrary`Private`filterIndentOptions[
        "FullFormStrings" -> True,
        "SomeOtherOptions" -> False
    ]
    ,
    {"FullFormStrings" -> True}
    ,
    TestID -> "filterIndentOptions-20150131-3QWWZ3"
]