(* Tests for: CalculateParse`GeneralLibrary`StringTakeByDelim

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/StringTakeByDelim.mt"]
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
    CalculateParse`GeneralLibrary`StringTakeByDelim[
        "CalculateParse`GeneralLibrary`Private`MyNewFunc",
        "`",
        -1
    ]
    ,
    "MyNewFunc"
    ,
    TestID -> "StringTakeByDelim-20150201-O7COPZ"
]