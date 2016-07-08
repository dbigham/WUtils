(* Tests for: CalculateParse`GeneralLibrary`ContextToFile

   Ignored by QA because I don't think they have Alpha on the
   $Path, rendering FindFile useless.

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/NonSQARun/ContextToFile.mt"]
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
    CalculateParse`GeneralLibrary`ContextToFile["CalculateParse`GeneralLibrary`"]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "ContextToFile-20150126-QUFW1C"
]

Test[
    CalculateParse`GeneralLibrary`ContextToFile["CalculateParse`GeneralLibrary`Private`"]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "ContextToFile-20150126-42UYIP"
]

(* My comment *)
Test[
    CalculateParse`GeneralLibrary`ContextToFile["CalculateParse`JavaTokenizer`"]
    ,
    FileNameJoin[
        {
            CalculateScan`CommonSymbols`$AlphaRootDirectory,
            "Source",
            "CalculateParse",
            "JavaTokenizer.m"
        }
    ]
    ,
    TestID -> "ContextToFile-20150126-AUFW2X"
]
