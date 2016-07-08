(* Tests for: CalculateParse`GeneralLibrary`UnitTestDirectory

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/NonSQARun/UnitTestDirectory.mt"]
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
    CalculateParse`GeneralLibrary`UnitTestDirectory["CalculateParse`LexiconLookup2`"]
    ,
    FileNameJoin[
        {
            CalculateScan`CommonSymbols`$AlphaRootDirectory,
            "Tests",
            "UnitTests",
            "CalculateParse",
            "LexiconLookup2"
        }
    ]
    ,
    TestID -> "UnitTestDirectory-20150202-P8UHW2"
]

Test[
    CalculateParse`GeneralLibrary`UnitTestDirectory[
        "CalculateParse`Prototype`VirtualAssistant`VaActions`Private`"
    ],
    "E:\\Users\\Daniel\\WolframWorkspaces\\Base2\\AlphaSource\\CalculateParse\\Prototype\\VirtualAssistant\\Tests\\UnitTests\\VaActions\\Private",
    TestID -> "UnitTestDirectory-20150202-WY4M5S"
]