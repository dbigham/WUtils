(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`UnitTestFilename

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
    CalculateParse`Prototype`VirtualAssistant`Utility`UnitTestFilename[
        CalculateParse`Prototype`VirtualAssistant`Utility`CouldBeWLSymbolQ
    ]
    ,
    ToFileName[
        {
            CalculateScan`CommonSymbols`$AlphaRootDirectory,
            "Source",
            "CalculateParse",
            "Prototype",
            "VirtualAssistant",
            "Tests",
            "UnitTests",
            "Utility"
        },
        "CouldBeWLSymbolQ.mt"
    ]
    ,
    TestID -> "UnitTestFilename-20150130-ZIQI0A"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`UnitTestFilename[
        CalculateParse`GeneralLibrary`SymbolToFile
    ]
    ,
    ToFileName[
        {
            CalculateScan`CommonSymbols`$AlphaRootDirectory,
            "Tests",
            "UnitTests",
            "CalculateParse",
            "GeneralLibrary",
            "NonSQARun"
        },
        "SymbolToFile.mt"
    ]
    ,
    TestID -> "UnitTestFilename-20150130-YP8EHY"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`UnitTestFilename[
        CalculateParse`JavaTokenizer`JFindTokens
    ]
    ,
    ToFileName[
        {
            CalculateScan`CommonSymbols`$AlphaRootDirectory,
            "Tests",
            "UnitTests",
            "CalculateParse",
            "JavaTokenizer"
        },
        "JFindTokens.mt"
    ]
    ,
    TestID -> "UnitTestFilename-20150402-UQ33PV"
]