(* Tests for: WUtils`WUtils`UnitTestFilename

   Author: danielb
*)

Test[
    WUtils`WUtils`UnitTestFilename[
        WUtils`WUtils`CouldBeWLSymbolQ
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
    WUtils`WUtils`UnitTestFilename[
        WUtils`WUtils`SymbolToFile
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
    WUtils`WUtils`UnitTestFilename[
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