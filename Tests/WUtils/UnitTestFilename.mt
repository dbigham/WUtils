(* Tests for: WUtils`WUtils`UnitTestFilename

   Author: danielb
*)

Test[
    WUtils`WUtils`UnitTestFilename[WUtils`WUtils`CouldBeWLSymbolQ],
    "C:\\Users\\Daniel\\WolframWorkspaces\\Base2\\WUtils\\Tests\\WUtils\\CouldBeWLSymbolQ.mt",
    TestID -> "UnitTestFilename-20150130-ZIQI0A"
]
Test[
    WUtils`WUtils`UnitTestFilename[WUtils`WUtils`SymbolToFile],
    "C:\\Users\\Daniel\\WolframWorkspaces\\Base2\\WUtils\\Tests\\WUtils\\SymbolToFile.mt",
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