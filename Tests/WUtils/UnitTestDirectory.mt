(* Tests for: WUtils`WUtils`UnitTestDirectory

   Author: danielb
*)

Test[
    WUtils`WUtils`UnitTestDirectory["CalculateParse`LexiconLookup2`"]
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
    WUtils`WUtils`UnitTestDirectory[
        "WUtils`WUtils`Private`"
    ],
    "E:\\Users\\Daniel\\WolframWorkspaces\\Base2\\AlphaSource\\CalculateParse\\Prototype\\VirtualAssistant\\Tests\\UnitTests\\VaActions\\Private",
    TestID -> "UnitTestDirectory-20150202-WY4M5S"
]