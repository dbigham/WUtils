(* Tests for: WUtils`WUtils`SymbolToFile

   Ignored by QA because I don't think they have Alpha on the
   $Path, rendering FindFile useless.

   Author: danielb
*)

Test[
    WUtils`WUtils`SymbolToFile[WUtils`WUtils`SymbolToFile]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "SymbolToFile-20150129-4RR5VA"
]

Test[
    WUtils`WUtils`SymbolToFile[WUtils`WUtils`Private`toSingleLine]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "SymbolToFile-20150129-KVX5GJ"
]

Test[
    WUtils`WUtils`SymbolToFile[CalculateParse`ParseAnalysis`ChartPrint]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "ParseAnalysis.m"
    ]
    ,
    TestID -> "SymbolToFile-20150129-LA8E4C"
]