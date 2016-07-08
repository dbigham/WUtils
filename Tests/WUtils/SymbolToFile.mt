(* Tests for: CalculateParse`GeneralLibrary`SymbolToFile

   Ignored by QA because I don't think they have Alpha on the
   $Path, rendering FindFile useless.

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
    CalculateParse`GeneralLibrary`SymbolToFile[CalculateParse`GeneralLibrary`SymbolToFile]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "SymbolToFile-20150129-4RR5VA"
]

Test[
    CalculateParse`GeneralLibrary`SymbolToFile[CalculateParse`GeneralLibrary`Private`toSingleLine]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "SymbolToFile-20150129-KVX5GJ"
]

Test[
    CalculateParse`GeneralLibrary`SymbolToFile[CalculateParse`ParseAnalysis`ChartPrint]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "ParseAnalysis.m"
    ]
    ,
    TestID -> "SymbolToFile-20150129-LA8E4C"
]