(* Tests for: CalculateParse`GeneralLibrary`ReloadFunction

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
    CalculateParse`GeneralLibrary`ReloadFunction[funcSymbol]
    ,
    Missing[]
    ,
    TestID -> "ReloadFunction-20150202-RG63HI"
]

Test[
    CalculateParse`GeneralLibrary`ReloadFunction[
        CalculateParse`GeneralLibrary`SymbolToFile
    ]
    ,
    CalculateParse`DeveloperFunctions`ReloadParserFiles
    ,
    TestID -> "ReloadFunction-20150202-8OTCQD"
]