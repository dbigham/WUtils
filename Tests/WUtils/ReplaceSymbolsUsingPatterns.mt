(* Tests for: CalculateParse`GeneralLibrary`ReplaceSymbolsUsingPatterns

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
    CalculateParse`GeneralLibrary`ReplaceSymbolsUsingPatterns[
        HoldComplete[{bindingAddedDynamicallyByGetOptionsPatternBinding1410$}],
        {
            RegularExpression["bindingAddedDynamicallyByGetOptionsPatternBinding.+"] ->
                "bindingAddedDynamicallyByGetOptionsPatternBinding"
        }
    ]
    ,
    HoldComplete[{bindingAddedDynamicallyByGetOptionsPatternBinding}]
    ,
    TestID -> "ReplaceSymbolsUsingPatterns-20150203-CV9G0P"
]

Test[
    CalculateParse`GeneralLibrary`ReplaceSymbolsUsingPatterns[
        {justTesting$},
        {Daniel`Tools`var__~~"$" :> Daniel`Tools`var}
    ]
    ,
    {justTesting}
    ,
    TestID -> "ReplaceSymbolsUsingPatterns-20150203-56QJUN"
]