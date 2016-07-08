(* Tests for: WUtils`WUtils`ReplaceSymbolsUsingPatterns

   Author: danielb
*)

Test[
    WUtils`WUtils`ReplaceSymbolsUsingPatterns[
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
    WUtils`WUtils`ReplaceSymbolsUsingPatterns[
        {justTesting$},
        {Daniel`Tools`var__~~"$" :> Daniel`Tools`var}
    ]
    ,
    {justTesting}
    ,
    TestID -> "ReplaceSymbolsUsingPatterns-20150203-56QJUN"
]