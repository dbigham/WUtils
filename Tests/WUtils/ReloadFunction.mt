(* Tests for: WUtils`WUtils`ReloadFunction

   Author: danielb
*)

Test[
    WUtils`WUtils`ReloadFunction[funcSymbol]
    ,
    Missing[]
    ,
    TestID -> "ReloadFunction-20150202-RG63HI"
]

Test[
    WUtils`WUtils`ReloadFunction[
        WUtils`WUtils`SymbolToFile
    ]
    ,
    CalculateParse`DeveloperFunctions`ReloadParserFiles
    ,
    TestID -> "ReloadFunction-20150202-8OTCQD"
]