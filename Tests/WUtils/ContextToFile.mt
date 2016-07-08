(* Tests for: WUtils`WUtils`ContextToFile

   Ignored by QA because I don't think they have Alpha on the
   $Path, rendering FindFile useless.

   Author: danielb
*)

Test[
    WUtils`WUtils`ContextToFile["WUtils`WUtils`"]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "ContextToFile-20150126-QUFW1C"
]

Test[
    WUtils`WUtils`ContextToFile["WUtils`WUtils`Private`"]
    ,
    ToFileName[
        {CalculateScan`CommonSymbols`$AlphaRootDirectory, "Source", "CalculateParse"},
        "GeneralLibrary.m"
    ]
    ,
    TestID -> "ContextToFile-20150126-42UYIP"
]

(* My comment *)
Test[
    WUtils`WUtils`ContextToFile["CalculateParse`JavaTokenizer`"]
    ,
    FileNameJoin[
        {
            CalculateScan`CommonSymbols`$AlphaRootDirectory,
            "Source",
            "CalculateParse",
            "JavaTokenizer.m"
        }
    ]
    ,
    TestID -> "ContextToFile-20150126-AUFW2X"
]
