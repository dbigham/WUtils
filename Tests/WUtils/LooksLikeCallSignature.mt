(* Tests for: CalculateParse`Prototype`VirtualAssistant`VaActions`LooksLikeCallSignature

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/VaActions/LooksLikeCallSignature.mt"]
   ]
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
    LooksLikeCallSignature[HoldComplete[CouldBeWLSymbolQ["test"]], CouldBeWLSymbolQ]
    ,
    False
    ,
    TestID -> "LooksLikeCallSignature-20150126-P5GM8X"
]

Test[
    LooksLikeCallSignature[HoldComplete[CouldBeWLSymbolQ[str]], CouldBeWLSymbolQ]
    ,
    True
    ,
    TestID -> "LooksLikeCallSignature-20150126-KG8LLB"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`LooksLikeCallSignature[
        HoldComplete[
            CalculateParse`GeneralLibrary`GetVariablePossiblyFromParentPackage[context, symbolName]
        ],
        CalculateParse`GeneralLibrary`GetVariablePossiblyFromParentPackage
    ]
    ,
    True
    ,
    TestID -> "LooksLikeCallSignature-20150202-8FT3MD"
]