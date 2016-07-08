(* Tests for: CalculateParse`Prototype`VirtualAssistant`VaSemantics`NewSemanticExpr

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
    CalculateParse`Prototype`VirtualAssistant`VaSemantics`NewSemanticExpr[
        "Notebook",
        {"Name" -> "NewSemanticExpr", "Path" -> "..."}
    ]
    ,
    CalculateParse`Prototype`VirtualAssistant`VaSemantics`S["Notebook"][
        "Name" -> "NewSemanticExpr",
        "Path" -> "..."
    ]
    ,
    TestID -> "NewSemanticExpr-20160102-EKKV6J"
]

Test[
    CalculateParse`GeneralLibrary`CapturePrint[
        CalculateParse`Prototype`VirtualAssistant`VaSemantics`NewSemanticExpr[
            "Notebook",
            {"BadArg" -> 1}
        ]
    ]
    ,
    {
        {
            "Unknown argument '",
            "BadArg",
            "' for concept '",
            "Notebook",
            "'. Known arguments:\n",
            "    {\"Name\" -> _String, \"Path\" -> _String}"
        }
    }
    ,
    TestID -> "NewSemanticExpr-20160102-C738I5"
]