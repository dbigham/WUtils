(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`ExampleToString

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
    CalculateParse`Prototype`VirtualAssistant`Utility`ExampleToString[1 + 1, 2]
    ,
    "    1 + 1 === 2"
    ,
    TestID -> "ExampleToString-20150221-XPK6ZK"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`ExampleToString[
        CalculateParse`GeneralLibrary`SymbolToFile[CalculateParse`JavaTokenizer`JFindTokens],
        "E:\\Users\\Daniel\\WolframWorkspaces\\Base2\\Alpha\\Source\\CalculateParse\\JavaTokenizer.m"
    ]
    ,
    "    SymbolToFile[JFindTokens]\n\n    ===\n\n    \"E:\\\\Users\\\\Daniel\\\\WolframWorkspaces\\\\Base2\\\\Alpha\\\\Source\\\\CalculateParse\\\\JavaTokenizer.m\""
    ,
    TestID -> "ExampleToString-20150221-GQCO4Y"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`ExampleToString[
        ReplaceAll[
            {"a", "b"},
            {x_, Repeated[y_, {0, 1}]} :>
                {x, CalculateParse`GeneralLibrary`KeepRuleIfNotSequence["OptionalSecondElement" -> y]}
        ],
        {"a", "OptionalSecondElement" -> "b"}
    ]
    ,
    "    ReplaceAll[\n        {\"a\", \"b\"},\n        {x_, Repeated[y_, {0, 1}]} :>\n            {x, KeepRuleIfNotSequence[\"OptionalSecondElement\" -> y]}\n    ]\n\n    ===\n\n    {\"a\", \"OptionalSecondElement\" -> \"b\"}"
    ,
    TestID -> "ExampleToString-20150221-30APZ1"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`ExampleToString[
        CalculateParse`GeneralLibrary`StringPositionToLineNumber["abc\ndef", 1],
        1
    ]
    ,
    "    StringPositionToLineNumber[\"abc\\ndef\", 1] === 1"
    ,
    TestID -> "ExampleToString-20150225-ZZRL8Z"
]