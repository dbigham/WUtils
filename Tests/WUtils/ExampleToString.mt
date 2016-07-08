(* Tests for: WUtils`WUtils`ExampleToString

   Author: danielb
*)

Test[
    WUtils`WUtils`ExampleToString[1 + 1, 2]
    ,
    "    1 + 1 === 2"
    ,
    TestID -> "ExampleToString-20150221-XPK6ZK"
]

Test[
    WUtils`WUtils`ExampleToString[
        WUtils`WUtils`SymbolToFile[CalculateParse`JavaTokenizer`JFindTokens],
        "E:\\Users\\Daniel\\WolframWorkspaces\\Base2\\Alpha\\Source\\CalculateParse\\JavaTokenizer.m"
    ]
    ,
    "    SymbolToFile[JFindTokens]\n\n    ===\n\n    \"E:\\\\Users\\\\Daniel\\\\WolframWorkspaces\\\\Base2\\\\Alpha\\\\Source\\\\CalculateParse\\\\JavaTokenizer.m\""
    ,
    TestID -> "ExampleToString-20150221-GQCO4Y"
]

Test[
    WUtils`WUtils`ExampleToString[
        ReplaceAll[
            {"a", "b"},
            {x_, Repeated[y_, {0, 1}]} :>
                {x, WUtils`WUtils`KeepRuleIfNotSequence["OptionalSecondElement" -> y]}
        ],
        {"a", "OptionalSecondElement" -> "b"}
    ]
    ,
    "    ReplaceAll[\n        {\"a\", \"b\"},\n        {x_, Repeated[y_, {0, 1}]} :>\n            {x, KeepRuleIfNotSequence[\"OptionalSecondElement\" -> y]}\n    ]\n\n    ===\n\n    {\"a\", \"OptionalSecondElement\" -> \"b\"}"
    ,
    TestID -> "ExampleToString-20150221-30APZ1"
]

Test[
    WUtils`WUtils`ExampleToString[
        WUtils`WUtils`StringPositionToLineNumber["abc\ndef", 1],
        1
    ]
    ,
    "    StringPositionToLineNumber[\"abc\\ndef\", 1] === 1"
    ,
    TestID -> "ExampleToString-20150225-ZZRL8Z"
]