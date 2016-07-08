(* Tests for: WUtils`WUtils`ExampleToString

   Author: danielb
*)

Test[
    WUtils`WUtils`ExampleToString[1 + 1, 2],
    "\t1 + 1 === 2",
    TestID -> "ExampleToString-20150221-XPK6ZK"
]

Test[
    WUtils`WUtils`ExampleToString[
        ReplaceAll[
            {"a", "b"},
            {x_, Repeated[y_, {0, 1}]} :>
                {x, WUtils`WUtils`KeepRuleIfNotSequence["OptionalSecondElement" -> y]}
        ],
        {"a", "OptionalSecondElement" -> "b"}
    ],
    "\tReplaceAll[\n\t\t{\"a\", \"b\"},\n\t\t{x_, Repeated[y_, {0, 1}]} :>\n\t\t\t{x, KeepRuleIfNotSequence[\"OptionalSecondElement\" -> y]}\n\t]\n\n\t===\n\n\t{\"a\", \"OptionalSecondElement\" -> \"b\"}",
    TestID -> "ExampleToString-20150221-30APZ1"
]

Test[
    WUtils`WUtils`ExampleToString[WUtils`WUtils`StringPositionToLineNumber["abc\ndef", 1], 1],
    "\tStringPositionToLineNumber[\"abc\\ndef\", 1] === 1",
    TestID -> "ExampleToString-20150225-ZZRL8Z"
]