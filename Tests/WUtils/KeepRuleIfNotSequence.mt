(* Tests for: WUtils`WUtils`KeepRuleIfNotSequence

   Author: danielb
*)

Test[
    ReplaceAll[
        {"a", "b"},
        {x_, Repeated[y_, {0, 1}]} :>
            {x, WUtils`WUtils`KeepRuleIfNotSequence["OptionalSecondElement" -> y]}
    ]
    ,
    {"a", "OptionalSecondElement" -> "b"}
    ,
    TestID -> "KeepRuleIfNotSequence-20150220-AZBF8C"
]

(* When the optional element isn't matched, the "OptionalSecondElement" option isn't added to the result. *)
Test[
    ReplaceAll[
        {"a"},
        {x_, Repeated[y_, {0, 1}]} :>
            {x, WUtils`WUtils`KeepRuleIfNotSequence["OptionalSecondElement" -> y]}
    ]
    ,
    {"a"}
    ,
    TestID -> "KeepRuleIfNotSequence-20150220-YE54BJ"
]