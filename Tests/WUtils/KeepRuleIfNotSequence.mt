(* Tests for: CalculateParse`GeneralLibrary`KeepRuleIfNotSequence

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/KeepRuleIfNotSequence.mt"]
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

(* When the optional element is matched, it results in a "OptionalSecondElement" option in the reuslt. *)
Test[
    ReplaceAll[
        {"a", "b"},
        {x_, Repeated[y_, {0, 1}]} :>
            {x, CalculateParse`GeneralLibrary`KeepRuleIfNotSequence["OptionalSecondElement" -> y]}
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
            {x, CalculateParse`GeneralLibrary`KeepRuleIfNotSequence["OptionalSecondElement" -> y]}
    ]
    ,
    {"a"}
    ,
    TestID -> "KeepRuleIfNotSequence-20150220-YE54BJ"
]