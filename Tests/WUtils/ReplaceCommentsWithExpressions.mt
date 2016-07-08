(* Tests for: CalculateParse`GeneralLibrary`ReplaceCommentsWithExpressions

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/ReplaceCommentsWithExpressions.mt"]
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
    CalculateParse`GeneralLibrary`ReplaceCommentsWithExpressions[""]
    ,
    ""
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-QGYTS7"
]

Test[
    CalculateParse`GeneralLibrary`ReplaceCommentsWithExpressions["a"]
    ,
    "a"
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-D30JQA"
]

Test[
    CalculateParse`GeneralLibrary`ReplaceCommentsWithExpressions[
        "(* Just testing *)\n\"a\"\n\"b\""
    ]
    ,
    "\"Comment\" -> Comment[\"Just testing\"]\n\"a\"\n\"b\""
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-NTT354"
]

(* Leave alone comments that are within strings. *)
Test[
    CalculateParse`GeneralLibrary`ReplaceCommentsWithExpressions[
        "{myFunc[\"(* my comment *)\"]}"
    ]
    ,
    "{myFunc[\"(* my comment *)\"]}"
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-99KIE6"
]