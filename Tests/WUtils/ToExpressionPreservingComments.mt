(* Tests for: CalculateParse`GeneralLibrary`ToExpressionPreservingComments

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
    CalculateParse`GeneralLibrary`ToExpressionPreservingComments[
        "(* Just testing *)\n\"a\"\n\"b\""
    ]
    ,
    HoldComplete[{"Comment" -> Comment["Just testing"], "a", "b"}]
    ,
    TestID -> "ToExpressionPreservingComments-20150226-C9R0EI"
]

Test[
    CalculateParse`GeneralLibrary`ToExpressionPreservingComments[
        "(* Just testing *)\n1 + 1\n2 + 2"
    ]
    ,
    HoldComplete[{"Comment" -> Comment["Just testing"], 2, 4}]
    ,
    TestID -> "ToExpressionPreservingComments-20150226-3XFFVP"
]

Test[
    CalculateParse`GeneralLibrary`ToExpressionPreservingComments[
        "(* Just testing *)\n1 + 1\n2 + 2",
        "HoldResult" -> True
    ]
    ,
    HoldComplete[{"Comment" -> Comment["Just testing"], 1 + 1, 2 + 2}]
    ,
    TestID -> "ToExpressionPreservingComments-20150226-SM7I1G"
]

Test[
    CalculateParse`GeneralLibrary`ToExpressionPreservingComments[
        "myFunc[\"(* my comment *)\"]",
        "HoldResult" -> True
    ]
    ,
    HoldComplete[{myFunc["(* my comment *)"]}]
    ,
    TestID -> "ToExpressionPreservingComments-20150226-65N49A"
]

Test[
    CalculateParse`GeneralLibrary`ToExpressionPreservingComments[
        "<< CalculateParse`Prototype`VirtualAssistant`\nmyFunc[1, 2, 3]",
        "HoldResult" -> True
    ]
    ,
    HoldComplete[{<< "CalculateParse`Prototype`VirtualAssistant`", myFunc[1, 2, 3]}]
    ,
    TestID -> "ToExpressionPreservingComments-20150226-JFLAUB"
]

Test[
    CalculateParse`GeneralLibrary`ToExpressionPreservingComments[
        "{\n(* Just testing *)\n\"a\",\n\"b\"\n}",
        "AppendCommas" -> True
    ]
    ,
    HoldComplete[{{"Comment" -> Comment["Just testing"], "a", "b"}}]
    ,
    TestID -> "ToExpressionPreservingComments-20150227-XYQO0P"
]