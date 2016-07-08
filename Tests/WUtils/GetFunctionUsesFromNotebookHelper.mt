(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper

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

(* Ensure that the Indent2 is not included as part of the function use, since it was likely there for formatting purposes. *)
Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
        myFunc,
        {{HoldComplete[CalculateParse`GeneralLibrary`Indent2[myFunc[1, 2, 3]]], CellObject[1]}}
    ]
    ,
    {
        CalculateParse`Prototype`VirtualAssistant`Utility`FunctionUse[
            "myFunc[1, 2, 3]",
            "CellObject" -> CellObject[1]
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-V7GNAK"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
        myFunc,
        {{HoldComplete[myFunc[1, 2, 3]], CellObject[1]}}
    ]
    ,
    {
        CalculateParse`Prototype`VirtualAssistant`Utility`FunctionUse[
            myFunc[1, 2, 3],
            "CellObject" -> CellObject[1]
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-XFPKK5"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
        myFunc,
        {
            {{HoldComplete["Comment"["Test comment"]], HoldComplete[myFunc[1, 2, 3]]}, CellObject[1]}
        }
    ]
    ,
    {
        CalculateParse`Prototype`VirtualAssistant`Utility`FunctionUse[
            HoldComplete[myFunc[1, 2, 3]],
            "CellObject" -> CellObject[1],
            "Comment" -> "Test comment"
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-BR1XLU"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
        CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper,
        {
            {
                {
                    HoldComplete[
                        "Comment"[
                            "Ensure that the Indent2 is not included as part of the function use, since it was likely there for formatting purposes."
                        ]
                    ],
                    HoldComplete[
                        CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
                            myFunc,
                            {{HoldComplete[CalculateParse`GeneralLibrary`Indent2[myFunc[1, 2, 3]]]}}
                        ]
                    ]
                },
                CellObject[1]
            }
        }
    ]
    ,
    {
        CalculateParse`Prototype`VirtualAssistant`Utility`FunctionUse[
            HoldComplete[
                CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
                    myFunc,
                    {{HoldComplete[CalculateParse`GeneralLibrary`Indent2[myFunc[1, 2, 3]]]}}
                ]
            ],
            "CellObject" -> CellObject[1],
            "Comment" ->
                "Ensure that the Indent2 is not included as part of the function use, since it was likely there for formatting purposes."
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-E49B09"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
        CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper,
        {
            {
                {
                    HoldComplete[
                        "Comment"[
                            "Ensure that the Indent2 is not included as part of the function use, since it was likely there for formatting purposes."
                        ]
                    ],
                    HoldComplete[
                        CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
                            myFunc,
                            {{HoldComplete[CalculateParse`GeneralLibrary`Indent2[myFunc[1, 2, 3]]]}}
                        ]
                    ]
                },
                CellObject[1]
            }
        }
    ]
    ,
    {
        CalculateParse`Prototype`VirtualAssistant`Utility`FunctionUse[
            HoldComplete[
                CalculateParse`Prototype`VirtualAssistant`Utility`GetFunctionUsesFromNotebookHelper[
                    myFunc,
                    {{HoldComplete[CalculateParse`GeneralLibrary`Indent2[myFunc[1, 2, 3]]]}}
                ]
            ],
            "CellObject" -> CellObject[1],
            "Comment" ->
                "Ensure that the Indent2 is not included as part of the function use, since it was likely there for formatting purposes."
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-H1RDMO"
]