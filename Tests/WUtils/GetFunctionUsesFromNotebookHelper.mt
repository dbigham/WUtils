(* Tests for: WUtils`WUtils`GetFunctionUsesFromNotebookHelper

   Author: danielb
*)


Test[
    WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
        myFunc,
        {{HoldComplete[WUtils`WUtils`Indent2[myFunc[1, 2, 3]]], CellObject[1]}}
    ]
    ,
    {
        WUtils`WUtils`FunctionUse[
            "myFunc[1, 2, 3]",
            "CellObject" -> CellObject[1]
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-V7GNAK"
]

Test[
    WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
        myFunc,
        {{HoldComplete[myFunc[1, 2, 3]], CellObject[1]}}
    ]
    ,
    {
        WUtils`WUtils`FunctionUse[
            myFunc[1, 2, 3],
            "CellObject" -> CellObject[1]
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-XFPKK5"
]

Test[
    WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
        myFunc,
        {
            {{HoldComplete["Comment"["Test comment"]], HoldComplete[myFunc[1, 2, 3]]}, CellObject[1]}
        }
    ]
    ,
    {
        WUtils`WUtils`FunctionUse[
            HoldComplete[myFunc[1, 2, 3]],
            "CellObject" -> CellObject[1],
            "Comment" -> "Test comment"
        ]
    }
    ,
    TestID -> "GetFunctionUsesFromNotebookHelper-20150220-BR1XLU"
]

Test[
    WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
        WUtils`WUtils`GetFunctionUsesFromNotebookHelper,
        {
            {
                {
                    HoldComplete[
                        "Comment"[
                            "Ensure that the Indent2 is not included as part of the function use, since it was likely there for formatting purposes."
                        ]
                    ],
                    HoldComplete[
                        WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
                            myFunc,
                            {{HoldComplete[WUtils`WUtils`Indent2[myFunc[1, 2, 3]]]}}
                        ]
                    ]
                },
                CellObject[1]
            }
        }
    ]
    ,
    {
        WUtils`WUtils`FunctionUse[
            HoldComplete[
                WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
                    myFunc,
                    {{HoldComplete[WUtils`WUtils`Indent2[myFunc[1, 2, 3]]]}}
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
    WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
        WUtils`WUtils`GetFunctionUsesFromNotebookHelper,
        {
            {
                {
                    HoldComplete[
                        "Comment"[
                            "Ensure that the Indent2 is not included as part of the function use, since it was likely there for formatting purposes."
                        ]
                    ],
                    HoldComplete[
                        WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
                            myFunc,
                            {{HoldComplete[WUtils`WUtils`Indent2[myFunc[1, 2, 3]]]}}
                        ]
                    ]
                },
                CellObject[1]
            }
        }
    ]
    ,
    {
        WUtils`WUtils`FunctionUse[
            HoldComplete[
                WUtils`WUtils`GetFunctionUsesFromNotebookHelper[
                    myFunc,
                    {{HoldComplete[WUtils`WUtils`Indent2[myFunc[1, 2, 3]]]}}
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