(* Tests for: WUtils`WUtils`Private`createWorkingNotebookReplacements

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`createWorkingNotebookReplacements[{Output["Here"]}]
    ,
    {TextCell["Here", "Output"]}
    ,
    TestID -> "createWorkingNotebookReplacements-20150224-ZMXNTV"
]

Test[
    WUtils`WUtils`Private`createWorkingNotebookReplacements[
        {CodeString["1 + 1;\n2 + 2;"]}
    ]
    ,
    {ExpressionCell[RawBoxes["1 + 1;\n2 + 2;"], "Code", InitializationCell -> False]}
    ,
    TestID -> "createWorkingNotebookReplacements-20150224-RPFVOG"
]