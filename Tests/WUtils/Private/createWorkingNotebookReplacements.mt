(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`createWorkingNotebookReplacements

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/Private/createWorkingNotebookReplacements.mt"]
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
    CalculateParse`GeneralLibrary`Private`createWorkingNotebookReplacements[{Output["Here"]}]
    ,
    {TextCell["Here", "Output"]}
    ,
    TestID -> "createWorkingNotebookReplacements-20150224-ZMXNTV"
]

Test[
    CalculateParse`GeneralLibrary`Private`createWorkingNotebookReplacements[
        {CodeString["1 + 1;\n2 + 2;"]}
    ]
    ,
    {ExpressionCell[RawBoxes["1 + 1;\n2 + 2;"], "Code", InitializationCell -> False]}
    ,
    TestID -> "createWorkingNotebookReplacements-20150224-RPFVOG"
]