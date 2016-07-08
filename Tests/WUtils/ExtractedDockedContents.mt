(* Tests for: CalculateParse`Prototype`VirtualAssistant`VaActions`ExtractedDockedContents

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/VaActions/ExtractedDockedContents.mt"]
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
    CalculateParse`Prototype`VirtualAssistant`VaActions`ExtractedDockedContents[
        {"Docked"["dockedContents"], "notebookContents1", "notebookContents2"}
    ]
    ,
    {{"notebookContents1", "notebookContents2"}, "dockedContents"}
    ,
    TestID -> "ExtractedDockedContents-20150223-PR39HT"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`ExtractedDockedContents[
        {"notebookContents1", "notebookContents2"}
    ]
    ,
    {{"notebookContents1", "notebookContents2"}, None}
    ,
    TestID -> "ExtractedDockedContents-20150223-IJSH07"
]