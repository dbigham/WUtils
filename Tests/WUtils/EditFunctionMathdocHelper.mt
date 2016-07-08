(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`EditFunctionMathdocHelper

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
    CalculateParse`Prototype`VirtualAssistant`Utility`EditFunctionMathdocHelper[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[\"CalculateParse`GeneralLibrary`Private`\"]\n    \n    ContextToFile[\"CalculateParse`GeneralLibrary`\"]\n\n    \\maintainer danielb\n*)",
        "    Unit tests:\n\n    RunUnitTests[CalculateParse`GeneralLibrary`ContextToFile]\n\n",
        {"\\maintainer", "\\related"},
        "SubstringMustNotExist" -> "Unit tests:"
    ]
    ,
    "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[\"CalculateParse`GeneralLibrary`Private`\"]\n    \n    ContextToFile[\"CalculateParse`GeneralLibrary`\"]\n\n    Unit tests:\n\n    RunUnitTests[CalculateParse`GeneralLibrary`ContextToFile]\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "EditFunctionMathdocHelper-20150221-695IP3"
]