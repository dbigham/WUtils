(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`MathdocContainsExampleQ

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/MathdocContainsExampleQ.mt"]
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
    CalculateParse`Prototype`VirtualAssistant`Utility`MathdocContainsExampleQ[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[\"CalculateParse`GeneralLibrary`Private`\"]\n    \n    ContextToFile[\"CalculateParse`GeneralLibrary`\"]\n\n    Unit tests:\n\n    RunUnitTests[CalculateParse`GeneralLibrary`ContextToFile]\n\n    \\maintainer danielb\n*)"
    ]
    ,
    True
    ,
    TestID -> "MathdocContainsExampleQ-20150221-FLKINE"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`MathdocContainsExampleQ[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[context] === TODO\n\n    \\maintainer danielb\n*)"
    ]
    ,
    False
    ,
    TestID -> "MathdocContainsExampleQ-20150221-H8Y7M3"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`MathdocContainsExampleQ[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n    \\maintainer danielb\n*)"
    ]
    ,
    False
    ,
    TestID -> "MathdocContainsExampleQ-20150221-C4MEV1"
]