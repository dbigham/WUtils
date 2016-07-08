(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`SetMathdocExample

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/SetMathdocExample.mt"]
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
    CalculateParse`Prototype`VirtualAssistant`Utility`SetMathdocExample[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n    \\maintainer danielb\n*)",
        CalculateParse`GeneralLibrary`ContextToFile["CalculateParse`JavaTokenizer`"],
        FileNameJoin[
            {
                CalculateScan`CommonSymbols`$AlphaRootDirectory,
                "Source",
                "CalculateParse",
                "JavaTokenizer.m"
            }
        ]
    ]
    ,
    "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n    Example:\n\n    ContextToFile[\"CalculateParse`JavaTokenizer`\"]\n\n    ===\n\n    FileNameJoin[{$AlphaRootDirectory, \"Source\", \"CalculateParse\", \"JavaTokenizer.m\"}]\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "SetMathdocExample-20150221-XPNZBV"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`SetMathdocExample[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n\tExamples:\n\n\tTODO\t\n\n    \\maintainer danielb\n*)",
        CalculateParse`GeneralLibrary`ContextToFile["CalculateParse`JavaTokenizer`"],
        FileNameJoin[
            {
                CalculateScan`CommonSymbols`$AlphaRootDirectory,
                "Source",
                "CalculateParse",
                "JavaTokenizer.m"
            }
        ]
    ]
    ,
    "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n\tExamples:\n\n    ContextToFile[\"CalculateParse`JavaTokenizer`\"]\n\n    ===\n\n    FileNameJoin[{$AlphaRootDirectory, \"Source\", \"CalculateParse\", \"JavaTokenizer.m\"}]\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "SetMathdocExample-20150221-IASW35"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`SetMathdocExample[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n\tExamples:\n\n    \\maintainer danielb\n*)",
        CalculateParse`GeneralLibrary`ContextToFile["CalculateParse`JavaTokenizer`"],
        FileNameJoin[
            {
                CalculateScan`CommonSymbols`$AlphaRootDirectory,
                "Source",
                "CalculateParse",
                "JavaTokenizer.m"
            }
        ]
    ]
    ,
    "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n\tExamples:\n\n    ContextToFile[\"CalculateParse`JavaTokenizer`\"]\n\n    ===\n\n    FileNameJoin[{$AlphaRootDirectory, \"Source\", \"CalculateParse\", \"JavaTokenizer.m\"}]\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "SetMathdocExample-20150221-WIHWB0"
]