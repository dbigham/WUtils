(* Tests for: WUtils`WUtils`SetMathdocExample

   Author: danielb
*)

Test[
    WUtils`WUtils`SetMathdocExample[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n    \\maintainer danielb\n*)",
        WUtils`WUtils`ContextToFile["CalculateParse`JavaTokenizer`"],
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
    WUtils`WUtils`SetMathdocExample[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n\tExamples:\n\n\tTODO\t\n\n    \\maintainer danielb\n*)",
        WUtils`WUtils`ContextToFile["CalculateParse`JavaTokenizer`"],
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
    WUtils`WUtils`SetMathdocExample[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n\tExamples:\n\n    \\maintainer danielb\n*)",
        WUtils`WUtils`ContextToFile["CalculateParse`JavaTokenizer`"],
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