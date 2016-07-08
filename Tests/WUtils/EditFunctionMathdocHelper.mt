(* Tests for: WUtils`WUtils`EditFunctionMathdocHelper

   Author: danielb
*)

Test[
    WUtils`WUtils`EditFunctionMathdocHelper[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[\"WUtils`WUtils`Private`\"]\n    \n    ContextToFile[\"WUtils`WUtils`\"]\n\n    \\maintainer danielb\n*)",
        "    Unit tests:\n\n    RunUnitTests[WUtils`WUtils`ContextToFile]\n\n",
        {"\\maintainer", "\\related"},
        "SubstringMustNotExist" -> "Unit tests:"
    ],
    "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[\"WUtils`WUtils`Private`\"]\n    \n    ContextToFile[\"WUtils`WUtils`\"]\n\n    Unit tests:\n\n    RunUnitTests[WUtils`WUtils`ContextToFile]\n\n\t\\maintainer danielb\n*)",
    TestID -> "EditFunctionMathdocHelper-20150221-695IP3"
]