(* Tests for: WUtils`WUtils`MathdocContainsExampleQ

   Author: danielb
*)

Test[
    WUtils`WUtils`MathdocContainsExampleQ[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[\"WUtils`WUtils`Private`\"]\n    \n    ContextToFile[\"WUtils`WUtils`\"]\n\n    Unit tests:\n\n    RunUnitTests[WUtils`WUtils`ContextToFile]\n\n    \\maintainer danielb\n*)"
    ]
    ,
    True
    ,
    TestID -> "MathdocContainsExampleQ-20150221-FLKINE"
]

Test[
    WUtils`WUtils`MathdocContainsExampleQ[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n    \n    Examples:\n    \n    ContextToFile[context] === TODO\n\n    \\maintainer danielb\n*)"
    ]
    ,
    False
    ,
    TestID -> "MathdocContainsExampleQ-20150221-H8Y7M3"
]

Test[
    WUtils`WUtils`MathdocContainsExampleQ[
        "(*!\n    \\function ContextToFile\n    \n    \\calltable\n        ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.\n\n    \\maintainer danielb\n*)"
    ]
    ,
    False
    ,
    TestID -> "MathdocContainsExampleQ-20150221-C4MEV1"
]