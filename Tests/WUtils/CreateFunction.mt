(* Tests for: CalculateParse`Prototype`VirtualAssistant`CodeAssist`CreateFunction

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
    CalculateParse`Prototype`VirtualAssistant`CodeAssist`CreateFunction[
        "performSrMap[sr, mapping]"
    ]
    ,
    "(*!\n    \\function performSrMap\n    \n    \\calltable\n        performSrMap[sr, mapping] '' comment\n\n    Examples:\n    \n    performSrMap[sr, mapping] === TODO\n    \n    \\related '\n    \n    \\maintainer danielb\n*)\nperformSrMap[sr_, mapping_] :=\n    Module[{},\n        TODO\n    ];"
    ,
    TestID -> "CreateFunction-20151016-KMBQ4C"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`CodeAssist`CreateFunction[
        "DockedToolbar[content, dynamicOutputvar]",
        "Description" ->
            "returns content that can be placed in the notebook's toolbar, wrapped with an appropriate dynamic output section."
    ]
    ,
    "(*!\n    \\function DockedToolbar\n    \n    \\calltable\n        DockedToolbar[content, dynamicOutputvar] '' returns content that can be placed in the notebook's toolbar, wrapped with an appropriate dynamic output section.\n\n    Examples:\n    \n    DockedToolbar[content, dynamicOutputvar] === TODO\n    \n    \\related '\n    \n    \\maintainer danielb\n*)\nDockedToolbar[content_, dynamicOutputvar_] :=\n    Module[{},\n        TODO\n    ];"
    ,
    TestID -> "CreateFunction-20151016-46EBU7"
]

(* If there aren't any args, don't bother with an "Examples:" section in the Mathdoc. *)
Test[
    CalculateParse`Prototype`VirtualAssistant`CodeAssist`CreateFunction["myFunc[]"]
    ,
    "(*!\n    \\function myFunc\n    \n    \\calltable\n        myFunc[] '' comment\n    \n    \\related '\n    \n    \\maintainer danielb\n*)\nmyFunc[] :=\n    Module[{},\n        TODO\n    ];"
    ,
    TestID -> "CreateFunction-20151016-34F97O"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`CodeAssist`CreateFunction[
        "myFunc[]",
        "UseTabs" -> True
    ]
    ,
    "(*!\n\t\\function myFunc\n\t\n\t\\calltable\n\t\tmyFunc[] '' comment\n\t\n\t\\related '\n\t\n\t\\maintainer danielb\n*)\nmyFunc[] :=\n\tModule[{},\n\t\tTODO\n\t];"
    ,
    TestID -> "CreateFunction-20160120-L2DE55"
]