(* Tests for: WUtils`WUtils`CreateFunction

   Author: danielb
*)

Test[
    WUtils`WUtils`CreateFunction["performSrMap[sr, mapping]"],
    "(*!\n\t\\function performSrMap\n\t\n\t\\calltable\n\t\tperformSrMap[sr, mapping] '' comment\n\n\tExamples:\n\t\n\tperformSrMap[sr, mapping] === TODO\n\t\n\t\\related '\n\t\n\t\\maintainer danielb\n*)\nperformSrMap[sr_, mapping_] :=\n\tBlock[{},\n\t\tTODO\n\t];",
    TestID -> "CreateFunction-20151016-KMBQ4C"
]

Test[
    WUtils`WUtils`CreateFunction[
        "DockedToolbar[content, dynamicOutputvar]",
        "Description" ->
            "returns content that can be placed in the notebook's toolbar, wrapped with an appropriate dynamic output section."
    ],
    "(*!\n\t\\function DockedToolbar\n\t\n\t\\calltable\n\t\tDockedToolbar[content, dynamicOutputvar] '' returns content that can be placed in the notebook's toolbar, wrapped with an appropriate dynamic output section.\n\n\tExamples:\n\t\n\tDockedToolbar[content, dynamicOutputvar] === TODO\n\t\n\t\\related '\n\t\n\t\\maintainer danielb\n*)\nDockedToolbar[content_, dynamicOutputvar_] :=\n\tBlock[{},\n\t\tTODO\n\t];",
    TestID -> "CreateFunction-20151016-46EBU7"
]

(* If there aren't any args, don't bother with an "Examples:" section in the Mathdoc. *)
Test[
    WUtils`WUtils`CreateFunction["myFunc[]"],
    "(*!\n\t\\function myFunc\n\t\n\t\\calltable\n\t\tmyFunc[] '' comment\n\t\n\t\\related '\n\t\n\t\\maintainer danielb\n*)\nmyFunc[] :=\n\tBlock[{},\n\t\tTODO\n\t];",
    TestID -> "CreateFunction-20151016-34F97O"
]

Test[
    WUtils`WUtils`CreateFunction[
        "myFunc[]",
        "UseTabs" -> True
    ]
    ,
    "(*!\n\t\\function myFunc\n\t\n\t\\calltable\n\t\tmyFunc[] '' comment\n\t\n\t\\related '\n\t\n\t\\maintainer danielb\n*)\nmyFunc[] :=\n\tBlock[{},\n\t\tTODO\n\t];"
    ,
    TestID -> "CreateFunction-20160120-L2DE55"
]