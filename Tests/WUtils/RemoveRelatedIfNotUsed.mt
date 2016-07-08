(* Tests for: WUtils`WUtils`RemoveRelatedIfNotUsed

   Author: danielb
*)

Test[
    WUtils`WUtils`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n    \\related '\n    \n    \\maintainer danielb\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-V0XA98"
]

Test[
    WUtils`WUtils`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n    \\related '\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-QDKI76"
]

Test[
    WUtils`WUtils`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n\tExamples:\n\n\tTODO\n\n    \\related '\n    \n    \\maintainer danielb\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n\tExamples:\n\n\tTODO\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-0UMCEO"
]

Test[
    WUtils`WUtils`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\related '\n    \\maintainer danielb\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-XMOEG8"
]

Test[
    WUtils`WUtils`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n\t\\related '\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-VIRNDW"
]

Test[
    WUtils`WUtils`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n\t\\related 'donotremove\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n\t\\related 'donotremove\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150226-XFUFF1"
]

Test[
    WUtils`WUtils`RemoveRelatedIfNotUsed[
        "(* my comment *)"
    ]
    ,
    "(* my comment *)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150226-RDE1HK"
]