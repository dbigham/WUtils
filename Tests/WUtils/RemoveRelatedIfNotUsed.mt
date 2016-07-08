(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed

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
    CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n    \\related '\n    \n    \\maintainer danielb\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-V0XA98"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n    \\related '\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-QDKI76"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n\tExamples:\n\n\tTODO\n\n    \\related '\n    \n    \\maintainer danielb\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n    \n\tExamples:\n\n\tTODO\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-0UMCEO"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\related '\n    \\maintainer danielb\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-XMOEG8"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n\t\\related '\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n    \\maintainer danielb\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150225-VIRNDW"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed[
        "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n\t\\related 'donotremove\n*)"
    ]
    ,
    "(*!\n    \\function RemoveRelatedIfNotUsed\n    \n    \\calltable\n        RemoveRelatedIfNotUsed[mathdoc] '' removes the \"related '\" boilerplate text from the mathdoc if it didn't end up being filled out.\n\n\t\\related 'donotremove\n*)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150226-XFUFF1"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RemoveRelatedIfNotUsed[
        "(* my comment *)"
    ]
    ,
    "(* my comment *)"
    ,
    TestID -> "RemoveRelatedIfNotUsed-20150226-RDE1HK"
]