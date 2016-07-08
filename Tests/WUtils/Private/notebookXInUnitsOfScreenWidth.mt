(* Tests for: CalculateParse`Prototype`VirtualAssistant`VaActions`Private`CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth

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
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        0,
        500,
        1000
    ]
    ,
    0.
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-JWV6VY"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        500,
        500,
        1000
    ]
    ,
    0.5
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-VNB0WU"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        250,
        500,
        1000
    ]
    ,
    0.25
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-O63LFB"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        750,
        500,
        1000
    ]
    ,
    0.75
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-FCUOQB"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        1000,
        500,
        1000
    ]
    ,
    1.
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-RZQD5Q"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        1500,
        500,
        1000
    ]
    ,
    1.5
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-59134W"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        2000,
        500,
        1000
    ]
    ,
    2.
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-MHD31O"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        0,
        800,
        1000
    ]
    ,
    0.
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-VSNWGW"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        200,
        800,
        1000
    ]
    ,
    0.5
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-U16W17"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        600,
        800,
        1000
    ]
    ,
    0.75
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-RRTS47"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        1000,
        800,
        1000
    ]
    ,
    1.
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-KWTXSW"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        1200,
        800,
        1000
    ]
    ,
    1.5
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-0Y9172"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        2000,
        800,
        1000
    ]
    ,
    2.
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-PI2HHR"
]

(* Just left of the screen's left border. *)
Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        1950,
        800,
        1000
    ]
    ,
    2
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-Y6M6AM"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`notebookXInUnitsOfScreenWidth[
        2452.,
        1575,
        1920
    ]
    ,
    1.5593650793650793
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-QV1NFG"
]