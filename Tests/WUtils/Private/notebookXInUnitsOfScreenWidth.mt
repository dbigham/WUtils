(* Tests for: WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
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
    WUtils`WUtils`Private`notebookXInUnitsOfScreenWidth[
        2452.,
        1575,
        1920
    ]
    ,
    1.5593650793650793
    ,
    TestID -> "notebookXInUnitsOfScreenWidth-20150226-QV1NFG"
]