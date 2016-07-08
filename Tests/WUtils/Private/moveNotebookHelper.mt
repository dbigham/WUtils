(* Tests for: WUtils`WUtils`Private`moveNotebookHelper

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`moveNotebookHelper[
        "Right",
        0,
        500,
        1000,
        1
    ]
    ,
    {Automatic, 0.}
    ,
    TestID -> "moveNotebookHelper-20150226-PWS59W"
]

Test[
    Block[
        {$NumberOfDisplays = 2},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Right",
            500,
            500,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {1000., Automatic}
    ,
    TestID -> "moveNotebookHelper-20150226-PHSKQ4"
]

Test[
    Block[
        {$NumberOfDisplays = 2},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Right",
            1000,
            500,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {Automatic, -1000.}
    ,
    TestID -> "moveNotebookHelper-20150226-J50CCW"
]

(* Can't move any further right. *)
Test[
    Block[
        {$NumberOfDisplays = 2},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Right",
            1500,
            500,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {Automatic, -1000.}
    ,
    TestID -> "moveNotebookHelper-20150226-MP67Y2"
]

(* Just off of the screen to the left. *)
Test[
    WUtils`WUtils`Private`moveNotebookHelper[
        "Right",
        -10,
        500,
        1000,
        1
    ]
    ,
    {Automatic, 0.}
    ,
    TestID -> "moveNotebookHelper-20150226-WQY3GU"
]

(* Can't move any further right. *)
Test[
    Block[
        {$NumberOfDisplays = 1},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Right",
            500,
            500,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {Automatic, 0.}
    ,
    TestID -> "moveNotebookHelper-20150226-BZYMPL"
]

Test[
    Block[
        {$NumberOfDisplays = 3},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Right",
            1500,
            500,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {2000., Automatic}
    ,
    TestID -> "moveNotebookHelper-20150226-LJE0G1"
]

Test[
    Block[
        {$NumberOfDisplays = 2},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Left",
            1200,
            800,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {1000., Automatic}
    ,
    TestID -> "moveNotebookHelper-20150226-05B769"
]

(* Regression test. *)
Test[
    WUtils`WUtils`Private`moveNotebookHelper[
        "Left",
        2705.,
        1595,
        1920,
        2
    ]
    ,
    {1920., Automatic}
    ,
    TestID -> "moveNotebookHelper-20150226-7V7BR4"
]

Test[
    Block[
        {$NumberOfDisplays = 2},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Right",
            0,
            1000,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {1000., Automatic}
    ,
    TestID -> "moveNotebookHelper-20150226-9UUO3C"
]

Test[
    Block[
        {$NumberOfDisplays = 2},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Right",
            1000,
            1000,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {1000., Automatic}
    ,
    TestID -> "moveNotebookHelper-20150226-RJ913K"
]

Test[
    Block[
        {$NumberOfDisplays = 2},
        WUtils`WUtils`Private`moveNotebookHelper[
            "Left",
            1000,
            1000,
            1000,
            $NumberOfDisplays
        ]
    ]
    ,
    {0., Automatic}
    ,
    TestID -> "moveNotebookHelper-20150226-KNOOT7"
]