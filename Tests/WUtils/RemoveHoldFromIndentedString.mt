(* Tests for: WUtils`WUtils`RemoveHoldFromIndentedString

   Author: danielb
*)

Test[
    WUtils`WUtils`RemoveHoldFromIndentedString["    HoldComplete[\"here\"]", "HoldComplete"]
    ,
    "    \"here\""
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-0BNL7R"
]

Test[
    WUtils`WUtils`RemoveHoldFromIndentedString["HoldComplete[\"here\"]", "HoldComplete"]
    ,
    "\"here\""
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-ZW3HTS"
]

Test[
    WUtils`WUtils`RemoveHoldFromIndentedString[
        "    HoldComplete[\n    Inner[\n        1,\n        2\n    ]\n    ]",
        "HoldComplete"
    ]
    ,
    "    Inner[\n        1,\n        2\n    ]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-D2DEVY"
]

Test[
    WUtils`WUtils`RemoveHoldFromIndentedString[
        "HoldComplete[\n    Inner[\n        1,\n        2\n    ]\n]",
        "HoldComplete"
    ]
    ,
    "Inner[\n    1,\n    2\n]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-BU8VRL"
]

Test[
    WUtils`WUtils`RemoveHoldFromIndentedString[
        "HoldComplete[\n\tLooksLikeCallSignature[CouldBeWLSymbolQ[\"test\"]], CouldBeWLSymbolQ]\n]",
        "HoldComplete"
    ]
    ,
    "LooksLikeCallSignature[CouldBeWLSymbolQ[\"test\"]], CouldBeWLSymbolQ]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-42MA28"
]

Test[
    WUtils`WUtils`RemoveHoldFromIndentedString[
        "pliChartHoldComplete$586145[\n    Inner[\n        1,\n        2\n    ]\n]",
        "pliChartHoldComplete$586145"
    ]
    ,
    "Inner[\n    1,\n    2\n]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-1OX9HS"
]

(* The HoldComplete string didn't match, so nothing was done.
   Alternatively, we could perhaps return $Failed in this situation. *)
Test[
    WUtils`WUtils`RemoveHoldFromIndentedString[
        "    HoldComplete[1]",
        "HoldComplete2"
    ]
    ,
    "    HoldComplete[1]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-WD01ND"
]