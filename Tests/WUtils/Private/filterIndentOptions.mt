(* Tests for: WUtils`WUtils`Private`filterIndentOptions

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`filterIndentOptions[
        "FullFormStrings" -> True,
        "SomeOtherOptions" -> False
    ]
    ,
    {"FullFormStrings" -> True}
    ,
    TestID -> "filterIndentOptions-20150131-3QWWZ3"
]