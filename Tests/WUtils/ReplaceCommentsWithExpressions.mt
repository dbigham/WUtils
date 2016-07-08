(* Tests for: WUtils`WUtils`ReplaceCommentsWithExpressions

   Author: danielb
*)

Test[
    WUtils`WUtils`ReplaceCommentsWithExpressions[""]
    ,
    ""
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-QGYTS7"
]

Test[
    WUtils`WUtils`ReplaceCommentsWithExpressions["a"]
    ,
    "a"
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-D30JQA"
]

Test[
    WUtils`WUtils`ReplaceCommentsWithExpressions[
        "(* Just testing *)\n\"a\"\n\"b\""
    ]
    ,
    "\"Comment\" -> Comment[\"Just testing\"]\n\"a\"\n\"b\""
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-NTT354"
]

(* Leave alone comments that are within strings. *)
Test[
    WUtils`WUtils`ReplaceCommentsWithExpressions[
        "{myFunc[\"(* my comment *)\"]}"
    ]
    ,
    "{myFunc[\"(* my comment *)\"]}"
    ,
    TestID -> "ReplaceCommentsWithExpressions-20150226-99KIE6"
]