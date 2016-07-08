(* Tests for: WUtils`WUtils`InsertStringAfterMatch

   Author: danielb
*)

Test[
    WUtils`WUtils`InsertStringAfterMatch["abc def ghi", " 123", "def"]
    ,
    "abc def 123 ghi"
    ,
    TestID -> "InsertStringAfterMatch-20160120-0JCPJ4"
]

Test[
    WUtils`WUtils`InsertStringAfterMatch["abc def ghi", " 123", "d"~~_~~"f"]
    ,
    "abc def 123 ghi"
    ,
    TestID -> "InsertStringAfterMatch-20160120-6BSQ91"
]