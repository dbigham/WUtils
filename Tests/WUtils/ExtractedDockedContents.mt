(* Tests for: WUtils`WUtils`ExtractedDockedContents

   Author: danielb
*)

Test[
    WUtils`WUtils`ExtractedDockedContents[
        {"Docked"["dockedContents"], "notebookContents1", "notebookContents2"}
    ]
    ,
    {{"notebookContents1", "notebookContents2"}, "dockedContents"}
    ,
    TestID -> "ExtractedDockedContents-20150223-PR39HT"
]

Test[
    WUtils`WUtils`ExtractedDockedContents[
        {"notebookContents1", "notebookContents2"}
    ]
    ,
    {{"notebookContents1", "notebookContents2"}, None}
    ,
    TestID -> "ExtractedDockedContents-20150223-IJSH07"
]