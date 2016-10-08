(* Tests for: WUtils`WUtils`InsertString

   Author: danielb
*)

Test[
	WUtils`WUtils`InsertString["just testing", 6, "X "]
	,
	"just X testing"
	,
	TestID -> "InsertString-20161007-IYVEHW"
]

Test[
	WUtils`WUtils`InsertString["just testing", 1, "X "]
	,
	"X just testing"
	,
	TestID -> "InsertString-20161007-SG1U4R"
]

Test[
	WUtils`WUtils`InsertString["just testing", 13, " X"]
	,
	"just testing X"
	,
	TestID -> "InsertString-20161007-1I1HPN"
]