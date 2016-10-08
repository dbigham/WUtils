(* Tests for: WUtils`WUtils`LastStringPosition

   Author: danielb
*)

Test[
	WUtils`WUtils`LastStringPosition["Just testing", "t"]
	,
	9
	,
	TestID -> "LastStringPosition-20161007-GFH4NL"
]

Test[
	WUtils`WUtils`LastStringPosition["Just testing", "x"]
	,
	None
	,
	TestID -> "LastStringPosition-20161007-YYVJT0"
]