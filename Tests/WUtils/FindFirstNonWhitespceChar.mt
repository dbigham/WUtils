(* Tests for: WUtils`WUtils`FindFirstNonWhitespceChar

   Author: danielb
*)

Test[
	WUtils`WUtils`FindFirstNonWhitespceChar["abc"]
	,
	1
	,
	TestID -> "FindFirstNonWhitespceChar-20160706-D6J244"
]

Test[
	WUtils`WUtils`FindFirstNonWhitespceChar["(* Comment *) abc"]
	,
	15
	,
	TestID -> "FindFirstNonWhitespceChar-20160706-D6J245"
]