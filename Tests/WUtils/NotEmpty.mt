(* Tests for: WUtils`WUtils`NotEmpty

   Author: danielb
*)

Test[
	WUtils`WUtils`NotEmpty[{}]
	,
	False
	,
	TestID -> "NotEmpty-20161006-ZOG6DP"
]

Test[
	WUtils`WUtils`NotEmpty[{1}]
	,
	True
	,
	TestID -> "NotEmpty-20161006-C4Y92A"
]