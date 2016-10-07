(* Tests for: WUtils`WUtils`EmptyQ

   Author: danielb
*)

Test[
	WUtils`WUtils`EmptyQ[{}]
	,
	True
	,
	TestID -> "EmptyQ-20161006-XXE0IH"
]

Test[
	WUtils`WUtils`EmptyQ[{1, 2, 3}]
	,
	False
	,
	TestID -> "EmptyQ-20161006-STVD38"
]