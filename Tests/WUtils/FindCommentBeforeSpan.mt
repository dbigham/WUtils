(* Tests for: WUtils`WUtils`FindCommentBeforeSpan

   Author: danielb
*)

Test[
	WUtils`WUtils`FindCommentBeforeSpan[
		"(* Comment *)\nFunc[] := 1",
		{15, -1}
	]
	,
	{{1, 13}}
	,
	TestID -> "FindCommentBeforeSpan-20160706-FVFQG4"
]

Test[
	WUtils`WUtils`FindCommentBeforeSpan["Func[] := 1", {1, -1}]
	,
	{}
	,
	TestID -> "FindCommentBeforeSpan-20160706-VAO7E6"
]

Test[
	WUtils`WUtils`FindCommentBeforeSpan["1+1;\nFunc[] := 1", {6, -1}]
	,
	{}
	,
	TestID -> "FindCommentBeforeSpan-20160706-2SBDWJ"
]