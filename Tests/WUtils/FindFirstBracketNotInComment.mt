(* Tests for: WUtils`WUtils`FindFirstBracketNotInComment

   Author: danielb
*)

Test[
	WUtils`WUtils`FindFirstBracketNotInComment["Func[]", 1]
	,
	5
	,
	TestID -> "FindFirstBracketNotInComment-20160706-B81WFZ"
]

Test[
	WUtils`WUtils`FindFirstBracketNotInComment["(1+1)", 1]
	,
	1
	,
	TestID -> "FindFirstBracketNotInComment-20160706-F40DT3"
]

Test[
	WUtils`WUtils`FindFirstBracketNotInComment["abc", 1]
	,
	None
	,
	TestID -> "FindFirstBracketNotInComment-20160706-M73FX1"
]

Test[
	WUtils`WUtils`FindFirstBracketNotInComment["(* Comment *)\n(1+1)", 1]
	,
	15
	,
	TestID -> "FindFirstBracketNotInComment-20160706-M73FX2"
]

Test[
	WUtils`WUtils`FindFirstBracketNotInComment["(* Comment *) (* Comment 2 *)\n(1+1)", 1]
	,
	31
	,
	TestID -> "FindFirstBracketNotInComment-20160706-M73FX3"
]

Test[
	WUtils`WUtils`FindFirstBracketNotInComment["(* Comment *)\n\tModule[{},\n\t\t1\n\t]", 1]
	,
	22
	,
	TestID -> "FindFirstBracketNotInComment-20160706-M73FX4"
]