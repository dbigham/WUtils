(* Tests for: WUtils`WUtils`ExpandSpansToIncludeTrailingSemiColons

   Author: danielb
*)

Test[
	WUtils`WUtils`ExpandSpansToIncludeTrailingSemiColons[
		"Func[]; Func2[];",
		{{1, 6}, {9, 15}}
	]
	,
	{{1, 7}, {9, 16}}
	,
	TestID -> "ExpandSpansToIncludeTrailingSemiColons-20160707-GWQFBT"
]