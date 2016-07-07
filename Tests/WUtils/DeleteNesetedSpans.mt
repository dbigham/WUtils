(* Tests for: WUtils`WUtils`DeleteNesetedSpans

   Author: danielb
*)

Test[
	WUtils`WUtils`DeleteNesetedSpans[{{1, 5}, {3, 5}, {4, 8}, {4, 4}, {8, 8}}]
	,
	{{1, 5}, {4, 8}}
	,
	TestID -> "DeleteNesetedSpans-20160707-BE1Y3W"
]

Test[
	WUtils`WUtils`DeleteNesetedSpans[{{1, 5}, {1, 5}}]
	,
	{{1, 5}}
	,
	TestID -> "DeleteNesetedSpans-20160707-ACYKNG"
]

Test[
	WUtils`WUtils`DeleteNesetedSpans[{{1, 5}, {1, 5}, {1, 5}}]
	,
	{{1, 5}}
	,
	TestID -> "DeleteNesetedSpans-20160707-88V497"
]

Test[
	WUtils`WUtils`DeleteNesetedSpans[{{1, 5}, {5, 5}}]
	,
	{{1, 5}}
	,
	TestID -> "DeleteNesetedSpans-20160707-CJD8SG"
]