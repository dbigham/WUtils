(* Tests for: WUtils`WUtils`ReplaceRange

   Author: danielb
*)

Test[
	WUtils`WUtils`ReplaceRange[{1, 2, 3, 4, 5, 6}, 2, 3, {888, 999}]
	,
	{1, 888, 999, 4, 5, 6}
	,
	TestID -> "ReplaceRange-20161007-E22EWS"
]

Test[
	WUtils`WUtils`ReplaceRange[{1, 2, 3, 4, 5, 6}, 2, 3, 999]
	,
	{1, 999, 4, 5, 6}
	,
	TestID -> "ReplaceRange-20161007-WFHP7U"
]

Test[
	WUtils`WUtils`ReplaceRange[{1, 2, 3, 4, 5, 6}, 1, 1, {888, 999}]
	,
	{888, 999, 2, 3, 4, 5, 6}
	,
	TestID -> "ReplaceRange-20161007-JDVTPS"
]

Test[
	WUtils`WUtils`ReplaceRange[{1, 2, 3, 4, 5, 6}, 6, 6, {888, 999}]
	,
	{1, 2, 3, 4, 5, 888, 999}
	,
	TestID -> "ReplaceRange-20161007-884JUV"
]

Test[
	WUtils`WUtils`ReplaceRange[{1, 2, 3, 4, 5, 6}, 2 ;; 3, {888, 999}]
	,
	{1, 888, 999, 4, 5, 6}
	,
	TestID -> "ReplaceRange-20161007-T6R76K"
]