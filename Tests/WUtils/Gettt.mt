(* Tests for: WUtils`WUtils`Gettt

   Author: danielb
*)

Test[
	WUtils`WUtils`Gettt[{Association["A" -> 1, "B" -> 2], Association["A" -> 3]}, "A"]
	,
	{1, 3}
	,
	TestID -> "Gettt-20161007-3WGR9H"
]

Test[
	WUtils`WUtils`Gettt[{{"A" -> 1, "B" -> 2}, {"A" -> 3}}, "A"]
	,
	{1, 3}
	,
	TestID -> "Gettt-20161007-S37LSE"
]