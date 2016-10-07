(* Tests for: WUtils`WUtils`LoadFile

   Author: danielb
*)

Test[
	WUtils`WUtils`WithTemporaryFiles[
		{file = "{1, 2, 3}"},
		(
			WUtils`WUtils`LoadFile[
				Association["Path" -> file, "Variable" -> HoldComplete[WUtils`WUtils`Tests`var]]
			];
			WUtils`WUtils`Tests`var
		)
	]
	,
	{1, 2, 3}
	,
	TestID -> "LoadFile-20161006-NDGPTK"
]