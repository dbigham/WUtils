(* Tests for: WUtils`WUtils`EnsureFileLoaded

   Author: danielb
*)

Test[
	WUtils`WUtils`WithTemporaryFiles[
		{file = "{1, 2, 3}"},
		(
			Clear[WUtils`WUtils`Tests`var];
			WUtils`WUtils`EnsureFileLoaded[
				Association["Path" -> file, "Variable" -> HoldComplete[WUtils`WUtils`Tests`var]]
			];
			WUtils`WUtils`Tests`var
		)
	]
	,
	{1, 2, 3}
	,
	TestID -> "EnsureFileLoaded-20161006-NZLEDK"
]

Test[
	WUtils`WUtils`WithTemporaryFiles[
		{file = ""},
		(
			WUtils`WUtils`Tests`var = {1, 2, 3};
			WUtils`WUtils`EnsureFileLoaded[
				Association["Path" -> file, "Variable" -> HoldComplete[WUtils`WUtils`Tests`var]]
			];
			WUtils`WUtils`Tests`var
		)
	]
	,
	{1, 2, 3}
	,
	TestID -> "EnsureFileLoaded-20161006-9WMMBS"
]