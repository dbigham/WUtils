(* Tests for: WUtils`WUtils`CreateDirectoryIfDoesntExist

   Author: danielb
*)

Test[
	Block[
		{res, dir},
		(
			dir = WUtils`WUtils`TemporaryDirectory[];
			res = {WUtils`WUtils`CreateDirectoryIfDoesntExist[dir], FileExistsQ[dir]};
			DeleteDirectory[dir];
			res
		)
	]
	,
	{Null, True}
	,
	TestID -> "CreateDirectoryIfDoesntExist-20161006-XMGYO6"
]

Test[
	Block[
		{res, dir},
		(
			dir = WUtils`WUtils`TemporaryDirectory[];
			DeleteDirectory[dir];
			res = {WUtils`WUtils`CreateDirectoryIfDoesntExist[dir], FileExistsQ[dir]};
			DeleteDirectory[dir];
			res
		)
	]
	,
	{True, True}
	,
	TestID -> "CreateDirectoryIfDoesntExist-20161006-9G08CM"
]