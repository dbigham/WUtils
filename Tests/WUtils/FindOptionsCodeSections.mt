(* Tests for: WUtils`WUtils`FindOptionsCodeSections

   Author: danielb
*)

Test[
	WUtils`WUtils`FindOptionsCodeSections[
		"Options[Func] =
{
	\"MyOption\" -> Automatic	(*< sample option *)
}
",
		"Func"
	]
	,
	{{1, 65}}
	,
	TestID -> "FindOptionsCodeSections-20160706-DQ5WIV"
]

Test[
	WUtils`WUtils`FindOptionsCodeSections[
		"Options[Func] =
{
	\"MyOption\" -> Automatic	(*< sample option with } to ignore *)
}
",
		"Func"
	]
	,
	{{1, 82}}
	,
	TestID -> "FindOptionsCodeSections-20160706-DQ5WI1"
]