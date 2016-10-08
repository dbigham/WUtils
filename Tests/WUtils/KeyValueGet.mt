(* Tests for: WUtils`WUtils`KeyValueGet

   Author: danielb
*)

Test[
	WUtils`WUtils`KeyValueGet[keyValues_, key_]
	,
	WUtils`WUtils`KeyValueGet[keyValues_, key_]
	,
	TestID -> "KeyValueGet-20161008-9HKQAL"
]

Test[
	WUtils`WUtils`KeyValueGet[{"a" -> "b", "c" -> "d"}, "a"]
	,
	"b"
	,
	TestID -> "KeyValueGet-20161008-CIE334"
]

Test[
	WUtils`WUtils`KeyValueGet[
		JitLang`JitLang`Chunk["String" -> "the", "Index" -> 2],
		"String"
	]
	,
	"the"
	,
	TestID -> "KeyValueGet-20161008-JGRI64"
]