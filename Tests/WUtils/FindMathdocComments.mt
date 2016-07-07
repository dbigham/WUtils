(* Tests for: WUtils`WUtils`FindMathdocComments

   Author: danielb
*)

Test[
	WUtils`WUtils`FindMathdocComments[
		"(*!\n\t\\function FindMathdocComments\n\t\n\t\\calltable\n\t\tFindMathdocComments[str, funcName] '' find top of function Mathdoc comments.\n\t\n\t\\maintainer danielb\n*)\n",
		"FindMathdocComments"
	]
	,
	{{1, 153}}
	,
	TestID -> "FindMathdocComments-20160706-1BJC16"
]