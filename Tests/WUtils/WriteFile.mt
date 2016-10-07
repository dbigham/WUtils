(* Tests for: WUtils`WUtils`WriteFile

   Author: danielb
*)

Test[
	WUtils`WUtils`WithTemporaryFiles[
		{file = ""},
		(
			WUtils`WUtils`Tests`var = Table[i, {i, 1, 60}];
			WUtils`WUtils`WriteFile[
				<|
					"Path" -> file,
					"Variable" -> HoldComplete[WUtils`WUtils`Tests`var],
					"Writer" -> WUtils`WUtils`WriteFileIndented
				|>
			];
			Import[file, "Text"]
		)
	]
	,
	"{\n\t1,\n\t2,\n\t3,\n\t4,\n\t5,\n\t6,\n\t7,\n\t8,\n\t9,\n\t10,\n\t11,\n\t12,\n\t13,\n\t14,\n\t15,\n\t16,\n\t17,\n\t18,\n\t19,\n\t20,\n\t21,\n\t22,\n\t23,\n\t24,\n\t25,\n\t26,\n\t27,\n\t28,\n\t29,\n\t30,\n\t31,\n\t32,\n\t33,\n\t34,\n\t35,\n\t36,\n\t37,\n\t38,\n\t39,\n\t40,\n\t41,\n\t42,\n\t43,\n\t44,\n\t45,\n\t46,\n\t47,\n\t48,\n\t49,\n\t50,\n\t51,\n\t52,\n\t53,\n\t54,\n\t55,\n\t56,\n\t57,\n\t58,\n\t59,\n\t60\n}"
	,
	TestID -> "WriteFile-20161006-0CFYEV"
]