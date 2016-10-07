(* Tests for: WUtils`WUtils`WriteFileIndented

   Author: danielb
*)

Test[
	WUtils`WUtils`WithTemporaryFiles[
		{file = ""},
		(
			WUtils`WUtils`WriteFileIndented[Table[i, {i, 1, 80}], file];
			Import[file, "Text"]
		)
	]
	,
	"{\n\t1,\n\t2,\n\t3,\n\t4,\n\t5,\n\t6,\n\t7,\n\t8,\n\t9,\n\t10,\n\t11,\n\t12,\n\t13,\n\t14,\n\t15,\n\t16,\n\t17,\n\t18,\n\t19,\n\t20,\n\t21,\n\t22,\n\t23,\n\t24,\n\t25,\n\t26,\n\t27,\n\t28,\n\t29,\n\t30,\n\t31,\n\t32,\n\t33,\n\t34,\n\t35,\n\t36,\n\t37,\n\t38,\n\t39,\n\t40,\n\t41,\n\t42,\n\t43,\n\t44,\n\t45,\n\t46,\n\t47,\n\t48,\n\t49,\n\t50,\n\t51,\n\t52,\n\t53,\n\t54,\n\t55,\n\t56,\n\t57,\n\t58,\n\t59,\n\t60,\n\t61,\n\t62,\n\t63,\n\t64,\n\t65,\n\t66,\n\t67,\n\t68,\n\t69,\n\t70,\n\t71,\n\t72,\n\t73,\n\t74,\n\t75,\n\t76,\n\t77,\n\t78,\n\t79,\n\t80\n}"
	,
	TestID -> "WriteFileIndented-20161006-AEUAVN"
]