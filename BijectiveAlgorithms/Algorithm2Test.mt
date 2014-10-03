(* Mathematica Test File *)

Test[
	Module[{
		T = {{6,2},{4,3},{5,1}},
		g = 0 &
		},
		Algorithm2[T][[1]]
	],
	{{1,4},{2,5},{3,6}},
	TestID->"Algorithm2Prime"
]