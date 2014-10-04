(* Mathematica Test File *)

Test[
	Module[{
		T0 = {{6,2},{4,3},{5,1}},T1,g1
		},
		{T1,g1} = Algorithm2[T0];
		{T1,MapIJ[g1,T1]}
	],
	{{{1,4},{2,5},{3,6}},
	{{0,-2},{0,0},{1,0}}},
	TestID->"Algorithm2"
]