(* Mathematica Test File *)

Test[
	Quiet[
		FixedPoint[
			FunctionP[5],
			{{5,1},{2,3},{4}}
		]
	],
	{{1,3},{2,5},{4}},
	TestID->"FunctionP"
]

Test[
	Quiet[AlgorithmP[{{5,1},{2,3},{4}},5]],
	{{1,3},{2,5},{4}},
	TestID->"AlgorithmP"
]

Test[
	Quiet[
		FixedPoint[
			FunctionPPrime[8,1,2],
			{{2, 1, 6}, {4, 3, 7}, {9, 5, 8}}
		]
	],
	{{2, 8, 1}, {4, 3, 6}, {9, 5, 7}},
	TestID->"FunctionPPrime"
]

Test[
	Quiet[AlgorithmPPrime[{{2, 1, 6}, {4, 3, 7}, {9, 5, 8}},8,1,2]],
	{{2, 8, 1}, {4, 3, 6}, {9, 5, 7}},
	TestID->"AlgorithmPPrime"
]
