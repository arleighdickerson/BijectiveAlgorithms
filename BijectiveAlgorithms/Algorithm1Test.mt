(* Mathematica Test File *)
T0 = {{6,2},{4,3},{5,1}};
J0 = {{0,0},{0,0},{0,0}};
least = LeastCoordinate[Shape[T0]];
expected = {
	{
		T0,
		J0
	},{
		{{6,2},
		 {4,1},
		 {5,3}},
		{{0,0},
		 {0,-1},
		 {0,0}}
	},{
		{{6,1},
		 {4,2},
		 {5,3}},
		{{0,-2},
		 {0,0},
		 {0,0}}
	},{
		{{6,1},
		 {4,2},
		 {3,5}},
		{{0,-2},
		 {0,0},
		 {1,0}}
	},{
		{{6,1},
		 {2,4},
		 {3,5}},
		{{0,-2},
		 {1,0},
		 {1,0}}
	},{
		{{1,4},
		 {2,5},
		 {3,6}},
		{{0,-2},
		 {0,0},
		 {1,0}}
	}
};

algorithm1Results := NestWhileList[
	Algorithm1 @@ # &,
	{T0,J0,least[[1]],least[[2]]},
	Not[#[[3]] == #[[4]] == 1] &
];

algorithm1PrimeResults := NestWhileList[
	Algorithm1Prime @@ # &,
	Last[algorithm1Results],
	{#[[3]],#[[4]]} != least &
];

Test[
	Take[#,2] & /@ algorithm1Results,
	expected,
	TestID->"Algorithm1"
]

Test[
	Take[#,2] & /@ algorithm1PrimeResults,
	Reverse[expected],
	TestID->"Algorithm1Prime"
]
