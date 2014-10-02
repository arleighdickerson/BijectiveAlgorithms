(* Mathematica Package *)

BeginPackage["AlgorithmP`",{"Combinatorica`","Utils`"}]

FunctionP::"bananas"
AlgorithmP::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

FunctionP[a_]:=Function[{A},
	Module[{i0,j0,n,b,c,least},

		n = Length[Flatten[A]];
		Assert[a <= n];

		{i0,j0} = First[Position[A,a]];

		b = If[j0 < Length[A[[i0]]],
			A[[i0,j0 + 1]],
			n+1
		];

		c = If[i0 < Length[TransposeTableau[A][[j0]]],
			A[[i0 + 1,j0]],
			n+1
		];

		least = Min[b,c];

		If[a <  least,
			A,
			Swap[A,a,least]
		]
	]
]

AlgorithmP[A:_,a_]:=FixedPoint[FunctionP[a], A]

End[] (* End Private Context *)

EndPackage[]