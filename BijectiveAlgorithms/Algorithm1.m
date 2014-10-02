(* Mathematica Package *)

BeginPackage["Algorithm1`",{"Predicates`","AlgorithmP`","Maps`"}]

Algorithm1::"bananas"

Begin["`Private`"] (* Begin Private Context *)

Algorithm1[A_,f_,i0_,j0_]:=Module[{i1,j1,i2,j2,a,B,g},
	Assert[Not[1 == i0 == j0]];
	Assert[OrderedToQ[A,i0,j0]];

	{i1,j1}=NextCoordinates[Shape[A],i0,j0];
	a = A[[i1,j1]];

	(* May need to suppress messages *)
	B = AlgorithmP[A,a];

	{i2,j2} = Flatten[Position[B,a]];

	g = Function[{i,j},
		If[
			j == j1,
			If[
				i==i2,
				j2-j1,
				f[i+1,j1]
			],
			f[i,j]
		]
	];
	{B,g,i1,j1}
]

End[] (* End Private Context *)

EndPackage[]