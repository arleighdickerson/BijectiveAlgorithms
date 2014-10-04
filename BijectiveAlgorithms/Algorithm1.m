(* Mathematica Package *)

BeginPackage["Algorithm1`",{"Utils`","Predicates`","AlgorithmP`","Maps`"}]

Algorithm1::"bananas bananas bananas"

Begin["`Private`"] (* Begin Private Context *)

Algorithm1[A_,f_,i0_,j0_]:=Module[{i1,j1,i2,j2,a,B,g},
	Assert[Not[1 == i0 == j0]];
	Assert[OrderedToQ[A,i0,j0]];

	{i1,j1}=NextCoordinate[Shape[A],i0,j0,1];
	a = A[[i1,j1]];

	B = AlgorithmP[A,a];

	{i2,j2} = Flatten[Position[B,a]];
	
	g[i_,j_]:=f[i,j];
	For[i = i1, i < i2, i++, g[i,j1] = f[i+1,j1] - 1];
	g[i2,j1] = j2 - j1;
	
	{B,g,i1,j1}
]

End[] (* End Private Context *)

EndPackage[]