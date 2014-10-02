(* Mathematica Package *)

BeginPackage["Algorithm2`",{"Algorithm1`","Maps`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm2::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

Algorithm2[T_] := Module[{A = T, f = Function[{i,j},0],i0,j0},
	{i0,j0} = Flatten[Position[A,Successors[A][[1]]]];
	While[Not[1 == i0 == j0],
		{A,f,i0,j0} = Algorithm1[A,f,i0,j0]
	];
	{A,f}
]

End[] (* End Private Context *)

EndPackage[]