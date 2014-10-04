(* Mathematica Package *)

BeginPackage["Algorithm2`",{"Utils`","Maps`","Algorithm1`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm2::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

Algorithm2[T_] := Module[{A = T, f = Function[{i,j},0],i0,j0},
	{i0,j0} = LeastCoordinate[Shape[A]];
	While[({A,f,i0,j0} = Algorithm1[A,f,i0,j0];{i0,j0} != {1,1})];
	{A,f}
]

End[] (* End Private Context *)

EndPackage[]