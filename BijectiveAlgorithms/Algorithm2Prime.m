(* Mathematica Package *)

BeginPackage["Algorithm2Prime`",{"Algorithm1Prime`","Predicates`","Maps`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm2Prime::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

Algorithm2Prime[T_,g_:(0 &),i0_:1,j0_:1]:= Module[{
	leastCell = {1,Shape[T][[1]]}
	},
	If[
		{i0,j0} == leastCell,
		{T,g},
		Algorithm2Prime @@ (Algorithm1Prime[T,g,i0,j0])
	]
]

End[] (* End Private Context *)

EndPackage[]