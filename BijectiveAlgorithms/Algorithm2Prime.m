(* Mathematica Package *)

BeginPackage["Algorithm2Prime`",{"Algorithm1Prime`","Predicates`","Maps`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm2Prime::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

Algorithm2Prime[T_,g_,i0_:1,j0_:1]:= If[
	CoordinateRank[T,i0,j0] == 1,
	T,
	Algorithm2Prime @@ (Algorithm1Prime[T,g,i0,j0])
]

End[] (* End Private Context *)

EndPackage[]