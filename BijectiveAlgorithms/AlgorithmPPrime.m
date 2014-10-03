(* Mathematica Package *)

BeginPackage["AlgorithmPPrime`",{"Predicates`","Utils`","Maps`"}]
(* Exported symbols added here with SymbolName::usage *)  

FunctionPPrime::"bananas"
AlgorithmPPrime::"bananas"

Begin["`Private`"] (* Begin Private Context *) 
FunctionPPrime[a_Integer,i0_Integer,j0_Integer]:=Function[{T},
	Module[{i1,j1},
		Assert[a > 0];

		{i1,j1} = First[Position[T,a,2,1]];

		If[
			i1 < i0 || (i1 == i0 && j1 <= j0),
			T,
			Module[{
				b = If[j1 > j0,T[[i1,j1-1]],0],
				c = If[i1 > i0, T[[i1-1,j1]],0]
			},
				Swap[T,a,Max[b,c]]
			]
		]
	]
]

AlgorithmPPrime[T:_?FillingQ,a_Integer,i0_Integer,j0_Integer]:=FixedPoint[
	FunctionPPrime[a,i0,j0],
	T
]
End[] (* End Private Context *)

EndPackage[]