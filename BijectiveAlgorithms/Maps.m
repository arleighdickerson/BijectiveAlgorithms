(* Mathematica Package *)

BeginPackage["Maps`", { "Combinatorica`","Utils`"}]
(* Exported symbols added here with SymbolName::usage *)  
CoordinateRank::"bananas"
NextCoordinate::"bananas"
LeastCoordinate::"bananas"
Coordinates::"bananas"
SuccessiveCoordinates::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

Coordinates[lambda_] := Flatten[
	MapIJ[
		{#1,#2} &, 
		Table[0,{#}] & /@ lambda
	],1
];

LeastCoordinate[lambda_]:=First[SuccessiveCoordinates[lambda]];

SuccessiveCoordinates[lambda_] := Sort[Coordinates[lambda], #1[[2]] > #2[[2]] || (#1[[2]] == #2[[2]] && #1[[1]] > #2[[1]]) &];

CoordinateRank[lambda_,i0_Integer,j0_Integer] := Module[{
	position = Flatten[Position[SuccessiveCoordinates[lambda],{i0,j0}]]
	},
	Assert[Length[position] > 0];
	position
];

NextCoordinate[lambda_,i0_,j0_,delta_]:= Module[{
	coords = SuccessiveCoordinates[lambda],
	index
	},
	index = First[Position[coords,{i0,j0},1,1]];
	Flatten[First[coords[[index + delta]]]]
];

End[] 

EndPackage[]