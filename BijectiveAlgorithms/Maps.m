(* Mathematica Package *)

BeginPackage["Maps`", { "Combinatorica`","Utils`"}]
(* Exported symbols added here with SymbolName::usage *)  
Successors::"bananas"
CoordinateRank::"bananas"
SuccessiveCoordinates::"bananas"
NextCoordinates::"bananas"
PreviousCoordinates::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

Successors[T_]:=Flatten[Reverse /@ Reverse[TransposeTableau[T]]]

Coordinates[lambda_] := MapIJV[
	{#1,#2} &, 
	Table[0,{#}] & /@ lambda
]

SuccessiveCoordinates[lambda_] := Flatten[Reverse /@ Reverse[TransposeTableau[Coordinates[lambda]]],1]

CoordinateRank[lambda_,i0_Integer,j0_Integer] := If[Length[#] == 0,0,First[#]] & @@ Position[SuccessiveCoordinates[lambda],{i0,j0},1,1]

NextCoordinates[lambda_,i0_,j0_,delta_]:= Module[{
	coords = SuccessiveCoordinates[lambda],
	index
	},
	index = First[Position[coords,{i0,j0},1,1]];
	Flatten[First[coords[[index + delta]]]]
]

PreviousCoordinates[lambda_,i0_,j0_]:= Module[{
		s = SuccessiveCoordinates[lambda],
		pos
	},
	pos = First[Flatten[Position[s,{i0,j0}]]];
	If[pos == 1,{},s[[pos - 1]]]
]

End[] 

EndPackage[]