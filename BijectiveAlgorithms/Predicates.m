(* Mathematica Package *)

BeginPackage["Predicates`", { "Utils`","Combinatorica`"}]
(* Exported symbols added here with SymbolName::usage *)  
ShapeQ::"bananas"
FillingQ::"bananas"
HookQ::"bananas"
OrderedToQ::"bananas"
StandardQ::"bananas"
HookBounds::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

ShapeQ[i_]:= PartitionQ[i] && WeaklyDecreasingQ[i] && And @@ IntegerQ /@ i;

FillingQ[i_] := ListQ[i] && 
				Depth[i] == 3 && 
				Equal @@ Depth /@ i &&
				ShapeQ[Shape[i]] && 
				Range[1,Total[Shape[i]]] == Sort[Flatten[i]];

LegLength[lambda_,i0_,j0_]:= Count[# >= j0 & /@ Drop[lambda,i0],True];

ArmLength[lambda_,i0_,j0_]:=lambda[[i0]] - j0

HookBounds[lambda:_?ShapeQ,i0_,j0_]:= {-LegLength[lambda,i0,j0],ArmLength[lambda,i0,j0]}

HookBounds[lambda:_?ShapeQ]:=Table[
	HookBounds[lambda,i,j],
	{i,1,Length[lambda]},
	{j,1,lambda[[i]]}
];

HookQ[f_,lambda_]:=And @@ Flatten[
		Table[
			Module[{bounds = HookBounds[lambda,i,j]},
				bounds[[1]]<= f[i,j] <= bounds[[2]]
			],
			{i,1,Length[lambda]},
			{j,1,lambda[[i]]}
		]
];

wildcard = "*";
WeaklyIncreasingOrWildcardQ[i_]:=And @@ Map2[#1 == wildcard || #2 == wildcard || #1 <= #2 &,i];

OrderedToQ[T_,i0_,j0_]:= Module[{
	skew = MapIJV[Function[{i,j,v},If[j > j0 || (j == j0 && i >= i0),v,wildcard]],T]
	},
	And @@ Flatten[{
		Evaluate[WeaklyIncreasingOrWildcardQ /@ skew], 
		Evaluate[WeaklyIncreasingOrWildcardQ /@ TransposeTableau[skew]]
	}]
];

StandardQ[T_] := OrderedToQ[T,1,1];


End[] (* End Private Context *)

EndPackage[]