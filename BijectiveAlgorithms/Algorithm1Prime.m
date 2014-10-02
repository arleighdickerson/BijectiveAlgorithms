(* Mathematica Package *)

BeginPackage["Algorithm1Prime`",{"Utils`","Predicates`","Maps`","AlgorithmPPrime`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm1Prime::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

north = "north";
nothing = "nothing";
west = "west";

rules = {north -> 1, nothing -> 0, west -> -1};

FunctionNWPath[T:_?FillingQ,i0_Integer,j0_Integer]:=Function[a,
	Rest[
		Reverse[
			Map2[
				Function[{T0,T1},
					First[Position[T0,a]] - First[Position[T1,a]] /. {
						{1,0} -> north,
						{0,1} -> west,
						{0,0} -> nothing
					}
				],
				FixedPointList[FunctionPPrime[a,i0,j0],T]
			]
		]
	]
]

MaxNWPath[paths_List]:=If[
	Not[Equal @@ Length /@ paths],
	MaxNWPath[PadPaths[paths]],
	If[
		Length[paths] > 0,
		Last[Sort[paths, (#1 /. rules) > (#2 /.rules) &]]
	]
]
(**
 * TODO: Do we pad on the right or left side of encoding?
 * i.e. do we pad before or after the reverse... (path read r to l)???
 *)
PadPaths[paths_] := PadRight[
	paths,
	{Length[paths],Max[Length /@ paths]},
	nothing
]

CandidateCells[U:_?FillingQ,g_,i1_Integer,j1_Integer]:= FilterIJV[
	Function[{i,j,v},
		i >= i1 && 
		g[i,j1] >= 0 && 
		j == (j1 + g[i,j1])
	],U
]

MaxCandidateCell[T_,g_,i0_,j0_] := Module[{
	f = FunctionNWPath[T,i0,j0],
	candidates = CandidateCells[T,g,i0,j0]
	},
	Last[Sort[candidates, (f[#1[[3]]] /.rules) > (f[#2[[3]]] /.rules) &]]
]


Algorithm1Prime[T:_?FillingQ,g_,i0_,j0_]:=Module[{i1,j1,p,U,f,prev},
	prev = PreviousCoordinates[Shape[T],i0,j0];

	{i1,j1,p} = MaxCandidateCell[T,g,i0,j0];

	U = AlgorithmPPrime[T,p,i0,j0];
	f = Function[{i,j}, 
		If[
			j == j0 && (i0 <= i <= i1),
			If[
				i == i0,
				0,
				g[i-1,j]+1
			],
			g[i,j]
		]
	];
	{U,f,prev[[1]],prev[[2]]}
]

End[] (* End Private Context *)

EndPackage[]