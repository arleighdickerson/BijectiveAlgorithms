(* Mathematica Package *)

BeginPackage["Algorithm1Prime`",{"Utils`","Predicates`","Maps`","AlgorithmPPrime`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm1Prime::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

CandidateCells[U_,g_,i1_Integer,j1_Integer]:= FilterIJV[
	Function[{i,j,v},
		i >= i1 && 
		g[i,j1] >= 0 && 
		j == (j1 + g[i,j1])
	],U
]

north = "north";
nothing = "nothing";
west = "west";

rules = {north -> 1, nothing -> 0, west -> -1};

FunctionNWPath[T_,i0_Integer,j0_Integer]:=Function[a,
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
	Last[Sort[paths, (#1 /. rules) > (#2 /.rules) &]]
]

PadPaths[paths_] := PadLeft[#,Max[Length /@ paths],nothing] & /@ paths

MaxCandidateCell[T_,g_,i0_,j0_] := Module[{},
	(*** TODO: Implement me! ************************************************)
	Assert[False];
	{0,0,0};
]


Algorithm1Prime[T_,g_: 0 & ,i0_:1,j0_:1]:=Module[{i1,j1,p,U,f,prev},
	{i1,j1,p} = MaxCandidateCell[T,g,i0,j0];
	
	prev = NextCoordinates[Shape[T],i0,j0,-1];

	U = AlgorithmPPrime[T,p,i0,j0];
	f[i_,j_]:=g[i,j];
	For[i = i0 + 1, i <= i1, i++, f[i,j0] = g[i - 1, j0] + 1];
	f[i0,j0] = 0;
	{U,f,prev[[1]],prev[[2]]}
]

End[] (* End Private Context *)

EndPackage[]