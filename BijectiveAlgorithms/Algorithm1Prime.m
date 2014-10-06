(* Mathematica Package *)

BeginPackage["Algorithm1Prime`",{"Utils`","Predicates`","Maps`","AlgorithmPPrime`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm1Prime::"bananas"
CandidateCells::"bananas"
MaxCandidateCell::"bananas"
FunctionNWPath::"bananas"
ComparePaths::"bananas"
MaxNWPath::"bla"

Begin["`Private`"] (* Begin Private Context *) 

CandidateCells[U_,g_,i1_Integer,j1_Integer]:= #[[3]] & /@ FilterIJV[
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
		Map2[
			Function[{T0,T1},
				First[Position[T0,a]] - First[Position[T1,a]] /. {
					{1,0} -> north,
					{0,1} -> west,
					{0,0} -> nothing
				}
			],
			Drop[
				FixedPointList[FunctionPPrime[a,i0,j0],T],
				-1
			]
	]
]

MaxNWPath[paths_List]:=If[
	Length[paths] < 2, 
	First[paths], 
 	Module[{
 		convert = Function[{path}, 
     		Reverse[
     			PadLeft[
     				path, 
     				Max[Length /@ paths], 
     				nothing
     			]
     		] /. rules
 		],
 		dict
 	},
 	Do[dict[convert[path]] = path,{path,paths}];
 	dict[First[Sort[convert /@ paths]]]
	]
]

MaxCandidateCell[T_,g_,i0_,j0_] := Module[{
		candidates = CandidateCells[T,g,i0,j0],
		f = FunctionNWPath[T,i0,j0]
	},
	Do[dict[f[candidate]] = candidate,{candidate,candidates}];
	dict[MaxNWPath[f /@ candidates]]
]

Algorithm1Prime[T_,g_,i0_:1,j0_:1]:=Module[{i1,j1,p,U,f,prev},
	p = MaxCandidateCell[T,g,i0,j0];
	{i1,j1} = Flatten[Position[T,p]];
	prev = NextCoordinate[Shape[T],i0,j0,-1];
	U = AlgorithmPPrime[T,p,i0,j0];
	f = Table[
   		If[
   			j == j0 && i0 <= i <= i1 ,
    		If[
    			i == i0, 
    			0, 
    			g[i - 1, j0] + 1
    		],
    		g[i, j]
    	], 
    	{i, 1, Length[T]}, {j, 1, Length[T][[i]]}
    ][[#1.#2]] &;
	{U,f,prev[[1]],prev[[2]]}
]

End[] (* End Private Context *)

EndPackage[]