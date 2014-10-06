(* Mathematica Package *)

BeginPackage["Algorithm1Prime`",{"Utils`","Predicates`","Maps`","AlgorithmPPrime`"}]
(* Exported symbols added here with SymbolName::usage *)  
Algorithm1Prime::"bananas"
CandidateCells::"bananas"
MaxCandidateCell::"bananas"
FunctionNWPath::"bananas"
ComparePaths::"bananas"

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

ComparePaths[p0_List,p1_List]:= Module[{
	(* PAD PATHS BEFORE GET VALUE *)
	getValue = Function[{path,index},
		Module[{
			length = Max[Length /@ {p0,p1}]
			},
			PadLeft[path,length,nothing][[-index]] /. rules
		]
	],
	compare
	},
	compare = Function[{index},
		Module[{
			v0 = getValue[p0,index],
			v1 = getValue[p1,index]
			},
			If[
				v0 == v1,
				compare[index + 1],
				If[v0 > v1,p0,p1]
			]
		]
	];
	compare[1]
]

MaxNWPath[paths_List]:=If[
	Length[paths] > 1,
	MaxNWPath[Drop[paths,If[ComparePaths[paths[[1]],paths[[2]]] == paths[[1]],1,2]]],
	First[paths]
]

MaxCandidateCell[T_,g_,i0_,j0_] := Module[{
		candidates = CandidateCells[T,g,i0,j0],
		f = FunctionNWPath[T,i0,j0]
	},
	First[Sort[candidates,ComparePaths[f[#1],f[#2]]&] ];
]

Algorithm1Prime[T_,g_,i0_:1,j0_:1]:=Module[{i1,j1,p,U,f,prev},
	{i1,j1,p} = MaxCandidateCell[T,g,i0,j0];
	prev = NextCoordinates[Shape[T],i0,j0,-1];
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