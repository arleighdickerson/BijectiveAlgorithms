(* Mathematica Package *)

BeginPackage["Utils`"]

Map2::"bananas"
WeaklyIncreasingQ::"bananas"
WeaklyDecreasingQ::"bananas"
Swap::"bananas"
MapIJV::"bananas"
MapIJ::"bananas"
FilterIJV::"bananas"
FilterIJ::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

Map2 = # @@@ Partition[#2, 2, 1] &;

WeaklyIncreasingQ[i_List]:= If[
	Length[i] < 2, 
	True,
	And @@ Map2[(#1 == Null || #2 == Null) || #1 <= #2 &, i]
]

WeaklyDecreasingQ[i_List] := WeaklyIncreasingQ[Reverse[i]]


MapIJV[f_,t_]:=Module[{},
	Table[
		f[i,j,t[[i,j]]],
		{i, 1, Length[t]},
  		{j, 1, Length[t[[i]]]}
	]
]

MapIJ[f_,t_]:=MapIJV[Function[{i,j,v},f[i,j]],t]

FilterIJV[predicate_,t_]:=Module[{},
	Select[
		Flatten[
			MapIJV[
				Function[{i,j,v},
					If[
						predicate[i,j,v],
						{i,j,v},
						{}
					]
				],t
			],1
		],# != {} &
	]
]

FilterIJ[predicate_,t_]:=FilterIJV[Function[{i,j,v},predicate[i,j]],t]


Swap[T_, a_, b_]:= Module[{pa,pb},
	pa=Flatten[Position[T,a]];
	pb=Flatten[Position[T,b]];
	If[FreeQ[Length[#] == 0 &/@{pa,pb},True],
		ReplacePart[T,{pa-> b,pb -> a}],
		Throw["Element(s) not in tableau"];
	]
]

End[] (* End Private Context *)

EndPackage[]