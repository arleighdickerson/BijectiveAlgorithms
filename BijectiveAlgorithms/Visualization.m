(* Mathematica Package *)

BeginPackage["Visualization`"]
(* Exported symbols added here with SymbolName::usage *)  
PlotTableau::"bananas"

Begin["`Private`"] (* Begin Private Context *) 

ItemF = Item[#,Frame -> Black] &;

PlotTableau[T_,itemF_Function:ItemF] := Grid[
	Function[row,itemF /@ row] /@ T
];

End[] (* End Private Context *)

EndPackage[]