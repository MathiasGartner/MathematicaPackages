(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["Utils`"]

Unprotect @@ Names["Utils`*"];
ClearAll @@ Names["Utils`*"];

color::usage = "color[n]";

FromScientificForm::usage = "parses a number from scientific form 1.234567890123e+02";ToScientificForm::usage = "returns a number in scientific notation eg. 1.234567890123e+02";

GetWithFourierFrequencies::usage = "GetWithFourierFrequencies[x, dt] get fourier frequencies for data points on grid dt";
F::usage = "f[{x0, x1, ...}, dt] discrete fourier transform of {x0, x1, ...} with spacing dt between data points";
FPlot::usage = "FPlot[{X0, X1, ...}] plots real part, imaginary part and absolute value of the list {X0, X1, ...}";
F2D::usage = "F[{{x00, ...}, ...}, {dt1, dt2}] two-dimensioal discrete fourier transform with spacing dt1, dt2 between data points";

GetLargestWithIndex::usage = "GetLargestWithIndex[list, n, indexList] returns the n largest elements of list with indices. indices can be provided with indexList";
GetLargestWithIndexBy::usage = "GetLargestWithIndex[list, f, n, indexList] returns the n largest elements of list with indices, ordered by f. indices can be provided with indexList";

Q::usage = "gets the q-th qudrant from a matrix Q[mat_, q_Integer: 1]";

Begin["Private`"]

$PlotTheme = {"Detailed"};

SetOptions[$FrontEnd, PrintingStyleEnvironment -> "Working"];

color[n_]:=Module[{cList, c, i},
cList = ColorData[97,"ColorList"];
i = Mod[n, Length[cList]];
c = If[i == 0, cList[[-1]], cList[[i]]];
c
];

FromScientificForm[numberString_]:=ToExpression/@StringSplit[StringReplace[StringReplace[numberString,RegularExpression["e([+-]?)(\\d+)"]:>"*^$1$2"], " "..->","], ","];

ToScientificForm[x_?NumericQ,ndig_Integer: 12]:=Module[{u,s,p,base,exp,sign,result},
	If[x==0,
		result="  0.000000000000e+00"
	,
		u=If[x==0,u=0,u=x];
		{s,p}=MantissaExponent[u];
		If[s!=0,{s=s*10;p=p-1}];
		base=ToString[PaddedForm[s,{ndig+2,ndig}]];
		exp=If[p>=0,ToString[p],ToString[-1*p]];
		If[StringLength[exp]<2,exp=StringJoin["0",exp],exp=exp];
		sign=If[p>=0,"e+","e-"];
		result=StringJoin[base,sign,exp];
	];
	result
];

GetWithFourierFrequencies[X_ /; OddQ[X // Length], dt_] := 
  Module[{n, \[Omega], XRot},
   n = X // Length;
   \[Omega] = Range[-(n - 1)/2, (n - 1)/2]*2\[Pi]/(n dt);
   XRot = RotateLeft[X, (n + 1) / 2];
   {\[Omega], XRot}
   ];
GetWithFourierFrequencies[X_ /; EvenQ[X // Length], dt_] := 
  Module[{n, \[Omega], XRot},
   n = X // Length;
   \[Omega] = Range[-n/2, n/2 - 1]*2\[Pi]/(n dt);
   XRot = RotateLeft[X, n / 2];
   {\[Omega], XRot}
   ];

F[x_, dt_] := Module[{X},
   X = Fourier[x, FourierParameters -> {-1, 1}];
   X = GetWithFourierFrequencies[X, dt];
   X
   ];

F2D[x_, d_] := Module[{X, \[Omega]1, \[Omega]2},
   X = Fourier[x, FourierParameters -> {-1, 1}];
   {\[Omega]1, X} = GetWithFourierFrequencies[X, d[[1]]];
{\[Omega]2, X}= GetWithFourierFrequencies[X//Transpose, d[[2]]];
X = X//Transpose;
{{\[Omega]1, \[Omega]2}, X}
   ];

FPlot[{\[Omega]_, X_}] := Module[{Xparts, plotData, labels},
   Xparts = X // {Re[#], Im[#], Abs[#]} &;
labels={"Re", "Im", "Abs"};
   plotData = Transpose@{\[Omega], #} & /@ Xparts;
MapIndexed[ListLinePlot[#1, PlotRange -> All, PlotMarkers->"OpenMarkers", PlotStyle->color[#2], PlotLegends->Placed[labels[[#2]],{0.1,0.1}]]&, plotData]
   (*ListLinePlot[#, PlotRange -> All] & /@ plotData*)
   ];

GetLargestWithIndex[l_, n_]:=Module[{indexList},
indexList=Range[0, Length[l] - 1];
GetLargestWithIndex[l, n, indexList]
];
GetLargestWithIndex[l_, n_, indexList_]:=Module[{indices},
indices=Ordering[l, -n];
({indexList, l}//Transpose)[[indices]]
];
GetLargestWithIndexBy[l_, f_, n_]:=Module[{indexList},
indexList=Range[0, Length[l] - 1];
GetLargestWithIndexBy[l, f, n, indexList]
];
GetLargestWithIndexBy[l_, f_, n_, indexList_]:=Module[{indices},
indices=OrderingBy[l, f, -n];
({indexList, l}//Transpose)[[indices]]
];

Q[mat_, q_Integer: 1] := Module[{d, nd1, nd2, matQ},
d = Ceiling[(Dimensions[mat]+1)/2];
{nd1, nd2} = Switch[q,
1, {;;d[[1]], d[[2]];;},
2, {;;d[[1]], ;;d[[2]]},
3, {d[[1]];;, ;;d[[2]]},
4, {d[[1]];;, d[[2]];;}
];
matQ = mat[[nd1, nd2]];
matQ
];

End[];

Protect @@ Names["Utils`*"];

EndPackage[];






