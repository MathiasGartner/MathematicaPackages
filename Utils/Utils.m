(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["Utils`"]

Unprotect @@ Names["Utils`*"];
ClearAll @@ Names["Utils`*"];

color::usage = "color[n]";

FromScientificForm::usage = "parses a number from scientific form 1.234567890123e+02";ToScientificForm::usage = "returns a number in scientific notation eg. 1.234567890123e+02";

F::usage = "f[{x0, x1, ...}, dt] discrete fourier transform of {x0, x1, ...} with spacing dt between data points";
FPlot::usage = "FPlot[{X0, X1, ...}] plots real part, imaginary part and absolute value of the list {X0, X1, ...}";

Begin["Private`"]

color[n_]:=ColorData[97,"ColorList"][[n]];

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
   \[Omega] = Range[-(n - 1)/2, (n - 1)/2]/(n dt);
   XRot = RotateLeft[X, (n + 1) / 2];
   {\[Omega], XRot}
   ];
GetWithFourierFrequencies[X_ /; EvenQ[X // Length], dt_] := 
  Module[{n, \[Omega], XRot},
   n = X // Length;
   \[Omega] = Range[-n/2, n/2 - 1]/(n dt);
   XRot = RotateLeft[X, n / 2];
   {\[Omega], XRot}
   ];

F[x_, dt_] := Module[{n, \[Omega], X},
   n = Length[x];
   X = Fourier[x, FourierParameters -> {-1, 1}];
   X = GetWithFourierFrequencies[X, dt];
   X
   ];

FPlot[{\[Omega]_, X_}] := Module[{Xparts, plotData},
   Xparts = X // {Re[#], Im[#], Abs[#]} &;
   plotData = Transpose@{\[Omega], #} & /@ Xparts;
MapIndexed[ListLinePlot[#1, PlotRange -> All, PlotMarkers->"\[FilledCircle]", PlotStyle->color[#2]]&, plotData]
   (*ListLinePlot[#, PlotRange -> All] & /@ plotData*)
   ];

End[];

Protect @@ Names["Utils`*"];

EndPackage[];






