(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["Plot`"]

Unprotect @@ Names["Plot`*"];
ClearAll @@ Names["Plot`*"];

PlotER::usage = "plots real part of energy";
PlotEI::usage = "plots imaginary part of energy";
PlotEKin::usage = "plots kinetic part of energy";
PlotEPot::usage = "plots potential part of energy";

PlotPR::usage = "plots up to 50 real parameters";
PlotPI::usage = "plots up to 50 imaginary parameters";
PlotPRForTimesteps::usage = "plots real parameters as line for several timesteps in order to give an approximate view of u_2(r_ij)";
PlotPRDiffs::usage = "plots change in parameters";

PlotLastGr::usage = "plots g_2(r_ij) for the last available timestamp";
PlotLastSk::usage = "plots S(k) for the last available timestamp";
PlotLastEk::usage = "plots E(k) for the last available timestamp";
PlotLastRho::usage = "plots \[Rho](r) for the last available timestamp";
PlotLastRho2::usage = "plots \!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)(\!\(\*SubscriptBox[\(r\), \(i\)]\), \!\(\*SubscriptBox[\(r\), \(j\)]\)) for the last available timestamp";

PlotGrData::usage = "plots g_2(r_ij, t) as matrix plot and line plot";
PlotSkData::usage = "plots S(k, t) as matrix plot and line plot";
PlotRhoData::usage = "plots \[Rho](r, t) as matrix plot and line plot";

PlotGrByPosition::usage = "plots g_2(r_ij, t) for fixed values of r_ij";

AnimateGr::usage = "animates g_2(r_ij, t)";
AnimateRho::usage = "animates \[Rho](r, t)";

CompareER::usage = "plots real part of energy for several datasets";

PlotAll::usage = "generates all plots";

ExportAllPlots::usage = "exports all plots generated by PlotAll[data]";
ExportPlot::usage = "ExportPlot[plot, directory, filename] exports plot to directory/filename.pdf";

Begin["Private`"]

Needs["Utils`", FileNameJoin[{NotebookDirectory[], "..", "Utils", "Utils.m"}]];
Needs["ListOperations`", FileNameJoin[{NotebookDirectory[], "..", "Utils", "ListOperations.m"}]];
Needs["Data`", FileNameJoin[{NotebookDirectory[], "Data.m"}]];

$PlotTheme={"Detailed"};

SetOptions[$FrontEnd,PrintingStyleEnvironment->"Working"];

imageSize = Medium;

PlotSingleObservable[x_, y_, label_String, colIndex_:1, legend_:""]:=Module[{count},
count = Min[Length[x], Length[y]];
ListLinePlot[{x[[;;count]], y[[;;count]]}//Transpose, PlotRange->All, PlotLegends->If[legend=="", None, LineLegend[color[colIndex], {legend}]], PlotLabel->label,  ImageSize->imageSize, PlotStyle->color[colIndex]]
];
PlotTemporalData[data_, times_, label_String]:=Module[{count},
count = Min[Length[data//Transpose], Length[times]];
ListLinePlot[TemporalData[data[[All, ;;count]], {times[[;;count]]}], PlotRange->All, PlotLegends->None, PlotLabel->label,PlotStyle->"Rainbow", ImageSize->imageSize]
];

PlotLists[data_, label_String]:=Module[{},
ListLinePlot[data, PlotRange->All, PlotLegends->None, PlotLabel->label,PlotStyle->"Rainbow", ImageSize->imageSize]
];

PlotER[data_] := PlotSingleObservable[data["timesSystem"], data["eR"], "Re[E(t)]"];
PlotEI[data_] := PlotSingleObservable[data["timesSystem"], data["eI"], "Im[E(t)]"];
PlotEKin[data_] := PlotSingleObservable[data["timesSystem"], data["other"][[1]], "Re[E_kin(t)]"];
PlotEPot[data_] := PlotSingleObservable[data["timesSystem"], data["other"][[2]], "E_pot(t)"];

PlotPR[data_] := PlotTemporalData[data["pR"][[;;-2]]//DS50, data["timesSystem"], "parameters real (t)"];
PlotPI[data_] := PlotTemporalData[data["pI"][[;;-2]]//DS50, data["timesSystem"], "parameters imag (t)"];
PlotPRForTimesteps[data_] := PlotLists[data["pR"][[;;-2]]//Transpose//DS10, "parmeters real (k)"]
PlotPRDiffs[data_] := PlotPRDiffs[data, 50];
PlotPRDiffs[data_, lastN_] := Module[{lastData},
lastData = If[Length[data["timesSystem"]]> lastN,
TakeData[data, -lastN;;-1;;1],
data
];
PlotTemporalData[(lastData["pR"][[;;-2]]-(lastData["pR"][[;;-2]]//Transpose)[[1]])//DS50, lastData["timesSystem"], "parameters real (t)"]
];

PlotLastGr[data_/;MatchQ[data, KeyValuePattern["gr"->{{}}]]] := Nothing;
PlotLastGr[data_]:= PlotSingleObservable[data["grGrid"], data["gr"]//Transpose//Last, "g_2(r_ij, t="<>ToString[data["timesAdditional"]//Last]<>")"];
PlotLastSk[data_/;MatchQ[data, KeyValuePattern["sk"->{{}}]]] := Nothing;
PlotLastSk[data_]:= PlotSingleObservable[data["skGrid"], data["sk"]//Transpose//Last, "S(k, t="<>ToString[data["timesAdditional"]//Last]<>")"];
PlotLastEk[data_/;MatchQ[data, KeyValuePattern["sk"->{{}}]]] := Nothing;
PlotLastEk[data_]:= PlotSingleObservable[data["skGrid"], ({data["skGrid"], data["sk"]//Transpose//Last}//Transpose)/.{k_, s_}-> k^2/s, "E(k, t="<>ToString[data["timesAdditional"]//Last]<>")"];
PlotLastRho[data_/;MatchQ[data, KeyValuePattern["rho"->{{}}]]] := Nothing;
PlotLastRho[data_]:= PlotSingleObservable[data["rhoGrid"], data["rho"]//Transpose//Last, "\[Rho](r, t="<>ToString[data["timesAdditional"]//Last]<>")"];
PlotLastRho2[data_/;MatchQ[data, KeyValuePattern["rho2"->{{}}]]] := Nothing;
PlotLastRho2[data_]:= Module[{rangeR1, rangeR2, range, dataRho, dataRho2, plotRho2, plotRho2Normalized, plots, t},
t=ToString[data["timesAdditional"]//Last];
rangeR1 = {data["rho2Grid"][[1]]//First, data["rho2Grid"][[1]]//Last};
rangeR2 = {data["rho2Grid"][[2]]//First, data["rho2Grid"][[2]]//Last};
range = {rangeR1, rangeR2};
dataRho2 = data["rho2"]//Transpose//Last//ArrayReshape[#, Length/@data["rho2Grid"]]&//Reverse;
dataRho = data["rho"]//Transpose//Last//KroneckerProduct[#, #]&;
plotRho2 = MatrixPlot[dataRho2,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize, PlotLabel->"\[Rho]_2(r_i, r_j, t) @ t="<>t,FrameLabel->{{"r_i", ""}, {"r_j", ""}}, DataRange->range];
plotRho2Normalized=MatrixPlot[dataRho2/dataRho,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize, PlotLabel->"\[Rho]_2(r_i, r_j, t) / (\[Rho](r_i, t)\[Rho](r_j, t)) @ t="<>t,FrameLabel->{{"r_i", ""}, {"r_j", ""}}, DataRange->range];
plots = {plotRho2, plotRho2Normalized};
plots
];

PlotGrData[data_/;MatchQ[data, KeyValuePattern["gr"->{{}}]]] := Nothing;
PlotGrData[data_]:=Module[{tmp, diffs, plotM, plotMD, plotL, plotLLog, rangeX, rangeT, range},
rangeX = {data["grGrid"]//First, data["grGrid"]//Last};
rangeT = {data["timesAdditional"]//First, data["timesAdditional"]//Last};
range = {rangeX, rangeT};
tmp=(data["gr"]//Transpose//Reverse);
diffs= ((#-tmp[[-1]])&)/@tmp;
plotM=tmp//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize, PlotLabel->"g_2(r_ij, t)",FrameLabel->{{"r_ij", ""}, {"t", ""}}, DataRange->range]&;
plotMD=diffs//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize,PlotLabel->"g_2(r_ij, t) - g_2(r_ij, 0)", FrameLabel->{{"r_ij", ""}, {"t", ""}}, DataRange->range]&;
plotL=tmp//Reverse//DS50//ListLinePlot[#, PlotRange->All, PlotStyle->"Rainbow", ImageSize->imageSize, PlotLabel->"g_2(r_ij, t)", DataRange->rangeX]&;
plotLLog=tmp//Reverse//DS50//ListLogLinearPlot[#, PlotRange->All, PlotStyle->"Rainbow", Joined->True, ImageSize->imageSize, PlotLabel->"g_2(r_ij, t)", DataRange->rangeX]&;
{plotM, plotMD, plotL, plotLLog}
];

PlotSkData[data_/;MatchQ[data, KeyValuePattern["sk"->{{}}]]] := Nothing;
PlotSkData[data_]:=Module[{tmp, diffs, plotM, plotMD, plotL, plotLLog, rangeX, rangeT, range},
rangeX = {data["skGrid"]//First, data["skGrid"]//Last};
rangeT = {data["timesAdditional"]//First, data["timesAdditional"]//Last};
range = {rangeX, rangeT};
tmp=(data["sk"]//Transpose//Reverse);
diffs= ((#-tmp[[-1]])&)/@tmp;
plotM=tmp//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize, PlotLabel->"S(k, t)",FrameLabel->{{"k", ""}, {"t", ""}}, DataRange->range]&;
plotMD=diffs//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize,PlotLabel->"S(k, t) - S(k, 0)", FrameLabel->{{"k", ""}, {"t", ""}}, DataRange->range]&;
plotL=tmp//Reverse//DS50//ListLinePlot[#, PlotRange->All, PlotStyle->"Rainbow", ImageSize->imageSize, PlotLabel->"S(k, t)", DataRange->rangeX]&;
plotLLog=tmp//Reverse//DS50//ListLogLinearPlot[#, PlotRange->All, PlotStyle->"Rainbow", Joined->True, ImageSize->imageSize, PlotLabel->"S(k, t)", DataRange->rangeX]&;
{plotM, plotMD, plotL, plotLLog}
];

PlotRhoData[data_/;MatchQ[data, KeyValuePattern["rho"->{{}}]]] := Nothing;
PlotRhoData[data_]:=Module[{tmp, diffs, plotM, plotMD, plotL, rangeX, rangeT, range},
rangeX = {data["rhoGrid"]//First, data["rhoGrid"]//Last};
rangeT = {data["timesAdditional"]//First, data["timesAdditional"]//Last};
range = {rangeX, rangeT};
tmp=(data["rho"]//Transpose//Reverse);
diffs= ((#-tmp[[-1]])&)/@tmp;
plotM=tmp//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize, PlotLabel->"\[Rho](r, t)",FrameLabel->{{"r", ""}, {"t", ""}}, DataRange->range]&;
plotMD=diffs//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->imageSize,PlotLabel->"\[Rho](r, t) - \[Rho](r, 0)", FrameLabel->{{"r", ""}, {"t", ""}}, DataRange->range]&;
plotL=tmp//Reverse//DS50//ListLinePlot[#, PlotRange->All, PlotStyle->"Rainbow", ImageSize->imageSize, PlotLabel->"\[Rho](r, t)", DataRange->rangeX]&;
{plotM, plotMD, plotL}
];

PlotGrByPosition[data_/;MatchQ[data, KeyValuePattern["gr"->{{}}]]] := Nothing;
PlotGrByPosition[data_]:=Module[{plots, rangeX, rangeT, range},
rangeX = {data["grGrid"]//First, data["grGrid"]//Last};
rangeT = {data["timesAdditional"]//First, data["timesAdditional"]//Last};
range = {rangeX, rangeT};
plots = Map[ListLinePlot[#[[1]], PlotRange->All, FrameLabel->{{"", ""}, {"", "\!\(\*SubscriptBox[\(g\), \(2\)]\)(\!\(\*SubscriptBox[\(r\), \(ij\)]\), t)"}}, PlotLegends->Placed["\!\(\*SubscriptBox[\(r\), \(ij\)]\)="<>ToString[#[[2]]],  {0.5, 0.2}], DataRange->rangeT]&, {data["gr"]//DS20, data["grGrid"]//DS20}//Transpose];
plots
];

AnimateGr[data_/;MatchQ[data, KeyValuePattern["gr"->{{}}]]] := Nothing;
AnimateGr[data_] := Module[{animation, plots, min, max, offset},
min = data["gr"]//Min;
max = data["gr"]//Max;
offset = (max - min)*0.1;
plots = PlotSingleObservable[data["grGrid"], #, "g_2(r_ij, t="<>ToString[data["timesAdditional"]//Last]<>")"]&/@(data["gr"]//Transpose);
animation = Manipulate[Show[plots[[i]], PlotRange->{All, {min - offset, max + offset}}], {i, 1, Length[plots], 1, AnimationRate->10,Appearance->"Open"}];
animation
];

AnimateRho[data_/;MatchQ[data, KeyValuePattern["rho"->{{}}]]] := Nothing;
AnimateRho[data_] := Module[{animation, plots, min, max, offset},
min = data["rho"]//Min;
max = data["rho"]//Max;
offset = (max - min)*0.1;
plots = PlotSingleObservable[data["rhoGrid"], #, "\[Rho](r, t="<>ToString[data["timesAdditional"]//Last]<>")"]&/@(data["rho"]//Transpose);
animation = Manipulate[Show[plots[[i]], PlotRange->{All, {min - offset, max + offset}}], {i, 1, Length[plots], 1, AnimationRate->10,Appearance->"Open"}];
animation
];

CompareER[data_] := Show[MapIndexed[PlotSingleObservable[#1["timesSystem"], #1["eR"], "Re[E(t)]", #2, FileNameTake[#1["directory"]]]&,data], PlotRange->All];

PlotAll[data_]:={
{PlotER[data], PlotEI[data], PlotEKin[data], PlotEPot[data]},
{PlotPR[data], PlotPI[data], PlotPRForTimesteps[data], PlotPRDiffs[data, 50]},
{PlotLastGr[data], PlotLastSk[data], PlotLastEk[data], PlotLastRho[data]},
PlotLastRho2[data],
PlotGrData[data],
PlotSkData[data],
PlotRhoData[data],
PlotGrByPosition[data]
};

PlotAllForExport[data_]:= Module[{plots},
plots = PlotAll[data];
Grid[{{plots[[1;;-2]]//Grid},{Partition[plots[[-1]], 4]//Grid}}]
];
ExportAllPlots[data_, filename_String:"data"]:=ExportAllPlots[data, data["directory"], filename];
ExportAllPlots[data_, directory_, filename_String:"data"]:=Export[FileNameJoin[{directory, filename<>".pdf"}], PlotAllForExport[data], ImageSize->1000];

ExportPlot[plot_, directory_, filename_String]:=
ExportPlot[plot, directory, filename, "pdf"];
ExportPlot[plot_, directory_, filename_String, filetype_String]:=Export[FileNameJoin[{directory, filename<>"."<>filetype}], plot, ImageSize->Large];

End[];

Protect @@ Names["Plot`*"];

EndPackage[];












