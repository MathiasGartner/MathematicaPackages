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

PlotLastGr::usage = "plots g_2(r_ij) for the last available timestamp";
PlotLastSk::usage = "plots S(k) for the last available timestamp";
PlotLastEk::usage = "plots E(k) for the last available timestamp";
PlotLastRho::usage = "plots \[Rho](r) for the last available timestamp";

PlotGrData::usage = "plots g_2(r_ij, t) as matrix plot and line plot";
PlotSkData::usage = "plots S(k, t) as matrix plot and line plot";
PlotRhoData::usage = "plots \[Rho](r, t) as matrix plot and line plot";

PlotAll::usage = "generates all plots";

ExportAllPlots::usage = "exports all plots generated by PlotAll[data]";

Begin["Private`"]

Needs["Utils`", FileNameJoin[{NotebookDirectory[], "..", "Utils", "Utils.m"}]];
Needs["ListOperations`", FileNameJoin[{NotebookDirectory[], "..", "Utils", "ListOperations.m"}]];

$PlotTheme={"Detailed"};

SetOptions[$FrontEnd,PrintingStyleEnvironment->"Working"];

PlotSingleObservable[x_, y_, label_String]:=Module[{count},
count = Min[Length[x], Length[y]];
ListLinePlot[{x[[;;count]], y[[;;count]]}//Transpose, PlotRange->All, PlotLegends->None, PlotLabel->label]
];
PlotTemporalData[data_, times_, label_String]:=Module[{count},
count = Min[Length[data//Transpose], Length[times]];
ListLinePlot[TemporalData[data[[All, ;;count]], {times[[;;count]]}], PlotRange->All, PlotLegends->None, PlotLabel->label,PlotStyle->"Rainbow"]
];

PlotER[data_] := PlotSingleObservable[data["timesSystem"], data["eR"], "E^R"];
PlotEI[data_] := PlotSingleObservable[data["timesSystem"], data["eI"], "E^I"];
PlotEKin[data_] := PlotSingleObservable[data["timesSystem"], data["other"][[1]], "E_kin"];
PlotEPot[data_] := PlotSingleObservable[data["timesSystem"], data["other"][[2]], "E_pot"];

PlotPR[data_] := PlotTemporalData[data["pR"][[;;-2]]//DS50, data["timesSystem"], "parameters real"];
PlotPI[data_] := PlotTemporalData[data["pI"][[;;-2]]//DS50, data["timesSystem"], "parameters imag"];

PlotLastGr[data_/;MatchQ[data, KeyValuePattern["gr"->{{}}]]] := Nothing;
PlotLastGr[data_]:= PlotSingleObservable[data["grGrid"], data["gr"]//Transpose//Last, "g_2(r_ij, t="<>ToString[data["timesAdditional"]//Last]<>")"];
PlotLastSk[data_/;MatchQ[data, KeyValuePattern["sk"->{{}}]]] := Nothing;
PlotLastSk[data_]:= PlotSingleObservable[data["skGrid"], data["sk"]//Transpose//Last, "S(k, t="<>ToString[data["timesAdditional"]//Last]<>")"];
PlotLastEk[data_/;MatchQ[data, KeyValuePattern["sk"->{{}}]]] := Nothing;
PlotLastEk[data_]:= PlotSingleObservable[data["skGrid"], ({data["skGrid"], data["sk"]//Transpose//Last}//Transpose)/.{k_, s_}-> k^2/s, "E(k, t="<>ToString[data["timesAdditional"]//Last]<>")"];
PlotLastRho[data_/;MatchQ[data, KeyValuePattern["rho"->{{}}]]] := Nothing;
PlotLastRho[data_]:= PlotSingleObservable[data["rhoGrid"], data["rho"]//Transpose//Last, "\[Rho](r, t="<>ToString[data["timesAdditional"]//Last]<>")"];

PlotGrData[data_/;MatchQ[data, KeyValuePattern["gr"->{{}}]]] := Nothing;
PlotGrData[data_]:=Module[{tmp, diffs, plotM, plotMD, plotL, plotLLog, rangeX, rangeT, range},
rangeX = {data["grGrid"]//First, data["grGrid"]//Last};
rangeT = {data["timesAdditional"]//First, data["timesAdditional"]//Last};
range = {rangeX, rangeT};
tmp=(data["gr"]//Transpose//Reverse);
diffs= ((#-tmp[[-1]])&)/@tmp;
plotM=tmp//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->Medium, PlotLabel->"g_2(r_ij, t)",FrameLabel->{{"r_ij", ""}, {"t", ""}}, DataRange->range]&;
plotMD=diffs//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->Medium,PlotLabel->"g_2(r_ij, t) - g_2(r_ij, 0)", FrameLabel->{{"r_ij", ""}, {"t", ""}}, DataRange->range]&;
plotL=tmp//Reverse//DS50//ListLinePlot[#, PlotRange->All, PlotStyle->"Rainbow", ImageSize->Medium, PlotLabel->"g_2(r_ij, t)", DataRange->rangeX]&;
plotLLog=tmp//Reverse//DS50//ListLogLinearPlot[#, PlotRange->All, PlotStyle->"Rainbow", Joined->True, ImageSize->Medium, PlotLabel->"g_2(r_ij, t)", DataRange->rangeX]&;
{plotM, plotMD, plotL, plotLLog}
];

PlotSkData[data_/;MatchQ[data, KeyValuePattern["sk"->{{}}]]] := Nothing;
PlotSkData[data_]:=Module[{tmp, diffs, plotM, plotMD, plotL, plotLLog, rangeX, rangeT, range},
rangeX = {data["skGrid"]//First, data["skGrid"]//Last};
rangeT = {data["timesAdditional"]//First, data["timesAdditional"]//Last};
range = {rangeX, rangeT};
tmp=(data["sk"]//Transpose//Reverse);
diffs= ((#-tmp[[-1]])&)/@tmp;
plotM=tmp//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->Medium, PlotLabel->"S(k, t)",FrameLabel->{{"k", ""}, {"t", ""}}, DataRange->range]&;
plotMD=diffs//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->Medium,PlotLabel->"S(k, t) - S(k, 0)", FrameLabel->{{"k", ""}, {"t", ""}}, DataRange->range]&;
plotL=tmp//Reverse//DS50//ListLinePlot[#, PlotRange->All, PlotStyle->"Rainbow", ImageSize->Medium, PlotLabel->"S(k, t)", DataRange->rangeX]&;
plotLLog=tmp//Reverse//DS50//ListLogLinearPlot[#, PlotRange->All, PlotStyle->"Rainbow", Joined->True, ImageSize->Medium, PlotLabel->"S(k, t)", DataRange->rangeX]&;
{plotM, plotMD, plotL, plotLLog}
];

PlotRhoData[data_/;MatchQ[data, KeyValuePattern["rho"->{{}}]]] := Nothing;
PlotRhoData[data_]:=Module[{tmp, diffs, plotM, plotMD, plotL, plotLLog, rangeX, rangeT, range},
rangeX = {data["rhoGrid"]//First, data["rhoGrid"]//Last};
rangeT = {data["timesAdditional"]//First, data["timesAdditional"]//Last};
range = {rangeX, rangeT};
tmp=(data["rho"]//Transpose//Reverse);
diffs= ((#-tmp[[-1]])&)/@tmp;
plotM=tmp//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->Medium, PlotLabel->"\[Rho](r, t)",FrameLabel->{{"r", ""}, {"t", ""}}, DataRange->range]&;
plotMD=diffs//MatrixPlot[#,ColorFunction->"Rainbow", AspectRatio->1, MaxPlotPoints->Infinity, ImageSize->Medium,PlotLabel->"\[Rho](r, t) - \[Rho](r, 0)", FrameLabel->{{"r", ""}, {"t", ""}}, DataRange->range]&;
plotL=tmp//Reverse//DS50//ListLinePlot[#, PlotRange->All, PlotStyle->"Rainbow", ImageSize->Medium, PlotLabel->"\[Rho](r, t)", DataRange->rangeX]&;
plotLLog=tmp//Reverse//DS50//ListLogLinearPlot[#, PlotRange->All, PlotStyle->"Rainbow", Joined->True, ImageSize->Medium, PlotLabel->"\[Rho](r, t)", DataRange->rangeX]&;
{plotM, plotMD, plotL, plotLLog}
];

PlotAll[data_]:={
{PlotER[data], PlotEI[data],PlotEKin[data], PlotEPot[data]},
{PlotPR[data], PlotPI[data]},
{PlotLastGr[data], PlotLastSk[data], PlotLastEk[data], PlotLastRho[data]},
PlotGrData[data],
PlotSkData[data],
PlotRhoData[data]
};

ExportAllPlots[data_, directory_, filename_String:"data"]:=Export[FileNameJoin[{directory, filename<>".pdf"}], PlotAll[data]//Grid, ImageSize->1000];

End[];

Protect @@ Names["Plot`*"];

EndPackage[];




