(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["Data`"]

Unprotect @@ Names["Data`*"];
ClearAll @@ Names["Data`*"];

ReadTVMCFiles::usage = "reads simulation data from directory <path>";

TakeData::usage = "TakeData[data, interval]";

Begin["Private`"]

Needs["Utils`", FileNameJoin[{NotebookDirectory[], "..", "Utils", "Utils.m"}]];

standardFileExtension = ".dat";
GetFilePath[directory_, fileName_]:=GetFilePath[directory, fileName, standardFileExtension];
GetFilePath[directory_, fileName_, fileExtension_]:=FileNameJoin[{directory, fileName<>fileExtension}];

ReadData::nofile="file not found --- `1`";
ReadData[path_]:=ReadData[path, 1];
ReadData[path_, headerLines_]:=ReadData[path, headerLines, 1];
ReadData[path_, headerLines_, everyNth_]:=ReadData[path, headerLines, everyNth, #&];
ReadData[path_, headerLines_, everyNth_, functionToApplyOnLine_]:=Module[{skip, stream, line, data, lineData},
	skip = everyNth - 1;
	If[FileExistsQ[path],
		stream=OpenRead[path];
		Do[Skip[stream, String], {i, headerLines}];
		data={};
		CheckAbort[
			line=ReadList[stream,Record,1, RecordLists->True];
			While[Length[line]>0,
				lineData=FromScientificForm[line[[1]]];
				AppendTo[data, lineData[[1]]//functionToApplyOnLine];
				Skip[stream,String,skip];
				line=ReadList[stream,Record,1, RecordLists->True];
			];
		, 
			Close[stream]
		];
		Close[stream];
		data=data//Transpose;
		data=If[(data//Length )== 1, data//Flatten, data];
	,
	Message[ReadData::nofile, path];
		data={{}};
	];
	data
];

ReadTVMCFiles[directory_]:=Module[{config, timesSystem, eR, eI, pR, pI, other, timesAdditional, grGrid, skGrid, rhoGrid, rho2Grid, gr, sk, rho, rho2, vext, grFinal, skFinal, rhoFinal, rho2Final, data},
	config = ResourceFunction["ToAssociations"][Import[GetFilePath[directory, "vmc", ".config"], "JSON"]];
	timesSystem = ReadData[GetFilePath[directory, "timesSystem"], 0];
	eR = ReadData[GetFilePath[directory, "LocalEnergyR"]];
	eI = ReadData[GetFilePath[directory, "LocalEnergyI"]];
	pR = ReadData[GetFilePath[directory, "ParametersR"]];
	pI = ReadData[GetFilePath[directory, "ParametersI"]];
	other=ReadData[GetFilePath[directory, "OtherExpectationValues"]];
	timesAdditional = ReadData[GetFilePath[directory, "timesAdditional"], 0];
	grGrid = ReadData[GetFilePath[directory, "gr_grid"]];
	skGrid = ReadData[GetFilePath[directory, "sk_grid"]];
	rhoGrid = ReadData[GetFilePath[directory, "rho_grid"]]//Quiet;
	rho2Grid = ReadData[GetFilePath[directory, "rho2_grid"]]//Quiet;
	gr = ReadData[GetFilePath[directory, "gr"], 0];
	sk = ReadData[GetFilePath[directory, "sk"], 0];
	rho = ReadData[GetFilePath[directory, "rho"], 0]//Quiet;
	grFinal = ReadData[GetFilePath[directory, "AdditionalObservables_pairDistribution"]]//Quiet;
	skFinal = ReadData[GetFilePath[directory, "AdditionalObservables_structureFactor"]]//Quiet;
	rhoFinal = ReadData[GetFilePath[directory, "AdditionalObservables_density"]]//Quiet;
	rho2Final = ReadData[GetFilePath[directory, "AdditionalObservables_pairDensity"]]//Quiet;
	rho2 = ReadData[GetFilePath[directory, "rho2_"]]//Quiet;
	vext = ReadData[GetFilePath[directory, "V_ext"], 0]//Quiet;
	data = <|"directory"-> directory, "config"->config, "timesSystem"->timesSystem, 
		"eR"->eR, "eI"->eI, "pR"->pR, "pI"->pI, "other"->other,
		"timesAdditional"->timesAdditional,
		"grGrid"->grGrid, "skGrid"->skGrid,"rhoGrid"->rhoGrid,"rho2Grid"->rho2Grid,
		"gr"->gr, "sk"->sk,"rho"->rho,"rho2"->rho2, "vext"->vext, "final"-> <|"gr"->grFinal, "sk"->skFinal, "rho"->rhoFinal, "rho2"->rho2Final|>|>;
	data
];

TakeData[data_, interval_] := Module[{new, keys, keys2D},
keys = {"timesSystem", "eR", "eI", "timesAdditional"};
keys2D = {"pR", "pI", "other", "gr", "sk", "rho", "rho2", "vext"};
new = data;
(If[new[#] !={}, new[#]=Check[new[#][[interval]], new[#]]])&/@keys;
(If[new[#] != {{}}, new[#]=Check[(new[#]//Transpose)[[interval]]//Transpose, new[#]]])&/@keys2D;
new
];

End[];

Protect @@ Names["Data`*"];

EndPackage[];



