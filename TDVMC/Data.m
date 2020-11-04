(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["Data`"]

Unprotect @@ Names["Data`*"];
ClearAll @@ Names["Data`*"];

ReadTVMCFiles::usage = "reads simulation data from directory <path>";

Begin["Private`"]

Needs["Utils`", FileNameJoin[{NotebookDirectory[], "..", "Utils", "Utils.m"}]];

standardFileExtension = ".dat";
GetFilePath[directory_, fileName_]:=FileNameJoin[{directory, fileName<>standardFileExtension}];

ReadData[path_]:=ReadData[path, 1, 1, #&];
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
		Print["file not found --- " <> path];
		data={{}};
	];
	data
];

ReadTVMCFiles[directory_]:=Module[{timesSystem, eR, eI, pR, pI, other, timesAdditional, grGrid, skGrid, rhoGrid, gr, sk, rho, data},
	timesSystem = ReadData[GetFilePath[directory, "timesSystem"]];
	eR = ReadData[GetFilePath[directory, "LocalEnergyR"]];
	eI = ReadData[GetFilePath[directory, "LocalEnergyI"]];
	pR = ReadData[GetFilePath[directory, "ParametersR"]];
	pI = ReadData[GetFilePath[directory, "ParametersI"]];
	other=ReadData[GetFilePath[directory, "OtherExpectationValues"]];
	timesAdditional = ReadData[GetFilePath[directory, "timesAdditional"]];
	grGrid = ReadData[GetFilePath[directory, "gr_grid"]];
	skGrid = ReadData[GetFilePath[directory, "sk_grid"]];
	rhoGrid = ReadData[GetFilePath[directory, "rho_grid"]];
	gr = ReadData[GetFilePath[directory, "gr"]];
	sk = ReadData[GetFilePath[directory, "sk"]];
	rho = ReadData[GetFilePath[directory, "rho"]];
	data = <|"timesSystem"->timesSystem, 
		"eR"->eR, "eI"->eI, "pR"->pR, "pI"->pI, "other"->other,
		"timesAdditional"->timesAdditional,
		"grGrid"->grGrid, "skGrid"->skGrid,"rhoGrid"->rhoGrid,
		"gr"->gr, "sk"->sk,"rho"->rho|>;
	data
];

End[];

Protect @@ Names["Data`*"];

EndPackage[];



