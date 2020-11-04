(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["ListOperations`"]

Unprotect @@ Names["ListOperations`*"];
ClearAll @@ Names["ListOperations`*"];

DS::usage = "DS[list, n] downsample list to n elements";
DS10::usage = "downsample list to 10 elements";
DS20::usage = "downsample list to 20 elements";
DS50::usage = "downsample list to 50 elements";
DS100::usage = "downsample list to 100 elements";

Begin["Private`"]

DS[list_, n_]:=list[[;;;;Ceiling[Length[list]/n]]];
DS10[list_]:=DS[list, 10];
DS20[list_]:=DS[list, 20];
DS50[list_]:=DS[list, 50];
DS100[list_]:=DS[list, 100];

End[];

Protect @@ Names["ListOperations`*"];

EndPackage[];



