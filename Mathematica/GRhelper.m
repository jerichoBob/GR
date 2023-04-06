(* ::Package:: *)

(********
 * GRhelper.m 
 * Mathematica functions written by Charles Evans (UNC Chapel Hill).
 * Slightly extended and converted to a mathematica package 
 * by David Brown (NC State University).
 * 2023.02.10 Bob Seaton - Got rid of pesky package name when printing out \[CapitolGamma]]
 * 2023.02.10 Bob Seaton - Christoffel Symbol code cleanup. Added RemoveZeros option.
 * 2023.02.08 Bob Seaton - added options FontSize and FontFamily to ChristoffelSymbols function
 * 2023.02.07 Bob Seaton - added ChristoffelSymbols function to 
 *                         pretty print Christoffel Symbols results.
 *                         credit: https://github.com/nathanaelnoir
********)

BeginPackage["GRhelper`"]

(** Declare functions available after loading
     by providing their usage explanation **)

InverseMetric::usage = "InverseMetric[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)] = Inverse metric"
 
Affine::usage = "Affine[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) = Christoffel symbols of the second kind (first index up, second and third indices down)"

AffineWig::usage = "AffineWig[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"inverse\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Christoffel symbols of the second kind (first index up, second and third indices down)"

ChristoffelSymbols::usage = "ChristoffelSymbols[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) = Christoffel symbols of the second kind (first index up, second and third indices down)"

PrettyCS::usage = "PrettyCS[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) = Prints out the Christoffel Symbols of the second kind in a visually recognizable way"

Riemann::usage = "Riemann[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coords\",\nFontSlant->\"Italic\"]\)] = Riemann tensor (first index up; second, third and fourth indices down)"
	
RiemannWaff::usage = "RiemannWaff[\!\(\*
StyleBox[\"Christoffel\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"symbols\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Riemann tensor (first index up; second, third and fourth indices down)"
	
RiemannAllDown::usage = "RiemannAllDown[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Riemann tensor with all indices down"

RiemannAllDownWriem::usage = "RiemannAllDownWriem[metric,Riemann,coordinates] = Riemann tensor with all indices down"
	
RicciTensor::usage = "RicciTensor[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Ricci tensor with indices down"
	
RicciTensorWriem::usage = "RicciTensorWriem[\!\(\*
StyleBox[\"Riemann\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Ricci tensor with indices down"

RicciTensorWaff::usage = "RicciTensorWaff[\!\(\*
StyleBox[\"Christoffel\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"symbols\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Ricci tensor with indices down"

RicciScalar::usage = "RicciScalar[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Ricci scalar (curvature scalar)"

RicciScalarWricci::usage = "RicciScalarWricci[\!\(\*
StyleBox[\"inverse\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"Ricci\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)]  = Ricci scalar (curvature scalar)"

Einstein::usage = "Einstein[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] =  Einstein tensor with indices down"

EinsteinWricci::usage = "EinsteinWricci[\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"Ricci\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"Ricci\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"scalar\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] =  Einstein tensor with indices down"

ListAffine::usage = "ListAffine[\!\(\*
StyleBox[\"Christoffel\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"symbols\",\nFontSlant->\"Italic\"]\)] = Christoffel symbols"

ListRiemann::usage = "ListRiemann[\!\(\*
StyleBox[\"Riemann\",\nFontSlant->\"Italic\"]\)] = Riemann tensor"

ListRiemannAllDown::usage = "ListRiemannAllDown[RiemannAllDown] = Riemann tensor with all indices down"

ListTwoIndex::usage = "ListTwoIndex[ ] lists second rank tensors"

GradScalar::usage = "GradScalar[\!\(\*
StyleBox[\"scalar\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Gradient of a scalar function"

CovDerOneForm::usage = "CovDerOneForm[\!\(\*
StyleBox[\"covector\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Covariant derivative of a covector field"

CovDerVector::usage = "CovDerVector[\!\(\*
StyleBox[\"vector\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Covariant derivative of a vector field"

CovDerTwoTensorDownDown::usage = "CovDerTwoTensorDownDown[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = Covariant derivative of a type (0,2) tensor field"

CovDivgVector::usage = "CovDivgVector[ , , ] = "

CovDivg2TensorUpUp::usage = "CovDivg2TensorUpUp[ , , ] = "

RForm::usage = "RForm[ , , ] = "
	
Worldline::usage = "Worldline[\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"parameter\",\nFontSlant->\"Italic\"]\)] = parametrized worldline"

GeodesicEqns::usage = "GeodesicEqns[\!\(\*
StyleBox[\"worldline\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"parameter\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = geodesic equations"
	
VelocityNorm::usage = "VelocityNorm[\!\(\*
StyleBox[\"worldline\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"parameter\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)] = squared norm of the worldine tangent vector"

UdotKVF::usage = "UdotKVF[\!\(\*
StyleBox[\"worldline\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"parameter\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"metric\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"coordinates\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"Killing\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"vector\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"field\",\nFontSlant->\"Italic\"]\)] = Vector product of worldline tangent and Killing vector field"
 
GRhelper::usage = "GRhelper \!\(\*
StyleBox[\"Main\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"Functions\",\nFontSlant->\"Italic\"]\): InverseMetric, ChristoffelSymbols, PrettyCS, PrettyR, Affine, Riemann, RicciTensor, RicciScalar, Einstein, CovDerVector, CovDerOneForm, 
	GradScalar, CovDerTwoTensorDownDown, Worldline, GeodesicEqns, VelocityNorm, UdotKVF
	\!\(\*
StyleBox[\"Other\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"Functions\",\nFontSlant->\"Italic\"]\): AffineWig, RiemannWaff, RiemannAllDown, ReimannAllDownWriem, RicciTensorWriem, RicciTensorWaff, 
	RicciScalarWricci, EinsteinWricci, ListAffine, ListRiemann, ListRiemannAllDown, ListTwoIndex, CovDivgVector, 
	CovDiv2TensorUpUup, RForm"

Begin["`Private`"]

InverseMetric[g_] := Block[{res}, res =Inverse[g]; Simplify[res]]

ChristoffelSymbols[metric_, coord_,
  OptionsPattern[{FontSize -> 16, FontFamily -> "American Typewriter", 
     Header -> True, RemoveZeros -> True}]] := 
  Module[{inversemetric, n, list}, n = Length[coord];
   inversemetric := Simplify[Inverse[metric]];
   
   (* Using Sean Carrol's Christoffel Equation p.93 *)
   Christoffel = 
    Simplify[Table[(1/2)*Sum[(inversemetric[[\[Lambda], \[Sigma]]])*(
          D[metric[[\[Nu], \[Sigma]]], coord[[\[Mu]]]] +
           D[metric[[\[Sigma], \[Mu]]], coord[[\[Nu]]]] -
           D[metric[[\[Mu], \[Nu]]], coord[[\[Sigma]]]]), {\[Sigma], 
         1, n}],
      {\[Lambda], 1, n}, {\[Mu], 1, n}, {\[Nu], 1, n}]];
   (* handling options *)
   fontSize = OptionValue[FontSize];
   fontFamily = OptionValue[FontFamily];
   removeZeros = OptionValue[RemoveZeros];
   useHeader = OptionValue[Header];
   myGamma = SymbolName[\[CapitalGamma]];
   
   (* Using Sean Carrol's Christoffel Equation p.93 *)
   list := Table[
     If[Christoffel[[\[Lambda], \[Mu], \[Nu]]] =!= 0 || ! 
        removeZeros,
      {
       Text[
        Style[myGamma[coord[[\[Lambda]]], coord[[\[Mu]]], 
          coord[[\[Nu]]]], Black, FontFamily -> fontFamily, 
         FontSize -> fontSize]], 
       Text[Style["=", Black, FontFamily -> fontFamily, 
         FontSize -> fontSize]], 
       Text[Style[Christoffel[[\[Lambda], \[Mu], \[Nu]]], Black, 
         FontFamily -> fontFamily, FontSize -> fontSize]]
       }
      ], {\[Lambda], 1, n}, {\[Mu], 1, n}, {\[Nu], 1, n}];
   
   a = Style["Christoffel Symbols", FontFamily -> fontFamily, 
     FontSize -> fontSize];
   b = Style["Values", FontFamily -> fontFamily, FontSize -> fontSize];
   TableForm[Partition[DeleteCases[Flatten[list], Null], 3], 
    TableSpacing -> {2, 1}, 
    If[SameQ[useHeader, True], TableHeadings -> {None, {a, "", b}}]]
   ];
   

PrettyCS[ARGcslist_, 
   OptionsPattern[{FontSize -> 16, 
     FontFamily -> "American Typewriter", UseSymmetry -> True, 
     Coords -> {}}]
   ] := 
  Block[{n, m, ARGcoord, fontSize, fontFamily, useHeader},
   (*Options Handling*)
   ARGcoord = OptionValue[Coords];
   fontSize = OptionValue[FontSize];
   fontFamily = OptionValue[FontFamily];
   useSymmetry = OptionValue[UseSymmetry];
   
   (* assuming cslist is of the form [\[Lambda], \[Mu], \[Nu]] *)
   n = CubeRoot[Length[Flatten[ARGcslist]]] ;(*dimensionlity of list*)
   m = Length[ARGcoord];
   If[m != n, {
     (*Print["Error! Dimensionallity of Christoffel Symbol list [", 
     n, "] doesn't match the number of coordinates passed in [",m, 
     "]... please fix it !"];
     *)
     ARGcoord =  Take[{ "1", "2", "3", "4", "5", "6"}, n];  
    }];
   (*misc Variables *)
   supersubsize = fontSize *.75;
   gammasize = fontSize * 1.25;
   myGamma = SymbolName[\[CapitalGamma]];
   list :=
    Table[If[ARGcslist[[\[Lambda], \[Mu], \[Nu]]] =!= 0,
      {
       upper = 
        Style[ToString[ARGcoord[[\[Lambda]]]], Black, 
         FontFamily -> fontFamily, FontSize -> supersubsize ];
       lower = 
        Style[StringJoin[ "  ", ToString[ARGcoord[[\[Mu]]]] , "", 
          ToString[ARGcoord[[\[Nu]]]]], Black, 
         FontFamily -> fontFamily, FontSize -> supersubsize];
       Text[
        Style[Subsuperscript[myGamma, lower, upper], Black, 
         FontFamily -> fontFamily, FontSize -> gammasize]],
       Text[
        Style["=", Black, FontFamily -> fontFamily, 
         FontSize -> gammasize]], 
       Text[Style[ARGcslist[[\[Lambda], \[Mu], \[Nu]]], Black, 
         FontFamily -> fontFamily, FontSize -> fontSize]]
       }], {\[Lambda], 1, n}, {\[Mu], 1, n}, {\[Nu], 1, 
      If[useSymmetry, \[Mu], n]}];
   TableForm[Partition[DeleteCases[Flatten[list], Null], 3], 
    TableSpacing -> {2, 1}
    ]];

PrettyR[ARGlist_, 
  OptionsPattern[{FontSize -> 16, FontFamily -> "American Typewriter",
     UseSymmetry -> True, Coords -> {}}]] := 
 Block[{n, m, ARGcoord, fontSize, fontFamily, useHeader},
  (*Options Handling*)
  ARGcoord = OptionValue[Coords];
  fontSize = OptionValue[FontSize];
  fontFamily = OptionValue[FontFamily];
  useSymmetry = OptionValue[UseSymmetry];
  (* ARGlist contains the matrix elements of the Riemann Tensor *)
  n = Power[Length[Flatten[ARGlist]], 0.25];
  (*dimensionlity of list*)
  m = Length[ARGcoord];
  If[m != 
    n, {(ARGcoord = Take[{"1", "2", "3", "4", "5", "6"}, n];)}];
  (*misc Variables*)supersubsize = fontSize*.75;
  Rsize = fontSize*1.25;
  myR = SymbolName[R];
  
  list := Table[
    If[ARGlist[[\[Alpha], \[Beta], \[Mu], \[Nu]]] =!= Undefined && 
      ARGlist[[\[Alpha], \[Beta], \[Mu], \[Nu]]] =!= 0,
     {
      upper = 
       Style[ToString[ARGcoord[[\[Alpha]]]], Black, 
        FontFamily -> fontFamily, FontSize -> supersubsize];
      lower = 
       Style[StringJoin["  ", ToString[ARGcoord[[\[Beta]]]], "", 
         ToString[ARGcoord[[\[Mu]]]], "", 
         ToString[ARGcoord[[\[Nu]]]]], Black, 
        FontFamily -> fontFamily, FontSize -> supersubsize];
      Text[
       Style[Subsuperscript[myR, lower, upper], Black, 
        FontFamily -> fontFamily, FontSize -> Rsize]], 
      Text[Style["=", Black, FontFamily -> fontFamily, 
        FontSize -> Rsize]], 
      Text[Style[ARGlist[[\[Alpha], \[Beta], \[Mu], \[Nu]]], Black, 
        FontFamily -> fontFamily, FontSize -> fontSize]]
      }], {\[Alpha], 1, n}, {\[Beta], 1, n}, {\[Mu], 1, n}, {\[Nu], 1,
      If[useSymmetry, \[Mu], n]}];
  
  (*Print["list: ",list];*)
  TableForm[Partition[DeleteCases[Flatten[list], Null], 3], 
   TableSpacing -> {2, 1}]]

Affine[g_,xx_]:=Block[{n,ig,res},n= Length[xx];ig=InverseMetric[g];
res=Table[(1/2)*Sum[ig[[i,s]]*(-D[g[[j,k]],xx[[s]]]+D[g[[j,s]],xx[[k]]]+D[g[[s,k]],xx[[j]]]),{s,1,n}],{i,1,n},{j,1,n},{k,1,n}];
Simplify[res]]

AffineWig[g_,ig_,xx_]:=Block[{n,res},n= Length[xx];
res=Table[(1/2)*Sum[ig[[i,s]]*(-D[g[[j,k]],xx[[s]]]+D[g[[j,s]],xx[[k]]]+D[g[[s,k]],xx[[j]]]),{s,1,n}],{i,1,n},{j,1,n},{k,1,n}];
Simplify[res]]

Riemann[g_,xx_]:=Block[{n,Aff,res},n= Length[xx];Aff=Affine[g,xx];
res=Table[D[Aff[[i,k,m]],xx[[l]]]-D[Aff[[i,k,l]],xx[[m]]]+Sum[Aff[[i,s,l]]*Aff[[s,k,m]],{s,1,n}]-Sum[Aff[[i,s,m]]*Aff[[s,k,l]],{s,1,n}],{i,1,n},{k,1,n},{l,1,n},{m,1,n}];
Simplify[res]]

RiemannWaff[Aff_,xx_]:=Block[{n,res},n= Length[xx];
res=Table[D[Aff[[i,k,m]],xx[[l]]]-D[Aff[[i,k,l]],xx[[m]]]+Sum[Aff[[i,s,l]]*Aff[[s,k,m]],{s,1,n}]-Sum[Aff[[i,s,m]]*Aff[[s,k,l]],{s,1,n}],{i,1,n},{k,1,n},{l,1,n},{m,1,n}];
Simplify[res]]

RiemannAllDown[g_,xx_]:= Block[{n,Riem,res},n= Length[xx];Riem = Riemann[g,xx];res = Table[Sum[g[[i,s]] Riem[[s,j,k,l]],{s,1,n}],{i,1,n},{j,1,n},{k,1,n},{l,1,n}];
Simplify[res]]

RiemannAllDownWriem[g_,Riem_,xx_]:= Block[{n,res},n= Length[xx];res = Table[Sum[g[[i,s]] Riem[[s,j,k,l]],{s,1,n}],{i,1,n},{j,1,n},{k,1,n},{l,1,n}];
Simplify[res]]

RicciTensor[g_,xx_]:=Block[{Riem,res,n},n= Length[xx];Riem=Riemann[g,xx];
res=Table[Sum[Riem[[s,i,s,j]],{s,1,n}],{i,1,n},{j,1,n}];
Simplify[res]]

RicciTensorWriem[Riem_,xx_]:=Block[{res,n},n= Length[xx];
res=Table[Sum[Riem[[s,i,s,j]],{s,1,n}],{i,1,n},{j,1,n}];
Simplify[res]]

RicciTensorWaff[Aff_,xx_]:=Block[{n,res},n= Length[xx];
res=Table[Sum[D[Aff[[l,k,m]],xx[[l]]]-D[Aff[[l,k,l]],xx[[m]]]+Sum[Aff[[l,s,l]]*Aff[[s,k,m]],{s,1,n}]-Sum[Aff[[l,s,m]]*Aff[[s,k,l]],{s,1,n}],{l,1,n}],{k,1,n},{m,1,n}];
Simplify[res]]

RicciScalar[g_,xx_]:=Block[{Ricci,ig,res,n},n= Length[xx];Ricci=RicciTensor[g,xx];ig=InverseMetric[g];
res=Sum[ig[[s,i]] Ricci[[s,i]],{s,1,n},{i,1,n}];
Simplify[res]]

RicciScalarWricci[ig_,Ricci_,xx_]:=Block[{res,n},n= Length[xx];
res=Sum[ig[[s,i]] Ricci[[s,i]],{s,1,n},{i,1,n}];
Simplify[res]]

Einstein[g_,xx_] := Block[{Ricci,Scalar,res,n},n= Length[xx];Ricci=RicciTensor[g,xx];Scalar = RicciScalar[g,xx];res = Ricci - (1/2) Scalar*g;Simplify[res]]

EinsteinWricci[g_,Ricci_,Scalar_,xx_] := Block[{res,n},n= Length[xx];
res = Ricci - (1/2) Scalar*g;
Simplify[res]]

ListAffine[aff_]:= Block[{i,j,k,n,res},n = Length[aff];res = Table[If[UnsameQ[aff[[i,j,k]],0], {ToString[\[CapitalGamma][i,j,k]], ToString["="],aff[[i,j,k]]}] ,{i,1,n},{j,1,n},{k,1,j}];
	TableForm[Partition[DeleteCases[Flatten[res],Null],3],TableSpacing->{3,3}]]
	
ListRiemann[riem_]:= Block[{i,j,k,l,n,res},n = Length[riem];res = Table[If[UnsameQ[riem[[i,j,k,l]],0],{ToString[R[i,j,k,l]],ToString["="],riem[[i,j,k,l]]}] ,{i,1,n},{j,1,n},{k,1,n},{l,1,k-1}];
	TableForm[Partition[DeleteCases[Flatten[res],Null],3],TableSpacing->{3,3}]]
	
ListRiemannAllDown[riemalldown_]:= Block[{i,j,k,l,n,res},n = Length[riemalldown];res = Table[If[UnsameQ[riemalldown[[i,j,k,l]],0],{ToString[R[i,j,k,l]],ToString["="],riemalldown[[i,j,k,l]]}] ,{i,1,n},{j,1,i-1},{k,1,n},{l,1,k-1}];
	TableForm[Partition[DeleteCases[Flatten[res],Null],3],TableSpacing->{3,3}]]
	
ListTwoIndex[ricci_]:= Block[{j,l,n,res},n = Length[ricci];res = Table[If[UnsameQ[ricci[[j,l]],0],{ToString[R[j,l]],ToString["="],ricci[[j,l]]}] ,{j,1,n},{l,1,j}];
	TableForm[Partition[DeleteCases[Flatten[res],Null],3],TableSpacing->{3,3}]]
	
GradScalar[\[Alpha]_,xx_]:=Block[{i,n,res},n= Length[xx];
res=Table[D[\[Alpha],xx[[i]]],{i,1,n}];
Simplify[res]]

CovDerOneForm[\[Omega]_,g_,xx_]:=Block[{i,j,k,n,Aff,res},n= Length[xx];Aff=Affine[g,xx];
res=Table[D[\[Omega][[j]],xx[[i]]]- Sum[Aff[[k,i,j]]*\[Omega][[k]],{k,1,n}],{i,1,n},{j,1,n}];
Simplify[res]]

CovDerVector[V_,g_,xx_]:=Block[{i,j,k,n,Aff,res},n= Length[xx];Aff=Affine[g,xx];
res=Table[D[V[[j]],xx[[i]]]+ Sum[Aff[[j,i,k]]*V[[k]],{k,1,n}],{i,1,n},{j,1,n}];
Simplify[res]]

CovDerTwoTensorDownDown[T_,g_,xx_]:=Block[{i,j,k,l,n,Aff,res},n= Length[xx];Aff=Affine[g,xx];
res=Table[D[T[[i,j]],xx[[k]]]- Sum[Aff[[l,k,i]]*T[[l,j]] + Aff[[l,k,j]]*T[[i,l]],{l,1,n}],{i,1,n},{j,1,n},{k,1,n}];
Simplify[res]]

CovDivgVector[V_,g_,xx_]:=Block[{i,j,k,n,Aff,res},n= Length[xx];Aff=Affine[g,xx];
res=Sum[D[V[[i]],xx[[i]]]+ Sum[Aff[[i,i,k]]*V[[k]],{k,1,n}],{i,1,n}];
Simplify[res]]

CovDivg2TensorUpUp[T_,g_,xx_]:=Block[{i,j,k,n,Aff,res},n= Length[xx];Aff=Affine[g,xx];
res=Table[Sum[(D[T[[i,j]],xx[[j]]]+ Sum[Aff[[j,j,k]]*T[[i,k]] + Aff[[i,j,k]]*T[[j,k]],{k,1,n}]),{j,1,n}],{i,1,n}];
Simplify[res]]

RForm[varC_, x2Rrule_, rname_] := Block[{varCR,rule,R}, R = rname; rule = x2Rrule ;
varCR = Simplify[varC //. rule , {R > 0,Element[R,Reals]}] ]

Worldline[xx_,param_] := Block[{n,i}, n=Length[xx]; Table[xx[[i]][param],{i,1,n}]]



GeodesicEqns[wl_,p_, g_,xx_] := Block[{n,i,j,k,evalxxonwl,aff,vel,acc}, n = Length[xx]; evalxxonwl = Table[xx[[i]]->wl[[i]],{i,1,n}];
	aff = (Affine[g,xx]/.evalxxonwl); vel = Table[D[wl[[i]],p],{i,1,n}]; acc = D[vel,p];
	Table[acc[[i]] + Sum[aff[[i,j,k]]*vel[[j]]*vel[[k]],{j,1,n},{k,1,n}] ,{i,1,n}]]
	
VelocityNorm[wl_,p_,g_,xx_] := Block[{n,i,vel,evalxxonwl,gee}, n = Length[xx]; vel = Table[D[wl[[i]],p],{i,1,n}]; 
	evalxxonwl = Table[xx[[i]]->wl[[i]],{i,1,n}]; gee = (g/.evalxxonwl); Simplify[vel . gee . vel]]

UdotKVF[wl_,p_,g_,xx_,k_] := Block[{n,i,vel,evalxxonwl,gee,kay}, n = Length[xx]; vel = Table[D[wl[[i]],p],{i,1,n}]; 
	evalxxonwl = Table[xx[[i]]->wl[[i]],{i,1,n}]; gee = (g/.evalxxonwl); kay = (k/.evalxxonwl); Simplify[vel . gee . kay]]
	
End[]

EndPackage[]


Print["Enter ?GRhelper for a list of functions"] 
Print["Enter ?FunctionName for a description of the function 'FunctionName'"]





