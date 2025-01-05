(* ::Package:: *)

(* ::Title:: *)
(*Rigid Geometric Algebra in 1D*)


(* ::Author:: *)
(*By Eric Lengyel, Copyright 2019-2025*)


(* ::Subsection:: *)
(*Prolog*)


BeginPackage["RigidAlgebra1D`"];


e::usage = "e[...] represents a basis element in the algebra";


VectorList::usage = "VectorList is a list of the 2 vector basis elements";


BasisList::usage = "BasisList is a list of all 4 basis elements";


BasisCollect::usage = "BasisCollect[x] collects terms of x with respect to all 4 basis elements";


BasisForm::usage = "BasisForm[x] prints x in traditional form after collecting on the basis elements";


BasisGrade::usage = "BasisGrade is a list of the grades of all 4 basis elements";


BasisAntigrade::usage = "BasisAntigrade is a list of the antigrades of all 4 basis elements";


ScalarPart::usage = "ScalarPart[x] returns the scalar (grade 0) component of x";


AntiscalarPart::usage = "AntiscalarPart[x] returns the antiscalar (grade 2) component of x";


VectorPart::usage = "VectorPart[x] returns the vector (grade 1) components of x";


BulkPart::usage = "BulkPart[x] returns the bulk components of x";


WeightPart::usage = "WeightPart[x] returns the weight components of x";


GradeSelect::usage = "GradeSelect[x,k] returns the components of x having grade k";


AntigradeSelect::usage = "AntigradeSelect[x,k] returns the components of x having antigrade k";


StripBasis::usage = "StripBasis[x] converts a multivector x on the basis elements to a 4-entry list";


Multivector::usage = "Multivector[x] converts a 4-entry list x to a multivector on the basis elements";


RightComp::usage = "RightComp[x] returns the right complement of x";


LeftComp::usage = "LeftComp[x] returns the left complement of x";


Rev::usage = "Rev[x] returns the reverse of x";


Antirev::usage = "Antirev[x] returns the antireverse of x";


Metric::usage = "Metric is the 2x2 metric tensor";


ApplyMetric::usage = "ApplyMetric[x] applies the metric exomorphism to the multivector x";


ApplyAntimetric::usage = "ApplyAntimetric[x] applies the metric antiexomorphism to the multivector x";


BulkDual::usage = "BulkDual[x] returns the right bulk dual of x";


WeightDual::usage = "WeightDual[x] returns the right weight dual of x";


DotProduct::usage = "DotProduct[x,y] returns the inner product of x and y";


AntidotProduct::usage = "AntidotProduct[x,y] returns the inner antiproduct of x and y";


BulkNorm::usage = "BulkNorm[x] returns the bulk norm of x, which is a scalar";


WeightNorm::usage = "WeightNorm[x] returns the weight norm of x, which is an antiscalar";


GeometricNorm::usage = "GeometricNorm[x] returns the geometric norm of x, which is the sum of the bulk and weight norms";


WedgeProduct::usage = "WedgeProduct[x,y] returns the exterior product of x and y";


AntiwedgeProduct::usage = "AntiwedgeProduct[x,y] returns the exterior antiproduct of x and y";


BulkContraction::usage = "BulkContraction[x,y] returns the right bulk contraction of x and y";


WeightContraction::usage = "WeightContraction[x,y] returns the right weight contraction of x and y";


BulkExpansion::usage = "BulkExpansion[x,y] returns the right bulk expansion of x and y";


WeightExpansion::usage = "WeightExpansion[x,y] returns the right weight expansion of x and y";


GeometricProduct::usage = "GeometricProduct[x,y] returns the geometric product of x and y";


GeometricAntiproduct::usage = "GeometricAntiproduct[x,y] returns the geometric antiproduct of x and y";


Sandwich::usage = "Sandwich[m,x] evaluates the geometric product of m, x, and the reverse of m";


Antisandwich::usage = "Antisandwich[m,x] evaluates the geometric antiproduct of m, x, and the antireverse of m";


AntiscalarTransform::usage = "AntiscalarTransform[m] returns the second compound matrix of a 2x2 matrix m that transforms vectors, which is a 1x1 matrix that transforms antiscalars";


Exomorphism::usage = "Exomorphism[m] returns the exomorphism matrix corresponding to the 2x2 matrix m that transforms vectors, which is a 4x4 block diagonal matrix that transforms multivectors";


OperatorMatrix::usage = "OperatorMatrix[op,constraints] returns the 2x2 vector transformation matrix corresponding to the geometric algebra operator op with optional geometric constraints";


CompOperatorMatrix::usage = "CompOperatorMatrix[op,constraints] returns the 2x2 vector transformation matrix corresponding to the geometric algebra complement operator op with optional geometric constraints";


DividerList::usage = "DividerList is a list of table divider thicknesses for a header plus 4 entries";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Basis Elements*)


e[1,1]=e[]; e[2,2]=0;


e[2,1]=-e[1,2];


VectorList={e[1],e[2]};


BasisList={e[]}~Join~VectorList~Join~{e[1,2]};


BasisCollect[x_]:=Collect[x,BasisList]


BasisForm[x_]:=TraditionalForm[BasisCollect[x]]


BasisGrade={0,1,1,2};


BasisAntigrade={2,1,1,0};


ScalarPart[x_]:=Coefficient[x,e[]]e[]


AntiscalarPart[x_]:=Coefficient[x,e[1,2]]e[1,2]


VectorPart[x_]:=Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]


BulkPart[x_]:=Coefficient[x,e[]]e[]+Coefficient[x,e[1]]e[1]


WeightPart[x_]:=Coefficient[x,e[2]]e[2]+Coefficient[x,e[1,2]]e[1,2]


GradeSelect[x_,k_]:=Switch[k,0,ScalarPart[x],1,VectorPart[x],2,AntiscalarPart[x],_,0]


AntigradeSelect[x_,k_]:=GradeSelect[x,2-k]


StripBasis[x_]:=Table[Coefficient[x,BasisList[[i]]],{i,1,4}]


Multivector[x_]:=Sum[x[[i]]BasisList[[i]],{i,1,4}]


(* ::Subsubsection:: *)
(*Complement*)


RightComp[e[1]]=e[2]; RightComp[e[2]]=-e[1];


RightComp[e[]]=e[1,2]; RightComp[e[1,2]]=e[];


RightComp[x_]:=x e[1,2]/;FreeQ[x,e]


RightComp[x_ y_]:=x RightComp[y]/;FreeQ[x,e]


RightComp[x_ y_]:=RightComp[x]y/;FreeQ[y,e]


RightComp[x_+y_]:=RightComp[x]+RightComp[y]


LeftComp[e[1]]=-e[2]; LeftComp[e[2]]=e[1];


LeftComp[e[]]=e[1,2]; LeftComp[e[1,2]]=e[];


LeftComp[x_]:=x e[1,2]/;FreeQ[x,e]


LeftComp[x_ y_]:=x LeftComp[y]/;FreeQ[x,e]


LeftComp[x_ y_]:=LeftComp[x]y/;FreeQ[y,e]


LeftComp[x_+y_]:=LeftComp[x]+LeftComp[y]


(* ::Subsubsection:: *)
(*Reverse*)


Rev[e[]]=e[]; Rev[e[1,2]]=-e[1,2];


Rev[x_]:=x/;FreeQ[x,e]


Rev[e[x_]]:=e[x]


Rev[x_ y_]:=x Rev[y]/;FreeQ[x,e]


Rev[x_ y_]:=Rev[x]y/;FreeQ[y,e]


Rev[x_+y_]:=Rev[x]+Rev[y]


Antirev[x_]:=LeftComp[Rev[RightComp[x]]]


(* ::Subsubsection:: *)
(*Metric*)


Metric={{1,0},{0,0}};


ApplyMetric[e[1]]=e[1]; ApplyMetric[e[2]]=0;


ApplyMetric[e[]]=e[]; ApplyMetric[e[1,2]]=0;


ApplyMetric[x_]:=x e[]/;FreeQ[x,e]


ApplyMetric[x_ y_]:=x ApplyMetric[y]/;FreeQ[x,e]


ApplyMetric[x_ y_]:=ApplyMetric[x]y/;FreeQ[y,e]


ApplyMetric[x_+y_]:=ApplyMetric[x]+ApplyMetric[y]


ApplyAntimetric[x_]:=LeftComp[ApplyMetric[RightComp[x]]]


(* ::Subsubsection:: *)
(*Dual*)


Table[BulkDual[BasisList[[x]]]=RightComp[ApplyMetric[BasisList[[x]]]],{x,1,4}];


Table[WeightDual[BasisList[[x]]]=RightComp[ApplyAntimetric[BasisList[[x]]]],{x,1,4}];


BulkDual[x_]:=x e[1,2]/;FreeQ[x,e]


BulkDual[x_ y_]:=x BulkDual[y]/;FreeQ[x,e]


BulkDual[x_ y_]:=BulkDual[x]y/;FreeQ[y,e]


BulkDual[x_+y_]:=BulkDual[x]+BulkDual[y]


WeightDual[x_]:=0/;FreeQ[x,e]


WeightDual[x_ y_]:=x WeightDual[y]/;FreeQ[x,e]


WeightDual[x_ y_]:=WeightDual[x]y/;FreeQ[y,e]


WeightDual[x_+y_]:=WeightDual[x]+WeightDual[y]


(* ::Subsubsection:: *)
(*Dot Product*)


DotProduct[x_,y_]:=(StripBasis[x] . StripBasis[ApplyMetric[y]])e[]


AntidotProduct[x_,y_]:=LeftComp[DotProduct[RightComp[x],RightComp[y]]]


(* ::Subsubsection:: *)
(*Norm*)


Unprotect[Power];


Power[e[],1/2]=e[]; Power[e[1,2],1/2]=e[1,2];


Power[Times[x_,e[]],1/2]:=Sqrt[x]e[]/;FreeQ[x,e]


Power[Times[x_,e[1,2]],1/2]:=Sqrt[x]e[1,2]/;FreeQ[x,e]


Protect[Power];


BulkNorm[x_]:=Sqrt[DotProduct[x,x]]


WeightNorm[x_]:=Sqrt[AntidotProduct[x,x]]


GeometricNorm[x_]:=BulkNorm[x]+WeightNorm[x]


(* ::Subsubsection:: *)
(*Exterior Product*)


WedgeProduct[e[],e[]]=e[];


WedgeProduct[e[],e[x_]]:=e[x]


WedgeProduct[e[],e[x_,y_]]:=e[x,y]


WedgeProduct[e[x_],e[]]:=e[x]


WedgeProduct[e[x_,y_],e[]]:=e[x,y]


WedgeProduct[e[x_],e[y_]]:=0/;x==y


WedgeProduct[e[x_],e[y_]]:=e[x,y]/;x!=y


WedgeProduct[e[x_,y_],e[z_]]:=0


WedgeProduct[e[x_],e[y_,z_]]:=0


WedgeProduct[e[x_,y_],e[z_,w_]]:=0


WedgeProduct[x_,y_]:=x y/;FreeQ[x,e]&&!FreeQ[y,e]


WedgeProduct[x_,y_]:=y x/;FreeQ[y,e]&&!FreeQ[x,e]


WedgeProduct[x_,y_]:=x y/;FreeQ[x,e]&&FreeQ[y,e]


WedgeProduct[x_ y_,z_]:=x WedgeProduct[y,z]/;FreeQ[x,e]


WedgeProduct[x_,y_ z_]:=y WedgeProduct[x,z]/;FreeQ[y,e]


WedgeProduct[x_ y_,z_ w_]:=x z WedgeProduct[y,w]/;FreeQ[x,e]&&FreeQ[z,e]


WedgeProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->WedgeProduct


WedgeProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->WedgeProduct


AntiwedgeProduct[x_,y_]:=LeftComp[WedgeProduct[RightComp[x],RightComp[y]]]


(* ::Subsubsection:: *)
(*Interior Products*)


BulkContraction[x_,y_]:=AntiwedgeProduct[x,BulkDual[y]]


WeightContraction[x_,y_]:=AntiwedgeProduct[x,WeightDual[y]]


BulkExpansion[x_,y_]:=WedgeProduct[x,BulkDual[y]]


WeightExpansion[x_,y_]:=WedgeProduct[x,WeightDual[y]]


(* ::Subsubsection:: *)
(*Geometric Product*)


GeometricProduct[e[1],e[1,2]]=e[2]; GeometricProduct[e[2],e[1,2]]=0; GeometricProduct[e[1,2],e[1,2]]=0;


GeometricProduct[e[1,2],e[x_]]:=GeometricProduct[e[1],e[2,x]]


GeometricProduct[e[],e[]]=e[]; GeometricProduct[e[1,2],e[1,2]]=0;


GeometricProduct[e[],e[x_]]:=e[x]


GeometricProduct[e[],e[x_,y_]]:=e[x,y]


GeometricProduct[e[x_],e[]]:=e[x]


GeometricProduct[e[x_,y_],e[]]:=e[x,y]


GeometricProduct[e[x_],e[y_]]:=e[x,y]


GeometricProduct[x_,y_]:=x y/;FreeQ[x,e]


GeometricProduct[x_,y_]:=y x/;FreeQ[y,e]


GeometricProduct[x_ y_,z_]:=x GeometricProduct[y,z]/;FreeQ[x,e]


GeometricProduct[x_,y_ z_]:=y GeometricProduct[x,z]/;FreeQ[y,e]


GeometricProduct[x_ y_,z_ w_]:=x z GeometricProduct[y,w]/;FreeQ[x,e]&&FreeQ[z,e]


GeometricProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->GeometricProduct


GeometricProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->GeometricProduct


GeometricAntiproduct[x_,y_]:=LeftComp[GeometricProduct[RightComp[x],RightComp[y]]]


Sandwich[m_,x_]:=GeometricProduct[GeometricProduct[m,x],Rev[m]]


Antisandwich[m_,x_]:=GeometricAntiproduct[GeometricAntiproduct[m,x],Antirev[m]]


(* ::Subsubsection:: *)
(*Exomorphism*)


AntiscalarTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]},
	{{Coefficient[Simplify[WedgeProduct[c1,c2]],e[1,2]]}}]


Exomorphism[m_]:=SparseArray[Band[{1,1}]->{{{1}},m,AntiscalarTransform[m]}]


OperatorMatrix[op_,constraints_:{}]:=Module[{x,y,v,xp,yp},v=Simplify[Antisandwich[op,x e[1]+y e[2]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];
	{{Coefficient[xp,x],Coefficient[xp,y]},
	{Coefficient[yp,x],Coefficient[yp,y]}}]


CompOperatorMatrix[op_,constraints_:{}]:=Module[{x,y,v,xp,yp},v=Simplify[Sandwich[op,x e[1]+y e[2]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];
	{{Coefficient[xp,x],Coefficient[xp,y]},
	{Coefficient[yp,x],Coefficient[yp,y]}}]


(* ::Subsubsection:: *)
(*Formatting*)


Format[e[],TraditionalForm]:=DisplayForm[Style["1",Bold,FontFamily->"Times New Roman",FontSize->16]]


Format[e[x_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[1,2],TraditionalForm]:=DisplayForm[StyleBox[FromCharacterCode[16^^1D7D9],FontFamily->"Segoe UI Symbol",FontSize->16]]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2022],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2022]->"~DotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2218],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2218]->"~AntidotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2227],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2227]->"~WedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2228],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2228]->"~AntiwedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27D1],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27D1]->"~GeometricProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27C7],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27C7]->"~GeometricAntiproduct~"],StandardForm]


DividerList={Thick,Thick,Thick,True,Thick,Thick};


(* ::Subsection:: *)
(*Epilog*)


End[];


EndPackage[];
