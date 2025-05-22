(* ::Package:: *)

(* ::Title:: *)
(*Conformal Geometric Algebra in 2D*)


(* ::Author:: *)
(*By Eric Lengyel, Copyright 2019-2025*)


(* ::Subsection:: *)
(*Prolog*)


BeginPackage["ConformalAlgebra2D`"];


e::usage = "e[...] represents a basis element in the algebra";


VectorList::usage = "VectorList is a list of the 4 vector basis elements";


BivectorList::usage = "BivectorList is a list of the 6 bivector basis elements";


TrivectorList::usage = "TrivectorList is a list of the 4 trivector basis elements";


BasisList::usage = "BasisList is a list of all 16 basis elements";


BasisCollect::usage = "BasisCollect[x] collects terms of x with respect to all 16 basis elements";


BasisForm::usage = "BasisForm[x] prints x in traditional form after collecting on the basis elements";


BasisGrade::usage = "BasisGrade is a list of the grades of all 16 basis elements";


BasisAntigrade::usage = "BasisAntigrade is a list of the antigrades of all 16 basis elements";


ScalarPart::usage = "ScalarPart[x] returns the scalar (grade 0) component of x";


AntiscalarPart::usage = "AntiscalarPart[x] returns the antiscalar (grade 4) component of x";


VectorPart::usage = "VectorPart[x] returns the vector (grade 1) components of x";


BivectorPart::usage = "BivectorPart[x] returns the bivector (grade 2) components of x";


TrivectorPart::usage = "TrivectorPart[x] returns the trivector (grade 3) components of x";


RoundBulkPart::usage = "RoundBulkPart[x] returns the round bulk components of x";


RoundWeightPart::usage = "RoundWeightPart[x] returns the round weight components of x";


FlatBulkPart::usage = "FlatBulkPart[x] returns the flat bulk components of x";


FlatWeightPart::usage = "FlatWeightPart[x] returns the flat weight components of x";


RoundPart::usage = "RoundPart[x] returns the round components of x";


FlatPart::usage = "FlatPart[x] returns the flat components of x";


BulkPart::usage = "BulkPart[x] returns the bulk components of x";


WeightPart::usage = "WeightPart[x] returns the weight components of x";


GradeSelect::usage = "GradeSelect[x,k] returns the components of x having grade k";


AntigradeSelect::usage = "AntigradeSelect[x,k] returns the components of x having antigrade k";


StripBasis::usage = "StripBasis[x] converts a multivector x on the basis elements to a 16-entry list";


Multivector::usage = "Multivector[x] converts a 16-entry list x to a multivector on the basis elements";


RightComp::usage = "RightComp[x] returns the right complement of x";


LeftComp::usage = "LeftComp[x] returns the left complement of x";


Rev::usage = "Rev[x] returns the reverse of x";


Antirev::usage = "Antirev[x] returns the antireverse of x";


ConformalConjugate::usage = "ConformalConjugate[x] returns the conformal conjugate of x";


Metric::usage = "Metric is the 4x4 metric tensor";


ApplyMetric::usage = "ApplyMetric[x] applies the metric exomorphism to the multivector x";


ApplyAntimetric::usage = "ApplyAntimetric[x] applies the metric antiexomorphism to the multivector x";


Dual::usage = "Dual[x] returns the right dual of x";


Antidual::usage = "Antidual[x] returns the right antidual of x";


DotProduct::usage = "DotProduct[x,y] returns the inner product of x and y";


AntidotProduct::usage = "AntidotProduct[x,y] returns the inner antiproduct of x and y";


CenterNorm::usage = "CenterNorm[x] returns the center norm of x";


RadiusNorm::usage = "RadiusNorm[x] returns the radius norm of x";


RoundBulkNorm::usage = "RoundBulkNorm[x] returns the round bulk norm of x";


RoundWeightNorm::usage = "RoundWeightNorm[x] returns the round weight norm of x";


FlatBulkNorm::usage = "FlatBulkNorm[x] returns the flat bulk norm of x";


FlatWeightNorm::usage = "FlatWeightNorm[x] returns the flat weight norm of x";


WedgeProduct::usage = "WedgeProduct[x,y] returns the exterior product of x and y";


AntiwedgeProduct::usage = "AntiwedgeProduct[x,y] returns the exterior antiproduct of x and y";


Contraction::usage = "Contraction[x,y] returns the right contraction of x and y";


Expansion::usage = "Expansion[x,y] returns the right expansion of x and y";


TranswedgeProduct::usage = "TranswedgeProduct[x,y,k] returns the transwedge product of order k of x and y";


TranswedgeAntiproduct::usage = "TranswedgeAntiproduct[x,y,k] returns the transwedge antiproduct of order k of x and y";


GeometricProduct::usage = "GeometricProduct[x,y] returns the geometric product of x and y";


GeometricAntiproduct::usage = "GeometricAntiproduct[x,y] returns the geometric antiproduct of x and y";


Sandwich::usage = "Sandwich[m,x] evaluates the geometric product of m, x, and the reverse of m";


Antisandwich::usage = "Antisandwich[m,x] evaluates the geometric antiproduct of m, x, and the antireverse of m";


Carrier::usage = "Carrier[x] returns the carrier of x";


Cocarrier::usage = "Cocarrier[x] returns the cocarrier of x";


FlatCenter::usage = "FlatCenter[x] returns the flat center of x, which is a flat point";


RoundCenter::usage = "RoundCenter[x] returns the round center of x, which is a round point";


Container::usage = "Container[x] returns the container of x, which is a sphere";


Partner::usage = "Partner[x] returns the partner of x";


Attitude::usage = "Attitude[x] returns the attitude of x";


FlatPoint::usage = "FlatPoint[px,py,pz:1] is a flat point with coordinates (px,py,pz)";


FlatLine::usage = "FlatLine[gx,gy,gz] is a line with coordinates (gx,gy,gz)";


LineSupport::usage = "LineSupport[l] returns the support of the line l as a round point";


LineDirection::usage = "LineDirection[l] returns the direction of the line l as a vector";


RoundPoint::usage = "RoundPoint[px,py,pz:1,r2:0] is a round point with center (px,py,pz) and squared radius r2";


PointSquaredRadius::usage = "PointSquaredRadius[a] returns the squared weighted radius of the round point a";


RoundDipole::usage = "RoundDipole[r2,cx,cy,nx,ny] is a dipole with squared radius r2, center (cx,cy) and direction (nx,ny)";


DipoleSquaredRadius::usage = "DipoleSquaredRadius[d] returns the squared weighted radius of the dipole d";


DipoleFlatCenter::usage = "DipoleFlatCenter[d] returns the center of the dipole d as a flat point";


DipoleSurface::usage = "DipoleSurface[d] returns a list containing the two round points on the surface of the dipole d";


RoundCircle::usage = "RoundCircle[r2,cx,cy,cz:1] is a circle with squared radius r2, center (cx,cy,cz)";


CircleSquaredRadius::usage = "CircleSquaredRadius[c] returns the squared weighted radius of the circle c";


CircleFlatCenter::usage = "CircleFlatCenter[c] returns the center of the circle c as a flat point";


ConformalProject::usage = "ConformalProject[x,y] returns the conformal projection of x onto y";


BivectorTransform::usage = "BivectorTransform[m] returns the second compound matrix of a 4x4 matrix m that transforms vectors, which is a 6x6 matrix that transforms bivectors";


TrivectorTransform::usage = "TrivectorTransform[m] returns the third compound matrix of a 4x4 matrix m that transforms vectors, which is a 4x4 matrix that transforms trivectors";


AntiscalarTransform::usage = "AntiscalarTransform[m] returns the fourth compound matrix of a 4x4 matrix m that transforms vectors, which is a 1x1 matrix that transforms antiscalars";


Exomorphism::usage = "Exomorphism[m] returns the exomorphism matrix corresponding to the 4x4 matrix m that transforms vectors, which is a 16x16 block diagonal matrix that transforms multivectors";


OperatorMatrix::usage = "OperatorMatrix[op,constraints] returns the 4x4 vector transformation matrix corresponding to the geometric algebra operator op with optional geometric constraints";


CompOperatorMatrix::usage = "CompOperatorMatrix[op,constraints] returns the 4x4 vector transformation matrix corresponding to the geometric algebra complement operator op with optional geometric constraints";


DividerList::usage = "DividerList is a list of table divider thicknesses for a header plus 16 entries";


DrawFlatPoint::usage = "DrawFlatPoint[p] returns graphics for a flat point p";


DrawLine::usage = "DrawLine[g] returns graphics for a line g";


DrawRoundPoint::usage = "DrawRoundPoint[a] returns graphics for a round point a";


DrawDipole::usage = "DrawDipole[d] returns graphics for a dipole d";


DrawCircle::usage = "DrawCircle[c] returns graphics for a circle c";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Basis Elements*)


e[3,2]=-e[2,3]; e[1,3]=-e[3,1]; e[2,1]=-e[1,2];


e[x_,4]:=-e[4,x]


e[1,2,3]=-e[3,2,1]; e[2,1,3]=e[3,2,1]; e[1,3,2]=e[3,2,1]; e[3,1,2]=-e[3,2,1]; e[2,3,1]=-e[3,2,1];


e[4,3,2]=-e[4,2,3]; e[4,1,3]=-e[4,3,1]; e[4,2,1]=-e[4,1,2];


Table[e[x,y,4]=e[4,x,y],{x,1,3},{y,1,3}];


Table[e[x,4,y]=-e[4,x,y],{x,1,3},{y,1,3}];


e[x_,y_,z_,w_]:=e[1,2,3,4]Signature[{x,y,z,w}]/;{x,y,z,w}!={1,2,3,4}


VectorList={e[1],e[2],e[3],e[4]};


BivectorList={e[2,3],e[3,1],e[1,2],e[4,1],e[4,2],e[4,3]};


TrivectorList={e[3,2,1],e[4,2,3],e[4,3,1],e[4,1,2]};


BasisList={e[]}~Join~VectorList~Join~BivectorList~Join~TrivectorList~Join~{e[1,2,3,4]};


BasisCollect[x_]:=Collect[x,BasisList]


BasisForm[x_]:=TraditionalForm[BasisCollect[x]]


BasisGrade={0,1,1,1,1,2,2,2,2,2,2,3,3,3,3,4};


BasisAntigrade={4,3,3,3,3,2,2,2,2,2,2,1,1,1,1,0};


ScalarPart[x_]:=Coefficient[x,e[]]e[]


AntiscalarPart[x_]:=Coefficient[x,e[1,2,3,4]]e[1,2,3,4]


VectorPart[x_]:=Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]+Coefficient[x,e[4]]e[4]


BivectorPart[x_]:=Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[1,2]]e[1,2]+
	Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,3]]e[4,3]


TrivectorPart[x_]:=Coefficient[x,e[3,2,1]]e[3,2,1]+Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[4,1,2]]e[4,1,2]


RoundBulkPart[x_]:=Coefficient[x,e[]]e[]+Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[1,2]]e[1,2]


RoundWeightPart[x_]:=Coefficient[x,e[3]]e[3]+Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[3,2,1]]e[3,2,1]


FlatBulkPart[x_]:=Coefficient[x,e[4]]e[4]+Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,1,2]]e[4,1,2]


FlatWeightPart[x_]:=Coefficient[x,e[4,3]]e[4,3]+Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[1,2,3,4]]e[1,2,3,4]


RoundPart[x_]:=RoundBulkPart[x]+RoundWeightPart[x]


FlatPart[x_]:=FlatBulkPart[x]+FlatWeightPart[x]


BulkPart[x_]:=RoundBulkPart[x]+FlatBulkPart[x]


WeightPart[x_]:=RoundWeightPart[x]+FlatWeightPart[x]


GradeSelect[x_,k_]:=Switch[k,0,ScalarPart[x],1,VectorPart[x],2,BivectorPart[x],3,TrivectorPart[x],4,AntiscalarPart[x],_,0]


AntigradeSelect[x_,k_]:=GradeSelect[x,4-k]


StripBasis[x_]:=Table[Coefficient[x,BasisList[[i]]],{i,1,16}]


Multivector[x_]:=Sum[x[[i]]BasisList[[i]],{i,1,16}]


(* ::Subsubsection:: *)
(*Complement*)


RightComp[e[1]]=e[2,3,4]; RightComp[e[2]]=e[3,1,4]; RightComp[e[3]]=e[1,2,4]; RightComp[e[4]]=e[3,2,1];


RightComp[e[2,3]]=-e[4,1]; RightComp[e[3,1]]=-e[4,2]; RightComp[e[1,2]]=-e[4,3];


RightComp[e[4,1]]=-e[2,3]; RightComp[e[4,2]]=-e[3,1]; RightComp[e[4,3]]=-e[1,2];


RightComp[e[4,2,3]]=-e[1]; RightComp[e[4,3,1]]=-e[2]; RightComp[e[4,1,2]]=-e[3]; RightComp[e[3,2,1]]=-e[4];


RightComp[e[]]=e[1,2,3,4]; RightComp[e[1,2,3,4]]=e[];


RightComp[x_]:=x e[1,2,3,4]/;FreeQ[x,e]


RightComp[x_ y_]:=x RightComp[y]/;FreeQ[x,e]


RightComp[x_ y_]:=RightComp[x]y/;FreeQ[y,e]


RightComp[x_+y_]:=RightComp[x]+RightComp[y]


LeftComp[e[1]]=-e[4,2,3]; LeftComp[e[2]]=-e[4,3,1]; LeftComp[e[3]]=-e[4,1,2]; LeftComp[e[4]]=-e[3,2,1];


LeftComp[e[2,3]]=-e[4,1]; LeftComp[e[3,1]]=-e[4,2]; LeftComp[e[1,2]]=-e[4,3];


LeftComp[e[4,1]]=-e[2,3]; LeftComp[e[4,2]]=-e[3,1]; LeftComp[e[4,3]]=-e[1,2];


LeftComp[e[4,2,3]]=e[1]; LeftComp[e[4,3,1]]=e[2]; LeftComp[e[4,1,2]]=e[3]; LeftComp[e[3,2,1]]=e[4];


LeftComp[e[]]=e[1,2,3,4]; LeftComp[e[1,2,3,4]]=e[];


LeftComp[x_]:=x e[1,2,3,4]/;FreeQ[x,e]


LeftComp[x_ y_]:=x LeftComp[y]/;FreeQ[x,e]


LeftComp[x_ y_]:=LeftComp[x]y/;FreeQ[y,e]


LeftComp[x_+y_]:=LeftComp[x]+LeftComp[y]


(* ::Subsubsection:: *)
(*Reverse*)


Rev[e[]]=e[]; Rev[e[1,2,3,4]]=e[1,2,3,4];


Table[Rev[e[x]]=e[x],{x,1,4}];


Table[Rev[BivectorList[[i]]]=-BivectorList[[i]],{i,1,Length[BivectorList]}];


Table[Rev[TrivectorList[[i]]]=-TrivectorList[[i]],{i,1,Length[TrivectorList]}];


Rev[x_]:=x/;FreeQ[Hold[x],e]


Rev[x_ y_]:=x Rev[y]/;FreeQ[Hold[x],e]


Rev[x_ y_]:=Rev[x]y/;FreeQ[Hold[y],e]


Rev[x_+y_]:=Rev[x]+Rev[y]


Antirev[x_]:=LeftComp[Rev[RightComp[x]]]


(* ::Subsubsection:: *)
(*Conformal Conjugate*)


ConformalConjugate[e[]]=e[]; ConformalConjugate[e[1,2,3,4]]=-e[1,2,3,4];


ConformalConjugate[e[4]]=-e[4]; Table[ConformalConjugate[e[x]]=e[x],{x,1,3}];


Module[{list},list=Cases[BivectorList,e[x_,_]/;x!=4]; Table[ConformalConjugate[list[[i]]]=list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[BivectorList,e[x_,_]/;x==4]; Table[ConformalConjugate[list[[i]]]=-list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[TrivectorList,e[x_,_,_]/;x!=4]; Table[ConformalConjugate[list[[i]]]=list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[TrivectorList,e[x_,_,_]/;x==4]; Table[ConformalConjugate[list[[i]]]=-list[[i]],{i,1,Length[list]}]];


ConformalConjugate[x_]:=x/;FreeQ[Hold[x],e]


ConformalConjugate[x_ y_]:=x ConformalConjugate[y]/;FreeQ[Hold[x],e]


ConformalConjugate[x_ y_]:=ConformalConjugate[x]y/;FreeQ[Hold[y],e]


ConformalConjugate[x_+y_]:=ConformalConjugate[x]+ConformalConjugate[y]


(* ::Subsubsection:: *)
(*Metric*)


Metric={{1,0,0,0},{0,1,0,0},{0,0,0,-1},{0,0,-1,0}};


ApplyMetric[e[1]]=e[1]; ApplyMetric[e[2]]=e[2]; ApplyMetric[e[3]]=-e[4]; ApplyMetric[e[4]]=-e[3];


ApplyMetric[e[1,2]]=e[1,2]; ApplyMetric[e[3,1]]=-e[4,1]; ApplyMetric[e[2,3]]=e[4,2]; ApplyMetric[e[4,1]]=-e[3,1]; ApplyMetric[e[4,2]]=e[2,3]; ApplyMetric[e[4,3]]=-e[4,3];


ApplyMetric[e[3,2,1]]=e[4,1,2]; ApplyMetric[e[4,1,2]]=e[3,2,1]; ApplyMetric[e[4,3,1]]=-e[4,3,1]; ApplyMetric[e[4,2,3]]=-e[4,2,3];


ApplyMetric[e[]]=e[]; ApplyMetric[e[1,2,3,4]]=-e[1,2,3,4];


ApplyMetric[x_]:=x e[]/;FreeQ[Hold[x],e]


ApplyMetric[x_ y_]:=x ApplyMetric[y]/;FreeQ[Hold[x],e]


ApplyMetric[x_ y_]:=ApplyMetric[x]y/;FreeQ[Hold[y],e]


ApplyMetric[x_+y_]:=ApplyMetric[x]+ApplyMetric[y]


ApplyAntimetric[x_]:=LeftComp[ApplyMetric[RightComp[x]]]


(* ::Subsubsection:: *)
(*Dual*)


Table[Dual[BasisList[[x]]]=RightComp[ApplyMetric[BasisList[[x]]]],{x,1,16}];


Table[Antidual[BasisList[[x]]]=RightComp[ApplyAntimetric[BasisList[[x]]]],{x,1,16}];


Dual[x_]:=x e[1,2,3,4]/;FreeQ[Hold[x],e]


Dual[x_ y_]:=x Dual[y]/;FreeQ[Hold[x],e]


Dual[x_ y_]:=Dual[x]y/;FreeQ[Hold[y],e]


Dual[x_+y_]:=Dual[x]+Dual[y]


Antidual[x_]:=-x e[1,2,3,4]/;FreeQ[Hold[x],e]


Antidual[x_ y_]:=x Antidual[y]/;FreeQ[Hold[x],e]


Antidual[x_ y_]:=Antidual[x]y/;FreeQ[Hold[y],e]


Antidual[x_+y_]:=Antidual[x]+Antidual[y]


(* ::Subsubsection:: *)
(*Dot Product*)


DotProduct[x_,y_]:=(StripBasis[x] . StripBasis[ApplyMetric[y]])e[]


AntidotProduct[x_,y_]:=LeftComp[DotProduct[RightComp[x],RightComp[y]]]


(* ::Subsubsection:: *)
(*Norm*)


Unprotect[Power];


Power[e[],1/2]=e[]; Power[e[1,2,3,4],1/2]=e[1,2,3,4];


Power[Times[x_,e[]],1/2]:=Sqrt[x]e[]/;FreeQ[x,e]


Power[Times[x_,e[1,2,3,4]],1/2]:=Sqrt[x]e[1,2,3,4]/;FreeQ[x,e]


Protect[Power];


CenterNorm[x_,rules_:{}]:=Module[{y},y=x/.rules; Sqrt[Simplify[DotProduct[y,ConformalConjugate[y]],rules/.Rule->Equal]]/.Reverse[rules,{2}]]


RadiusNorm[x_,rules_:{}]:=Module[{y},y=x/.rules; Sqrt[Simplify[AntidotProduct[y,y],rules/.Rule->Equal]]/.Reverse[rules,{2}]]


RoundBulkNorm[x_,rules_:{}]:=Module[{y},y=AntiwedgeProduct[WedgeProduct[x,e[4]],e[3,2,1]]/.rules; Sqrt[Collect[Simplify[DotProduct[y,y],rules/.Rule->Equal],e[]]/.Reverse[rules,{2}]]]


RoundWeightNorm[x_]:=AntiwedgeProduct[Module[{y},y=WedgeProduct[x,e[4]]; Sqrt[Simplify[AntidotProduct[y,y]]]],e[3,2,1]]


FlatBulkNorm[x_,rules_:{}]:=WedgeProduct[Module[{y},y=AntiwedgeProduct[x,e[3,2,1]]/.rules; FullSimplify[Sqrt[Collect[Simplify[DotProduct[y,y],rules/.Rule->Equal],e[]]/.Reverse[rules,{2}]]]],e[4]]


FlatWeightNorm[x_,rules_:{}]:=Module[{y},y=WedgeProduct[AntiwedgeProduct[x,e[3,2,1]],e[4]]/.rules; Sqrt[Simplify[AntidotProduct[y,y],rules/.Rule->Equal]/.Reverse[rules,{2}]]]


(* ::Subsubsection:: *)
(*Exterior Product*)


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]&&FreeQ[Hold[y],e]


WedgeProduct[e[],x_]:=x/;!FreeQ[Hold[x],e]


WedgeProduct[x_,e[]]:=x/;!FreeQ[Hold[x],e]


WedgeProduct[e[x_],e[y_]]:=0/;x==y


WedgeProduct[e[x_],e[y_]]:=e[x,y]/;x!=y


WedgeProduct[e[x_,y_],e[z_]]:=0/;x==z||y==z


WedgeProduct[e[x_,y_],e[z_]]:=e[x,y,z]/;x!=z&&y!=z


WedgeProduct[e[x_],e[y_,z_]]:=0/;x==y||x==z


WedgeProduct[e[x_],e[y_,z_]]:=e[x,y,z]/;x!=y&&x!=z


WedgeProduct[e[x_,y_],e[z_,w_]]:=0/;x==z||x==w||y==z||y==w


WedgeProduct[e[x_,y_],e[z_,w_]]:=e[x,y,z,w]/;x!=z&&x!=w&&y!=z&&y!=w


WedgeProduct[e[x_,y_,z_],e[w_]]:=0/;x==w||y==w||z==w


WedgeProduct[e[x_,y_,z_],e[w_]]:=e[x,y,z,w]/;x!=w&&y!=w&&z!=w


WedgeProduct[e[x_],e[y_,z_,w_]]:=0/;x==y||x==z||x==w


WedgeProduct[e[x_],e[y_,z_,w_]]:=WedgeProduct[e[x,y,z],e[w]]/;x!=y&&x!=z&&x!=w


WedgeProduct[e[s_],e[x_,y_,z_,w_]]:=0


WedgeProduct[e[s_,t_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_],e[x_,y_,z_,w_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_,z_,w_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_,y_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_,y_,z_,w_]]:=0


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]


WedgeProduct[x_,y_]:=y x/;FreeQ[Hold[y],e]&&!FreeQ[Hold[x],e]


WedgeProduct[x_ y_,z_]:=x WedgeProduct[y,z]/;FreeQ[Hold[x],e]


WedgeProduct[x_,y_ z_]:=y WedgeProduct[x,z]/;FreeQ[Hold[y],e]


WedgeProduct[x_ y_,z_ w_]:=x z WedgeProduct[y,w]/;FreeQ[Hold[x],e]&&FreeQ[Hold[z],e]


WedgeProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->WedgeProduct


WedgeProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->WedgeProduct


AntiwedgeProduct[x_,y_]:=LeftComp[WedgeProduct[RightComp[x],RightComp[y]]]


(* ::Subsubsection:: *)
(*Interior Products*)


Contraction[x_,y_]:=AntiwedgeProduct[x,Dual[y]]


Expansion[x_,y_]:=WedgeProduct[x,Antidual[y]]


(* ::Subsubsection:: *)
(*Transwedge Products*)


TranswedgeProduct[x_,y_,0]:=WedgeProduct[x,y]


TranswedgeProduct[x_,y_,1]:=Sum[Module[{c},c=VectorList[[i]];WedgeProduct[AntiwedgeProduct[LeftComp[c],x],AntiwedgeProduct[y,Dual[c]]]],{i,1,4}]


TranswedgeProduct[x_,y_,2]:=Sum[Module[{c},c=BivectorList[[i]];WedgeProduct[AntiwedgeProduct[LeftComp[c],x],AntiwedgeProduct[y,Dual[c]]]],{i,1,6}]


TranswedgeProduct[x_,y_,3]:=Sum[Module[{c},c=TrivectorList[[i]];WedgeProduct[AntiwedgeProduct[LeftComp[c],x],AntiwedgeProduct[y,Dual[c]]]],{i,1,4}]


TranswedgeProduct[x_,y_,4]:=-WedgeProduct[AntiwedgeProduct[e[],x],AntiwedgeProduct[y,e[]]]


TranswedgeAntiproduct[x_,y_,k_]:=LeftComp[TranswedgeProduct[RightComp[x],RightComp[y],k]]


(* ::Subsubsection:: *)
(*Geometric Product*)


GeometricProduct[x_,y_]:=WedgeProduct[x,y]+TranswedgeProduct[x,y,1]-TranswedgeProduct[x,y,2]-TranswedgeProduct[x,y,3]+TranswedgeProduct[x,y,4]


GeometricAntiproduct[x_,y_]:=LeftComp[GeometricProduct[RightComp[x],RightComp[y]]]


Sandwich[m_,x_]:=GeometricProduct[GeometricProduct[m,x],Rev[m]]


Antisandwich[m_,x_]:=GeometricAntiproduct[GeometricAntiproduct[m,x],Antirev[m]]


(* ::Subsubsection:: *)
(*Properties*)


Carrier[x_]:=WedgeProduct[e[4],x]


Cocarrier[x_]:=WedgeProduct[e[4],Antidual[x]]


FlatCenter[x_]:=BasisCollect[AntiwedgeProduct[Carrier[x],Cocarrier[x]]]


RoundCenter[x_]:=BasisCollect[AntiwedgeProduct[Cocarrier[x],x]]


Container[x_]:=BasisCollect[WedgeProduct[x,Antidual[Carrier[x]]]]


Partner[x_]:=BasisCollect[AntiwedgeProduct[Container[Antidual[x]],Carrier[x]]]


Attitude[x_]:=AntiwedgeProduct[x,e[4,1,2]]


(* ::Subsubsection:: *)
(*Flat Point*)


FlatPoint[x_,y_,z_:1] := x e[4,1] + y e[4,2] + z e[4,3]


(* ::Subsubsection:: *)
(*Line*)


FlatLine[gx_,gy_,gz_]:=gx e[4,2,3]+gy e[4,3,1]+gz e[4,1,2]


LineSupport[g_]:=AntiwedgeProduct[g,WedgeProduct[e[3],Dual[g]]]


LineDirection[g_]:=Coefficient[g,e[4,3,1]]e[1] - Coefficient[g,e[4,2,3]]e[2]


(* ::Subsubsection:: *)
(*Round Point*)


RoundPoint[px_,py_,pz_:1,r2_:0] := px e[1] + py e[2] + pz e[3] + (px^2 + py^2 + pz^2 r2)/(2pz)e[4]


PointSquaredRadius[a_]:=Module[{x=Coefficient[a,e[1]],y=Coefficient[a,e[2]],z=Coefficient[a,e[3]],w=Coefficient[a,e[4]]},2z w - x^2 - y^2]


(* ::Subsubsection:: *)
(*Dipole*)


RoundDipole[r2_,cx_,cy_,nx_,ny_] := nx e[2,3] + ny e[3,1] - (cx nx + cy ny)e[1,2] + (cx^2 + cy^2 + r2)/2(ny e[4,1]- nx e[4,2]) - (cx ny - cy nx)(cx e[4,1] + cy e[4,2] + e[4,3])


DipoleSquaredRadius[d_]:=Module[{gx=Coefficient[d,e[2,3]],gy=Coefficient[d,e[3,1]],gz=Coefficient[d,e[1,2]],
	px=Coefficient[d,e[4,1]],py=Coefficient[d,e[4,2]],pz=Coefficient[d,e[4,3]]},
	pz^2 - gz^2 - 2(gx py - gy px)]


DipoleFlatCenter[d_]:=Module[{gx=Coefficient[d,e[2,3]],gy=Coefficient[d,e[3,1]],gz=Coefficient[d,e[1,2]],pz=Coefficient[d,e[4,3]]},
	(-gx gz - gy pz)e[4,1] + (gx pz - gy gz)e[4,2] + (gx^2 + gy^2)e[4,3]]


DipoleSurface[d_]:=Module[{cen=RoundCenter[d],dp=Attitude[d]Sqrt[Abs[DipoleSquaredRadius[d]]]},{cen-dp,cen+dp}]


(* ::Subsubsection:: *)
(*Circle*)


RoundCircle[r2_,cx_,cy_,cz_:1] := cx e[4,2,3] + cy e[4,3,1] - cz e[3,2,1] + (cz^2 r2 - cx^2 - cy^2)/(2cz)e[4,1,2]


CircleSquaredRadius[c_]:=Module[{x=Coefficient[c,e[4,2,3]],y=Coefficient[c,e[4,3,1]],z=Coefficient[c,e[4,1,2]],w=Coefficient[c,e[3,2,1]]},x^2 + y^2 - 2z w]


CircleFlatCenter[c_]:=Module[{x=Coefficient[c,e[4,2,3]],y=Coefficient[c,e[4,3,1]],w=Coefficient[c,e[3,2,1]]},x e[4,1] + y e[4,2] - w e[4,3]]


(* ::Subsubsection:: *)
(*Projection*)


ConformalProject[x_,y_]:=AntiwedgeProduct[y,Expansion[x,y]]


(* ::Subsubsection:: *)
(*Exomorphism*)


BivectorTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4],
	p1,p2,p3,p4,p5,p6},
	
	p1=Simplify[WedgeProduct[c2,c3]];
	p2=Simplify[WedgeProduct[c3,c1]];
	p3=Simplify[WedgeProduct[c1,c2]];
	p4=Simplify[WedgeProduct[c4,c1]];
	p5=Simplify[WedgeProduct[c4,c2]];
	p6=Simplify[WedgeProduct[c4,c3]];
	
	Transpose[{
	Table[Coefficient[p1,BivectorList[[k]]],{k,1,6}],
	Table[Coefficient[p2,BivectorList[[k]]],{k,1,6}],
	Table[Coefficient[p3,BivectorList[[k]]],{k,1,6}],
	Table[Coefficient[p4,BivectorList[[k]]],{k,1,6}],
	Table[Coefficient[p5,BivectorList[[k]]],{k,1,6}],
	Table[Coefficient[p6,BivectorList[[k]]],{k,1,6}]}]]


TrivectorTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4],
	p1,p2,p3,p4},

	p1=Simplify[WedgeProduct[WedgeProduct[c3,c2],c1]];
	p2=Simplify[WedgeProduct[WedgeProduct[c4,c2],c3]];
	p3=Simplify[WedgeProduct[WedgeProduct[c4,c3],c1]];
	p4=Simplify[WedgeProduct[WedgeProduct[c4,c1],c2]];

	Transpose[{
	Table[Coefficient[p1,TrivectorList[[k]]],{k,1,4}],
	Table[Coefficient[p2,TrivectorList[[k]]],{k,1,4}],
	Table[Coefficient[p3,TrivectorList[[k]]],{k,1,4}],
	Table[Coefficient[p4,TrivectorList[[k]]],{k,1,4}]}]]


AntiscalarTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4]},
	{{Coefficient[Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c1,c2],c3],c4]],e[1,2,3,4]]}}]


Exomorphism[m_]:=SparseArray[Band[{1,1}]->{{{1}},m,BivectorTransform[m],TrivectorTransform[m],AntiscalarTransform[m]}]


OperatorMatrix[op_,constraints_:{}]:=Module[{x,y,z,w,v,xp,yp,zp,wp},v=Simplify[Antisandwich[op,x e[1]+y e[2]+z e[3]+w e[4]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];wp=Coefficient[v,e[4]];
	{{Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z],Coefficient[xp,w]},
	{Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z],Coefficient[yp,w]},
	{Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z],Coefficient[zp,w]},
	{Coefficient[wp,x],Coefficient[wp,y],Coefficient[wp,z],Coefficient[wp,w]}}]


CompOperatorMatrix[op_,constraints_:{}]:=Module[{x,y,z,w,v,xp,yp,zp,wp},v=Simplify[Sandwich[op,x e[1]+y e[2]+z e[3]+w e[4]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];wp=Coefficient[v,e[4]];
	{{Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z],Coefficient[xp,w]},
	{Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z],Coefficient[yp,w]},
	{Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z],Coefficient[zp,w]},
	{Coefficient[wp,x],Coefficient[wp,y],Coefficient[wp,z],Coefficient[wp,w]}}]


(* ::Subsubsection:: *)
(*Drawing*)


LineColor=RGBColor["#4080FF"];


RoundPointColor[r2_]:=If[r2>=0,RGBColor["#80FF80"],RGBColor["#D980FF"]];


DipoleColor[r2_]:=If[r2>=0,RGBColor["#408CC0"],RGBColor["#C04040"]];


CircleColor[r2_]:=If[r2>=0,RGBColor["#408CC0"],RGBColor["#C04040"]];


VectorUnitCoords[v_]:=Module[{x=Coefficient[v,e[1]],y=Coefficient[v,e[2]]},Normalize[{x,y}]]


RoundPointCoords[a_]:=Module[{x=Coefficient[a,e[1]],y=Coefficient[a,e[2]],z=Coefficient[a,e[3]]},{x/z,y/z}]


FlatPointCoords[p_]:=Module[{x=Coefficient[p,e[4,1]],y=Coefficient[p,e[4,2]],z=Coefficient[p,e[4,3]]},{x/z,y/z}]


DrawFlatPoint[p_]:=Graphics[{AbsolutePointSize[8],Point[FlatPointCoords[p]]}]


DrawLine[l_]:=Module[{sup=RoundPointCoords[LineSupport[l]],dir=VectorUnitCoords[LineDirection[l]]},
	Graphics[{LineColor,AbsoluteThickness[4],Line[{sup-6dir,sup+6dir}]}]]


DrawRoundPoint[a_]:=Module[{r2=PointSquaredRadius[a],cen=RoundPointCoords[a]},
	Graphics[{RoundPointColor[r2],Opacity[0.5],Disk[cen,Sqrt[Abs[r2]]/Abs[Coefficient[a,e[3]]]],RGBColor[0,0,0],AbsolutePointSize[5],Point[cen]}]]


DrawDipole[d_]:=Module[{r2=DipoleSquaredRadius[d],cen=FlatPointCoords[DipoleFlatCenter[d]],dir=VectorUnitCoords[Attitude[d]],rad},
	rad=Sqrt[Abs[r2]]/Coefficient[RoundWeightNorm[d],e[3,2,1]];
	Graphics[{AbsolutePointSize[8],RGBColor[0,0,0],Point[cen+dir rad],Point[cen-dir rad],DipoleColor[r2],AbsoluteThickness[2],AbsoluteDashing[4],Line[{cen+dir rad,cen-dir rad}]}]]


DrawCircle[c_]:=Module[{r2=CircleSquaredRadius[c],cen=FlatPointCoords[CircleFlatCenter[c]],rad},
	rad=Sqrt[Abs[r2]]/Coefficient[RoundWeightNorm[c],e[3,2,1]];
	{ParametricPlot[cen+{rad Cos[\[CurlyPhi]],rad Sin[\[CurlyPhi]]},{\[CurlyPhi],0,2Pi},PlotStyle->CircleColor[r2]],
	Graphics[{AbsolutePointSize[8],RGBColor[0,0,0],Point[cen]}]}]


(* ::Subsubsection:: *)
(*Formatting*)


Format[e[],TraditionalForm]:=DisplayForm[Style["1",Bold,FontFamily->"Times New Roman",FontSize->16]]


Format[e[x_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_,z_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y]<>TextString[z],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[1,2,3,4],TraditionalForm]:=DisplayForm[StyleBox[FromCharacterCode[16^^1D7D9],FontFamily->"Segoe UI Symbol",FontSize->16]]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2022],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2022]->"~DotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2218],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2218]->"~AntidotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2227],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2227]->"~WedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2228],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2228]->"~AntiwedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27D1],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27D1]->"~GeometricProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27C7],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27C7]->"~GeometricAntiproduct~"],StandardForm]


DividerList={Thick,Thick,Thick,True,True,True,Thick,True,True,True,True,True,Thick,True,True,True,Thick,Thick};


(* ::Subsection:: *)
(*Epilog*)


End[];


EndPackage[];
