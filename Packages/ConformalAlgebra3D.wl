(* ::Package:: *)

(* ::Title:: *)
(*Conformal Geometric Algebra in 3D*)


(* ::Author:: *)
(*By Eric Lengyel, Copyright 2019-2025*)


(* ::Subsection:: *)
(*Prolog*)


BeginPackage["ConformalAlgebra3D`"];


e::usage = "e[...] represents a basis element in the algebra";


VectorList::usage = "VectorList is a list of the 5 vector basis elements";


BivectorList::usage = "BivectorList is a list of the 10 bivector basis elements";


TrivectorList::usage = "TrivectorList is a list of the 10 trivector basis elements";


QuadrivectorList::usage = "QuadrivectorList is a list of the 5 quadrivector basis elements";


BasisList::usage = "BasisList is a list of all 32 basis elements";


BasisCollect::usage = "BasisCollect[x] collects terms of x with respect to all 32 basis elements";


BasisForm::usage = "BasisForm[x] prints x in traditional form after collecting on the basis elements";


BasisGrade::usage = "BasisGrade is a list of the grades of all 32 basis elements";


BasisAntigrade::usage = "BasisAntigrade is a list of the antigrades of all 32 basis elements";


ScalarPart::usage = "ScalarPart[x] returns the scalar (grade 0) component of x";


AntiscalarPart::usage = "AntiscalarPart[x] returns the antiscalar (grade 5) component of x";


VectorPart::usage = "VectorPart[x] returns the vector (grade 1) components of x";


BivectorPart::usage = "BivectorPart[x] returns the bivector (grade 2) components of x";


TrivectorPart::usage = "TrivectorPart[x] returns the trivector (grade 3) components of x";


QuadrivectorPart::usage = "QuadrivectorPart[x] returns the trivector (grade 4) components of x";


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


StripBasis::usage = "StripBasis[x] converts a multivector x on the basis elements to a 32-entry list";


Multivector::usage = "Multivector[x] converts a 32-entry list x to a multivector on the basis elements";


Comp::usage = "Comp[x] returns the complement of x";


Rev::usage = "Rev[x] returns the reverse of x";


ConformalConjugate::usage = "ConformalConjugate[x] returns the conformal conjugate of x";


Metric::usage = "Metric is the 5x5 metric tensor";


ApplyMetric::usage = "ApplyMetric[x] applies the metric exomorphism to the multivector x";


ApplyAntimetric::usage = "ApplyAntimetric[x] applies the metric antiexomorphism to the multivector x";


Dual::usage = "Dual[x] returns the dual of x";


Antidual::usage = "Antidual[x] returns the antidual of x";


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


FlatPoint::usage = "FlatPoint[px,py,pz,pw:1] is a flat point with coordinates (px,py,pz,pw)";


FlatLine::usage = "FlatLine[vx,vy,vz,mx,my,mz] is a line with direction (vx,vy,vz) and moment (mx,my,mz)";


LineSupport::usage = "LineSupport[l] returns the support of the line l as a round point";


LineDirection::usage = "LineDirection[l] returns the direction of the line l as a vector";


LineConstraints::usage = "LineConstraints[l] returns the geometric constraints for the line l";


FlatPlane::usage = "FlatPlane[gx,gy,gz,gw] is a plane with coordinates (gx,gy,gz,gw)";


PlaneSupport::usage = "PlaneSupport[g] returns the support of the plane g as a round point";


PlaneNormal::usage = "PlaneNormal[g] returns the normal of the plane g as a vector";


RoundPoint::usage = "RoundPoint[px,py,pz,pw:1,r2:0] is a round point with center (px,py,pz,pw) and squared radius r2";


PointSquaredRadius::usage = "PointSquaredRadius[a] returns the squared weighted radius of the round point a";


RoundDipole::usage = "RoundDipole[r2,cx,cy,cz,nx,ny,nz] is a dipole with squared radius r2, center (cx,cy,cz) and direction (nx,ny,nz)";


DipoleSquaredRadius::usage = "DipoleSquaredRadius[d] returns the squared weighted radius of the dipole d";


DipoleFlatCenter::usage = "DipoleFlatCenter[d] returns the center of the dipole d as a flat point";


DipoleSurface::usage = "DipoleSurface[d] returns a list containing the two round points on the surface of the dipole d";


DipoleConstraints::usage = "DipoleConstraints[d] returns the geometric constraints for the dipole d";


RoundCircle::usage = "RoundCircle[r2,cx,cy,cz,nx,ny,nz] is a circle with squared radius r2, center (cx,cy,cz) and normal (nx,ny,nz)";


CircleSquaredRadius::usage = "CircleSquaredRadius[c] returns the squared weighted radius of the circle c";


CircleFlatCenter::usage = "CircleFlatCenter[c] returns the center of the circle c as a flat point";


CircleNormal::usage = "CircleNormal[c] returns the normal of the circle c as a vector";


CircleConstraints::usage = "CircleConstraints[c] returns the geometric constraints for the circle c";


RoundSphere::usage = "RoundSphere[r2,cx,cy,cz,cw:1] is a sphere with squared radius r2 and center (cx,cy,cz,cw)";


SphereSquaredRadius::usage = "SphereSquaredRadius[s] returns the squared weighted radius of the sphere s";


SphereFlatCenter::usage = "SphereFlatCenter[d] returns the center of the sphere s as a flat point";


ConformalProject::usage = "ConformalProject[x,y] returns the conformal projection of x onto y";


BivectorTransform::usage = "BivectorTransform[m] returns the second compound matrix of a 5x5 matrix m that transforms vectors, which is a 10x10 matrix that transforms bivectors";


TrivectorTransform::usage = "TrivectorTransform[m] returns the third compound matrix of a 5x5 matrix m that transforms vectors, which is a 10x10 matrix that transforms trivectors";


QuadrivectorTransform::usage = "QuadrivectorTransform[m] returns the fourth compound matrix of a 5x5 matrix m that transforms vectors, which is a 5x5 matrix that transforms quadrivectors";


AntiscalarTransform::usage = "AntiscalarTransform[m] returns the fifth compound matrix of a 5x5 matrix m that transforms vectors, which is a 1x1 matrix that transforms antiscalars";


Exomorphism::usage = "Exomorphism[m] returns the exomorphism matrix corresponding to the 5x5 matrix m that transforms vectors, which is a 32x32 block diagonal matrix that transforms multivectors";


OperatorMatrix::usage = "OperatorMatrix[op,constraints] returns the 5x5 vector transformation matrix corresponding to the geometric algebra operator op with optional geometric constraints";


CompOperatorMatrix::usage = "CompOperatorMatrix[op,constraints] returns the 5x5 vector transformation matrix corresponding to the geometric algebra complement operator op with optional geometric constraints";


DividerList::usage = "DividerList is a list of table divider thicknesses for a header plus 32 entries";


DrawFlatPoint::usage = "DrawFlatPoint[p] returns graphics for a flat point p";


DrawLine::usage = "DrawLine[l] returns graphics for a line l";


DrawPlane::usage = "DrawPlane[g] returns graphics for a plane g";


DrawRoundPoint::usage = "DrawRoundPoint[a] returns graphics for a round point a";


DrawDipole::usage = "DrawDipole[d] returns graphics for a dipole d";


DrawCircle::usage = "DrawCircle[c] returns graphics for a circle c";


DrawSphere::usage = "DrawSphere[s] returns graphics for a sphere s";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Basis Elements*)


OtherPermutations[x_]:=DeleteCases[Permutations[x],x]


e[3,2]=-e[2,3]; e[1,3]=-e[3,1]; e[2,1]=-e[1,2];


e[1,4]=-e[4,1]; e[2,4]=-e[4,2]; e[3,4]=-e[4,3];


e[5,1]=-e[1,5]; e[5,2]=-e[2,5]; e[5,3]=-e[3,5]; e[5,4]=-e[4,5];


e[1,3,2]=e[3,2,1]; e[2,1,3]=e[3,2,1]; e[1,2,3]=-e[3,2,1]; e[2,3,1]=-e[3,2,1]; e[3,1,2]=-e[3,2,1];


e[3,4,2]=e[4,2,3]; e[2,3,4]=e[4,2,3]; e[2,4,3]=-e[4,2,3]; e[4,3,2]=-e[4,2,3]; e[3,2,4]=-e[4,2,3];


e[1,4,3]=e[4,3,1]; e[3,1,4]=e[4,3,1]; e[3,4,1]=-e[4,3,1]; e[4,1,3]=-e[4,3,1]; e[1,3,4]=-e[4,3,1];


e[2,4,1]=e[4,1,2]; e[1,2,4]=e[4,1,2]; e[1,4,2]=-e[4,1,2]; e[4,2,1]=-e[4,1,2]; e[2,1,4]=-e[4,1,2];


e[3,2,5]=-e[2,3,5]; e[1,3,5]=-e[3,1,5]; e[2,1,5]=-e[1,2,5]; e[1,4,5]=-e[4,1,5]; e[2,4,5]=-e[4,2,5]; e[3,4,5]=-e[4,3,5];


Table[e[5,x,y]=e[x,y,5],{x,1,5},{y,1,4}];


Table[e[x,5,y]=-e[x,y,5],{x,1,5},{y,1,4}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=e[1,2,3,4]Signature[x],{x,OtherPermutations[{1,2,3,4}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=e[4,2,3,5]Signature[x],{x,OtherPermutations[{4,2,3,5}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=-e[4,3,1,5]Signature[x],{x,OtherPermutations[{4,3,1,5}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=e[4,1,2,5]Signature[x],{x,OtherPermutations[{4,1,2,5}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=-e[3,2,1,5]Signature[x],{x,OtherPermutations[{3,2,1,5}]}];


e[x_,y_,z_,w_,v_]:=e[1,2,3,4,5]Signature[{x,y,z,w,v}]/;{x,y,z,w,v}!={1,2,3,4,5}


VectorList={e[1],e[2],e[3],e[4],e[5]};


BivectorList={e[4,1],e[4,2],e[4,3],e[2,3],e[3,1],e[1,2],e[1,5],e[2,5],e[3,5],e[4,5]};


TrivectorList={e[4,2,3],e[4,3,1],e[4,1,2],e[3,2,1],e[4,1,5],e[4,2,5],e[4,3,5],e[2,3,5],e[3,1,5],e[1,2,5]};


QuadrivectorList={e[1,2,3,4],e[4,2,3,5],e[4,3,1,5],e[4,1,2,5],e[3,2,1,5]};


BasisList={e[]}~Join~VectorList~Join~BivectorList~Join~TrivectorList~Join~QuadrivectorList~Join~{e[1,2,3,4,5]};


BasisCollect[x_]:=Collect[x,BasisList]


BasisForm[x_]:=TraditionalForm[BasisCollect[x]]


BasisGrade={0,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,5};


BasisAntigrade={5,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,0};


ScalarPart[x_]:=Coefficient[x,e[]]e[]


AntiscalarPart[x_]:=Coefficient[x,e[1,2,3,4,5]]e[1,2,3,4,5]


VectorPart[x_]:=Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]+Coefficient[x,e[4]]e[4]+Coefficient[x,e[5]]e[5]


BivectorPart[x_]:=Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,3]]e[4,3]+
	Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[1,2]]e[1,2]+
	Coefficient[x,e[1,5]]e[1,5]+Coefficient[x,e[2,5]]e[2,5]+Coefficient[x,e[3,5]]e[3,5]+Coefficient[x,e[4,5]]e[4,5]


TrivectorPart[x_]:=Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[4,1,2]]e[4,1,2]+Coefficient[x,e[3,2,1]]e[3,2,1]+
	Coefficient[x,e[4,1,5]]e[4,1,5]+Coefficient[x,e[4,2,5]]e[4,2,5]+Coefficient[x,e[4,3,5]]e[4,3,5]+
	Coefficient[x,e[2,3,5]]e[2,3,5]+Coefficient[x,e[3,1,5]]e[3,1,5]+Coefficient[x,e[1,2,5]]e[1,2,5]


QuadrivectorPart[x_]:=Coefficient[x,e[1,2,3,4]]e[1,2,3,4]+Coefficient[x,e[4,2,3,5]]e[4,2,3,5]+Coefficient[x,e[4,3,1,5]]e[4,3,1,5]+Coefficient[x,e[4,1,2,5]]e[4,1,2,5]+Coefficient[x,e[3,2,1,5]]e[3,2,1,5]


RoundBulkPart[x_]:=Coefficient[x,e[]]e[]+Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]+Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[1,2]]e[1,2]+Coefficient[x,e[3,2,1]]e[3,2,1]


RoundWeightPart[x_]:=Coefficient[x,e[4]]e[4]+Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,3]]e[4,3]+Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[4,1,2]]e[4,1,2]+Coefficient[x,e[1,2,3,4]]e[1,2,3,4]


FlatBulkPart[x_]:=Coefficient[x,e[5]]e[5]+Coefficient[x,e[1,5]]e[1,5]+Coefficient[x,e[2,5]]e[2,5]+Coefficient[x,e[3,5]]e[3,5]+Coefficient[x,e[2,3,5]]e[2,3,5]+Coefficient[x,e[3,1,5]]e[3,1,5]+Coefficient[x,e[1,2,5]]e[1,2,5]+Coefficient[x,e[3,2,1,5]]e[3,2,1,5]


FlatWeightPart[x_]:=Coefficient[x,e[4,5]]e[4,5]+Coefficient[x,e[4,1,5]]e[4,1,5]+Coefficient[x,e[4,2,5]]e[4,2,5]+Coefficient[x,e[4,3,5]]e[4,3,5]+Coefficient[x,e[4,2,3,5]]e[4,2,3,5]+Coefficient[x,e[4,3,1,5]]e[4,3,1,5]+Coefficient[x,e[4,1,2,5]]e[4,1,2,5]+Coefficient[x,e[1,2,3,4,5]]e[1,2,3,4,5]


RoundPart[x_]:=RoundBulkPart[x]+RoundWeightPart[x]


FlatPart[x_]:=FlatBulkPart[x]+FlatWeightPart[x]


BulkPart[x_]:=RoundBulkPart[x]+FlatBulkPart[x]


WeightPart[x_]:=RoundWeightPart[x]+FlatWeightPart[x]


GradeSelect[x_,k_]:=Switch[k,0,ScalarPart[x],1,VectorPart[x],2,BivectorPart[x],3,TrivectorPart[x],4,QuadrivectorPart[x],5,AntiscalarPart[x],_,0]


AntigradeSelect[x_,k_]:=GradeSelect[x,5-k]


StripBasis[x_]:=Table[Coefficient[x,BasisList[[i]]],{i,1,32}]


Multivector[x_]:=Sum[x[[i]]BasisList[[i]],{i,1,32}]


(* ::Subsubsection:: *)
(*Complement*)


Comp[e[1]]=e[4,2,3,5]; Comp[e[2]]=e[4,3,1,5]; Comp[e[3]]=e[4,1,2,5]; Comp[e[4]]=e[3,2,1,5]; Comp[e[5]]=e[1,2,3,4];


Comp[e[2,3]]=-e[4,1,5]; Comp[e[3,1]]=-e[4,2,5]; Comp[e[1,2]]=-e[4,3,5]; Comp[e[4,1]]=-e[2,3,5]; Comp[e[4,2]]=-e[3,1,5]; Comp[e[4,3]]=-e[1,2,5];


Comp[e[1,5]]=-e[4,2,3]; Comp[e[2,5]]=-e[4,3,1]; Comp[e[3,5]]=-e[4,1,2]; Comp[e[4,5]]=-e[3,2,1];


Comp[e[4,2,3]]=-e[1,5]; Comp[e[4,3,1]]=-e[2,5]; Comp[e[4,1,2]]=-e[3,5]; Comp[e[3,2,1]]=-e[4,5];


Comp[e[2,3,5]]=-e[4,1]; Comp[e[3,1,5]]=-e[4,2]; Comp[e[1,2,5]]=-e[4,3]; Comp[e[4,1,5]]=-e[2,3]; Comp[e[4,2,5]]=-e[3,1]; Comp[e[4,3,5]]=-e[1,2];


Comp[e[4,2,3,5]]=e[1]; Comp[e[4,3,1,5]]=e[2]; Comp[e[4,1,2,5]]=e[3]; Comp[e[3,2,1,5]]=e[4]; Comp[e[1,2,3,4]]=e[5];


Comp[e[]]=e[1,2,3,4,5]; Comp[e[1,2,3,4,5]]=e[];


Comp[x_]:=x e[1,2,3,4,5]/;FreeQ[Hold[x],e]


Comp[x_ y_]:=x Comp[y]/;FreeQ[Hold[x],e]


Comp[x_ y_]:=Comp[x]y/;FreeQ[Hold[y],e]


Comp[x_+y_]:=Comp[x]+Comp[y]


(* ::Subsubsection:: *)
(*Reverse*)


Rev[e[]]=e[]; Rev[e[1,2,3,4,5]]=e[1,2,3,4,5];


Table[Rev[e[x]]=e[x],{x,1,5}];


Table[Rev[BivectorList[[i]]]=-BivectorList[[i]],{i,1,Length[BivectorList]}];


Table[Rev[TrivectorList[[i]]]=-TrivectorList[[i]],{i,1,Length[TrivectorList]}];


Table[Rev[QuadrivectorList[[i]]]=QuadrivectorList[[i]],{i,1,Length[QuadrivectorList]}];


Rev[x_]:=x/;FreeQ[Hold[x],e]


Rev[x_ y_]:=x Rev[y]/;FreeQ[Hold[x],e]


Rev[x_ y_]:=Rev[x]y/;FreeQ[Hold[y],e]


Rev[x_+y_]:=Rev[x]+Rev[y]


(* ::Subsubsection:: *)
(*Conformal Conjugate*)


ConformalConjugate[e[]]=e[]; ConformalConjugate[e[1,2,3,4,5]]=-e[1,2,3,4,5];


ConformalConjugate[e[5]]=-e[5]; Table[ConformalConjugate[e[x]]=e[x],{x,1,4}];


Module[{list},list=Cases[BivectorList,e[_,y_]/;y!=5]; Table[ConformalConjugate[list[[i]]]=list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[BivectorList,e[_,y_]/;y==5]; Table[ConformalConjugate[list[[i]]]=-list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[TrivectorList,e[_,_,z_]/;z!=5]; Table[ConformalConjugate[list[[i]]]=list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[TrivectorList,e[_,_,z_]/;z==5]; Table[ConformalConjugate[list[[i]]]=-list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[QuadrivectorList,e[_,_,_,w_]/;w!=5]; Table[ConformalConjugate[list[[i]]]=list[[i]],{i,1,Length[list]}]];


Module[{list},list=Cases[QuadrivectorList,e[_,_,_,w_]/;w==5]; Table[ConformalConjugate[list[[i]]]=-list[[i]],{i,1,Length[list]}]];


ConformalConjugate[x_]:=x/;FreeQ[Hold[x],e]


ConformalConjugate[x_ y_]:=x ConformalConjugate[y]/;FreeQ[Hold[x],e]


ConformalConjugate[x_ y_]:=ConformalConjugate[x]y/;FreeQ[Hold[y],e]


ConformalConjugate[x_+y_]:=ConformalConjugate[x]+ConformalConjugate[y]


(* ::Subsubsection:: *)
(*Metric*)


Metric={{1,0,0,0,0},{0,1,0,0,0},{0,0,1,0,0},{0,0,0,0,-1},{0,0,0,-1,0}};


ApplyMetric[e[1]]=e[1]; ApplyMetric[e[2]]=e[2]; ApplyMetric[e[3]]=e[3];


ApplyMetric[e[4]]=-e[5]; ApplyMetric[e[5]]=-e[4];


ApplyMetric[e[2,3]]=e[2,3]; ApplyMetric[e[3,1]]=e[3,1]; ApplyMetric[e[1,2]]=e[1,2];


ApplyMetric[e[4,1]]=e[1,5]; ApplyMetric[e[4,2]]=e[2,5]; ApplyMetric[e[4,3]]=e[3,5];


ApplyMetric[e[1,5]]=e[4,1]; ApplyMetric[e[2,5]]=e[4,2]; ApplyMetric[e[3,5]]=e[4,3];


ApplyMetric[e[4,5]]=-e[4,5];


ApplyMetric[e[4,2,3]]=-e[2,3,5]; ApplyMetric[e[4,3,1]]=-e[3,1,5]; ApplyMetric[e[4,1,2]]=-e[1,2,5];


ApplyMetric[e[3,2,1]]=e[3,2,1];


ApplyMetric[e[2,3,5]]=-e[4,2,3]; ApplyMetric[e[3,1,5]]=-e[4,3,1]; ApplyMetric[e[1,2,5]]=-e[4,1,2];


ApplyMetric[e[4,1,5]]=-e[4,1,5]; ApplyMetric[e[4,2,5]]=-e[4,2,5]; ApplyMetric[e[4,3,5]]=-e[4,3,5];


ApplyMetric[e[4,2,3,5]]=-e[4,2,3,5]; ApplyMetric[e[4,3,1,5]]=-e[4,3,1,5]; ApplyMetric[e[4,1,2,5]]=-e[4,1,2,5];


ApplyMetric[e[3,2,1,5]]=e[1,2,3,4]; ApplyMetric[e[1,2,3,4]]=e[3,2,1,5];


ApplyMetric[e[]]=e[]; ApplyMetric[e[1,2,3,4,5]]=-e[1,2,3,4,5];


ApplyMetric[x_]:=x e[]/;FreeQ[Hold[x],e]


ApplyMetric[x_ y_]:=x ApplyMetric[y]/;FreeQ[Hold[x],e]


ApplyMetric[x_ y_]:=ApplyMetric[x]y/;FreeQ[Hold[y],e]


ApplyMetric[x_+y_]:=ApplyMetric[x]+ApplyMetric[y]


ApplyAntimetric[x_]:=Comp[ApplyMetric[Comp[x]]]


(* ::Subsubsection:: *)
(*Dual*)


Table[Dual[BasisList[[x]]]=Comp[ApplyMetric[BasisList[[x]]]],{x,1,32}];


Table[Antidual[BasisList[[x]]]=Comp[ApplyAntimetric[BasisList[[x]]]],{x,1,32}];


Dual[x_]:=x e[1,2,3,4,5]/;FreeQ[Hold[x],e]


Dual[x_ y_]:=x Dual[y]/;FreeQ[Hold[x],e]


Dual[x_ y_]:=Dual[x]y/;FreeQ[Hold[y],e]


Dual[x_+y_]:=Dual[x]+Dual[y]


Antidual[x_]:=-x e[1,2,3,4,5]/;FreeQ[Hold[x],e]


Antidual[x_ y_]:=x Antidual[y]/;FreeQ[Hold[x],e]


Antidual[x_ y_]:=Antidual[x]y/;FreeQ[Hold[y],e]


Antidual[x_+y_]:=Antidual[x]+Antidual[y]


(* ::Subsubsection:: *)
(*Dot Product*)


DotProduct[x_,y_]:=(StripBasis[x] . StripBasis[ApplyMetric[y]])e[]


AntidotProduct[x_,y_]:=Comp[DotProduct[Comp[x],Comp[y]]]


(* ::Subsubsection:: *)
(*Norm*)


Unprotect[Power];


Power[e[],1/2]=e[]; Power[e[1,2,3,4,5],1/2]=e[1,2,3,4,5];


Power[Times[x_,e[]],1/2]:=Sqrt[x]e[]/;FreeQ[x,e]


Power[Times[x_,e[1,2,3,4,5]],1/2]:=Sqrt[x]e[1,2,3,4,5]/;FreeQ[x,e]


Protect[Power];


CenterNorm[x_,rules_:{}]:=Module[{y},y=x/.rules; Sqrt[Simplify[DotProduct[y,ConformalConjugate[y]],rules/.Rule->Equal]]/.Reverse[rules,{2}]]


RadiusNorm[x_,rules_:{}]:=Module[{y},y=x/.rules; Sqrt[Simplify[AntidotProduct[y,y],rules/.Rule->Equal]]/.Reverse[rules,{2}]]


RoundBulkNorm[x_,rules_:{}]:=Module[{y},y=AntiwedgeProduct[WedgeProduct[x,e[5]],e[1,2,3,4]]/.rules; Sqrt[Collect[Simplify[DotProduct[y,y],rules/.Rule->Equal],e[]]/.Reverse[rules,{2}]]]


RoundWeightNorm[x_]:=AntiwedgeProduct[Module[{y},y=WedgeProduct[x,e[5]]; Sqrt[Simplify[AntidotProduct[y,y]]]],e[1,2,3,4]]


FlatBulkNorm[x_,rules_:{}]:=WedgeProduct[Module[{y},y=AntiwedgeProduct[x,e[1,2,3,4]]/.rules; FullSimplify[Sqrt[Collect[Simplify[DotProduct[y,y],rules/.Rule->Equal],e[]]/.Reverse[rules,{2}]]]],e[5]]


FlatWeightNorm[x_,rules_:{}]:=Module[{y},y=WedgeProduct[AntiwedgeProduct[x,e[1,2,3,4]],e[5]]/.rules; Sqrt[Simplify[AntidotProduct[y,y],rules/.Rule->Equal]/.Reverse[rules,{2}]]]


(* ::Subsubsection:: *)
(*Exterior Product*)


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]&&FreeQ[Hold[y],e]&&FreeQ[Hold[x],\[CurlyEpsilon]]&&FreeQ[Hold[y],\[CurlyEpsilon]]


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


WedgeProduct[e[x_,y_,z_,w_],e[v_]]:=0/;x==v||y==v||z==v||w==v


WedgeProduct[e[x_,y_,z_,w_],e[v_]]:=e[x,y,z,w,v]/;x!=v&&y!=v&&z!=v&&w!=v


WedgeProduct[e[x_],e[y_,z_,w_,v_]]:=0/;x==y||x==z||x==w||x==v


WedgeProduct[e[x_],e[y_,z_,w_,v_]]:=WedgeProduct[e[x,y,z,w],e[v]]/;x!=y&&x!=z&&x!=w&&x!=v


WedgeProduct[e[x_,y_,z_],e[w_,v_]]:=0/;x==w||x==v||y==w||y==v||z==w||z==v


WedgeProduct[e[x_,y_,z_],e[w_,v_]]:=e[x,y,z,w,v]/;x!=w&&x!=v&&y!=w&&y!=v&&z!=w&&z!=v


WedgeProduct[e[x_,y_],e[z_,w_,v_]]:=0/;x==z||x==w||x==v||y==z||y==w||y==v


WedgeProduct[e[x_,y_],e[z_,w_,v_]]:=e[x,y,z,w,v]/;x!=z&&x!=w&&x!=v&&y!=z&&y!=w&&y!=v


WedgeProduct[e[s_],e[x_,y_,z_,w_,v_]]:=0


WedgeProduct[e[s_,t_],e[x_,y_,z_,w_]]:=0


WedgeProduct[e[s_,t_],e[x_,y_,z_,w_,v_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_,z_,w_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_,z_,w_,v_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_,y_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_,y_,z_,w_]]:=0


WedgeProduct[e[s_,t_,p_,q_],e[x_,y_,z_,w_,v_]]:=0


WedgeProduct[e[s_,t_,p_,q_,r_],e[x_]]:=0


WedgeProduct[e[s_,t_,p_,q_,r_],e[x_,y_]]:=0


WedgeProduct[e[s_,t_,p_,q_,r_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_,p_,q_,r_],e[x_,y_,z_,w_]]:=0


WedgeProduct[e[s_,t_,p_,q_,r_],e[x_,y_,z_,w_,v_]]:=0


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]&&!FreeQ[Hold[y],e]


WedgeProduct[x_,y_]:=y x/;FreeQ[Hold[y],e]&&!FreeQ[Hold[x],e]


WedgeProduct[x_ y_,z_]:=x WedgeProduct[y,z]/;FreeQ[Hold[x],e]


WedgeProduct[x_,y_ z_]:=y WedgeProduct[x,z]/;FreeQ[Hold[y],e]


WedgeProduct[x_ y_,z_ w_]:=x z WedgeProduct[y,w]/;FreeQ[Hold[x],e]&&FreeQ[Hold[z],e]


WedgeProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->WedgeProduct


WedgeProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->WedgeProduct


AntiwedgeProduct[x_,y_]:=Comp[WedgeProduct[Comp[x],Comp[y]]]


(* ::Subsubsection:: *)
(*Interior Products*)


Contraction[x_,y_]:=AntiwedgeProduct[x,Dual[y]]


Expansion[x_,y_]:=WedgeProduct[x,Antidual[y]]


(* ::Subsubsection:: *)
(*Geometric Product*)


\[CurlyEpsilon][1,1]=\[CurlyEpsilon][]; \[CurlyEpsilon][2,2]=\[CurlyEpsilon][]; \[CurlyEpsilon][3,3]=\[CurlyEpsilon][]; \[CurlyEpsilon][4,4]=-\[CurlyEpsilon][]; \[CurlyEpsilon][5,5]=\[CurlyEpsilon][];


\[CurlyEpsilon][3,2]=-\[CurlyEpsilon][2,3]; \[CurlyEpsilon][1,3]=-\[CurlyEpsilon][3,1]; \[CurlyEpsilon][2,1]=-\[CurlyEpsilon][1,2];


\[CurlyEpsilon][1,4]=-\[CurlyEpsilon][4,1]; \[CurlyEpsilon][2,4]=-\[CurlyEpsilon][4,2]; \[CurlyEpsilon][3,4]=-\[CurlyEpsilon][4,3];


\[CurlyEpsilon][5,1]=-\[CurlyEpsilon][1,5]; \[CurlyEpsilon][5,2]=-\[CurlyEpsilon][2,5]; \[CurlyEpsilon][5,3]=-\[CurlyEpsilon][3,5]; \[CurlyEpsilon][5,4]=-\[CurlyEpsilon][4,5];


\[CurlyEpsilon][1,3,2]=\[CurlyEpsilon][3,2,1]; \[CurlyEpsilon][2,1,3]=\[CurlyEpsilon][3,2,1]; \[CurlyEpsilon][1,2,3]=-\[CurlyEpsilon][3,2,1]; \[CurlyEpsilon][2,3,1]=-\[CurlyEpsilon][3,2,1]; \[CurlyEpsilon][3,1,2]=-\[CurlyEpsilon][3,2,1];


\[CurlyEpsilon][3,4,2]=\[CurlyEpsilon][4,2,3]; \[CurlyEpsilon][2,3,4]=\[CurlyEpsilon][4,2,3]; \[CurlyEpsilon][2,4,3]=-\[CurlyEpsilon][4,2,3]; \[CurlyEpsilon][4,3,2]=-\[CurlyEpsilon][4,2,3]; \[CurlyEpsilon][3,2,4]=-\[CurlyEpsilon][4,2,3];


\[CurlyEpsilon][1,4,3]=\[CurlyEpsilon][4,3,1]; \[CurlyEpsilon][3,1,4]=\[CurlyEpsilon][4,3,1]; \[CurlyEpsilon][3,4,1]=-\[CurlyEpsilon][4,3,1]; \[CurlyEpsilon][4,1,3]=-\[CurlyEpsilon][4,3,1]; \[CurlyEpsilon][1,3,4]=-\[CurlyEpsilon][4,3,1];


\[CurlyEpsilon][2,4,1]=\[CurlyEpsilon][4,1,2]; \[CurlyEpsilon][1,2,4]=\[CurlyEpsilon][4,1,2]; \[CurlyEpsilon][1,4,2]=-\[CurlyEpsilon][4,1,2]; \[CurlyEpsilon][4,2,1]=-\[CurlyEpsilon][4,1,2]; \[CurlyEpsilon][2,1,4]=-\[CurlyEpsilon][4,1,2];


\[CurlyEpsilon][3,2,5]=-\[CurlyEpsilon][2,3,5]; \[CurlyEpsilon][1,3,5]=-\[CurlyEpsilon][3,1,5]; \[CurlyEpsilon][2,1,5]=-\[CurlyEpsilon][1,2,5]; \[CurlyEpsilon][1,4,5]=-\[CurlyEpsilon][4,1,5]; \[CurlyEpsilon][2,4,5]=-\[CurlyEpsilon][4,2,5]; \[CurlyEpsilon][3,4,5]=-\[CurlyEpsilon][4,3,5];


Table[\[CurlyEpsilon][5,x,y]=\[CurlyEpsilon][x,y,5],{x,1,5},{y,1,4}];


Table[\[CurlyEpsilon][x,5,y]=-\[CurlyEpsilon][x,y,5],{x,1,5},{y,1,4}];


Table[\[CurlyEpsilon][x[[1]],x[[2]],x[[3]],x[[4]]]=\[CurlyEpsilon][1,2,3,4]Signature[x],{x,OtherPermutations[{1,2,3,4}]}];


Table[\[CurlyEpsilon][x[[1]],x[[2]],x[[3]],x[[4]]]=\[CurlyEpsilon][4,2,3,5]Signature[x],{x,OtherPermutations[{4,2,3,5}]}];


Table[\[CurlyEpsilon][x[[1]],x[[2]],x[[3]],x[[4]]]=-\[CurlyEpsilon][4,3,1,5]Signature[x],{x,OtherPermutations[{4,3,1,5}]}];


Table[\[CurlyEpsilon][x[[1]],x[[2]],x[[3]],x[[4]]]=\[CurlyEpsilon][4,1,2,5]Signature[x],{x,OtherPermutations[{4,1,2,5}]}];


Table[\[CurlyEpsilon][x[[1]],x[[2]],x[[3]],x[[4]]]=-\[CurlyEpsilon][3,2,1,5]Signature[x],{x,OtherPermutations[{3,2,1,5}]}];


WedgeProduct[\[CurlyEpsilon][],x_]:=x/;!FreeQ[Hold[x],\[CurlyEpsilon]]


WedgeProduct[x_,\[CurlyEpsilon][]]:=x/;!FreeQ[Hold[x],\[CurlyEpsilon]]


\[CurlyEpsilon][x_,y_,z_]:=WedgeProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z]]/;x==y


\[CurlyEpsilon][x_,y_,z_]:=-WedgeProduct[\[CurlyEpsilon][x,z],\[CurlyEpsilon][y]]/;x==z


\[CurlyEpsilon][x_,y_,z_]:=WedgeProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y,z]]/;y==z


\[CurlyEpsilon][x_,y_,z_,w_]:=WedgeProduct[\[CurlyEpsilon][x,w],\[CurlyEpsilon][y,z]]/;x==w||y==z


\[CurlyEpsilon][x_,y_,z_,w_]:=-WedgeProduct[\[CurlyEpsilon][x,z],\[CurlyEpsilon][y,w]]/;x==z||y==w


\[CurlyEpsilon][x_,y_,z_,w_]:=WedgeProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z,w]]/;x==y||z==w


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=WedgeProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z,w,v]]/;x==y


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=-WedgeProduct[\[CurlyEpsilon][x,z],\[CurlyEpsilon][y,w,v]]/;x==z


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=WedgeProduct[\[CurlyEpsilon][x,w],\[CurlyEpsilon][y,z,v]]/;x==w


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=-WedgeProduct[\[CurlyEpsilon][x,v],\[CurlyEpsilon][y,z,w]]/;x==v


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=WedgeProduct[\[CurlyEpsilon][y,z],\[CurlyEpsilon][x,w,v]]/;y==z


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=-WedgeProduct[\[CurlyEpsilon][y,w],\[CurlyEpsilon][x,z,v]]/;y==w


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=WedgeProduct[\[CurlyEpsilon][y,v],\[CurlyEpsilon][x,z,w]]/;y==v


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=WedgeProduct[\[CurlyEpsilon][z,w],\[CurlyEpsilon][x,y,v]]/;z==w


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=-WedgeProduct[\[CurlyEpsilon][z,v],\[CurlyEpsilon][x,y,w]]/;z==v


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=WedgeProduct[\[CurlyEpsilon][x,y,z],\[CurlyEpsilon][w,v]]/;w==v


\[CurlyEpsilon][x_,y_,z_,w_,v_]:=\[CurlyEpsilon][1,2,3,4,5]Signature[{x,y,z,w,v}]/;{x,y,z,w,v}!={1,2,3,4,5}


DiagonalBasisList={\[CurlyEpsilon][],\[CurlyEpsilon][1],\[CurlyEpsilon][2],\[CurlyEpsilon][3],\[CurlyEpsilon][4],\[CurlyEpsilon][5],
\[CurlyEpsilon][4,1],\[CurlyEpsilon][4,2],\[CurlyEpsilon][4,3],\[CurlyEpsilon][2,3],\[CurlyEpsilon][3,1],\[CurlyEpsilon][1,2],\[CurlyEpsilon][1,5],\[CurlyEpsilon][2,5],\[CurlyEpsilon][3,5],\[CurlyEpsilon][4,5],
\[CurlyEpsilon][4,2,3],\[CurlyEpsilon][4,3,1],\[CurlyEpsilon][4,1,2],\[CurlyEpsilon][3,2,1],\[CurlyEpsilon][4,1,5],\[CurlyEpsilon][4,2,5],\[CurlyEpsilon][4,3,5],\[CurlyEpsilon][2,3,5],\[CurlyEpsilon][3,1,5],\[CurlyEpsilon][1,2,5],
\[CurlyEpsilon][1,2,3,4],\[CurlyEpsilon][4,2,3,5],\[CurlyEpsilon][4,3,1,5],\[CurlyEpsilon][4,1,2,5],\[CurlyEpsilon][3,2,1,5],\[CurlyEpsilon][1,2,3,4,5]};


DiagonalBasisMap={\[CurlyEpsilon][],\[CurlyEpsilon][1],\[CurlyEpsilon][2],\[CurlyEpsilon][3],(\[CurlyEpsilon][4]-\[CurlyEpsilon][5])/2,\[CurlyEpsilon][4]+\[CurlyEpsilon][5],
(\[CurlyEpsilon][4,1]-\[CurlyEpsilon][5,1])/2,(\[CurlyEpsilon][4,2]-\[CurlyEpsilon][5,2])/2,(\[CurlyEpsilon][4,3]-\[CurlyEpsilon][5,3])/2,\[CurlyEpsilon][2,3],\[CurlyEpsilon][3,1],\[CurlyEpsilon][1,2],\[CurlyEpsilon][1,4]+\[CurlyEpsilon][1,5],\[CurlyEpsilon][2,4]+\[CurlyEpsilon][2,5],\[CurlyEpsilon][3,4]+\[CurlyEpsilon][3,5],\[CurlyEpsilon][4,5],
(\[CurlyEpsilon][4,2,3]-\[CurlyEpsilon][2,3,5])/2,(\[CurlyEpsilon][4,3,1]-\[CurlyEpsilon][3,1,5])/2,(\[CurlyEpsilon][4,1,2]-\[CurlyEpsilon][1,2,5])/2,\[CurlyEpsilon][3,2,1],\[CurlyEpsilon][4,1,5],\[CurlyEpsilon][4,2,5],\[CurlyEpsilon][4,3,5],\[CurlyEpsilon][4,2,3]+\[CurlyEpsilon][2,3,5],\[CurlyEpsilon][4,3,1]+\[CurlyEpsilon][3,1,5],\[CurlyEpsilon][4,1,2]+\[CurlyEpsilon][1,2,5],
(\[CurlyEpsilon][1,2,3,4]+\[CurlyEpsilon][3,2,1,5])/2,\[CurlyEpsilon][4,2,3,5],\[CurlyEpsilon][4,3,1,5],\[CurlyEpsilon][4,1,2,5],-\[CurlyEpsilon][1,2,3,4]+\[CurlyEpsilon][3,2,1,5],\[CurlyEpsilon][1,2,3,4,5]};


GeometricBasisMap={e[],e[1],e[2],e[3],e[5]/2+e[4],e[5]/2-e[4],
e[5,1]/2+e[4,1],e[5,2]/2+e[4,2],e[5,3]/2+e[4,3],e[2,3],e[3,1],e[1,2],e[1,5]/2-e[1,4],e[2,5]/2-e[2,4],e[3,5]/2-e[3,4],e[4,5],
e[2,3,5]/2+e[4,2,3],e[3,1,5]/2+e[4,3,1],e[1,2,5]/2+e[4,1,2],e[3,2,1],e[4,1,5],e[4,2,5],e[4,3,5],e[2,3,5]/2-e[4,2,3],e[3,1,5]/2-e[4,3,1],e[1,2,5]/2-e[4,1,2],
-e[3,2,1,5]/2+e[1,2,3,4],e[4,2,3,5],e[4,3,1,5],e[4,1,2,5],e[3,2,1,5]/2+e[1,2,3,4],e[1,2,3,4,5]};


ToDiagonalBasis[x_]:=x/.Table[BasisList[[a]]->DiagonalBasisMap[[a]],{a,1,32}]


ToGeometricBasis[x_]:=x/.Table[DiagonalBasisList[[a]]->GeometricBasisMap[[a]],{a,1,32}]


GeoProduct[\[CurlyEpsilon][1],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,2,3,5]; GeoProduct[\[CurlyEpsilon][2],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,3,1,5]; GeoProduct[\[CurlyEpsilon][3],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,1,2,5]; GeoProduct[\[CurlyEpsilon][4],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][3,2,1,5]; GeoProduct[\[CurlyEpsilon][5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][1,2,3,4];


GeoProduct[\[CurlyEpsilon][2,3],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,1,5]; GeoProduct[\[CurlyEpsilon][3,1],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,2,5]; GeoProduct[\[CurlyEpsilon][1,2],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,3,5];


GeoProduct[\[CurlyEpsilon][4,1],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][2,3,5]; GeoProduct[\[CurlyEpsilon][4,2],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][3,1,5]; GeoProduct[\[CurlyEpsilon][4,3],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][1,2,5];


GeoProduct[\[CurlyEpsilon][1,5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,2,3]; GeoProduct[\[CurlyEpsilon][2,5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,3,1]; GeoProduct[\[CurlyEpsilon][3,5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,1,2]; GeoProduct[\[CurlyEpsilon][4,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][3,2,1];


GeoProduct[\[CurlyEpsilon][2,3,4],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][1,5]; GeoProduct[\[CurlyEpsilon][3,1,4],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][2,5]; GeoProduct[\[CurlyEpsilon][1,2,4],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][3,5]; GeoProduct[\[CurlyEpsilon][3,2,1],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,5];


GeoProduct[\[CurlyEpsilon][2,3,5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,1]; GeoProduct[\[CurlyEpsilon][3,1,5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,2]; GeoProduct[\[CurlyEpsilon][1,2,5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4,3];


GeoProduct[\[CurlyEpsilon][4,1,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][2,3]; GeoProduct[\[CurlyEpsilon][4,2,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][3,1]; GeoProduct[\[CurlyEpsilon][4,3,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][1,2];


GeoProduct[\[CurlyEpsilon][1,2,3,4],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][5]; GeoProduct[\[CurlyEpsilon][4,2,3,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][1]; GeoProduct[\[CurlyEpsilon][4,3,1,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][2]; GeoProduct[\[CurlyEpsilon][4,1,2,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][3]; GeoProduct[\[CurlyEpsilon][3,2,1,5],\[CurlyEpsilon][1,2,3,4,5]]=\[CurlyEpsilon][4];


Table[GeoProduct[\[CurlyEpsilon][1,2,3,4,5],\[CurlyEpsilon][x]]=GeoProduct[\[CurlyEpsilon][1],\[CurlyEpsilon][4,2,3,5,x]],{x,1,5}];


Table[GeoProduct[\[CurlyEpsilon][1,2,3,4,5],\[CurlyEpsilon][x,y]]=GeoProduct[\[CurlyEpsilon][1,2],\[CurlyEpsilon][3,4,5,x,y]],{x,1,5},{y,1,5}];


GeoProduct[\[CurlyEpsilon][1,2,3,4,5],\[CurlyEpsilon][x_,y_,z_]]:=GeoProduct[\[CurlyEpsilon][1,2,3],\[CurlyEpsilon][4,5,x,y,z]]


GeoProduct[\[CurlyEpsilon][1,2,3,4,5],\[CurlyEpsilon][x_,y_,z_,w_]]:=GeoProduct[\[CurlyEpsilon][1,2,3,4],\[CurlyEpsilon][5,x,y,z,w]]


GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][]]=\[CurlyEpsilon][]; GeoProduct[\[CurlyEpsilon][1,2,3,4,5],\[CurlyEpsilon][1,2,3,4,5]]=-\[CurlyEpsilon][];


Table[GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x]]=\[CurlyEpsilon][x],{x,1,5}];


Table[GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x,y]]=\[CurlyEpsilon][x,y],{x,1,5},{y,1,5}];


GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x_,y_,z_]]:=\[CurlyEpsilon][x,y,z]


GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x_,y_,z_,w_]]:=\[CurlyEpsilon][x,y,z,w]


GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x_,y_,z_,w_,v_]]:=\[CurlyEpsilon][x,y,z,w,v]


Table[GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][]]=\[CurlyEpsilon][x],{x,1,5}];


Table[GeoProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][]]=\[CurlyEpsilon][x,y],{x,1,5},{y,1,5}];


GeoProduct[\[CurlyEpsilon][x_,y_,z_],\[CurlyEpsilon][]]:=\[CurlyEpsilon][x,y,z]


GeoProduct[\[CurlyEpsilon][x_,y_,z_,w_],\[CurlyEpsilon][]]:=\[CurlyEpsilon][x,y,z,w]


GeoProduct[\[CurlyEpsilon][x_,y_,z_,w_,v_],\[CurlyEpsilon][]]:=\[CurlyEpsilon][x,y,z,w,v]


Table[GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y]]=\[CurlyEpsilon][x,y],{x,1,5},{y,1,5}];


GeoProduct[\[CurlyEpsilon][x_],\[CurlyEpsilon][y_,z_]]:=\[CurlyEpsilon][x,y,z]


GeoProduct[\[CurlyEpsilon][x_,y_],\[CurlyEpsilon][z_]]:=\[CurlyEpsilon][x,y,z]


GeoProduct[\[CurlyEpsilon][x_,y_],\[CurlyEpsilon][z_,w_]]:=\[CurlyEpsilon][x,y,z,w]


GeoProduct[\[CurlyEpsilon][x_],\[CurlyEpsilon][y_,z_,w_]]:=\[CurlyEpsilon][x,y,z,w]


GeoProduct[\[CurlyEpsilon][x_,y_,z_],\[CurlyEpsilon][w_]]:=\[CurlyEpsilon][x,y,z,w]


GeoProduct[\[CurlyEpsilon][x_],\[CurlyEpsilon][y_,z_,w_,v_]]:=\[CurlyEpsilon][x,y,z,w,v]


GeoProduct[\[CurlyEpsilon][x_,y_,z_,w_],\[CurlyEpsilon][v_]]:=\[CurlyEpsilon][x,y,z,w,v]


GeoProduct[\[CurlyEpsilon][x_,y_],\[CurlyEpsilon][s_,t_,p_]]:=GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y,s,t,p]]


GeoProduct[\[CurlyEpsilon][x_,y_,z_],\[CurlyEpsilon][s_,t_]]:=GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y,z,s,t]]


GeoProduct[\[CurlyEpsilon][x_,y_,z_],\[CurlyEpsilon][s_,t_,p_]]:=GeoProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z,s,t,p]]


GeoProduct[\[CurlyEpsilon][x_,y_],\[CurlyEpsilon][s_,t_,p_,q_]]:=GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y,s,t,p,q]]


GeoProduct[\[CurlyEpsilon][x_,y_,z_,w_],\[CurlyEpsilon][s_,t_]]:=GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y,z,w,s,t]]


GeoProduct[\[CurlyEpsilon][x_,y_,z_],\[CurlyEpsilon][s_,t_,p_,q_]]:=GeoProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z,s,t,p,q]]


GeoProduct[\[CurlyEpsilon][x_,y_,z_,w_],\[CurlyEpsilon][s_,t_,p_]]:=GeoProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z,w,s,t,p]]


GeoProduct[\[CurlyEpsilon][x_,y_,z_,w_],\[CurlyEpsilon][s_,t_,p_,q_]]:=GeoProduct[\[CurlyEpsilon][x,y,z],\[CurlyEpsilon][w,s,t,p,q]]


GeoProduct[x_,y_]:=x y/;FreeQ[Hold[x],\[CurlyEpsilon]]; GeoProduct[x_,y_]:=x y/;FreeQ[Hold[x],\[CurlyEpsilon]]; GeoProduct[x_,y_]:=y x/;FreeQ[Hold[y],\[CurlyEpsilon]]


GeoProduct[x_ y_,z_]:=x GeoProduct[y,z]/;FreeQ[Hold[x],\[CurlyEpsilon]]; GeoProduct[x_,y_ z_]:=y GeoProduct[x,z]/;FreeQ[Hold[y],\[CurlyEpsilon]]


GeoProduct[x_ y_,z_ w_]:=x z GeoProduct[y,w]/;FreeQ[Hold[x],\[CurlyEpsilon]]&&FreeQ[Hold[z],\[CurlyEpsilon]]


GeoProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->GeoProduct


GeoProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->GeoProduct


GeometricProduct[x_,y_]:=ToGeometricBasis[GeoProduct[ToDiagonalBasis[x],ToDiagonalBasis[y]]]


GeometricAntiproduct[x_,y_]:=Comp[GeometricProduct[Comp[x],Comp[y]]]


Sandwich[m_,x_]:=GeometricProduct[GeometricProduct[m,x],Rev[m]]


Antisandwich[m_,x_]:=GeometricAntiproduct[GeometricAntiproduct[m,x],Rev[m]]


(* ::Subsubsection:: *)
(*Properties*)


Carrier[x_]:=WedgeProduct[x,e[5]]


Cocarrier[x_]:=WedgeProduct[Antidual[x],e[5]]


FlatCenter[x_]:=BasisCollect[AntiwedgeProduct[Carrier[x],Cocarrier[x]]]


RoundCenter[x_]:=BasisCollect[AntiwedgeProduct[Cocarrier[x],x]]


Container[x_]:=BasisCollect[WedgeProduct[x,Antidual[Carrier[x]]]]


Partner[x_]:=BasisCollect[AntiwedgeProduct[Container[Antidual[x]],Carrier[x]]]


Attitude[x_]:=AntiwedgeProduct[x,e[3,2,1,5]]


(* ::Subsubsection:: *)
(*Flat Point*)


FlatPoint[x_,y_,z_,w_:1] := x e[1,5] + y e[2,5] + z e[3,5] + w e[4,5]


(* ::Subsubsection:: *)
(*Line*)


FlatLine[vx_,vy_,vz_,mx_,my_,mz_] := vx e[4,1,5] + vy e[4,2,5] + vz e[4,3,5] + mx e[2,3,5] + my e[3,1,5] + mz e[1,2,5]


LineSupport[l_]:=AntiwedgeProduct[l,WedgeProduct[e[4],Dual[l]]]


LineDirection[l_]:=Module[{vx=Coefficient[l,e[4,1,5]],vy=Coefficient[l,e[4,2,5]],vz=Coefficient[l,e[4,3,5]]},
	vx e[1] + vy e[2] + vz e[3]]


LineConstraints[l_]:=Module[{vx=Coefficient[l,e[4,1,5]],vy=Coefficient[l,e[4,2,5]],vz=Coefficient[l,e[4,3,5]],
	mx=Coefficient[l,e[2,3,5]],my=Coefficient[l,e[3,1,5]],mz=Coefficient[l,e[1,2,5]]},
	{vx mx + vy my + vz mz == 0}]


(* ::Subsubsection:: *)
(*Plane*)


FlatPlane[gx_,gy_,gz_,gw_] := gx e[4,2,3,5] + gy e[4,3,1,5] + gz e[4,1,2,5] + gw e[3,2,1,5]


PlaneSupport[g_]:=AntiwedgeProduct[g,WedgeProduct[e[4],Dual[g]]]


PlaneNormal[g_]:=Module[{gx=Coefficient[g,e[4,2,3,5]],gy=Coefficient[g,e[4,3,1,5]],gz=Coefficient[g,e[4,1,2,5]]},
	gx e[1] + gy e[2] + gz e[3]]


(* ::Subsubsection:: *)
(*Round Point*)


RoundPoint[px_,py_,pz_,pw_:1,r2_:0] := px e[1] + py e[2] + pz e[3] + pw e[4] + (px^2 + py^2 + pz^2 + pw^2 r2)/(2pw)e[5]


PointSquaredRadius[a_]:=Module[{x=Coefficient[a,e[1]],y=Coefficient[a,e[2]],z=Coefficient[a,e[3]],w=Coefficient[a,e[4]],u=Coefficient[a,e[5]]},
	2w u - x^2 - y^2 - z^2]


(* ::Subsubsection:: *)
(*Dipole*)


RoundDipole[r2_,cx_,cy_,cz_,nx_,ny_,nz_] := nx e[4,1] + ny e[4,2] + nz e[4,3] +
	(cy nz - cz ny) e[2,3] + (cz nx - cx nz) e[3,1] + (cx ny - cy nx) e[1,2] +
	(cx nx + cy ny + cz nz)(cx e[1,5] + cy e[2,5] + cz e[3,5] + e[4,5]) -
	(cx^2 + cy^2 + cz^2 + r2)/2(nx e[1,5] + ny e[2,5] + nz e[3,5])


DipoleSquaredRadius[d_]:=Module[{px=Coefficient[d,e[1,5]],py=Coefficient[d,e[2,5]],pz=Coefficient[d,e[3,5]],pw=Coefficient[d,e[4,5]],
	mx=Coefficient[d,e[2,3]],my=Coefficient[d,e[3,1]],mz=Coefficient[d,e[1,2]],
	vx=Coefficient[d,e[4,1]],vy=Coefficient[d,e[4,2]],vz=Coefficient[d,e[4,3]]},
	pw^2 - mx^2 - my^2 - mz^2 - 2px vx - 2py vy - 2pz vz]


DipoleFlatCenter[d_]:=Module[{px=Coefficient[d,e[1,5]],py=Coefficient[d,e[2,5]],pz=Coefficient[d,e[3,5]],pw=Coefficient[d,e[4,5]],
	mx=Coefficient[d,e[2,3]],my=Coefficient[d,e[3,1]],mz=Coefficient[d,e[1,2]],
	vx=Coefficient[d,e[4,1]],vy=Coefficient[d,e[4,2]],vz=Coefficient[d,e[4,3]]},
	(vy mz - vz my + vx pw)e[1,5] + (vz mx - vx mz + vy pw)e[2,5] + (vx my - vy mx + vz pw)e[3,5] + (vx^2 + vy^2 + vz^2)e[4,5]]


DipoleSurface[d_]:=Module[{cen=RoundCenter[d],dp=Attitude[d]Sqrt[Abs[DipoleSquaredRadius[d]]]},{cen-dp,cen+dp}]


DipoleConstraints[d_]:=Module[{px=Coefficient[d,e[1,5]],py=Coefficient[d,e[2,5]],pz=Coefficient[d,e[3,5]],pw=Coefficient[d,e[4,5]],
	mx=Coefficient[d,e[2,3]],my=Coefficient[d,e[3,1]],mz=Coefficient[d,e[1,2]],
	vx=Coefficient[d,e[4,1]],vy=Coefficient[d,e[4,2]],vz=Coefficient[d,e[4,3]]},
	{py vz - pz vy - pw mx == 0,
	pz vx - px vz - pw my == 0,
	px vy - py vx - pw mz == 0,
	px mx + py my + pz mz == 0,
	mx vx + my vy + mz vz == 0}]


(* ::Subsubsection:: *)
(*Circle*)


RoundCircle[r2_,cx_,cy_,cz_,nx_,ny_,nz_] := nx e[4,2,3] + ny e[4,3,1] + nz e[4,1,2] +
	(cy nz - cz ny)e[4,1,5] + (cz nx - cx nz)e[4,2,5] + (cx ny - cy nx)e[4,3,5] +
	(cx nx + cy ny + cz nz)(cx e[2,3,5] + cy e[3,1,5] + cz e[1,2,5] - e[3,2,1]) +
	(r2 - cx^2 - cy^2 - cz^2)/2(nx e[2,3,5] + ny e[3,1,5] + nz e[1,2,5])


CircleSquaredRadius[c_]:=Module[{gx=Coefficient[c,e[4,2,3]],gy=Coefficient[c,e[4,3,1]],gz=Coefficient[c,e[4,1,2]],gw=Coefficient[c,e[3,2,1]],
	vx=Coefficient[c,e[4,1,5]],vy=Coefficient[c,e[4,2,5]],vz=Coefficient[c,e[4,3,5]],
	mx=Coefficient[c,e[2,3,5]],my=Coefficient[c,e[3,1,5]],mz=Coefficient[c,e[1,2,5]]},
	vx^2 + vy^2 + vz^2 - gw^2 + 2gx mx + 2gy my + 2gz mz]


CircleFlatCenter[c_]:=Module[{gx=Coefficient[c,e[4,2,3]],gy=Coefficient[c,e[4,3,1]],gz=Coefficient[c,e[4,1,2]],gw=Coefficient[c,e[3,2,1]],
	vx=Coefficient[c,e[4,1,5]],vy=Coefficient[c,e[4,2,5]],vz=Coefficient[c,e[4,3,5]],
	mx=Coefficient[c,e[2,3,5]],my=Coefficient[c,e[3,1,5]],mz=Coefficient[c,e[1,2,5]]},
	(gy vz - gz vy - gx gw)e[1,5] + (gz vx - gx vz - gy gw)e[2,5] + (gx vy - gy vx - gz gw)e[3,5] + (gx^2 + gy^2 + gz^2)e[4,5]]


CircleNormal[c_]:=Module[{gx=Coefficient[c,e[4,2,3]],gy=Coefficient[c,e[4,3,1]],gz=Coefficient[c,e[4,1,2]]},
	gx e[1] + gy e[2] + gz e[3]]


CircleConstraints[c_]:=Module[{gx=Coefficient[c,e[4,2,3]],gy=Coefficient[c,e[4,3,1]],gz=Coefficient[c,e[4,1,2]],gw=Coefficient[c,e[3,2,1]],
	vx=Coefficient[c,e[4,1,5]],vy=Coefficient[c,e[4,2,5]],vz=Coefficient[c,e[4,3,5]],
	mx=Coefficient[c,e[2,3,5]],my=Coefficient[c,e[3,1,5]],mz=Coefficient[c,e[1,2,5]]},
	{gy mz - gz my - gw vx == 0,
	gz mx - gx mz - gw vy == 0,
	gx my - gy mx - gw vz == 0,
	gx vx + gy vy + gz vz == 0,
	vx mx + vy my + vz mz == 0}]


(* ::Subsubsection:: *)
(*Sphere*)


RoundSphere[r2_,cx_,cy_,cz_,cw_:1] := cx e[4,2,3,5] + cy e[4,3,1,5] + cz e[4,1,2,5] - cw e[1,2,3,4] + (cw^2 r2 - cx^2 - cy^2 - cz^2)/(2cw)e[3,2,1,5]


SphereSquaredRadius[s_]:=Module[{u=Coefficient[s,e[1,2,3,4]],x=Coefficient[s,e[2,3,4,5]],y=Coefficient[s,e[3,1,4,5]],z=Coefficient[s,e[1,2,4,5]],w=Coefficient[s,e[3,2,1,5]]},
	x^2 + y^2 + z^2 - 2w u]


SphereFlatCenter[s_]:=Module[{u=Coefficient[s,e[1,2,3,4]],x=Coefficient[s,e[2,3,4,5]],y=Coefficient[s,e[3,1,4,5]],z=Coefficient[s,e[1,2,4,5]]},
	x e[1,5] + y e[2,5] + z e[3,5] - u e[4,5]]


(* ::Subsubsection:: *)
(*Projection*)


ConformalProject[x_,y_]:=AntiwedgeProduct[y,Expansion[x,y]]


(* ::Subsubsection:: *)
(*Exomorphism*)


BivectorTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4]+m[[5]][[1]]e[5],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4]+m[[5]][[2]]e[5],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4]+m[[5]][[3]]e[5],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4]+m[[5]][[4]]e[5],
	c5=m[[1]][[5]]e[1]+m[[2]][[5]]e[2]+m[[3]][[5]]e[3]+m[[4]][[5]]e[4]+m[[5]][[5]]e[5],
	p1,p2,p3,p4,p5,p6,p7,p8,p9,p10},
	
	p1=Simplify[WedgeProduct[c4,c1]];
	p2=Simplify[WedgeProduct[c4,c2]];
	p3=Simplify[WedgeProduct[c4,c3]];
	p4=Simplify[WedgeProduct[c2,c3]];
	p5=Simplify[WedgeProduct[c3,c1]];
	p6=Simplify[WedgeProduct[c1,c2]];
	p7=Simplify[WedgeProduct[c1,c5]];
	p8=Simplify[WedgeProduct[c2,c5]];
	p9=Simplify[WedgeProduct[c3,c5]];
	p10=Simplify[WedgeProduct[c4,c5]];
	
	Transpose[{
	Table[Coefficient[p1,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p2,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p3,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p4,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p5,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p6,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p7,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p8,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p9,BivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p10,BivectorList[[k]]],{k,1,10}]}]]


TrivectorTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4]+m[[5]][[1]]e[5],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4]+m[[5]][[2]]e[5],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4]+m[[5]][[3]]e[5],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4]+m[[5]][[4]]e[5],
	c5=m[[1]][[5]]e[1]+m[[2]][[5]]e[2]+m[[3]][[5]]e[3]+m[[4]][[5]]e[4]+m[[5]][[5]]e[5],
	p1,p2,p3,p4,p5,p6,p7,p8,p9,p10},
	
	p1=Simplify[WedgeProduct[WedgeProduct[c4,c2],c3]];
	p2=Simplify[WedgeProduct[WedgeProduct[c4,c3],c1]];
	p3=Simplify[WedgeProduct[WedgeProduct[c4,c1],c2]];
	p4=Simplify[WedgeProduct[WedgeProduct[c3,c2],c1]];
	p5=Simplify[WedgeProduct[WedgeProduct[c4,c1],c5]];
	p6=Simplify[WedgeProduct[WedgeProduct[c4,c2],c5]];
	p7=Simplify[WedgeProduct[WedgeProduct[c4,c3],c5]];
	p8=Simplify[WedgeProduct[WedgeProduct[c2,c3],c5]];
	p9=Simplify[WedgeProduct[WedgeProduct[c3,c1],c5]];
	p10=Simplify[WedgeProduct[WedgeProduct[c1,c2],c5]];
	
	Transpose[{
	Table[Coefficient[p1,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p2,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p3,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p4,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p5,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p6,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p7,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p8,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p9,TrivectorList[[k]]],{k,1,10}],
	Table[Coefficient[p10,TrivectorList[[k]]],{k,1,10}]}]]


QuadrivectorTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4]+m[[5]][[1]]e[5],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4]+m[[5]][[2]]e[5],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4]+m[[5]][[3]]e[5],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4]+m[[5]][[4]]e[5],
	c5=m[[1]][[5]]e[1]+m[[2]][[5]]e[2]+m[[3]][[5]]e[3]+m[[4]][[5]]e[4]+m[[5]][[5]]e[5],
	p1,p2,p3,p4,p5},

	p1=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c1,c2],c3],c4]];
	p2=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c4,c2],c3],c5]];
	p3=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c4,c3],c1],c5]];
	p4=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c4,c1],c2],c5]];
	p5=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c3,c2],c1],c5]];

	Transpose[{
	Table[Coefficient[p1,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p2,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p3,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p4,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p5,QuadrivectorList[[k]]],{k,1,5}]}]]


AntiscalarTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4]+m[[5]][[1]]e[5],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4]+m[[5]][[2]]e[5],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4]+m[[5]][[3]]e[5],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4]+m[[5]][[4]]e[5],
	c5=m[[1]][[5]]e[1]+m[[2]][[5]]e[2]+m[[3]][[5]]e[3]+m[[4]][[5]]e[4]+m[[5]][[5]]e[5]},
	{{Coefficient[Simplify[WedgeProduct[WedgeProduct[WedgeProduct[WedgeProduct[c1,c2],c3],c4],c5]],e[1,2,3,4,5]]}}]


Exomorphism[m_]:=SparseArray[Band[{1,1}]->{{{1}},m,BivectorTransform[m],TrivectorTransform[m],QuadrivectorTransform[m],AntiscalarTransform[m]}]


OperatorMatrix[op_,constraints_:{}]:=Module[{x,y,z,w,u,v,xp,yp,zp,wp,up},v=Simplify[Antisandwich[op,x e[1]+y e[2]+z e[3]+w e[4]+u e[5]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];wp=Coefficient[v,e[4]];up=Coefficient[v,e[5]];
	{{Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z],Coefficient[xp,w],Coefficient[xp,u]},
	{Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z],Coefficient[yp,w],Coefficient[yp,u]},
	{Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z],Coefficient[zp,w],Coefficient[zp,u]},
	{Coefficient[wp,x],Coefficient[wp,y],Coefficient[wp,z],Coefficient[wp,w],Coefficient[wp,u]},
	{Coefficient[up,x],Coefficient[up,y],Coefficient[up,z],Coefficient[up,w],Coefficient[up,u]}}]


CompOperatorMatrix[op_,constraints_:{}]:=Module[{x,y,z,w,u,v,xp,yp,zp,wp,up},v=Simplify[Sandwich[op,x e[1]+y e[2]+z e[3]+w e[4]+u e[5]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];wp=Coefficient[v,e[4]];up=Coefficient[v,e[5]];
	{{Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z],Coefficient[xp,w],Coefficient[xp,u]},
	{Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z],Coefficient[yp,w],Coefficient[yp,u]},
	{Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z],Coefficient[zp,w],Coefficient[zp,u]},
	{Coefficient[wp,x],Coefficient[wp,y],Coefficient[wp,z],Coefficient[wp,w],Coefficient[wp,u]},
	{Coefficient[up,x],Coefficient[up,y],Coefficient[up,z],Coefficient[up,w],Coefficient[up,u]}}]


(* ::Subsubsection:: *)
(*Drawing*)


LineColor=RGBColor["#4080FF"];


PlaneColor=RGBColor["#FFFF80"];


RoundPointColor[r2_]:=If[r2>=0,RGBColor["#80FF80"],RGBColor["#D980FF"]];


DipoleColor[r2_]:=If[r2>=0,RGBColor["#408CC0"],RGBColor["#C04040"]];


CircleColor[r2_]:=If[r2>=0,RGBColor["#408CC0"],RGBColor["#C04040"]];


SphereColor[r2_]:=If[r2>=0,RGBColor["#59C0FF"],RGBColor["#FF4040"]];


VectorUnitCoords[v_]:=Module[{x=Coefficient[v,e[1]],y=Coefficient[v,e[2]],z=Coefficient[v,e[3]]},Normalize[{x,y,z}]]


RoundPointCoords[a_]:=Module[{x=Coefficient[a,e[1]],y=Coefficient[a,e[2]],z=Coefficient[a,e[3]],w=Coefficient[a,e[4]]},{x/w,y/w,z/w}]


FlatPointCoords[p_]:=Module[{x=Coefficient[p,e[1,5]],y=Coefficient[p,e[2,5]],z=Coefficient[p,e[3,5]],w=Coefficient[p,e[4,5]]},{x/w,y/w,z/w}]


MakePerp[v_]:=Module[{ax=Abs[v[[1]]],ay=Abs[v[[2]]],az=Abs[v[[3]]]},Normalize[If[az<Min[ax,ay],{v[[2]],-v[[1]],0},If[ay<ax,{-v[[3]],0,v[[1]]},{0,v[[3]],-v[[2]]}]]]]


DrawFlatPoint[p_]:=Graphics3D[{AbsolutePointSize[8],Point[FlatPointCoords[p]]}]


DrawLine[l_]:=Module[{sup=RoundPointCoords[LineSupport[l]],dir=VectorUnitCoords[LineDirection[l]]},
	Graphics3D[{LineColor,AbsoluteThickness[4],Line[{sup-6dir,sup+6dir}]}]]


DrawPlane[g_]:=Module[{sup=RoundPointCoords[PlaneSupport[g]],nrm=VectorUnitCoords[PlaneNormal[g]],tan,bit},
	tan=MakePerp[nrm]; bit=Cross[nrm,tan];
	Graphics3D[{PlaneColor,Opacity[0.5],EdgeForm[{Black,AbsoluteThickness[1]}],Polygon[{sup-6tan-6bit,sup+6tan-6bit,sup+6tan+6bit,sup-6tan+6bit}]}]]


DrawRoundPoint[a_]:=Module[{r2=PointSquaredRadius[a],cen=RoundPointCoords[a]},
	Graphics3D[{RoundPointColor[r2],Opacity[0.5],Sphere[cen,Sqrt[Abs[r2]]/Abs[Coefficient[a,e[4]]]],RGBColor[0,0,0],AbsolutePointSize[5],Point[cen]}]]


DrawDipole[d_]:=Module[{r2=DipoleSquaredRadius[d],cen=FlatPointCoords[DipoleFlatCenter[d]],dir=VectorUnitCoords[Attitude[d]],rad},
	rad=Sqrt[Abs[r2]]/Coefficient[RoundWeightNorm[d],e[1,2,3,4]];
	Graphics3D[{AbsolutePointSize[8],RGBColor[0,0,0],Point[cen+dir rad],Point[cen-dir rad],DipoleColor[r2],AbsoluteThickness[2],AbsoluteDashing[4],Line[{cen+dir rad,cen-dir rad}]}]]


DrawCircle[c_]:=Module[{r2=CircleSquaredRadius[c],cen=FlatPointCoords[CircleFlatCenter[c]],nrm=VectorUnitCoords[CircleNormal[c]],rad,tan},
	rad=Sqrt[Abs[r2]]/Coefficient[RoundWeightNorm[c],e[1,2,3,4]];tan=MakePerp[nrm] rad;
	{ParametricPlot3D[cen+tan Cos[\[CurlyPhi]]+Cross[nrm,tan] Sin[\[CurlyPhi]],{\[CurlyPhi],0,2Pi},PlotStyle->CircleColor[r2]],
	Graphics3D[{AbsolutePointSize[8],RGBColor[0,0,0],Point[cen]}]}]


DrawSphere[s_]:=Module[{r2=SphereSquaredRadius[s],cen=FlatPointCoords[SphereFlatCenter[s]]},
	Graphics3D[{SphereColor[r2],Opacity[0.5],Sphere[cen,Sqrt[Abs[r2]]/Abs[Coefficient[s,e[1,2,3,4]]]],RGBColor[0,0,0],Opacity[1],AbsolutePointSize[8],Point[cen]}]]


(* ::Subsubsection:: *)
(*Formatting*)


Format[e[],TraditionalForm]:=DisplayForm[Style["1",Bold,FontFamily->"Times New Roman",FontSize->16]]


Format[e[x_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_,z_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y]<>TextString[z],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_,z_,w_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y]<>TextString[z]<>TextString[w],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[1,2,3,4,5],TraditionalForm]:=DisplayForm[StyleBox[FromCharacterCode[16^^1D7D9],FontFamily->"Segoe UI Symbol",FontSize->16]]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2022],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2022]->"~DotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2218],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2218]->"~AntidotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2227],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2227]->"~WedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2228],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2228]->"~AntiwedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27D1],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27D1]->"~GeometricProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27C7],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27C7]->"~GeometricAntiproduct~"],StandardForm]


DividerList={Thick,Thick,Thick,True,True,True,True,Thick,True,True,True,True,True,True,True,True,True,Thick,True,True,True,True,True,True,True,True,True,Thick,True,True,True,True,Thick,Thick};


(* ::Subsection:: *)
(*Epilog*)


End[];


EndPackage[];
