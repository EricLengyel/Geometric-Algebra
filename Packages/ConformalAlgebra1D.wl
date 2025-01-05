(* ::Package:: *)

(* ::Title:: *)
(*Conformal Geometric Algebra in 1D*)


(* ::Author:: *)
(*By Eric Lengyel, Copyright 2019-2025*)


(* ::Subsection:: *)
(*Prolog*)


BeginPackage["ConformalAlgebra1D`"];


e::usage = "e[...] represents a basis element in the algebra";


VectorList::usage = "VectorList is a list of the 3 vector basis elements";


BivectorList::usage = "BivectorList is a list of the 3 bivector basis elements";


BasisList::usage = "BasisList is a list of all 8 basis elements";


BasisCollect::usage = "BasisCollect[x] collects terms of x with respect to all 8 basis elements";


BasisForm::usage = "BasisForm[x] prints x in traditional form after collecting on the basis elements";


BasisGrade::usage = "BasisGrade is a list of the grades of all 8 basis elements";


BasisAntigrade::usage = "BasisAntigrade is a list of the antigrades of all 8 basis elements";


ScalarPart::usage = "ScalarPart[x] returns the scalar (grade 0) component of x";


AntiscalarPart::usage = "AntiscalarPart[x] returns the antiscalar (grade 3) component of x";


VectorPart::usage = "VectorPart[x] returns the vector (grade 1) components of x";


BivectorPart::usage = "BivectorPart[x] returns the bivector (grade 2) components of x";


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


StripBasis::usage = "StripBasis[x] converts a multivector x on the basis elements to a 8-entry list";


Multivector::usage = "Multivector[x] converts a 8-entry list x to a multivector on the basis elements";


Comp::usage = "Comp[x] returns the complement of x";


Rev::usage = "Rev[x] returns the reverse of x";


Antirev::usage = "Antirev[x] returns the antireverse of x";


ConformalConjugate::usage = "ConformalConjugate[x] returns the conformal conjugate of x";


Metric::usage = "Metric is the 3x3 metric tensor";


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


FlatPoint::usage = "FlatPoint[px,py:1] is a flat point with coordinates (px,py)";


RoundPoint::usage = "RoundPoint[px,py:1,r2:0] is a round point with center (px,py) and squared radius r2";


PointSquaredRadius::usage = "PointSquaredRadius[a] returns the squared weighted radius of the round point a";


RoundDipole::usage = "RoundDipole[r2,cx] is a dipole with squared radius r2 and center (cx)";


DipoleSquaredRadius::usage = "DipoleSquaredRadius[d] returns the squared weighted radius of the dipole d";


DipoleFlatCenter::usage = "DipoleFlatCenter[d] returns the center of the dipole d as a flat point";


DipoleSurface::usage = "DipoleSurface[d] returns a list containing the two round points on the surface of the dipole d";


BivectorTransform::usage = "BivectorTransform[m] returns the second compound matrix of a 3x3 matrix m that transforms vectors, which is a 3x3 matrix that transforms bivectors";


AntiscalarTransform::usage = "AntiscalarTransform[m] returns the third compound matrix of a 3x3 matrix m that transforms vectors, which is a 1x1 matrix that transforms antiscalars";


Exomorphism::usage = "Exomorphism[m] returns the exomorphism matrix corresponding to the 3x3 matrix m that transforms vectors, which is a 8x8 block diagonal matrix that transforms multivectors";


OperatorMatrix::usage = "OperatorMatrix[op,constraints] returns the 3x3 vector transformation matrix corresponding to the geometric algebra operator op with optional geometric constraints";


CompOperatorMatrix::usage = "CompOperatorMatrix[op,constraints] returns the 3x3 vector transformation matrix corresponding to the geometric algebra complement operator op with optional geometric constraints";


DividerList::usage = "DividerList is a list of table divider thicknesses for a header plus 8 entries";


DrawFlatPoint::usage = "DrawFlatPoint[p] returns graphics for a flat point p";


DrawRoundPoint::usage = "DrawRoundPoint[a] returns graphics for a round point a";


DrawDipole::usage = "DrawDipole[d] returns graphics for a dipole d";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Basis Elements*)


e[2,1]=-e[1,2]; e[3,2]=-e[2,3]; e[1,3]=-e[3,1];


e[3,2,1]=-e[1,2,3]; e[2,1,3]=-e[1,2,3]; e[1,3,2]=-e[1,2,3]; e[3,1,2]=e[1,2,3]; e[2,3,1]=e[1,2,3];


VectorList={e[1],e[2],e[3]};


BivectorList={e[1,2],e[2,3],e[3,1]};


BasisList={e[]}~Join~VectorList~Join~BivectorList~Join~{e[1,2,3]};


BasisCollect[x_]:=Collect[x,BasisList]


BasisForm[x_]:=TraditionalForm[BasisCollect[x]]


BasisGrade={0,1,1,1,2,2,2,3};


BasisAntigrade={3,2,2,2,1,1,1,0};


ScalarPart[x_]:=Coefficient[x,e[]]e[]


AntiscalarPart[x_]:=Coefficient[x,e[1,2,3]]e[1,2,3]


VectorPart[x_]:=Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]


BivectorPart[x_]:=Coefficient[x,e[1,2]]e[1,2]+Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]


RoundBulkPart[x_]:=Coefficient[x,e[]]e[]+Coefficient[x,e[1]]e[1]


RoundWeightPart[x_]:=Coefficient[x,e[2]]e[2]+Coefficient[x,e[1,2]]e[1,2]


FlatBulkPart[x_]:=Coefficient[x,e[3]]e[3]+Coefficient[x,e[3,1]]e[3,1]


FlatWeightPart[x_]:=Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[1,2,3]]e[1,2,3]


RoundPart[x_]:=RoundBulkPart[x]+RoundWeightPart[x]


FlatPart[x_]:=FlatBulkPart[x]+FlatWeightPart[x]


BulkPart[x_]:=RoundBulkPart[x]+FlatBulkPart[x]


WeightPart[x_]:=RoundWeightPart[x]+FlatWeightPart[x]


GradeSelect[x_,k_]:=Switch[k,0,ScalarPart[x],1,VectorPart[x],2,BivectorPart[x],3,AntiscalarPart[x],_,0]


AntigradeSelect[x_,k_]:=GradeSelect[x,3-k]


StripBasis[x_]:=Table[Coefficient[x,BasisList[[i]]],{i,1,8}]


Multivector[x_]:=Sum[x[[i]]BasisList[[i]],{i,1,8}]


(* ::Subsubsection:: *)
(*Complement*)


Comp[e[1]]=e[2,3]; Comp[e[2]]=e[3,1]; Comp[e[3]]=e[1,2];


Comp[e[1,2]]=e[3]; Comp[e[3,1]]=e[2]; Comp[e[2,3]]=e[1];


Comp[e[]]=e[1,2,3]; Comp[e[1,2,3]]=e[];


Comp[x_]:=x e[1,2,3]/;FreeQ[x,e]


Comp[x_ y_]:=x Comp[y]/;FreeQ[x,e]


Comp[x_ y_]:=Comp[x]y/;FreeQ[y,e]


Comp[x_+y_]:=Comp[x]+Comp[y]


(* ::Subsubsection:: *)
(*Reverse*)


Rev[e[]]=e[]; Rev[e[1,2,3]]=-e[1,2,3];


Table[Rev[e[x]]=e[x],{x,1,3}];


Table[Rev[BivectorList[[i]]]=-BivectorList[[i]],{i,1,3}];


Rev[x_]:=x/;FreeQ[Hold[x],e]


Rev[x_ y_]:=x Rev[y]/;FreeQ[Hold[x],e]


Rev[x_ y_]:=Rev[x]y/;FreeQ[Hold[y],e]


Rev[x_+y_]:=Rev[x]+Rev[y]


Antirev[x_]:=Comp[Rev[Comp[x]]]


(* ::Subsubsection:: *)
(*Conformal Conjugate*)


ConformalConjugate[e[]]=e[]; ConformalConjugate[e[1,2,3]]=-e[1,2,3];


ConformalConjugate[e[1]]=e[1]; ConformalConjugate[e[2]]=e[2]; ConformalConjugate[e[3]]=-e[3];


ConformalConjugate[e[1,2]]=e[1,2]; ConformalConjugate[e[2,3]]=-e[2,3]; ConformalConjugate[e[3,1]]=-e[3,1];


ConformalConjugate[x_]:=x/;FreeQ[Hold[x],e]


ConformalConjugate[x_ y_]:=x ConformalConjugate[y]/;FreeQ[Hold[x],e]


ConformalConjugate[x_ y_]:=ConformalConjugate[x]y/;FreeQ[Hold[y],e]


ConformalConjugate[x_+y_]:=ConformalConjugate[x]+ConformalConjugate[y]


(* ::Subsubsection:: *)
(*Metric*)


Metric={{1,0,0},{0,0,-1},{0,-1,0}};


ApplyMetric[e[1]]=e[1]; ApplyMetric[e[2]]=-e[3]; ApplyMetric[e[3]]=-e[2];


ApplyMetric[e[2,3]]=-e[2,3]; ApplyMetric[e[3,1]]=e[1,2]; ApplyMetric[e[1,2]]=e[3,1];


ApplyMetric[e[]]=e[]; ApplyMetric[e[1,2,3]]=-e[1,2,3];


ApplyMetric[x_]:=x e[]/;FreeQ[Hold[x],e]


ApplyMetric[x_ y_]:=x ApplyMetric[y]/;FreeQ[Hold[x],e]


ApplyMetric[x_ y_]:=ApplyMetric[x]y/;FreeQ[Hold[y],e]


ApplyMetric[x_+y_]:=ApplyMetric[x]+ApplyMetric[y]


ApplyAntimetric[x_]:=Comp[ApplyMetric[Comp[x]]]


(* ::Subsubsection:: *)
(*Dual*)


Table[Dual[BasisList[[x]]]=Comp[ApplyMetric[BasisList[[x]]]],{x,1,8}];


Table[Antidual[BasisList[[x]]]=Comp[ApplyAntimetric[BasisList[[x]]]],{x,1,8}];


Dual[x_]:=-x e[1,2,3]/;FreeQ[Hold[x],e]


Dual[x_ y_]:=x Dual[y]/;FreeQ[Hold[x],e]


Dual[x_ y_]:=Dual[x]y/;FreeQ[Hold[y],e]


Dual[x_+y_]:=Dual[x]+Dual[y]


Antidual[x_]:=x e[1,2,3]/;FreeQ[Hold[x],e]


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


Power[e[],1/2]=e[]; Power[e[1,2,3],1/2]=e[1,2,3];


Power[Times[x_,e[]],1/2]:=Sqrt[x]e[]/;FreeQ[x,e]


Power[Times[x_,e[1,2,3]],1/2]:=Sqrt[x]e[1,2,3]/;FreeQ[x,e]


Protect[Power];


CenterNorm[x_,rules_:{}]:=Module[{y},y=x/.rules; Sqrt[Simplify[DotProduct[y,ConformalConjugate[y]],rules/.Rule->Equal]]/.Reverse[rules,{2}]]


RadiusNorm[x_,rules_:{}]:=Module[{y},y=x/.rules; Sqrt[Simplify[AntidotProduct[y,y],rules/.Rule->Equal]]/.Reverse[rules,{2}]]


RoundBulkNorm[x_,rules_:{}]:=Module[{y},y=AntiwedgeProduct[WedgeProduct[x,e[3]],e[1,2]]/.rules; Sqrt[Collect[Simplify[DotProduct[y,y],rules/.Rule->Equal],e[]]/.Reverse[rules,{2}]]]


RoundWeightNorm[x_]:=AntiwedgeProduct[Module[{y},y=WedgeProduct[x,e[3]]; Sqrt[Simplify[AntidotProduct[y,y]]]],e[1,2]]


FlatBulkNorm[x_,rules_:{}]:=WedgeProduct[Module[{y},y=AntiwedgeProduct[x,e[1,2]]/.rules; FullSimplify[Sqrt[Collect[Simplify[DotProduct[y,y],rules/.Rule->Equal],e[]]/.Reverse[rules,{2}]]]],e[3]]


FlatWeightNorm[x_,rules_:{}]:=Module[{y},y=WedgeProduct[AntiwedgeProduct[x,e[1,2]],e[3]]/.rules; Sqrt[Simplify[AntidotProduct[y,y],rules/.Rule->Equal]/.Reverse[rules,{2}]]]


(* ::Subsubsection:: *)
(*Exterior Product*)


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]&&FreeQ[Hold[y],e]&&FreeQ[Hold[x],\[CurlyEpsilon]]&&FreeQ[Hold[y],\[CurlyEpsilon]]


WedgeProduct[x_ \[CurlyEpsilon][],y_]:=x y/;FreeQ[Hold[x],\[CurlyEpsilon]]


WedgeProduct[x_,y_ \[CurlyEpsilon][]]:=x y/;FreeQ[Hold[y],\[CurlyEpsilon]]


WedgeProduct[\[CurlyEpsilon][],x_]:=x/;!FreeQ[Hold[x],\[CurlyEpsilon]]


WedgeProduct[x_,\[CurlyEpsilon][]]:=x/;!FreeQ[Hold[x],\[CurlyEpsilon]]


WedgeProduct[e[],x_]:=x/;!FreeQ[Hold[x],e]


WedgeProduct[x_,e[]]:=x/;!FreeQ[Hold[x],e]


WedgeProduct[e[x_],e[y_]]:=0/;x==y


WedgeProduct[e[x_],e[y_]]:=e[x,y]/;x!=y


WedgeProduct[e[x_,y_],e[z_]]:=0/;x==z||y==z


WedgeProduct[e[x_,y_],e[z_]]:=e[x,y,z]/;x!=z&&y!=z


WedgeProduct[e[x_],e[y_,z_]]:=0/;x==y||x==z


WedgeProduct[e[x_],e[y_,z_]]:=WedgeProduct[e[x,y],e[z]]/;x!=y&&x!=z


WedgeProduct[e[x_,y_],e[z_,w_]]:=0


WedgeProduct[e[s_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_],e[x_,y_,z_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_]]:=0


WedgeProduct[e[s_,t_,p_],e[x_,y_,z_]]:=0


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]&&!FreeQ[Hold[y],e]


WedgeProduct[x_,y_]:=y x/;FreeQ[Hold[y],e]&&!FreeQ[Hold[x],e]


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]&&FreeQ[Hold[y],e]


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


\[CurlyEpsilon][1,1]=\[CurlyEpsilon][]; \[CurlyEpsilon][2,2]=-\[CurlyEpsilon][]; \[CurlyEpsilon][3,3]=\[CurlyEpsilon][];


\[CurlyEpsilon][2,1]=-\[CurlyEpsilon][1,2]; \[CurlyEpsilon][3,2]=-\[CurlyEpsilon][2,3]; \[CurlyEpsilon][1,3]=-\[CurlyEpsilon][3,1];


\[CurlyEpsilon][3,2,1]=-\[CurlyEpsilon][1,2,3]; \[CurlyEpsilon][2,1,3]=-\[CurlyEpsilon][1,2,3]; \[CurlyEpsilon][1,3,2]=-\[CurlyEpsilon][1,2,3]; \[CurlyEpsilon][3,1,2]=\[CurlyEpsilon][1,2,3]; \[CurlyEpsilon][2,3,1]=\[CurlyEpsilon][1,2,3];


\[CurlyEpsilon][x_,y_,z_]:=WedgeProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z]]/;x==y


\[CurlyEpsilon][x_,y_,z_]:=-WedgeProduct[\[CurlyEpsilon][x,z],\[CurlyEpsilon][y]]/;x==z


\[CurlyEpsilon][x_,y_,z_]:=WedgeProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y,z]]/;y==z


DiagonalBasisList={\[CurlyEpsilon][],\[CurlyEpsilon][1],\[CurlyEpsilon][2],\[CurlyEpsilon][3],\[CurlyEpsilon][1,2],\[CurlyEpsilon][2,3],\[CurlyEpsilon][3,1],\[CurlyEpsilon][1,2,3]};


DiagonalBasisMap={\[CurlyEpsilon][],\[CurlyEpsilon][1],(\[CurlyEpsilon][2]-\[CurlyEpsilon][3])/2,\[CurlyEpsilon][2]+\[CurlyEpsilon][3],(\[CurlyEpsilon][1,2]-\[CurlyEpsilon][1,3])/2,\[CurlyEpsilon][2,3],\[CurlyEpsilon][2,1]+\[CurlyEpsilon][3,1],\[CurlyEpsilon][1,2,3]};


GeometricBasisMap={e[],e[1],e[3]/2+e[2],e[3]/2-e[2],e[1,3]/2+e[1,2],e[2,3],e[3,1]/2-e[2,1],e[1,2,3]};


ToDiagonalBasis[x_]:=x/.Table[BasisList[[a]]->DiagonalBasisMap[[a]],{a,1,8}]


ToGeometricBasis[x_]:=x/.Table[DiagonalBasisList[[a]]->GeometricBasisMap[[a]],{a,1,8}]


GeoProduct[\[CurlyEpsilon][1],\[CurlyEpsilon][1,2,3]]=\[CurlyEpsilon][2,3];
GeoProduct[\[CurlyEpsilon][2],\[CurlyEpsilon][1,2,3]]=-\[CurlyEpsilon][3,1];
GeoProduct[\[CurlyEpsilon][3],\[CurlyEpsilon][1,2,3]]=\[CurlyEpsilon][1,2];


GeoProduct[\[CurlyEpsilon][2,3],\[CurlyEpsilon][1,2,3]]=\[CurlyEpsilon][1]; GeoProduct[\[CurlyEpsilon][3,1],\[CurlyEpsilon][1,2,3]]=-\[CurlyEpsilon][2]; GeoProduct[\[CurlyEpsilon][1,2],\[CurlyEpsilon][1,2,3]]=\[CurlyEpsilon][3];


Table[GeoProduct[\[CurlyEpsilon][1,2,3],\[CurlyEpsilon][x]]=GeoProduct[\[CurlyEpsilon][1],\[CurlyEpsilon][2,3,x]],{x,1,3}];


Table[GeoProduct[\[CurlyEpsilon][1,2,3],\[CurlyEpsilon][x,y]]=GeoProduct[\[CurlyEpsilon][1,2],\[CurlyEpsilon][3,x,y]],{x,1,3},{y,1,3}];


GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][]]=\[CurlyEpsilon][]; GeoProduct[\[CurlyEpsilon][1,2,3],\[CurlyEpsilon][1,2,3]]=\[CurlyEpsilon][];


Table[GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x]]=\[CurlyEpsilon][x],{x,1,3}];


Table[GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x,y]]=\[CurlyEpsilon][x,y],{x,1,3},{y,1,3}];


GeoProduct[\[CurlyEpsilon][],\[CurlyEpsilon][x_,y_,z_]]:=\[CurlyEpsilon][x,y,z]


Table[GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][]]=\[CurlyEpsilon][x],{x,1,3}];


Table[GeoProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][]]=\[CurlyEpsilon][x,y],{x,1,3},{y,1,3}];


GeoProduct[\[CurlyEpsilon][x_,y_,z_],\[CurlyEpsilon][]]:=\[CurlyEpsilon][x,y,z]


Table[GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y]]=\[CurlyEpsilon][x,y],{x,1,3},{y,1,3}];


GeoProduct[\[CurlyEpsilon][x_],\[CurlyEpsilon][y_,z_]]:=\[CurlyEpsilon][x,y,z]


GeoProduct[\[CurlyEpsilon][x_,y_],\[CurlyEpsilon][z_]]:=\[CurlyEpsilon][x,y,z]


GeoProduct[\[CurlyEpsilon][x_,y_],\[CurlyEpsilon][s_,t_]]:=GeoProduct[\[CurlyEpsilon][x],\[CurlyEpsilon][y,s,t]]


GeoProduct[\[CurlyEpsilon][x_,y_,z_],\[CurlyEpsilon][s_,t_]]:=GeoProduct[\[CurlyEpsilon][x,y],\[CurlyEpsilon][z,s,t]]


GeoProduct[x_,y_]:=x y/;FreeQ[Hold[x],\[CurlyEpsilon]]; GeoProduct[x_,y_]:=x y/;FreeQ[Hold[x],\[CurlyEpsilon]]; GeoProduct[x_,y_]:=y x/;FreeQ[Hold[y],\[CurlyEpsilon]]


GeoProduct[x_ y_,z_]:=x GeoProduct[y,z]/;FreeQ[Hold[x],\[CurlyEpsilon]]; GeoProduct[x_,y_ z_]:=y GeoProduct[x,z]/;FreeQ[Hold[y],\[CurlyEpsilon]]


GeoProduct[x_ y_,z_ w_]:=x z GeoProduct[y,w]/;FreeQ[Hold[x],\[CurlyEpsilon]]&&FreeQ[Hold[z],\[CurlyEpsilon]]


GeoProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->GeoProduct


GeoProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->GeoProduct


GeometricProduct[x_,y_]:=ToGeometricBasis[GeoProduct[ToDiagonalBasis[x],ToDiagonalBasis[y]]]


GeometricAntiproduct[x_,y_]:=Comp[GeometricProduct[Comp[x],Comp[y]]]


Sandwich[m_,x_]:=GeometricProduct[GeometricProduct[m,x],Rev[m]]


Antisandwich[m_,x_]:=GeometricAntiproduct[GeometricAntiproduct[m,x],Antirev[m]]


(* ::Subsubsection:: *)
(*Properties*)


Carrier[x_]:=WedgeProduct[e[3],x]


Cocarrier[x_]:=WedgeProduct[e[3],Antidual[x]]


FlatCenter[x_]:=BasisCollect[AntiwedgeProduct[Carrier[x],Cocarrier[x]]]


RoundCenter[x_]:=BasisCollect[AntiwedgeProduct[Cocarrier[x],x]]


Container[x_]:=BasisCollect[WedgeProduct[x,Antidual[Carrier[x]]]]


Partner[x_]:=BasisCollect[AntiwedgeProduct[Container[Antidual[x]],Carrier[x]]]


Attitude[x_]:=AntiwedgeProduct[x,e[3,1]]


(* ::Subsubsection:: *)
(*Flat Point*)


FlatPoint[x_,y_:1] := px e[3,1] - py e[2,3]


(* ::Subsubsection:: *)
(*Round Point*)


RoundPoint[px_,py_:1,r2_:0] := px e[1] + py e[2] + (px^2 + py^2 r2)/(2py)e[3]


PointSquaredRadius[a_]:=Module[{x=Coefficient[a,e[1]],y=Coefficient[a,e[2]],z=Coefficient[a,e[3]]},2y z - x^2]


(* ::Subsubsection:: *)
(*Dipole*)


RoundDipole[r2_,cx_,cy_:1] := cx e[2,3] - cy e[1,2] + (r2 - cx^2)/(2cy)e[3,1]


DipoleSquaredRadius[d_]:=Module[{x=Coefficient[d,e[2,3]],y=Coefficient[d,e[3,1]],z=Coefficient[d,e[1,2]]},x^2 - 2y z]


DipoleFlatCenter[d_]:=Module[{x=Coefficient[d,e[2,3]],z=Coefficient[d,e[1,2]]},-x e[3,1] + z e[2,3]]


DipoleSurface[d_]:=Module[{cen=RoundCenter[d],dp=Attitude[d]Sqrt[Abs[DipoleSquaredRadius[d]]]},{cen-dp,cen+dp}]


(* ::Subsubsection:: *)
(*Exomorphism*)


BivectorTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]],
	p1,p2,p3},
	
	p1=Simplify[WedgeProduct[c1,c2]];
	p2=Simplify[WedgeProduct[c2,c3]];
	p3=Simplify[WedgeProduct[c3,c1]];
	
	Transpose[{
	Table[Coefficient[p1,BivectorList[[k]]],{k,1,3}],
	Table[Coefficient[p2,BivectorList[[k]]],{k,1,3}],
	Table[Coefficient[p3,BivectorList[[k]]],{k,1,3}]}]]


AntiscalarTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]},
	{{Coefficient[Simplify[WedgeProduct[WedgeProduct[c1,c2],c3]],e[1,2,3]]}}]


Exomorphism[m_]:=SparseArray[Band[{1,1}]->{{{1}},m,BivectorTransform[m],AntiscalarTransform[m]}]


OperatorMatrix[op_,constraints_:{}]:=Module[{x,y,z,v,xp,yp,zp},v=Simplify[Antisandwich[op,x e[1]+y e[2]+z e[3]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];
	{{Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z]},
	{Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z]},
	{Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z]}}]


CompOperatorMatrix[op_,constraints_:{}]:=Module[{x,y,z,v,xp,yp,zp},v=Simplify[Sandwich[op,x e[1]+y e[2]+z e[3]],constraints];
	xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];
	{{Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z]},
	{Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z]},
	{Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z]}}]


(* ::Subsubsection:: *)
(*Drawing*)


LineColor=RGBColor["#4080FF"];


RoundPointColor[r2_]:=If[r2>=0,RGBColor["#80FF80"],RGBColor["#D980FF"]];


DipoleColor[r2_]:=If[r2>=0,RGBColor["#408CC0"],RGBColor["#C04040"]];


RoundPointCoords[a_]:=Module[{x=Coefficient[a,e[1]],y=Coefficient[a,e[2]]},{x/y,0}]


FlatPointCoords[p_]:=Module[{x=Coefficient[p,e[3,1]],y=Coefficient[p,e[2,3]]},{-x/y,0}]


DrawFlatPoint[p_]:=Graphics[{AbsolutePointSize[8],Point[FlatPointCoords[p]]}]


DrawRoundPoint[a_]:=Module[{r2=PointSquaredRadius[a],cen=RoundPointCoords[a]},
	Graphics[{RoundPointColor[r2],Opacity[0.5],Disk[cen,Sqrt[Abs[r2]]/Abs[Coefficient[a,e[2]]]],RGBColor[0,0,0],AbsolutePointSize[5],Point[cen]}]]


DrawDipole[d_]:=Module[{r2=DipoleSquaredRadius[d],cen=FlatPointCoords[DipoleFlatCenter[d]],rad},
	rad=Sqrt[Abs[r2]]/Coefficient[RoundWeightNorm[d],e[1,2]];
	Graphics[{AbsolutePointSize[8],RGBColor[0,0,0],Point[cen+{rad,0}],Point[cen-{rad,0}],DipoleColor[r2],AbsoluteThickness[2],AbsoluteDashing[4],Line[{cen+{rad,0},cen-{rad,0}}]}]]


(* ::Subsubsection:: *)
(*Formatting*)


Format[e[],TraditionalForm]:=DisplayForm[Style["1",Bold,FontFamily->"Times New Roman",FontSize->16]]


Format[e[x_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[1,2,3],TraditionalForm]:=DisplayForm[StyleBox[FromCharacterCode[16^^1D7D9],FontFamily->"Segoe UI Symbol",FontSize->16]]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2022],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2022]->"~DotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2218],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2218]->"~AntidotProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2227],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2227]->"~WedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^2228],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^2228]->"~AntiwedgeProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27D1],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27D1]->"~GeometricProduct~"],StandardForm]


MakeExpression[RowBox[x:{_,PatternSequence[FromCharacterCode[16^^27C7],_]..}],StandardForm]:=MakeExpression[RowBox[x/.FromCharacterCode[16^^27C7]->"~GeometricAntiproduct~"],StandardForm]


DividerList={Thick,Thick,Thick,True,True,Thick,True,True,Thick,Thick};


(* ::Subsection:: *)
(*Epilog*)


End[];


EndPackage[];
