(* ::Package:: *)

(* ::Title:: *)
(*Projective Spacetime Geometric Algebra*)


(* ::Author:: *)
(*By Eric Lengyel, Copyright 2019-2025*)


(* ::Subsection:: *)
(*Prolog*)


BeginPackage["SpacetimeAlgebra`"];


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


QuadrivectorPart::usage = "TrivectorPart[x] returns the trivector (grade 4) components of x";


BulkPart::usage = "BulkPart[x] returns the bulk components of x";


WeightPart::usage = "WeightPart[x] returns the weight components of x";


GradeSelect::usage = "GradeSelect[x,k] returns the components of x having grade k";


AntigradeSelect::usage = "AntigradeSelect[x,k] returns the components of x having antigrade k";


StripBasis::usage = "StripBasis[x] converts a multivector x on the basis elements to a 32-entry list";


Multivector::usage = "Multivector[x] converts a 32-entry list x to a multivector on the basis elements";


Comp::usage = "Comp[x] returns the complement of x";


Rev::usage = "Rev[x] returns the reverse of x";


Metric::usage = "Metric is the 5x5 metric tensor";


ApplyMetric::usage = "ApplyMetric[x] applies the metric exomorphism to the multivector x";


ApplyAntimetric::usage = "ApplyAntimetric[x] applies the metric antiexomorphism to the multivector x";


BulkDual::usage = "BulkDual[x] returns the bulk dual of x";


WeightDual::usage = "WeightDual[x] returns the weight dual of x";


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


TranswedgeProduct::usage = "TranswedgeProduct[x,y,k] returns the transwedge product of order k of x and y";


TranswedgeAntiproduct::usage = "TranswedgeAntiproduct[x,y,k] returns the transwedge antiproduct of order k of x and y";


GeometricProduct::usage = "GeometricProduct[x,y] returns the geometric product of x and y";


GeometricAntiproduct::usage = "GeometricAntiproduct[x,y] returns the geometric antiproduct of x and y";


Sandwich::usage = "Sandwich[m,x] evaluates the geometric product of m, x, and the reverse of m";


Antisandwich::usage = "Antisandwich[m,x] evaluates the geometric antiproduct of m, x, and the antireverse of m";


Attitude::usage = "Attitude[x] returns the attitude of x";


BivectorTransform::usage = "BivectorTransform[m] returns the second compound matrix of a 5x5 matrix m that transforms vectors, which is a 10x10 matrix that transforms bivectors";


TrivectorTransform::usage = "TrivectorTransform[m] returns the third compound matrix of a 5x5 matrix m that transforms vectors, which is a 10x10 matrix that transforms trivectors";


QuadrivectorTransform::usage = "QuadrivectorTransform[m] returns the fourth compound matrix of a 5x5 matrix m that transforms vectors, which is a 5x5 matrix that transforms quadrivectors";


AntiscalarTransform::usage = "AntiscalarTransform[m] returns the fifth compound matrix of a 5x5 matrix m that transforms vectors, which is a 1x1 matrix that transforms antiscalars";


Exomorphism::usage = "Exomorphism[m] returns the exomorphism matrix corresponding to the 5x5 matrix m that transforms vectors, which is a 32x32 block diagonal matrix that transforms multivectors";


OperatorMatrix::usage = "OperatorMatrix[op,constraints] returns the 5x5 vector transformation matrix corresponding to the geometric algebra operator op with optional geometric constraints";


CompOperatorMatrix::usage = "CompOperatorMatrix[op,constraints] returns the 5x5 vector transformation matrix corresponding to the geometric algebra complement operator op with optional geometric constraints";


LineConstraints::usage = "LineConstraints[l] returns the geometric constraints for the line l";


DividerList::usage = "DividerList is a list of table divider thicknesses for a header plus 16 entries";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Basis Elements*)


OtherPermutations[x_]:=DeleteCases[Permutations[x],x]


e[0,0]=-e[]; e[1,1]=e[]; e[2,2]=e[]; e[3,3]=e[]; e[4,4]=0;


e[1,4]=-e[4,1]; e[2,4]=-e[4,2]; e[3,4]=-e[4,3]; e[3,2]=-e[2,3]; e[1,3]=-e[3,1]; e[2,1]=-e[1,2];


e[0,1]=-e[1,0]; e[0,2]=-e[2,0]; e[0,3]=-e[3,0]; e[0,4]=-e[4,0];


Table[e[x[[1]],x[[2]],x[[3]]]=-e[3,2,1]Signature[x],{x,OtherPermutations[{3,2,1}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=e[4,2,3]Signature[x],{x,OtherPermutations[{4,2,3}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=-e[4,3,1]Signature[x],{x,OtherPermutations[{4,3,1}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=e[4,1,2]Signature[x],{x,OtherPermutations[{4,1,2}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=e[2,3,0]Signature[x],{x,OtherPermutations[{2,3,0}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=-e[3,1,0]Signature[x],{x,OtherPermutations[{3,1,0}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=e[1,2,0]Signature[x],{x,OtherPermutations[{1,2,0}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=-e[4,1,0]Signature[x],{x,OtherPermutations[{4,1,0}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=-e[4,2,0]Signature[x],{x,OtherPermutations[{4,2,0}]}];


Table[e[x[[1]],x[[2]],x[[3]]]=-e[4,3,0]Signature[x],{x,OtherPermutations[{4,3,0}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=e[3,2,1,0]Signature[x],{x,OtherPermutations[{3,2,1,0}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=e[1,2,3,4]Signature[x],{x,OtherPermutations[{1,2,3,4}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=-e[4,2,3,0]Signature[x],{x,OtherPermutations[{4,2,3,0}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=e[4,3,1,0]Signature[x],{x,OtherPermutations[{4,3,1,0}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]]]=-e[4,1,2,0]Signature[x],{x,OtherPermutations[{4,1,2,0}]}];


Table[e[x[[1]],x[[2]],x[[3]],x[[4]],x[[5]]]=e[0,1,2,3,4]Signature[x],{x,OtherPermutations[{0,1,2,3,4}]}];


e[x_,y_,z_]:=WedgeProduct[e[x,y],e[z]]/;x==y


e[x_,y_,z_]:=-WedgeProduct[e[x,z],e[y]]/;x==z


e[x_,y_,z_]:=WedgeProduct[e[x],e[y,z]]/;y==z


e[x_,y_,z_,w_]:=WedgeProduct[e[x,w],e[y,z]]/;x==w||y==z


e[x_,y_,z_,w_]:=-WedgeProduct[e[x,z],e[y,w]]/;x==z||y==w


e[x_,y_,z_,w_]:=WedgeProduct[e[x,y],e[z,w]]/;x==y||z==w


e[x_,y_,z_,w_,v_]:=WedgeProduct[e[x,y],e[z,w,v]]/;x==y


e[x_,y_,z_,w_,v_]:=-WedgeProduct[e[x,z],e[y,w,v]]/;x==z


e[x_,y_,z_,w_,v_]:=WedgeProduct[e[x,w],e[y,z,v]]/;x==w


e[x_,y_,z_,w_,v_]:=-WedgeProduct[e[x,v],e[y,z,w]]/;x==v


e[x_,y_,z_,w_,v_]:=WedgeProduct[e[y,z],e[x,w,v]]/;y==z


e[x_,y_,z_,w_,v_]:=-WedgeProduct[e[y,w],e[x,z,v]]/;y==w


e[x_,y_,z_,w_,v_]:=WedgeProduct[e[y,v],e[x,z,w]]/;y==v


e[x_,y_,z_,w_,v_]:=WedgeProduct[e[z,w],e[x,y,v]]/;z==w


e[x_,y_,z_,w_,v_]:=-WedgeProduct[e[z,v],e[x,y,w]]/;z==v


e[x_,y_,z_,w_,v_]:=WedgeProduct[e[x,y,z],e[w,v]]/;w==v


VectorList={e[0],e[1],e[2],e[3],e[4]};


BivectorList={e[4,0],e[4,1],e[4,2],e[4,3],e[2,3],e[3,1],e[1,2],e[1,0],e[2,0],e[3,0]};


TrivectorList={e[3,2,1],e[2,3,0],e[3,1,0],e[1,2,0],e[4,1,0],e[4,2,0],e[4,3,0],e[4,2,3],e[4,3,1],e[4,1,2]};


QuadrivectorList={e[1,2,3,4],e[4,2,3,0],e[4,3,1,0],e[4,1,2,0],e[3,2,1,0]};


BasisList={e[]}~Join~VectorList~Join~BivectorList~Join~TrivectorList~Join~QuadrivectorList~Join~{e[0,1,2,3,4]};


BasisCollect[x_]:=Collect[x,BasisList]


BasisForm[x_]:=TraditionalForm[BasisCollect[x]]


BasisGrade={0,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,5};


BasisAntigrade={5,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,0};


ScalarPart[x_]:=Coefficient[x,e[]]e[]


AntiscalarPart[x_]:=Coefficient[x,e[1,2,3,4,5]]e[1,2,3,4,5]


VectorPart[x_]:=Coefficient[x,e[0]]e[0]+Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]+Coefficient[x,e[4]]e[4]


BivectorPart[x_]:=Coefficient[x,e[4,0]]e[4,0]+Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,3]]e[4,3]+
	Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[1,2]]e[1,2]+
	Coefficient[x,e[1,0]]e[1,0]+Coefficient[x,e[2,0]]e[2,0]+Coefficient[x,e[3,0]]e[3,0]


TrivectorPart[x_]:=Coefficient[x,e[3,2,1]]e[3,2,1]+Coefficient[x,e[2,3,0]]e[2,3,0]+Coefficient[x,e[3,1,0]]e[3,1,0]+Coefficient[x,e[1,2,0]]e[1,2,0]+
	Coefficient[x,e[4,1,0]]e[4,1,0]+Coefficient[x,e[4,2,0]]e[4,2,0]+Coefficient[x,e[4,3,0]]e[4,3,0]+
	Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[4,1,2]]e[4,1,2]


QuadrivectorPart[x_]:=Coefficient[x,e[1,2,3,4]]e[1,2,3,4]+Coefficient[x,e[4,2,3,0]]e[4,2,3,0]+Coefficient[x,e[4,3,1,0]]e[4,3,1,0]+Coefficient[x,e[4,1,2,0]]e[4,1,2,0]+Coefficient[x,e[3,2,1,0]]e[3,2,1,0]


BulkPart[x_]:=Coefficient[x,e[]]e[]+Coefficient[x,e[0]]e[0]+Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]+
	Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[1,0]]e[1,0]+Coefficient[x,e[2,0]]e[2,0]+Coefficient[x,e[3,0]]e[3,0]+
	Coefficient[x,e[3,2,1]]e[3,2,1]+Coefficient[x,e[2,3,0]]e[2,3,0]+Coefficient[x,e[3,1,0]]e[3,1,0]+Coefficient[x,e[1,2,0]]e[1,2,0]+
	Coefficient[x,e[3,2,1,0]]e[3,2,1,0]


WeightPart[x_]:=Coefficient[x,e[4]]e[4]+Coefficient[x,e[4,0]]e[4,0]+Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,3]]e[4,3]+
	Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[4,1,0]]e[4,1,0]+Coefficient[x,e[4,2,0]]e[4,2,0]+Coefficient[x,e[4,3,0]]e[4,3,0]+
	Coefficient[x,e[1,2,3,4]]e[1,2,3,4]+Coefficient[x,e[4,2,3,0]]e[4,2,3,0]+Coefficient[x,e[4,3,1,0]]e[4,3,1,0]+Coefficient[x,e[4,1,2,0]]e[4,1,2,0]+
	Coefficient[x,e[0,1,2,3,4]]e[0,1,2,3,4]


GradeSelect[x_,k_]:=Switch[k,0,ScalarPart[x],1,VectorPart[x],2,BivectorPart[x],3,TrivectorPart[x],4,QuadrivectorPart[x],5,AntiscalarPart[x],_,0]


AntigradeSelect[x_,k_]:=GradeSelect[x,5-k]


StripBasis[x_]:=Table[Coefficient[x,BasisList[[i]]],{i,1,32}]


Multivector[x_]:=Sum[x[[i]]BasisList[[i]],{i,1,32}]


(* ::Subsubsection:: *)
(*Complement*)


Comp[e[0]]=e[1,2,3,4]; Comp[e[1]]=e[4,2,3,0]; Comp[e[2]]=e[4,3,1,0]; Comp[e[3]]=e[4,1,2,0]; Comp[e[4]]=e[3,2,1,0];


Comp[e[4,1]]=-e[2,3,0]; Comp[e[4,2]]=-e[3,1,0]; Comp[e[4,3]]=-e[1,2,0];


Comp[e[2,3]]=-e[4,1,0]; Comp[e[3,1]]=-e[4,2,0]; Comp[e[1,2]]=-e[4,3,0];


Comp[e[1,0]]=-e[4,2,3]; Comp[e[2,0]]=-e[4,3,1]; Comp[e[3,0]]=-e[4,1,2]; Comp[e[4,0]]=-e[3,2,1];


Comp[e[4,2,3]]=-e[1,0]; Comp[e[4,3,1]]=-e[2,0]; Comp[e[4,1,2]]=-e[3,0]; Comp[e[3,2,1]]=-e[4,0];


Comp[e[4,1,0]]=-e[2,3]; Comp[e[4,2,0]]=-e[3,1]; Comp[e[4,3,0]]=-e[1,2];


Comp[e[2,3,0]]=-e[4,1]; Comp[e[3,1,0]]=-e[4,2]; Comp[e[1,2,0]]=-e[4,3];


Comp[e[1,2,3,4]]=e[0]; Comp[e[4,2,3,0]]=e[1]; Comp[e[4,3,1,0]]=e[2]; Comp[e[4,1,2,0]]=e[3]; Comp[e[3,2,1,0]]=e[4];


Comp[e[]]=e[0,1,2,3,4]; Comp[e[0,1,2,3,4]]=e[];


Comp[x_]:=x e[0,1,2,3,4]/;FreeQ[x,e]


Comp[x_ y_]:=x Comp[y]/;FreeQ[x,e]


Comp[x_ y_]:=Comp[x]y/;FreeQ[y,e]


Comp[x_+y_]:=Comp[x]+Comp[y]


(* ::Subsubsection:: *)
(*Reverse*)


Rev[e[]]=e[]; Rev[e[0,1,2,3,4]]=e[0,1,2,3,4];


Rev[x_]:=x/;FreeQ[x,e]


Rev[e[x_]]:=e[x]


Rev[e[x_,y_]]:=-e[x,y]


Rev[e[x_,y_,z_]]:=-e[x,y,z]


Rev[e[x_,y_,z_,w_]]:=e[x,y,z,w]


Rev[x_ y_]:=x Rev[y]/;FreeQ[x,e]


Rev[x_ y_]:=Rev[x]y/;FreeQ[y,e]


Rev[x_+y_]:=Rev[x]+Rev[y]


(* ::Subsubsection:: *)
(*Metric*)


Metric={{-1,0,0,0,0},{0,1,0,0,0},{0,0,1,0,0},{0,0,0,1,0},{0,0,0,0,0}};


ApplyMetric[e[0]]=-e[0]; ApplyMetric[e[1]]=e[1]; ApplyMetric[e[2]]=e[2]; ApplyMetric[e[3]]=e[3]; ApplyMetric[e[4]]=0;


ApplyMetric[e[4,1]]=0; ApplyMetric[e[4,2]]=0; ApplyMetric[e[4,3]]=0;


ApplyMetric[e[2,3]]=e[2,3]; ApplyMetric[e[3,1]]=e[3,1]; ApplyMetric[e[1,2]]=e[1,2];


ApplyMetric[e[1,0]]=-e[1,0]; ApplyMetric[e[2,0]]=-e[2,0]; ApplyMetric[e[3,0]]=-e[3,0]; ApplyMetric[e[4,0]]=0;


ApplyMetric[e[4,2,3]]=0; ApplyMetric[e[4,3,1]]=0; ApplyMetric[e[4,1,2]]=0; ApplyMetric[e[3,2,1]]=e[3,2,1];


ApplyMetric[e[4,1,0]]=0; ApplyMetric[e[4,2,0]]=0; ApplyMetric[e[4,3,0]]=0;


ApplyMetric[e[2,3,0]]=-e[2,3,0]; ApplyMetric[e[3,1,0]]=-e[3,1,0]; ApplyMetric[e[1,2,0]]=-e[1,2,0];


ApplyMetric[e[1,2,3,4]]=0; ApplyMetric[e[4,2,3,0]]=0; ApplyMetric[e[4,3,1,0]]=0; ApplyMetric[e[4,1,2,0]]=0; ApplyMetric[e[3,2,1,0]]=-e[3,2,1,0];


ApplyMetric[e[]]=e[]; ApplyMetric[e[0,1,2,3,4]]=0;


ApplyMetric[x_]:=x e[]/;FreeQ[x,e]


ApplyMetric[x_ y_]:=x ApplyMetric[y]/;FreeQ[x,e]


ApplyMetric[x_ y_]:=ApplyMetric[x]y/;FreeQ[y,e]


ApplyMetric[x_+y_]:=ApplyMetric[x]+ApplyMetric[y]


ApplyAntimetric[x_]:=Comp[ApplyMetric[Comp[x]]]


(* ::Subsubsection:: *)
(*Dual*)


Table[BulkDual[BasisList[[x]]]=Comp[ApplyMetric[BasisList[[x]]]],{x,1,32}];


Table[WeightDual[BasisList[[x]]]=Comp[ApplyAntimetric[BasisList[[x]]]],{x,1,32}];


BulkDual[x_]:=x e[0,1,2,3,4]/;FreeQ[x,e]


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


AntidotProduct[x_,y_]:=Comp[DotProduct[Comp[x],Comp[y]]]


(* ::Subsubsection:: *)
(*Norm*)


Unprotect[Power];


Power[e[],1/2]=e[]; Power[e[0,1,2,3,4],1/2]=e[0,1,2,3,4];


Power[Times[x_,e[]],1/2]:=Sqrt[x]e[]/;FreeQ[x,e]


Power[Times[x_,e[0,1,2,3,4]],1/2]:=Sqrt[x]e[0,1,2,3,4]/;FreeQ[x,e]


Protect[Power];


BulkNorm[x_]:=Sqrt[DotProduct[x,x]]


WeightNorm[x_]:=Sqrt[AntidotProduct[x,x]]


GeometricNorm[x_]:=BulkNorm[x]+WeightNorm[x]


(* ::Subsubsection:: *)
(*Exterior Product*)


WedgeProduct[e[],e[]]=e[];


WedgeProduct[e[],e[x_]]:=e[x]


WedgeProduct[e[],e[x_,y_]]:=e[x,y]


WedgeProduct[e[],e[x_,y_,z_]]:=e[x,y,z]


WedgeProduct[e[],e[x_,y_,z_,w_]]:=e[x,y,z,w]


WedgeProduct[e[],e[x_,y_,z_,w_,v_]]:=e[x,y,z,w,v]


WedgeProduct[e[x_],e[]]:=e[x]


WedgeProduct[e[x_,y_],e[]]:=e[x,y]


WedgeProduct[e[x_,y_,z_],e[]]:=e[x,y,z]


WedgeProduct[e[x_,y_,z_,w_],e[]]:=e[x,y,z,w]


WedgeProduct[e[x_,y_,z_,w_,v_],e[]]:=e[x,y,z,w,v]


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


WedgeProduct[x_,y_]:=x y/;FreeQ[Hold[x],e]


WedgeProduct[x_,y_]:=y x/;FreeQ[Hold[y],e]&&!FreeQ[Hold[x],e]


WedgeProduct[x_ y_,z_]:=x WedgeProduct[y,z]/;FreeQ[Hold[x],e]


WedgeProduct[x_,y_ z_]:=y WedgeProduct[x,z]/;FreeQ[Hold[y],e]


WedgeProduct[x_ y_,z_ w_]:=x z WedgeProduct[y,w]/;FreeQ[Hold[x],e]&&FreeQ[Hold[z],e]


WedgeProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->WedgeProduct


WedgeProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->WedgeProduct


AntiwedgeProduct[x_,y_]:=Comp[WedgeProduct[Comp[x],Comp[y]]]


(* ::Subsubsection:: *)
(*Interior Products*)


BulkContraction[x_,y_]:=AntiwedgeProduct[x,BulkDual[y]]


WeightContraction[x_,y_]:=AntiwedgeProduct[x,WeightDual[y]]


BulkExpansion[x_,y_]:=WedgeProduct[x,BulkDual[y]]


WeightExpansion[x_,y_]:=WedgeProduct[x,WeightDual[y]]


(* ::Subsubsection:: *)
(*Transwedge Products*)


TranswedgeProduct[x_,y_,0]:=WedgeProduct[x,y]


TranswedgeProduct[x_,y_,1]:=Sum[Module[{c},c=VectorList[[i]];WedgeProduct[AntiwedgeProduct[Comp[c],x],AntiwedgeProduct[y,BulkDual[c]]]],{i,1,5}]


TranswedgeProduct[x_,y_,2]:=Sum[Module[{c},c=BivectorList[[i]];WedgeProduct[AntiwedgeProduct[Comp[c],x],AntiwedgeProduct[y,BulkDual[c]]]],{i,1,10}]


TranswedgeProduct[x_,y_,3]:=Sum[Module[{c},c=TrivectorList[[i]];WedgeProduct[AntiwedgeProduct[Comp[c],x],AntiwedgeProduct[y,BulkDual[c]]]],{i,1,10}]


TranswedgeProduct[x_,y_,4]:=Sum[Module[{c},c=QuadrivectorList[[i]];WedgeProduct[AntiwedgeProduct[Comp[c],x],AntiwedgeProduct[y,BulkDual[c]]]],{i,1,5}]


TranswedgeProduct[x_,y_,5]:=0


TranswedgeAntiproduct[x_,y_,k_]:=Comp[TranswedgeProduct[Comp[x],Comp[y],k]]


(* ::Subsubsection:: *)
(*Geometric Product*)


GeometricProduct[e[],e[]]=e[];
GeometricProduct[e[0,1,2,3,4],e[0,1,2,3,4]]=0;


GeometricProduct[e[0],e[0,1,2,3,4]]=-e[1,2,3,4];
GeometricProduct[e[1],e[0,1,2,3,4]]=e[4,2,3,0];
GeometricProduct[e[2],e[0,1,2,3,4]]=-e[4,1,3,0];
GeometricProduct[e[3],e[0,1,2,3,4]]=e[4,1,2,0];
GeometricProduct[e[4],e[0,1,2,3,4]]=0;


GeometricProduct[e[4,1],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,2],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,3],e[0,1,2,3,4]]=0;
GeometricProduct[e[2,3],e[0,1,2,3,4]]=e[4,1,0];
GeometricProduct[e[3,1],e[0,1,2,3,4]]=e[4,2,0];
GeometricProduct[e[1,2],e[0,1,2,3,4]]=e[4,3,0];
GeometricProduct[e[1,0],e[0,1,2,3,4]]=-e[4,2,3];
GeometricProduct[e[2,0],e[0,1,2,3,4]]=-e[4,3,1];
GeometricProduct[e[3,0],e[0,1,2,3,4]]=-e[4,1,2];
GeometricProduct[e[4,0],e[0,1,2,3,4]]=0;


GeometricProduct[e[4,2,3],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,3,1],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,1,2],e[0,1,2,3,4]]=0;
GeometricProduct[e[3,2,1],e[0,1,2,3,4]]=e[4,0];
GeometricProduct[e[4,1,0],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,2,0],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,3,0],e[0,1,2,3,4]]=0;
GeometricProduct[e[2,3,0],e[0,1,2,3,4]]=-e[4,1];
GeometricProduct[e[3,1,0],e[0,1,2,3,4]]=-e[4,2];
GeometricProduct[e[1,2,0],e[0,1,2,3,4]]=-e[4,3];


GeometricProduct[e[1,2,3,4],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,2,3,0],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,3,1,0],e[0,1,2,3,4]]=0;
GeometricProduct[e[4,1,2,0],e[0,1,2,3,4]]=0;
GeometricProduct[e[3,2,1,0],e[0,1,2,3,4]]=-e[4];


Table[GeometricProduct[e[0,1,2,3,4],e[x]]=GeometricProduct[e[0],e[1,2,3,4,x]],{x,0,4}];


Table[GeometricProduct[e[0,1,2,3,4],e[x,y]]=GeometricProduct[e[0,1],e[2,3,4,x,y]],{x,0,4},{y,0,4}];


GeometricProduct[e[0,1,2,3,4],e[x_,y_,z_]]:=GeometricProduct[e[0,1,2],e[3,4,x,y,z]]


GeometricProduct[e[0,1,2,3,4],e[x_,y_,z_,w_]]:=GeometricProduct[e[0,1,2,3],e[4,x,y,z,w]]


Table[GeometricProduct[e[],e[x]]=e[x],{x,0,4}];


Table[GeometricProduct[e[],e[x,y]]=e[x,y],{x,0,4},{y,0,4}];


GeometricProduct[e[],e[x_,y_,z_]]:=e[x,y,z]


GeometricProduct[e[],e[x_,y_,z_,w_]]:=e[x,y,z,w]


GeometricProduct[e[],e[x_,y_,z_,w_,u_]]:=e[x,y,z,w,u]


Table[GeometricProduct[e[x],e[]]=e[x],{x,0,4}];


Table[GeometricProduct[e[x,y],e[]]=e[x,y],{x,0,4},{y,0,4}];


GeometricProduct[e[x_,y_,z_],e[]]:=e[x,y,z]


GeometricProduct[e[x_,y_,z_,w_],e[]]:=e[x,y,z,w]


GeometricProduct[e[x_,y_,z_,w_,u_],e[]]:=e[x,y,z,w,u]


Table[GeometricProduct[e[x],e[y]]=e[x,y],{x,0,4},{y,0,4}];


GeometricProduct[e[x_],e[y_,z_]]:=e[x,y,z];
GeometricProduct[e[x_,y_],e[z_]]:=e[x,y,z]


GeometricProduct[e[x_,y_],e[z_,w_]]:=e[x,y,z,w];
GeometricProduct[e[x_],e[y_,z_,w_]]:=e[x,y,z,w];
GeometricProduct[e[x_,y_,z_],e[w_]]:=e[x,y,z,w]


GeometricProduct[e[x_],e[s_,t_,p_,q_]]:=e[x,s,t,p,q];
GeometricProduct[e[x_,y_,z_,w_],e[s_]]:=e[x,y,z,w,s]


GeometricProduct[e[x_,y_],e[s_,t_,p_]]:=GeometricProduct[e[x],e[y,s,t,p]];
GeometricProduct[e[x_,y_,z_],e[s_,t_]]:=GeometricProduct[e[x],e[y,z,s,t]]


GeometricProduct[e[x_,y_,z_],e[s_,t_,p_]]:=GeometricProduct[e[x,y],e[z,s,t,p]]


GeometricProduct[e[x_,y_],e[s_,t_,p_,q_]]:=GeometricProduct[e[x],e[y,s,t,p,q]];
GeometricProduct[e[x_,y_,z_,w_],e[s_,t_]]:=GeometricProduct[e[x],e[y,z,w,s,t]]


GeometricProduct[e[x_,y_,z_],e[s_,t_,p_,q_]]:=GeometricProduct[e[x,y],e[z,s,t,p,q]];
GeometricProduct[e[x_,y_,z_,w_],e[s_,t_,p_]]:=GeometricProduct[e[x,y],e[z,w,s,t,p]]


GeometricProduct[e[x_,y_,z_,w_],e[s_,t_,p_,q_]]:=GeometricProduct[e[x,y,z],e[w,s,t,p,q]]


GeometricProduct[x_,y_]:=x y/;FreeQ[x,e];GeometricProduct[x_,y_]:=y x/;FreeQ[y,e]


GeometricProduct[x_ y_,z_]:=x GeometricProduct[y,z]/;FreeQ[x,e];GeometricProduct[x_,y_ z_]:=y GeometricProduct[x,z]/;FreeQ[y,e]


GeometricProduct[x_ y_,z_ w_]:=x z GeometricProduct[y,w]/;FreeQ[x,e]&&FreeQ[z,e]


GeometricProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->GeometricProduct


GeometricProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->GeometricProduct


GeometricAntiproduct[x_,y_]:=Comp[GeometricProduct[Comp[x],Comp[y]]]


Sandwich[m_,x_]:=GeometricProduct[GeometricProduct[m,x],Rev[m]]


Antisandwich[m_,x_]:=GeometricAntiproduct[GeometricAntiproduct[m,x],Rev[m]]


(* ::Subsubsection:: *)
(*Attitude*)


Att[x_]:=AntiwedgeProduct[x,e[3,2,1,0]]


(* ::Subsubsection:: *)
(*Exomorphism*)


BivectorTransform[m_]:=Module[{
	c0=m[[1]][[1]]e[0]+m[[2]][[1]]e[1]+m[[3]][[1]]e[2]+m[[4]][[1]]e[3]+m[[5]][[1]]e[4],
	c1=m[[1]][[2]]e[0]+m[[2]][[2]]e[1]+m[[3]][[2]]e[2]+m[[4]][[2]]e[3]+m[[5]][[2]]e[4],
	c2=m[[1]][[3]]e[0]+m[[2]][[3]]e[1]+m[[3]][[3]]e[2]+m[[4]][[3]]e[3]+m[[5]][[3]]e[4],
	c3=m[[1]][[4]]e[0]+m[[2]][[4]]e[1]+m[[3]][[4]]e[2]+m[[4]][[4]]e[3]+m[[5]][[4]]e[4],
	c4=m[[1]][[5]]e[0]+m[[2]][[5]]e[1]+m[[3]][[5]]e[2]+m[[4]][[5]]e[3]+m[[5]][[5]]e[4],
	p1,p2,p3,p4,p5,p6,p7,p8,p9,p10},
	
	p1=Simplify[WedgeProduct[c4,c0]];
	p2=Simplify[WedgeProduct[c4,c1]];
	p3=Simplify[WedgeProduct[c4,c2]];
	p4=Simplify[WedgeProduct[c4,c3]];
	p5=Simplify[WedgeProduct[c2,c3]];
	p6=Simplify[WedgeProduct[c3,c1]];
	p7=Simplify[WedgeProduct[c1,c2]];
	p8=Simplify[WedgeProduct[c1,c0]];
	p9=Simplify[WedgeProduct[c2,c0]];
	p10=Simplify[WedgeProduct[c3,c0]];
	
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
	c0=m[[1]][[1]]e[0]+m[[2]][[1]]e[1]+m[[3]][[1]]e[2]+m[[4]][[1]]e[3]+m[[5]][[1]]e[4],
	c1=m[[1]][[2]]e[0]+m[[2]][[2]]e[1]+m[[3]][[2]]e[2]+m[[4]][[2]]e[3]+m[[5]][[2]]e[4],
	c2=m[[1]][[3]]e[0]+m[[2]][[3]]e[1]+m[[3]][[3]]e[2]+m[[4]][[3]]e[3]+m[[5]][[3]]e[4],
	c3=m[[1]][[4]]e[0]+m[[2]][[4]]e[1]+m[[3]][[4]]e[2]+m[[4]][[4]]e[3]+m[[5]][[4]]e[4],
	c4=m[[1]][[5]]e[0]+m[[2]][[5]]e[1]+m[[3]][[5]]e[2]+m[[4]][[5]]e[3]+m[[5]][[5]]e[4],
	p1,p2,p3,p4,p5,p6,p7,p8,p9,p10},
	
	p1=Simplify[WedgeProduct[WedgeProduct[c3,c2],c1]];
	p2=Simplify[WedgeProduct[WedgeProduct[c2,c3],c0]];
	p3=Simplify[WedgeProduct[WedgeProduct[c3,c1],c0]];
	p4=Simplify[WedgeProduct[WedgeProduct[c1,c2],c0]];
	p5=Simplify[WedgeProduct[WedgeProduct[c4,c1],c0]];
	p6=Simplify[WedgeProduct[WedgeProduct[c4,c2],c0]];
	p7=Simplify[WedgeProduct[WedgeProduct[c4,c3],c0]];
	p8=Simplify[WedgeProduct[WedgeProduct[c4,c2],c3]];
	p9=Simplify[WedgeProduct[WedgeProduct[c4,c3],c1]];
	p10=Simplify[WedgeProduct[WedgeProduct[c4,c1],c2]];
	
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
	c0=m[[1]][[1]]e[0]+m[[2]][[1]]e[1]+m[[3]][[1]]e[2]+m[[4]][[1]]e[3]+m[[5]][[1]]e[4],
	c1=m[[1]][[2]]e[0]+m[[2]][[2]]e[1]+m[[3]][[2]]e[2]+m[[4]][[2]]e[3]+m[[5]][[2]]e[4],
	c2=m[[1]][[3]]e[0]+m[[2]][[3]]e[1]+m[[3]][[3]]e[2]+m[[4]][[3]]e[3]+m[[5]][[3]]e[4],
	c3=m[[1]][[4]]e[0]+m[[2]][[4]]e[1]+m[[3]][[4]]e[2]+m[[4]][[4]]e[3]+m[[5]][[4]]e[4],
	c4=m[[1]][[5]]e[0]+m[[2]][[5]]e[1]+m[[3]][[5]]e[2]+m[[4]][[5]]e[3]+m[[5]][[5]]e[4],
	p1,p2,p3,p4,p5},

	p1=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c1,c2],c3],c4]];
	p2=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c4,c2],c3],c0]];
	p3=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c4,c3],c1],c0]];
	p4=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c4,c1],c2],c0]];
	p5=Simplify[WedgeProduct[WedgeProduct[WedgeProduct[c3,c2],c1],c0]];

	Transpose[{
	Table[Coefficient[p1,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p2,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p3,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p4,QuadrivectorList[[k]]],{k,1,5}],
	Table[Coefficient[p5,QuadrivectorList[[k]]],{k,1,5}]}]]


AntiscalarTransform[m_]:=Module[{
	c0=m[[1]][[1]]e[0]+m[[2]][[1]]e[1]+m[[3]][[1]]e[2]+m[[4]][[1]]e[3]+m[[5]][[1]]e[4],
	c1=m[[1]][[2]]e[0]+m[[2]][[2]]e[1]+m[[3]][[2]]e[2]+m[[4]][[2]]e[3]+m[[5]][[2]]e[4],
	c2=m[[1]][[3]]e[0]+m[[2]][[3]]e[1]+m[[3]][[3]]e[2]+m[[4]][[3]]e[3]+m[[5]][[3]]e[4],
	c3=m[[1]][[4]]e[0]+m[[2]][[4]]e[1]+m[[3]][[4]]e[2]+m[[4]][[4]]e[3]+m[[5]][[4]]e[4],
	c4=m[[1]][[5]]e[0]+m[[2]][[5]]e[1]+m[[3]][[5]]e[2]+m[[4]][[5]]e[3]+m[[5]][[5]]e[4]},
	{{Coefficient[Simplify[WedgeProduct[WedgeProduct[WedgeProduct[WedgeProduct[c0,c1],c2],c3],c4]],e[0,1,2,3,4]]}}]


Exomorphism[m_]:=SparseArray[Band[{1,1}]->{{{1}},m,BivectorTransform[m],TrivectorTransform[m],QuadrivectorTransform[m],AntiscalarTransform[m]}]


OperatorMatrix[op_,constraints_:{}]:=Module[{t,x,y,z,w,v,tp,xp,yp,zp,wp},v=Simplify[Antisandwich[op,t e[0]+x e[1]+y e[2]+z e[3]+w e[4]],constraints];
	tp=Coefficient[v,e[0]];xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];wp=Coefficient[v,e[4]];
	{{Coefficient[tp,t],Coefficient[tp,x],Coefficient[tp,y],Coefficient[tp,z],Coefficient[tp,w]},
	{Coefficient[xp,t],Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z],Coefficient[xp,w]},
	{Coefficient[yp,t],Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z],Coefficient[yp,w]},
	{Coefficient[zp,t],Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z],Coefficient[zp,w]},
	{Coefficient[wp,t],Coefficient[wp,x],Coefficient[wp,y],Coefficient[wp,z],Coefficient[wp,w]}}]


CompOperatorMatrix[op_,constraints_:{}]:=Module[{t,x,y,z,w,v,tp,xp,yp,zp,wp},v=Simplify[Sandwich[op,t e[0]+x e[1]+y e[2]+z e[3]+w e[4]],constraints];
	tp=Coefficient[v,e[0]];xp=Coefficient[v,e[1]];yp=Coefficient[v,e[2]];zp=Coefficient[v,e[3]];wp=Coefficient[v,e[4]];
	{{Coefficient[tp,t],Coefficient[tp,x],Coefficient[tp,y],Coefficient[tp,z],Coefficient[tp,w]},
	{Coefficient[xp,t],Coefficient[xp,x],Coefficient[xp,y],Coefficient[xp,z],Coefficient[xp,w]},
	{Coefficient[yp,t],Coefficient[yp,x],Coefficient[yp,y],Coefficient[yp,z],Coefficient[yp,w]},
	{Coefficient[zp,t],Coefficient[zp,x],Coefficient[zp,y],Coefficient[zp,z],Coefficient[zp,w]},
	{Coefficient[wp,t],Coefficient[wp,x],Coefficient[wp,y],Coefficient[wp,z],Coefficient[wp,w]}}]


(* ::Subsubsection:: *)
(*Constraints*)


LineConstraints[l_]:=Module[{vx=Coefficient[l,e[4,1,0]],vy=Coefficient[l,e[4,2,0]],vz=Coefficient[l,e[4,3,0]],
	mx=Coefficient[l,e[2,3,0]],my=Coefficient[l,e[3,1,0]],mz=Coefficient[l,e[1,2,0]]},
	{vx mx + vy my + vz mz == 0}]


(* ::Subsubsection:: *)
(*Formatting*)


Format[e[],TraditionalForm]:=DisplayForm[Style["1",Bold,FontFamily->"Times New Roman",FontSize->16]]


Format[e[x_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_,z_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y]<>TextString[z],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[x_,y_,z_,w_],TraditionalForm]:=DisplayForm[SubscriptBox[Style["e",Bold,FontFamily->"Times New Roman",FontSize->16],Style[TextString[x]<>TextString[y]<>TextString[z]<>TextString[w],FontFamily->"Times New Roman",FontSize->9]]]


Format[e[0,1,2,3,4],TraditionalForm]:=DisplayForm[StyleBox[FromCharacterCode[16^^1D7D9],FontFamily->"Segoe UI Symbol",FontSize->16]]


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
