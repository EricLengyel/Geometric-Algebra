(* ::Package:: *)

(* ::Title:: *)
(*Rigid Geometric Algebra in 3D*)


(* ::Author:: *)
(*By Eric Lengyel, Copyright 2019-2025*)


(* ::Subsection:: *)
(*Prolog*)


BeginPackage["RigidAlgebra3D`"];


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


Metric::usage = "Metric is the 4x4 metric tensor";


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


Attitude::usage = "Attitude[x] returns the attitude of x";


HomogeneousDistance::usage = "HomogeneousDistance[x,y] returns the absolute Euclidean distance between x and y as a homogeneous magnitude";


HomogeneousAngle::usage = "HomogeneousAngle[x,y] returns the cosine of the absolute Euclidean angle between x and y as a homogeneous magnitude";


OrthoProject::usage = "OrthoProject[x,y] returns the orthogonal projection of x onto y";


OrthoAntiproject::usage = "OrthoAntiproject[x,y] returns the orthogonal antiprojection of x onto y";


CentralProject::usage = "CentralProject[x,y] returns the central projection of x onto y";


CentralAntiproject::usage = "CentralAntiproject[x,y] returns the central antiprojection of x onto y";


Support::usage = "Support[x] returns the support point of the object x";


Antisupport::usage = "Antisupport[x] returns the antisupport plane of the object x";


BivectorTransform::usage = "BivectorTransform[m] returns the second compound matrix of a 4x4 matrix m that transforms vectors, which is a 6x6 matrix that transforms bivectors";


TrivectorTransform::usage = "TrivectorTransform[m] returns the third compound matrix of a 4x4 matrix m that transforms vectors, which is a 4x4 matrix that transforms trivectors";


AntiscalarTransform::usage = "AntiscalarTransform[m] returns the fourth compound matrix of a 4x4 matrix m that transforms vectors, which is a 1x1 matrix that transforms antiscalars";


Exomorphism::usage = "Exomorphism[m] returns the exomorphism matrix corresponding to the 4x4 matrix m that transforms vectors, which is a 16x16 block diagonal matrix that transforms multivectors";


OperatorMatrix::usage = "OperatorMatrix[op,constraints] returns the 4x4 vector transformation matrix corresponding to the geometric algebra operator op with optional geometric constraints";


CompOperatorMatrix::usage = "CompOperatorMatrix[op,constraints] returns the 4x4 vector transformation matrix corresponding to the geometric algebra complement operator op with optional geometric constraints";


LineConstraints::usage = "LineConstraints[l] returns the geometric constraints for the line l";


MotorConstraints::usage = "MotorConstraints[Q] returns the geometric constraints for the motor Q";


FlectorConstraints::usage = "FlectorConstraints[F] returns the geometric constraints for the flector F";


DividerList::usage = "DividerList is a list of table divider thicknesses for a header plus 16 entries";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Basis Elements*)


e[1,1]=e[]; e[2,2]=e[]; e[3,3]=e[]; e[4,4]=0;


e[3,2]=-e[2,3]; e[1,3]=-e[3,1]; e[2,1]=-e[1,2]; e[1,4]=-e[4,1]; e[2,4]=-e[4,2]; e[3,4]=-e[4,3];


e[1,3,2]=e[3,2,1]; e[2,1,3]=e[3,2,1]; e[1,2,3]=-e[3,2,1]; e[2,3,1]=-e[3,2,1]; e[3,1,2]=-e[3,2,1];


e[3,4,2]=e[4,2,3]; e[2,3,4]=e[4,2,3]; e[2,4,3]=-e[4,2,3]; e[4,3,2]=-e[4,2,3]; e[3,2,4]=-e[4,2,3];


e[1,4,3]=e[4,3,1]; e[3,1,4]=e[4,3,1]; e[3,4,1]=-e[4,3,1]; e[4,1,3]=-e[4,3,1]; e[1,3,4]=-e[4,3,1];


e[2,4,1]=e[4,1,2]; e[1,2,4]=e[4,1,2]; e[1,4,2]=-e[4,1,2]; e[4,2,1]=-e[4,1,2]; e[2,1,4]=-e[4,1,2];


e[2,3,4,1]=-e[1,2,3,4]; e[3,4,1,2]=e[1,2,3,4]; e[4,1,2,3]=-e[1,2,3,4]; e[2,1,4,3]=e[1,2,3,4]; e[1,4,3,2]=-e[1,2,3,4];


e[4,3,2,1]=e[1,2,3,4]; e[3,2,1,4]=-e[1,2,3,4]; e[1,3,4,2]=e[1,2,3,4]; e[3,4,2,1]=-e[1,2,3,4]; e[4,2,1,3]=e[1,2,3,4];


e[2,1,3,4]=-e[1,2,3,4]; e[2,3,1,4]=e[1,2,3,4]; e[3,1,4,2]=-e[1,2,3,4]; e[1,4,2,3]=e[1,2,3,4]; e[4,2,3,1]=-e[1,2,3,4];


e[1,3,2,4]=-e[1,2,3,4]; e[3,2,4,1]=e[1,2,3,4]; e[2,4,1,3]=-e[1,2,3,4]; e[4,1,3,2]=e[1,2,3,4]; e[1,2,4,3]=-e[1,2,3,4];


e[2,4,3,1]=e[1,2,3,4]; e[4,3,1,2]=-e[1,2,3,4]; e[3,1,2,4]=e[1,2,3,4];


e[x_,y_,z_]:=WedgeProduct[e[x,y],e[z]]/;x==y


e[x_,y_,z_]:=-WedgeProduct[e[x,z],e[y]]/;x==z


e[x_,y_,z_]:=WedgeProduct[e[x],e[y,z]]/;y==z


e[x_,y_,z_,w_]:=WedgeProduct[e[x,w],e[y,z]]/;x==w||y==z


e[x_,y_,z_,w_]:=-WedgeProduct[e[x,z],e[y,w]]/;x==z||y==w


e[x_,y_,z_,w_]:=WedgeProduct[e[x,y],e[z,w]]/;x==y||z==w


VectorList={e[1],e[2],e[3],e[4]};


BivectorList={e[4,1],e[4,2],e[4,3],e[2,3],e[3,1],e[1,2]};


TrivectorList={e[4,2,3],e[4,3,1],e[4,1,2],e[3,2,1]};


BasisList={e[]}~Join~VectorList~Join~BivectorList~Join~TrivectorList~Join~{e[1,2,3,4]};


BasisCollect[x_]:=Collect[x,BasisList]


BasisForm[x_]:=TraditionalForm[BasisCollect[x]]


BasisGrade={0,1,1,1,1,2,2,2,2,2,2,3,3,3,3,4};


BasisAntigrade={4,3,3,3,3,2,2,2,2,2,2,1,1,1,1,0};


ScalarPart[x_]:=Coefficient[x,e[]]e[]


AntiscalarPart[x_]:=Coefficient[x,e[1,2,3,4]]e[1,2,3,4]


VectorPart[x_]:=Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]+Coefficient[x,e[4]]e[4]


BivectorPart[x_]:=Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,3]]e[4,3]+Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[1,2]]e[1,2]


TrivectorPart[x_]:=Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[4,1,2]]e[4,1,2]+Coefficient[x,e[3,2,1]]e[3,2,1]


BulkPart[x_]:=Coefficient[x,e[]]e[]+Coefficient[x,e[1]]e[1]+Coefficient[x,e[2]]e[2]+Coefficient[x,e[3]]e[3]+Coefficient[x,e[2,3]]e[2,3]+Coefficient[x,e[3,1]]e[3,1]+Coefficient[x,e[1,2]]e[1,2]+Coefficient[x,e[3,2,1]]e[3,2,1]


WeightPart[x_]:=Coefficient[x,e[4]]e[4]+Coefficient[x,e[4,1]]e[4,1]+Coefficient[x,e[4,2]]e[4,2]+Coefficient[x,e[4,3]]e[4,3]+Coefficient[x,e[4,2,3]]e[4,2,3]+Coefficient[x,e[4,3,1]]e[4,3,1]+Coefficient[x,e[4,1,2]]e[4,1,2]+Coefficient[x,e[1,2,3,4]]e[1,2,3,4]


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


LeftComp[e[1]]=-e[4,2,3];LeftComp[e[2]]=-e[4,3,1]; LeftComp[e[3]]=-e[4,1,2]; LeftComp[e[4]]=-e[3,2,1];


LeftComp[e[2,3]]=-e[4,1];LeftComp[e[3,1]]=-e[4,2]; LeftComp[e[1,2]]=-e[4,3];


LeftComp[e[4,1]]=-e[2,3];LeftComp[e[4,2]]=-e[3,1]; LeftComp[e[4,3]]=-e[1,2];


LeftComp[e[4,2,3]]=e[1];LeftComp[e[4,3,1]]=e[2]; LeftComp[e[4,1,2]]=e[3]; LeftComp[e[3,2,1]]=e[4];


LeftComp[e[]]=e[1,2,3,4];LeftComp[e[1,2,3,4]]=e[];


LeftComp[x_]:=x e[1,2,3,4]/;FreeQ[x,e]


LeftComp[x_ y_]:=x LeftComp[y]/;FreeQ[x,e]


LeftComp[x_ y_]:=LeftComp[x]y/;FreeQ[y,e]


LeftComp[x_+y_]:=LeftComp[x]+LeftComp[y]


(* ::Subsubsection:: *)
(*Reverse*)


Rev[e[]]=e[]; Rev[e[1,2,3,4]]=e[1,2,3,4];


Rev[x_]:=x/;FreeQ[x,e]


Rev[e[x_]]:=e[x]


Rev[e[x_,y_]]:=-e[x,y]


Rev[e[x_,y_,z_]]:=-e[x,y,z]


Rev[x_ y_]:=x Rev[y]/;FreeQ[x,e]


Rev[x_ y_]:=Rev[x]y/;FreeQ[y,e]


Rev[x_+y_]:=Rev[x]+Rev[y]


Antirev[x_]:=LeftComp[Rev[RightComp[x]]]


(* ::Subsubsection:: *)
(*Metric*)


Metric={{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,0}};


ApplyMetric[e[1]]=e[1]; ApplyMetric[e[2]]=e[2]; ApplyMetric[e[3]]=e[3]; ApplyMetric[e[4]]=0;


ApplyMetric[e[2,3]]=e[2,3]; ApplyMetric[e[3,1]]=e[3,1]; ApplyMetric[e[1,2]]=e[1,2];


ApplyMetric[e[4,1]]=0; ApplyMetric[e[4,2]]=0; ApplyMetric[e[4,3]]=0;


ApplyMetric[e[4,2,3]]=0; ApplyMetric[e[4,3,1]]=0; ApplyMetric[e[4,1,2]]=0; ApplyMetric[e[3,2,1]]=e[3,2,1];


ApplyMetric[e[]]=e[]; ApplyMetric[e[1,2,3,4]]=0;


ApplyMetric[x_]:=x e[]/;FreeQ[x,e]


ApplyMetric[x_ y_]:=x ApplyMetric[y]/;FreeQ[x,e]


ApplyMetric[x_ y_]:=ApplyMetric[x]y/;FreeQ[y,e]


ApplyMetric[x_+y_]:=ApplyMetric[x]+ApplyMetric[y]


ApplyAntimetric[x_]:=LeftComp[ApplyMetric[RightComp[x]]]


(* ::Subsubsection:: *)
(*Dual*)


Table[BulkDual[BasisList[[x]]]=RightComp[ApplyMetric[BasisList[[x]]]],{x,1,16}];


Table[WeightDual[BasisList[[x]]]=RightComp[ApplyAntimetric[BasisList[[x]]]],{x,1,16}];


BulkDual[x_]:=x e[1,2,3,4]/;FreeQ[x,e]


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


Power[e[],1/2]=e[]; Power[e[1,2,3,4],1/2]=e[1,2,3,4];


Power[Times[x_,e[]],1/2]:=Sqrt[x]e[]/;FreeQ[x,e]


Power[Times[x_,e[1,2,3,4]],1/2]:=Sqrt[x]e[1,2,3,4]/;FreeQ[x,e]


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


WedgeProduct[e[x_],e[]]:=e[x]


WedgeProduct[e[x_,y_],e[]]:=e[x,y]


WedgeProduct[e[x_,y_,z_],e[]]:=e[x,y,z]


WedgeProduct[e[x_,y_,z_,w_],e[]]:=e[x,y,z,w]


WedgeProduct[e[x_],e[y_]]:=0/;x==y


WedgeProduct[e[x_],e[y_]]:=e[x,y]/;x!=y


WedgeProduct[e[x_,y_],e[z_]]:=0/;x==z||y==z


WedgeProduct[e[x_,y_],e[z_]]:=e[x,y,z]/;x!=z&&y!=z


WedgeProduct[e[x_],e[y_,z_]]:=0/;x==y||x==z


WedgeProduct[e[x_],e[y_,z_]]:=WedgeProduct[e[x,y],e[z]]/;x!=y&&x!=z


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


GeometricProduct[e[1],e[1,2,3,4]]=e[4,2,3]; GeometricProduct[e[2],e[1,2,3,4]]=e[4,3,1]; GeometricProduct[e[3],e[1,2,3,4]]=e[4,1,2]; GeometricProduct[e[4],e[1,2,3,4]]=0;


GeometricProduct[e[2,3],e[1,2,3,4]]=e[4,1]; GeometricProduct[e[3,1],e[1,2,3,4]]=e[4,2]; GeometricProduct[e[1,2],e[1,2,3,4]]=e[4,3];


GeometricProduct[e[4,1],e[1,2,3,4]]=0; GeometricProduct[e[4,2],e[1,2,3,4]]=0; GeometricProduct[e[4,3],e[1,2,3,4]]=0;


GeometricProduct[e[4,2,3],e[1,2,3,4]]=0; GeometricProduct[e[4,3,1],e[1,2,3,4]]=0; GeometricProduct[e[4,1,2],e[1,2,3,4]]=0; GeometricProduct[e[3,2,1],e[1,2,3,4]]=e[4];


GeometricProduct[e[1,2,3,4],e[x_]]:=GeometricProduct[e[1],e[2,3,4,x]]


GeometricProduct[e[1,2,3,4],e[x_,y_]]:=GeometricProduct[e[1,2],e[3,4,x,y]]


GeometricProduct[e[1,2,3,4],e[x_,y_,z_]]:=GeometricProduct[e[1,2,3],e[4,x,y,z]]


GeometricProduct[e[],e[]]=e[]; GeometricProduct[e[1,2,3,4],e[1,2,3,4]]=0;


GeometricProduct[e[],e[x_]]:=e[x]


GeometricProduct[e[],e[x_,y_]]:=e[x,y]


GeometricProduct[e[],e[x_,y_,z_]]:=e[x,y,z]


GeometricProduct[e[],e[x_,y_,z_,w_]]:=e[x,y,z,w]


GeometricProduct[e[x_],e[]]:=e[x]


GeometricProduct[e[x_,y_],e[]]:=e[x,y]


GeometricProduct[e[x_,y_,z_],e[]]:=e[x,y,z]


GeometricProduct[e[x_,y_,z_,w_],e[]]:=e[x,y,z,w]


GeometricProduct[e[x_],e[y_]]:=e[x,y]


GeometricProduct[e[x_],e[y_,z_]]:=e[x,y,z]; GeometricProduct[e[x_,y_],e[z_]]:=e[x,y,z]


GeometricProduct[e[x_,y_],e[z_,w_]]:=e[x,y,z,w]; GeometricProduct[e[x_],e[y_,z_,w_]]:=e[x,y,z,w]; GeometricProduct[e[x_,y_,z_],e[w_]]:=e[x,y,z,w]


GeometricProduct[e[x_,y_],e[s_,t_,u_]]:=GeometricProduct[e[x],e[y,s,t,u]]


GeometricProduct[e[x_,y_,z_],e[s_,t_]]:=GeometricProduct[e[x],e[y,z,s,t]]


GeometricProduct[e[x_,y_,z_],e[s_,t_,u_]]:=GeometricProduct[e[x,y],e[z,s,t,u]]


GeometricProduct[x_,y_]:=x y/;FreeQ[x,e]; GeometricProduct[x_,y_]:=y x/;FreeQ[y,e]


GeometricProduct[x_ y_,z_]:=x GeometricProduct[y,z]/;FreeQ[x,e]; GeometricProduct[x_,y_ z_]:=y GeometricProduct[x,z]/;FreeQ[y,e]


GeometricProduct[x_ y_,z_ w_]:=x z GeometricProduct[y,w]/;FreeQ[x,e]&&FreeQ[z,e]


GeometricProduct[x_,y_Plus]:=Distribute[temp[x,y]]/.temp->GeometricProduct


GeometricProduct[x_Plus,y_]:=Distribute[temp[x,y]]/.temp->GeometricProduct


GeometricAntiproduct[x_,y_]:=LeftComp[GeometricProduct[RightComp[x],RightComp[y]]]


Sandwich[m_,x_]:=GeometricProduct[GeometricProduct[m,x],Rev[m]]


Antisandwich[m_,x_]:=GeometricAntiproduct[GeometricAntiproduct[m,x],Antirev[m]]


(* ::Subsubsection:: *)
(*Attitude*)


Attitude[x_]:=AntiwedgeProduct[x,e[3,2,1]]


(* ::Subsubsection:: *)
(*Measurements*)


HomogeneousDistance[x_,y_]:=BulkNorm[Attitude[WedgeProduct[x,y]]]+WeightNorm[WedgeProduct[x,Attitude[y]]]


HomogeneousAngle[x_,y_]:=BulkNorm[WeightContraction[x,y]]+AntiwedgeProduct[WeightNorm[x],WeightNorm[y]]


(* ::Subsubsection:: *)
(*Projection*)


OrthoProject[x_,y_]:=AntiwedgeProduct[y,WeightExpansion[x,y]]


OrthoAntiproject[x_,y_]:=WedgeProduct[y,WeightContraction[x,y]]


CentralProject[x_,y_]:=AntiwedgeProduct[y,BulkExpansion[x,y]]


CentralAntiproject[x_,y_]:=WedgeProduct[y,BulkContraction[x,y]]


Support[x_]:=OrthoProject[e[4],x]


Antisupport[x_]:=CentralAntiproject[e[3,2,1],x]


(* ::Subsubsection:: *)
(*Exomorphism*)


BivectorTransform[m_]:=Module[{
	c1=m[[1]][[1]]e[1]+m[[2]][[1]]e[2]+m[[3]][[1]]e[3]+m[[4]][[1]]e[4],
	c2=m[[1]][[2]]e[1]+m[[2]][[2]]e[2]+m[[3]][[2]]e[3]+m[[4]][[2]]e[4],
	c3=m[[1]][[3]]e[1]+m[[2]][[3]]e[2]+m[[3]][[3]]e[3]+m[[4]][[3]]e[4],
	c4=m[[1]][[4]]e[1]+m[[2]][[4]]e[2]+m[[3]][[4]]e[3]+m[[4]][[4]]e[4],
	p1,p2,p3,p4,p5,p6},
	
	p1=Simplify[WedgeProduct[c4,c1]];
	p2=Simplify[WedgeProduct[c4,c2]];
	p3=Simplify[WedgeProduct[c4,c3]];
	p4=Simplify[WedgeProduct[c2,c3]];
	p5=Simplify[WedgeProduct[c3,c1]];
	p6=Simplify[WedgeProduct[c1,c2]];
	
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

	p1=Simplify[WedgeProduct[WedgeProduct[c4,c2],c3]];
	p2=Simplify[WedgeProduct[WedgeProduct[c4,c3],c1]];
	p3=Simplify[WedgeProduct[WedgeProduct[c4,c1],c2]];
	p4=Simplify[WedgeProduct[WedgeProduct[c3,c2],c1]];

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
(*Constraints*)


LineConstraints[l_]:=Module[{vx=Coefficient[l,e[4,1]],vy=Coefficient[l,e[4,2]],vz=Coefficient[l,e[4,3]],
	mx=Coefficient[l,e[2,3]],my=Coefficient[l,e[3,1]],mz=Coefficient[l,e[1,2]]},
	{vx mx + vy my + vz mz == 0}]


MotorConstraints[Q_]:=Module[{vx=Coefficient[Q,e[4,1]],vy=Coefficient[Q,e[4,2]],vz=Coefficient[Q,e[4,3]],vw=Coefficient[Q,e[1,2,3,4]],
	mx=Coefficient[Q,e[2,3]],my=Coefficient[Q,e[3,1]],mz=Coefficient[Q,e[1,2]],mw=Coefficient[Q,e[]]},
	{vx mx + vy my + vz mz + vw mw == 0}]


FlectorConstraints[F_]:=Module[{px=Coefficient[F,e[1]],py=Coefficient[F,e[2]],pz=Coefficient[F,e[3]],pw=Coefficient[F,e[4]],
	gx=Coefficient[F,e[4,2,3]],gy=Coefficient[F,e[4,3,1]],gz=Coefficient[F,e[4,1,2]],gw=Coefficient[F,e[3,2,1]]},
	{px gx + py gy + pz gz + pw gw == 0}]


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
