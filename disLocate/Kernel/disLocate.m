(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["disLocate`"]
(* Step 1. - Merge All Cells Toghether *)
(* Step 2. - Convert To Initialization Cell *)
(* Step 3. - Save as .M *)
disLocateVersion:="9.2023-06-29."<>ToString["thesis-M.V9"];
If[$VersionNumber<10,Needs["ComputationalGeometry`"];];

(* Global Definiations of Colour Schemes *)

(* Voronoi Colour Schemes *)
TypeVoronoiQ[string_] :=Block[{},
If[ToUpperCase[string]==ToUpperCase["VORONOI"]||
ToUpperCase[string]==ToUpperCase["VOR"]||
ToUpperCase[string]==ToUpperCase["V"]||
ToUpperCase[string]==ToUpperCase["Voronoi"]
,True,False
]
];
AssignVoronoiColour[vor_]:=Block[{intensity,hexagonalVoronoi},
intensity=Length[vor]/Total[PolyArea[ # ]&/@vor];
hexagonalVoronoi = HexagonArea[Sqrt[2./(intensity*Sqrt[3.])]/2.];
(VoronoiHexagonAreaDeviationColour[#]&/@(Abs[PolyArea[ # ]-hexagonalVoronoi]/hexagonalVoronoi&/@vor))
];
VoronoiCellColourScheme:={
RGBColor[43/255,49/255,135/255]   (* Deep Blue *),
RGBColor[26/255,117/255,188/255] (* Lighter Deep Blue *),
RGBColor[1/255,174/255,240/255], (* Light Blue *)
RGBColor[110/255,179/255,104/255], (* Light Green *)
RGBColor[254/255,205/255,6/255], (* Yellow *)
RGBColor[242/255,107/255,41/255], (* Orange *)
RGBColor[203/255,32/255,38/255], (* Red *)
RGBColor[203/255,32/255,38/255], (* Red *)
RGBColor[203/255,32/255,38/255], (* Red *)
RGBColor[203/255,32/255,38/255], (* Red *)
RGBColor[203/255,32/255,38/255], (* Red *)
RGBColor[203/255,32/255,38/255](* Red *)
};
VoronoiHexagonAreaDeviationColour[col_] := Which[
col<=0.05,VoronoiCellColourScheme[[1]],
col<=0.10,VoronoiCellColourScheme[[2]],
col<=0.15,VoronoiCellColourScheme[[3]],
col<=0.20,VoronoiCellColourScheme[[4]],
col<=0.25,VoronoiCellColourScheme[[5]],
col<=  0.30,VoronoiCellColourScheme[[6]],
col> 0.30,VoronoiCellColourScheme[[7]]
];

(* Bond Order Colour Schemes *)
TypeBondOrderQ[string_]:=Block[{},
If[ToUpperCase[string]==ToUpperCase["BOND ORDER"]||
ToUpperCase[string]==ToUpperCase["BONDORDER"]||
ToUpperCase[string]==ToUpperCase["BOND"]||
ToUpperCase[string]==ToUpperCase["BOOP"]||
ToUpperCase[string]==ToUpperCase["BOP"],
True,False]
];



greenBlend[q_]:=Blend[ {White,Blend[{RGBColor[147/255,3/255,46/255],Green},(0.5)]},q];
purpleBlend[q_]:=   Blend[{Blend[{Blend[{Purple,Blue},0.25],White},0.25](*RGBColor[147/255,3/255,46/255]*),White},((*1.33*)q(*/BOPexp[l]*))];

BondOrderColourScheme[q_,l_Integer]:=

If[q>1,Blend[ {White,Blend[{RGBColor[147/255,3/255,46/255],Green},(0.5)]},10(q-1)], Blend[{Blend[{Blend[{Purple,Blue},0.25],White},0.25](*RGBColor[147/255,3/255,46/255]*),White},((*1.33*)q(*/BOPexp[l]*))]];
(* Bond Order Parameter - Expectation Values *)
SqrBopQ4:=0.8290387427498793;
HexBopQ6:=0.7408293494456062;

BopQ1One:=1.0;
BopQ2Two:=0.5;
BopQ3Tri:=0.7905688454904419;
BopQ4Sqr:=0.8290387427498793;
BopQ5Pent:=0.7015607600201141;
BopQ6Hex:=0.7408293494456062;
BopQ7Sept:=0.6472598492877493;
BopQ8Oct:=0.6837611402200334;

BOPexp[l_]:=Part[{BopQ1One,BopQ2Two,BopQ3Tri,BopQ4Sqr,BopQ5Pent,BopQ6Hex,BopQ7Sept,BopQ8Oct},l];

(* Coordination Number Colour Scheme *)
TypeCoordinationQ[string_] :=Block[{},
If[ToUpperCase[string]==ToUpperCase["coordination"]||
ToUpperCase[string]==ToUpperCase["coord"]||
ToUpperCase[string]==ToUpperCase["neighbours"]||
ToUpperCase[string]==ToUpperCase["neighbors"]||
ToUpperCase[string]==ToUpperCase["neighbour"]||
ToUpperCase[string]==ToUpperCase["neighbor"]||
ToUpperCase[string]==ToUpperCase["NN"]||
ToUpperCase[string]==ToUpperCase["contact"]
,True,False]
];
AssignCoordinationColour[vor_]:=Block[{},
CoordinationNumberColourScheme[Length[#]]&/@vor
];
(* Explicitly assumes a closed Voronoi Polygon (first point = last point) *)
(*CoordinationNumberColourScheme[in_]:= Which[
in\[LessEqual] 3,Black,
in\[Equal]4,Blend[{Blue,Purple},0.75],
in\[Equal]5,Blend[{Blue,Green},0.35],
in\[Equal]6,Blend[{Green,GrayLevel[0.46]},0.65], 
in\[Equal]7,GrayLevel[0.85],
in\[Equal]8,Blend[{Orange,GrayLevel[0.95]},0.1],
in\[Equal]9,Blend[{Blend[{Purple,Blue},0.25],White},0.5] ,
in\[Equal]10,Blend[{Pink,Purple},0.6],
in\[Equal]11,Blend[{Purple,Red},0.75],
in\[GreaterEqual]12 , Blend[{Yellow,Black},(15-in)/5]];*)

CoordinationNumberColourScheme[in_]:= Which[
in==3,Purple,
in==4,Blend[{Blue,GrayLevel[0.5]},0.5],
in==5,Blend[{Blue,Green},0.45],
in==6, Blend[{Green,GrayLevel[0.3]},0.5],
in==7,GrayLevel[0.75],
in==8,Blend[{Orange,GrayLevel[0.5]},0.1],
in==9,Blend[{Red,GrayLevel[0.5]},0.5],
in==10,Blend[{Black,Red},0.5],
in>=  11,Black];




(* Angular Orientation Colour Scheme *)
TypeAngularDirectionQ[string_] :=Block[{},
If[ToUpperCase[string]==ToUpperCase["angle"] ||
ToUpperCase[string]==ToUpperCase["angular"] ||
ToUpperCase[string]==ToUpperCase["ang"] ||

ToUpperCase[string]==ToUpperCase["orientation"] ||
ToUpperCase[string]==ToUpperCase["direction"] ||
ToUpperCase[string]==ToUpperCase["direct"] ||
ToUpperCase[string]==ToUpperCase["orient"] ||

ToUpperCase[string]==ToUpperCase["cluster"]||
ToUpperCase[string]==ToUpperCase["clust"]||
ToUpperCase[string]==ToUpperCase["vector"] ||
ToUpperCase[string]==ToUpperCase["vec"]
,True,False]
];
BjornColourScheme:={
RGBColor[{255,51,51}/256],(*bjorn red*)
Yellow,
RGBColor[{0,153,51}/256], (*bjorn green*)
Blue
(*RGBColor[147/255,3/255,46/255]*)
(*Yellow*)
(*RGBColor[{51,51,153}/256]*)(*bjorn blue*)
(*RGBColor[0.545,0.0,0.8],*) (*bjorn orange*)
};
BjornAngleScheme[inTheta_,rot_]:=Block[{
modAngle,
angBin=Table[n/rot ,{n,(Pi/2)/2,2Pi,Pi/2}],
mod\[Theta]
},
modAngle[\[Theta]_]:= Mod[\[Theta],2.Pi/rot];
mod\[Theta]=modAngle@inTheta;

Which[
mod\[Theta]<= angBin[[1]]||mod\[Theta]>  angBin[[4]],
BjornColourScheme[[1]],(*bjorn red*)
mod\[Theta]<=  angBin[[2]]&& mod\[Theta]>  angBin[[1]],
BjornColourScheme[[2]],(*bjorn purple*)
mod\[Theta]<=  angBin[[3]]&& mod\[Theta]> angBin[[2]],
BjornColourScheme[[3]],(*bjorn green*)
mod\[Theta]<=  angBin[[4]]&& mod\[Theta]> angBin[[3]],
BjornColourScheme[[4]](*bjorn blue*)
]

];

(* Regular Polygon Colour Code  *)
RegularPolygonColourScheme:=
{RGBColor[95/255,128/255,181/255],RGBColor[225/255,156/255,37/255],RGBColor[142/255,176/255,63/255],RGBColor[234/255,97/255,51/255],RGBColor[134/255,121/255,177/255],Gray};



SetAttributes[PlotPairCorrelationFunction,HoldFirst];
SetAttributes[PlotVoronoiTessellations,HoldFirst];

(* Usages of disLocate Private Functions *)


inPolyQ2::usage= "inPolyQ2[ polygon, x, y ]: 
 A simple method to determine if a point (x,y) lays inside a regular polygon.  Returns 'True' if it does say inside, or 'False' if not.  Taken from http://mathematica.stackexchange.com/questions/9405/how-to-check-if-a-2d-point-is-in-a-polygon
Example:  inPolyQ2[Table[{Cos[x],Sin[x]},{x,2Pi,0,-2Pi/30}],0.5,0.5]  \n"

PolyArea::usage= "PolyArea[ polygon]: Solves the Area of teh Polygon."

ExpectedDiameterFromLattice::usage="ExpectedDiameterFromLattice[data2d_ , OptionsPattern[]

]:= Returns the expected Lattice Diameter from Point Pattern. Options[ExpectedDiameterFromLattice]={ box\[Rule]{} ,lattice\[Rule]ToString[hex]  or lattice-> ToString[square]};"

HexagonalDiameter::usage="HexagonalDiameter[numberOfPoints_, inBox_]:= returns the expected hexagonal diameter from number density.  Need N and Box."
HexagonArea::usage="HexagonArea[R_]:= returns the area of a hexagon with an apothem of distance R. Useful with closed packed circles, where their radius is R."
Hexaticize::usage="Hexaticize[d_,dr_]:= Used to create the hexatic lattice - Randomly positions the List of points (d) by (dr) standard deviation. Code= d+RandomVariate[NormalDistribution[0.0,dr],{Length[d],Length[d[[1]]]}]"

negQ::usage="negQ[number]:= Test to see if it is negative, returns true if negative."

hexToRGB::usage="A function that takes a string of hex colour values and converts it to RGB.  Make sure to include '#':  ToString[#000000]"

standardPolygon::usage="standardPolygon[n_]:= Gives a polygon of n points. As the number n \[Rule] \[Infinity], this polygon becomes a circle."

ScaleBars::usage="ScaleBars[type_]: Returns the ScaleBar for a specific type of plot.  Types incude: ToString[voronoi], ToString[bop], ToString[angle]."
(*New tester/helper Functions 2023*)
findBadPoints::usage="Checks if the voronoi generates a thing with two data points in one cell"
coordFromPolygon::usage="It does another thing"
cellNum::usage=" given voronoi cells written as in VoronoiCells, returns the number in list a point is in.  "
pcell::usage="Given points, finds cell Voronoi point is in. "
(* *********************** *)

closedBindingBox::usage="closedBindingBox:={{0.,0.},{1.,0.},{1.,1.},{0.,1.},{0.,0.}};"
bindingBox::usage="bindingBox:={{0.,0.},{1.,0.},{1.,1.},{0.,1.}};"
rectangleBindingBox::usage="rectangleBindingBox[a_,b_]:=Block[{},closedBindingBox.{{a,0},{0,b}}];"
standardGrowthRate::usage="standardGrowthRate:=\!\(\*SuperscriptBox[\(10\), \(-5\)]\);"
(*----------------------------------------------------------------------*)
ApplyPeriodicBoundary::usage = "ApplyPeriodicBoundary[inData2d_,box_, OptionsPattern[] ]: Positions particles outside the box by translating points from the other side. \!\(\*
StyleBox[\"Options\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\":\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\){SetDistance\[Rule]1,ReturnDim\[Rule]2}"

ApplyImageChargeBoundary::usage = "ApplyImageChargeBoundary[data2d_,box_,OptionsPattern[]]: Positions particles outside the box by placing it equidistant and perpindicular to the box wall. Returns the original list of points with mirror positions outside of a box appended to the end."

ApplyGeneralPBC::usage="ApplyGeneralPBC[data2d_,box_, numNN___]:= Creates a list (2d) of PBC points "

SolvePBCBoundary::usage="ApplyGeneralPBC[data2d_,box_, numNN___]: Solves for the convex hull that encapsulates all particles for Periodic Boundary Clipping."


(*----------------------------------------------------------------------*)
radialDistributionC::usage="radialDistributionC[{dlist,_Real,1},{r,_Real},{eps,_Real}]:= The function that bins the distances for the pair correlation function."

distancesWithEdgeCorrections::usage="distancesC[data2d_, NumberOfParticlesInBox_ ]:= Returns a list of point-to-point distances.  Calculations are performed for only particles inside the box (i.e. Only the first NumberOfParticlesInBox are evaluated).  Double counting and the self-to-self distance of zero are returned as distance=(-1)."

PairCorrelationFunction::usage="PairCorrelationFunction[ data2d , OptionsPattern[] ]=Given a set of data2d={{\!\(\*SubscriptBox[\(x\), \(1\)]\),\!\(\*SubscriptBox[\(y\), \(1\)]\)}...{\!\(\*SubscriptBox[\(x\), \(n\)]\),\!\(\*SubscriptBox[\(y\), \(n\)]\)}}, returns a table {g(r),r} from r=0.0 to r=(maxBins * binSpacing).  This is an ensemble definition. Multiple data2d={{\!\(\*SubscriptBox[\(conf\), \(1\)]\)}...{\!\(\*SubscriptBox[\(conf\), \(n\)]\)}} with the same density can be input and will be averaged.

OptionsPattern=binSpacing\[Rule]1,maxBins\[Rule]1,box\[Rule]{},edge\[Rule]ToString[trunc],lattice\[Rule]ToString[none],hexNormalized\[Rule]False.
Select edge corrections (edge\[Rule] # ): ToString[pbc] || ToString[image] || ToString[noedge]
Choosing lattice\[Rule]ToString[hex] will Apply the proper Periodic Boundaries when using HexagonalLattice[periodic \[Rule] True]
"

SquareLattice::usage="Returns the square lattice. To select different spacing, define the intensity (N/Area):  n\[Rule] NumberOfParticles, box\[Rule] BindingBoxPolygon."

HexagonalLattice::usage="Returns a set of points describing a hexagonal lattice with a certain intensity.  
To select different spacing, define the intensity (N/Area):  n\[Rule] NumberOfParticles, box\[Rule] BindingBoxPolygon. 
 
Select periodic\[Rule] True for use with PairCorrelationFunction[].
Default OptionsPattern: Options[{n\[Rule] 400, box\[Rule] closedBindingBox, radius\[Rule] 1., periodic\[Rule] False};"

TranslationalOrderParameter::usage="TranslationalOrderParameter[g(r)_ , {min_, max_}, \[Rho]_]:= Returns the Translational Order Parameter by integrating the g(r)=PairCorrelationFunction[].  

Note: g(r) function needs to have units of molecular radius (r=diameter/2). 
The Size of the Shell can be set with \!\(\*SubscriptBox[\(r\), \(inner\)]\)=min and \!\(\*SubscriptBox[\(r\), \(outter\)]\)=max.  
The Number Density is set as \[Rho] = N/Area.
"

TranslationalNumberOfNeighbours::usage="TranslationalNumberOfNeighbours[g(r)_ , {min_, max_}, \[Rho]_]:= Returns the Average Number of Neighbours by integrating the g(r)=PairCorrelationFunction[].  

Note: g(r) function needs to have units of molecular radius (r=diameter/2). 
The Size of the Shell can be set with \!\(\*SubscriptBox[\(r\), \(inner\)]\)=min and \!\(\*SubscriptBox[\(r\), \(outter\)]\)=max.  
The Number Density is set as \[Rho] = N/Area "

TranslationalExcessEntropy::usage="TranslationalExcessEntropy[g(r)_,{xMin_,xMax_},\[Rho]_]:= Returns the Two Point Particle contribution towards the Excess Entropy obtained from integrating the g(r)=PairCorrelationFunction[].

Note: g(r) function needs to have units of molecular radius (r=diameter/2). 
The Size of the Shell can be set with \!\(\*SubscriptBox[\(r\), \(inner\)]\)=min and \!\(\*SubscriptBox[\(r\), \(outter\)]\)=max.  
The Number Density is set as \[Rho] = N/Area.
  "

InterpolationFunctionDifference::usage="InterpolationFunctionDifference[reference_, diffFunct_ ,xMax___]:=  Root Mean Square difference by Linear Interpolation of refernce and diffFunct.  

If xMax===Null, the difference is taken between the maximum overlap.  
Setting xMax will clip to an x-value inside the largest shared x-value

 Returns the List containing:
{rms,differencedFunction,interpolatedReference,interpolatedComparing}

rms = Root Mean Square
differencedFunction = Residual from subtracting the diffFunct_ from reference_.
interpolatedReference = Linear Interpolation List of refernce_
interpolatedComparing = Linear Interpolation List of diffFunct_"

PlotPairCorrelationFunction::usage=""


PlotPcfDifference::usage="PlotPcfDifference[ PairCorrelationList , OptionsPattern[] ]=Difference Spectrum of PairCorrelationFunction[].  The values for: ref={g(r),r} and func={g(r),r} need to be lists.
OptionsPattern[]=(xmax\[Rule]0,normalized\[Rule]True,shapes\[Rule]{},stacked\[Rule]False,names\[Rule]{},xLabel\[Rule]{},refIndex\[Rule]1)

xmax \[Rule] 1 : Cut-off distance for g(r) where \!\(\*SubscriptBox[\(r\), \(max\)]\)=xmax.

normalized \[Rule] True : Automatic rescalling of the x-axis to

shapes \[Rule] {} : List of Polygons to be used as the Legend Markers where - shapes\[Rule]{\!\(\*SubscriptBox[\(polygon\), \(1\)]\), ... , \!\(\*SubscriptBox[\(polygon\), \(n\)]\)} 

stacked \[Rule] False :  Option to overlay curves, or to stack them with an offset.  If stacked\[Rule]True, it will produce the stacked version.  Multiple PairCorrelationFunction[] can be set with:  func={g(r\!\(\*SubscriptBox[\()\), \(1\)]\) , ..., g(r\!\(\*SubscriptBox[\()\), \(n\)]\)};

xLabel \[Rule] {}  :  String used to label the distance axis. If xLabel\[Rule]ToString[Normalized Diameter], the function changes the gridlines to the expected positions of the hexagonal lattice.

refIndex \[Rule] 1 : Defines which PairCorrelation[] in the list is chosen as the reference for all others to be subtracted from. "


(*----------------------------------------------------------------------*)
AutoBoxTransform::usage="AutoBoxTransform[data2D_]:= {box polygon points}.  Used to auto-detect edge particles for removal.  Added to improve user experience - only 1 input is needed for PlotVoronoiTessellations[ ]"

MinMaxBox::usage=
"MinMaxBox[data2d_]:={box}. Returns a box that clips close to the data."

VoronoiCells::usage="VoronoiCells[data2Din_, OptionsPattern[]]=Provides all base information needed to  calculate disorder metrics.  Default OptionsPattens[] include: box\[Rule]{}, edge\[Rule]ToString[NOEDGE], full\[Rule]False, fast\[Rule]True, debug\[Rule]False.  
Default Output={VoronoiPolygons, data2DinWithEdgeParticles, delaunayNearestNeighbours, commonVertex}
This set is needed when removing edge effects.  Output (data2DinWithEdgeParticles) refers to a list {allParticles, insideQTruthList}.  The Position[] of a (True) in the (insideQTruthList) represents the index of a particle in (allParticles) which is not removed from analysis.  This information is used with (VoronoiPolygons) to remove cells outside.  Variable (delaunayNearestNeighbours) is a set of indices points to which particles belong to the neighbour list.  The (commonVertex) provides the xy coordinates of the shared Voronoi Edge associated with the particle to neighbour vector (used in BondOrderParameter[]).

box->{}=This parameter can be set to a polygon to remove the automatic detection of the box.:
edge->ToSting[NoEdge]=Applies the appropriate edge corrections to the image.  The default option is ToString[NOEDGE], which automatically removes Voronoi Cells that lie on the box edge.  Other implementations include: edge->ToString[pbc] which applies periodic boundary conditions, and edge->ToString[image] which generates virtual particles outside the box such that the resulting Voronoi cells are confined (or clipped) to the box.:
full->False=When used in conjuction with edge->(pbc/image), the returned list contains only the Voronoi Polyhedra for the input data2Din.  Setting full->True is needed for many other applications. Functions that need Delaunary neighbours will over-ride the user setting if information is needed.:
fast->True=This option sets a quick parameter for solving virtual boundary particle. The default True limits the distance to which edge particles are periodically(imageodically) traslated to approximately 1.4 times the particle radius.  Option fast->False, applies a full boundary estimation, resulting in Voronoi Tessellation of about 9*Length[data2Din] (making it much slower).  Use only when the results show obviously improper boundary conditions or in low density systems.:
debug->False=Developer option - do not use - writes out specific messages when key routines are solved.
This based on Mathematica's VoronoiMesh[] and will NOT work if the $VersionNumber \[LessEqual] 9 ."

PlotVoronoiTessellations::usage="PlotVoronoiTessellations[ data2Din_, OptionsPattern[] ]:= Returns a Graphics of the  VoronoiCells[] that has been colour coded to associate disorder metrics to the planar arrangements.  
OptionsPattern=(type\[Rule]ToString[vor],symmetry\[Rule]6,cells\[Rule]{},points\[Rule]True,scalebar\[Rule]False,stats\[Rule]ToString[Count],boundary\[Rule]ToString[NOEDGE],box\[Rule]{})

type->ToString[voronoi]=The Main variable that changes the information provided. Option: type\[Rule]ToString[voronoi] Uses the areas of the Voronoi Cells to calculate an expected hexagonal spacing with the same intensity (shortname ToString[vor]). Option: type\[Rule]ToString[coordination] Uses the number of facets each individual Voronoi polygon has (shortname ToString[coord]).  Option:  type\[Rule]ToString[BondOrder] Uses the neighbours -as defined by the Delaunaly Triangulation- to calculate the BondOrderParameter[].  Default symmetry is (symmetry->6) but can be manually set to any variable.

symmetry -> 6 : Sets the symmetry number associated with the BondOrderParameter[]. No effect with any (type).

cells -> {} : This option provides a way to manually pass VoronoiCells[] that have been precalculated.  It is meant to save time by passing the automatic VoronoiCells[] call for a large systems that might be run many times through this function.

points -> True : A graphics option that includes the centroids of the particles on the final plot.  These can be turned off with this option.

scalebar -> False : An option to include the scalebar assocciated with the colour code of (type).

stats -> ToString[Count] : Sets the type of stats passed into VoronoiHistogram[].  Recommended settings - ToString[Count] or ToString[Probability].  See 'hspec' Option for Histogram[].

boundary -> ToString[NoEdge] : Settings for boundary corrections.  Default boundary\[Rule]ToString[NoEdge] automatically removes any Voronoi Cells that lay on or outside of (box).  Options include: boundary\[Rule]ToString[periodic] includes edge Voronoi polygons by adding periodically translated neighbours outside of the box (shortname pbc).  boundary\[Rule]ToString[image] includes edge Voronoi polygons by adding virtual particles that produce Voronoi cells with facets exactly on the edge of (box).  This effectively truncates or clips the edges of the Voronoi cells to the (box). (shortname img).

box -> {} :  Explicitly defines the edges of a box.  Note! - setting a polygon will not change the (boundary), as it will default boundary\[Rule]ToString[NoEdge] which autodetects the box from the data.  Remember to set a (boundary) option."


VoronoiHistogram::usage="VoronoiHistogram[ inVoronoiCells, OptionsPattern[] ]=Returns a Histogram from the information in VoronoiCells[]. 
OptionsPattern=type\[Rule]ToString[vor], stats\[Rule]ToString[Probability], scalebar\[Rule]True, sym\[Rule]1, bopdata\[Rule]Null."


AverageFirstNeighbourDistance::usage="AverageFirstNeighbourDistance[data2d_,numberOfNN___]: Returns the average EuclideanDistance[] of numberOfNN neighbours.  Default is set to 1 neighbour."

FirstPeakValue::usage="FirstPeakValue[ List{x,f[x]} ]=Finds the x-position of the first peak. Typically used with PairCorrelationFunction[]."

ExpectedHexagonalDiameter::usage="ExpectedHexagonalDiameter[data2d, OptionsPattern[]]=Solves the VoronoiCells[] for a system, the calculates the expected hexagonal diameter with the same number density. The Formula to calculate the radius of a particle:  r[a_]:=Sqrt[a/(6 Tan[Pi/6])].  Output={2 Mean[ r[areaDistribution] ], Variance[ r[areaDistribution] ]}.
OptionsPattern[]=(boundary->ToString[NoEdge],box->{})"


ImageDiffractionPattern::usage="ImageDiffractionPattern[img]:  Calculates the planar FFT on the image.  Returns the diffraction pattern as a Graphic"

DiffractionPatternFromPosition::usage="DiffractionPatternFromPosition[data2d_]:  Calculates the planar FFT from a set of coordinates.  Makes no assumptions on boundary conditions."


BondOrderParameter::usage="BondOrderParameter[ unBoundedData2D , whichL , OptionsPattern[] ]= Calculates the Bond Order using Delaunay triangulation for neighbour definition.  Defaults are to remove edge particles and apply weighting of (voronoiBondFacet/totalVoronoiPerimeter) to every bond.

OptionsPattern=passVor\[Rule]{},fast\[Rule]True,edge->ToString[NoEdge],vorbop\[Rule]True,box\[Rule]{},normalized\[Rule]False,debug\[Rule]False. "

BondOrderPointCloud::usage="BondOrderPointCloud[ data2D , OptionsPattern[] ]=Creates a 2d probability map of the relative angular separation and radial distance for all first neighbours of each particle. It is similar to a planar map of the pair correlation function [g(r)]  where only the first neighbour shell is considered.

OptionsPattern=mod\[Theta]\[Rule]1,poly\[Rule]{},box\[Rule]{},simlist\[Rule]{},growthR\[Rule]{},debug\[Rule]False."


CoordinationNeighbourCloud::usage=" CoordinationNeighbourCloud[ data2D , OptionsPattern[] ]=Creates a 2d probability map of the relative angular separation and radial distance for only first neighbours for only the subsection of particles that share a similar coordination number. It is similar to a planar map of the pair correlation function [g(r)]  where only the first neighbour shell is considered.

OptionsPattern=passVor\[Rule]{},fast\[Rule]True,coord\[Rule]1,mod\[Rule]1,poly\[Rule]{},simstepList\[Rule]{},growthR\[Rule]{}"





(*----------------------------------------------------------------------*)
PlotFinalConfiguration::usage= "PlotFinalConfiguration[ Configuration, polygon, SimStep, GrowthRate, box , ToString[Type], args___ ]:  Takes a set of 2D
points {x,y,\[Theta]} of a polygon packing and generates the final configuration.  Uses the size as SimStep * 10^-5. Type can be: Automatic, STM.  If type = BOP or Angle, args can be specified as an integer that represents Q_arg or Mod[arg].
=========================================================  \n \n"


colourFunction::usage="colourFunction[data2D_,type_,box_, OptionsPattern[]] and \n Options[colourFunction]={bop\[Rule]Null,voronoiArea\[Rule]Null,edge\[Rule]ToString[pbc],arg\[Rule]Null}"

PlotPointCloud::usage= "PlotPointCloud[ Ensemble ]:  Takes a set of 2D points {x,y,\[Theta]} of ensembles. This auto-detects the rotation angle and removes it for proper plotting.  The size of the points are also automatically generated. 
=========================================================  \n
 \n"

PlotPlanarDensity::usage= "PlotPlanarDensity[ Ensemble ]:  Takes an ensemble set of 2D points: {{x,y,\[Theta]},...,{{x,y,\[Theta]}}} of the final configurations and computes the probability map. 
=========================================================  \n 
\n"

PlotRegistryMap::usage= "PlotRegistryMap[ Ensemble,  VeiwingDimension ]:  Takes a set of 2D points {x,y,\[Theta]} of the final configurations and computes the Registry Map of most common positions.  For 2D planar view, use VeiwingDimension=2. For 3D histograms, use VeiwingDimension=3.
=========================================================  \n
 \n"

PackingFraction::usage="PackingFraction[ Configuration, polygon, SimStep, GrowthRate, box ]: 
 Takes a set of 2D points of a final configuration {x,y,\[Theta]}, and inflates the monodisperse polygons {poly} to a size of (1.E-5)*SimStep .  The confining area is defined by the {box}. \n
Example: Packing Randomly Placed Circles inside a 1x1 box.
PackingFraction[RandomReal[1,{10,3}], Table[{Cos[x],Sin[x]},{x,2Pi,0,-2Pi/30}],RandomInteger[12345],\!\(\*SuperscriptBox[\(10\), \(-5\)]\),{{0,0},{1,0},{1,1},{0,1},{0,0}}]
=========================================================  \n
 \n"

OverlapNetwork::usage= "OverlapNetwork[ Configuration, polygon, SimStep, inflateRatio, box] - 
 is a set of functions that inflates the polygons past their final configuration and calculates the amount of overlap in the system.  The networks are plotted overtop of the final configurations with the size and colour of the line being proportional to that amount of overlap.  

Uses a series of Functions -> Inflate , FindNearestForOverlap, PolygonsThatOverlap
=========================================================  \n
 \n"


sortByAngle::usage= "sortByAngle[ polygon ] - takes a set of points and sorts them by angle to form a closed polygon. 
=========================================================  \n
 \n"

Inflate::usage= "Inflate[ Configuration, polygon, SimStep, GrowthRate, OverlapScale ] - 
 takes a set of centroids {x,y} and inflates the polygons at those positions
 a certain percentage starting at Simstep. 

For example, OverlapScale = 1.30 inflates the objects by 130%. 
=========================================================  \n
 \n"

FindNearestForOverlap::usage= "FindNearestForOverlap[ pointPattern2D ] - 
takes a set of centroids {x,y} and finds the nearest N number of closest neighbours.  If the number of centroids are less than 18, it sets that to the maximum number of neighbours.  If it is higher than 18, then it sets 18 as the max neighbours. 
=========================================================  \n
 \n"

PolygonsThatOverlap::usage= "PolygonsThatOverlap[ polys, nearestN ] :
takes a set of inflated polygon points and the nearest neighbour list and creates a list of all the particles that overlap.  The nearest neighbour list is required to speed up the estimation, suggesting that only the closets neighbours overlap with eachother.
=========================================================  \n
 \n"

ADOsort::usage= "ADOsort[ Configuration ] - Sorts the configuration to allow for proper Pattern Matching. Returns a list degeneracy array map with the first entry being the begining parcel configuration, with an array 
=========================================================  \n
 \n"

pcSize::usage="Used to select point size for PointClouds."

GeneralizedPBCLocalVoronoiBondOrder::usage="Test"


ApplyBoxSymmetryToEnsemble::usage="ApplyBoxSymmetryToEnsemble[Ensemble_,modNumber___]: Creates an ensemble by rotating/flipping according to the symmery of the 1x1 box. "
ParcelBySimilarity::usage="ParcelBySimilarity[Ensemble_,ConfidenceRange_,SymRot___]:= Leave SymRot out (or use Null to avoid)."
SortParcelPositions::usage="SortParcelPositions[Ensemble_,parcels_]:= Send the ensemble {{x,y,\[Theta]}..} with results from ParcelBySimilarity[] to sort the patterns into the same orientation. Will return a Table that is similar to Ensemble."

LoneNormCalc::usage="Used for Pattern Matching"
QuickDistanceSimilarityCheckQ::usage="Used for Pattern Matching"
QuickDistanceSimilarityCheckQuiet::usage="Used for Pattern Matching"
SimNormCalc::usage="Used for Pattern Matching"
QuickSimilarityCheckQ::usage="Used for Pattern Matching"
QuickSimilarityCheckQuiet::usage="Used for Pattern Matching"
QuickSimilarityMax::usage="Used for Pattern Matching"

FindMaxProbabilityFromList::usage="FindMaxProbabilityFromList[LIST, args___] := Possible args include 'all' and 'random' to return all found and a random choice"
PdfDensityDistribution::usage="PdfDensityDistribution[]:="
ParticleDensityDistribution::usage="ParticleDensityDistribution[pf_,pfNames_,args___]"
AreaFractionDistribution::usage="ParticleDensityDistribution[pf_,OptionsPattern[]] : Options[AreaFractionDistribution]={labels\[Rule]{},colours\[Rule] RegularPolygonColourScheme}=Generates Probability DistributionChart that is formatted to look nice."

RotationHistogram::usage="RotationHistogram[Ensemble_,Args___]"
RotationWheel::usage="RotationWheel[Ensemble_,Args___]"
PbcDelaunayNeighbours::usage="PbcDelaunayNeighbours[unBoundedData2D_,imagebox_,sym_]"

ApplyLimitedPBC::usage="ApplyLimitedPBC[data2d_, box_ ,extraArea___]"


vorColourer::usage="vorColourer[colNum, OptionsPattern[] ] "
binGen::usage="Does Even less"

Begin["`Private`"]


compilerCheckerLongNameVeryConfusing123Variable = False;
Quiet@Check[Compile[{{secretTinyLilvar}},1,CompilationTarget->"C" ]; compilerCheckerLongNameVeryConfusing123Variable = False;, compilerCheckerLongNameVeryConfusing123Variable=True;];

Off[Compile::nogen];
Off[CCompilerDriver`CreateLibrary::nocomp]

If[compilerCheckerLongNameVeryConfusing123Variable == True,Print@"No C compiler has been found. Certain functions will be compiled in Wolfram instead. (No action required).";,Print@"Your C compiler has been succesfully located and will be used to decrease runtime of certain functions.";];
Clear[compilerCheckerLongNameVeryConfusing123Variable];







Options[vorColourer]= {colourGrad->{purpleBlend,greenBlend}, bind->True, binDstr->Table[i/10,{i,11}]  };
SetOptions[vorColourer, colourGrad->{purpleBlend,greenBlend}, bind->True, binDstr->Table[i/10,{i,11}]  ]; 
vorColourer[ colNum_,  OptionsPattern[]] := Block[{colcount,  returnCol, binType = OptionValue[bind], 
colourFunc1 = OptionValue[colourGrad][[1]],
colourFunc2 = OptionValue[colourGrad][[2]],
binDist = OptionValue[binDstr]
},

If[binType==True,
Do[  If[  binDist[[colcount]]  <= colNum<binDist[[colcount+1]] , 

If[Mean[   { binDist[[colcount]]  ,binDist[[colcount+1]] }        ]<=1,


 returnCol=     colourFunc1[ Mean[   { binDist[[colcount]]  ,binDist[[colcount+1]] }        ]]; Break[];           
 
 
 ]      ;     



If[Mean[   { binDist[[colcount]]  ,binDist[[colcount+1]] }        ]>1,

returnCol= colourFunc2[    Mean@ { (binDist[[colcount]]-1 )/0.1 , (binDist[[colcount+1]]  -1 ) /0.1   }               ] ; Break[];      

]
];

,{colcount, Length@binDist-1}]; 

If[Max@binDist<=1 && colNum>Max@binDist, returnCol = colourFunc1[Mean@{Max@binDist,1}]];

(*If[colNum==1, returnCol= colourFunc1[1]];*)

If[colNum>=Max@binDist,
returnCol=colourFunc2[1]
]; ,

returnCol = Which[ colNum<=1, returnCol = colourFunc1[colNum], 1<colNum<=1.1, returnCol = colourFunc2[(colNum-1)/0.1], colNum>1.1, returnCol = colourFunc2[1] ]

];

Return[returnCol]

]

Options[binGen]={ stx->0,mxx->1.1};
binGen[dw_, OptionsPattern[]] := Block[{ returnB,endn,st=OptionValue[stx],mx = OptionValue[mxx]},

		endn=0;
		While[ st + dw*endn<mx, 
			endn++;
			
		    ];
		
		returnB = Table[st + n*dw, {n,0,endn}];
		
		Return[returnB]

] 

(* Scale Bars for Voronoi Visualizations *)
Options[ScaleBars]={sym->6,minmax-> {3,8}, bopOps->{True, {purpleBlend,greenBlend}, Table[ 0.1*nn, {nn,11}] }};
ScaleBars[type_, OptionsPattern[] ]:=Block[{
voronoiColourRange,bopColourRange,angleColourRange,coordColourRange,i,returnedBar={},
coordOffset,
symmetryO=OptionValue[sym],
minmaxO=OptionValue[minmax],
bopOs = OptionValue[bopOps]
},

Which[
(*---------------------*)
TypeVoronoiQ[type],

voronoiColourRange={5,10,20,30,40,100};
returnedBar = BarLegend[
{VoronoiCellColourScheme[[1;;7]],{0.0,100.}},
{0,5,10,15,20,25,30},
LegendMarkerSize->200,
LabelStyle->Directive[FontSize->14],
LegendMargins->0,
LegendLabel->"   Percent (%) \n off Hexagonal"
],

(*---------------------*)
TypeBondOrderQ[type], (*bopscb*)



If[bopOs[[1]]==True && Length@bopOs==3,

If[bopOs[[3]] =={}, bopOs[[3]]= Table[aa/10,{aa,10}]];
bopColourRange= vorColourer[#,bind->bopOs[[1]] ,colourGrad->bopOs[[2]], binDstr-> bopOs[[3]]   ] &/@ Table[ Mean@ bopOs[[3]][[aa;;aa+1]], {aa, Length@bopOs[[3]]-1} ] ;

returnedBar = BarLegend[

{bopColourRange,  If[  bopOs[[1]]==True && Length@bopOs==3, {Min@bopOs[[3]], Max@bopOs[[3]] }  ,{0,1} ]      }, bopOs[[3]],

LabelStyle->Directive[FontSize->14],
LegendMarkerSize->200,
LegendMargins->0,
LegendLabel->"Bond Order \nParameter"
]  ,


returnedBar = BarLegend[

{vorColourer[#, bind->False, colourGrad->bopOs[[2]]]& , {0,1.1} }, 
Ticks-> Table[aa/10//N,{aa,10}] ,
LabelStyle->Directive[FontSize->14],
LegendMarkerSize->200,
LegendMargins->0,
LegendLabel->"Bond Order \nParameter"
];

returnedBar = First@Cases[ToBoxes@returnedBar,gr_GraphicsBox:>ToExpression[gr],\[Infinity]]
 
   ];



 ,

(*---------------------*)
ToUpperCase[type]==ToUpperCase["OVER-BOP"],

bopColourRange=Table[BondOrderColourScheme[x,symmetryO],{x,0,1.1,0.1}];
returnedBar = BarLegend[
{bopColourRange,{0.0,1.1}},
Ticks->Table[i,{i,0.,1.1,0.2}],
LabelStyle->Directive[FontSize->14],
LegendMarkerSize->200,
LegendMargins->0,
LegendLabel->"Bond Order \nParameter"
]   
    
     
       ,


(*---------------------*)
TypeCoordinationQ[type],
If[minmaxO[[2]]-minmaxO[[1]]<= 3, minmaxO= {minmaxO[[2]]-3,minmaxO[[2]]} ];
If[minmaxO[[1]]<=   3,minmaxO[[1]]=3;coordOffset=+1;,coordOffset=0;];
coordColourRange=(minmaxO[[1]]-1+Range[minmaxO[[2]]-1]);

returnedBar = BarLegend[{(CoordinationNumberColourScheme/@(coordColourRange+coordOffset)),{minmaxO[[1]]-0.5,minmaxO[[2]]+0.5}},coordColourRange-0.5,LegendMarkerSize->200,LabelStyle->Directive[FontSize->14],LegendMargins->10,
LegendLabel-> "Local Voronoi\nCoordination",
"Ticks"->({#,#}&/@coordColourRange),If[minmaxO[[2]]-minmaxO[[1]]>6,Unevaluated[Sequence[]],
"TickLabels"->(Style[#,14,CoordinationNumberColourScheme[#+1]]&/@(coordColourRange))]
] ,


(*---------------------*)
TypeAngularDirectionQ[type],

angleColourRange=Table[x,{x,0, 2 Pi/(symmetryO),2Pi/(4symmetryO)}];
returnedBar= BarLegend[{BjornColourScheme,{0.,2.Pi/symmetryO}},
angleColourRange,
"Ticks"->angleColourRange,
LegendMargins->0,
LegendLabel->"Angle"]
];

Return[returnedBar]
];





(* Generic Functions - Multipurpose *)
closedBindingBox:={{0.,0.},{1.,0.},{1.,1.},{0.,1.},{0.,0.}};
bindingBox:={{0.,0.},{1.,0.},{1.,1.},{0.,1.}};
rectangleBindingBox[a_,b_]:=Block[{},closedBindingBox . {{a,0},{0,b}}];
standardPolygon[numberOfSides_]:=Block[{},N@Table[{Cos[\[Theta]],Sin[\[Theta]]},{\[Theta],0,2Pi,2Pi/numberOfSides}]];
PolyArea=Compile[{ {poly,_Real,2}}, 
Block[{Xi,Yi,xR,yR},  
{Xi,Yi}=Transpose@poly;
xR = RotateRight@Xi;
yR=RotateRight@Yi;
Abs[(Total[xR*Yi]  - Total[Xi*yR])/2.] ],CompilationTarget->"C"  ];
(*(* Returns "True" or "False" depending on if it is inside the polygon *)
(* http://mathematica.stackexchange.com/questions/9405/how-to-check-if-a-2d-point-is-in-a-polygon *)*)
inPolyQ2= Compile[{{poly,_Real,2},{x,_Real},{y,_Real}},
Block[{Xi,Yi,Xip1,Yip1,u,v,w}, 
{Xi,Yi}=Transpose@poly;
Xip1=RotateLeft@Xi;
Yip1=RotateLeft@Yi;
u=UnitStep[y-Yi];
v=RotateLeft@u;
w=UnitStep[-((Xip1-Xi)(y-Yi)-(x-Xi)(Yip1-Yi))];
Total[(u (1-v)(1-w) - (1-u) v w)] !=0],CompilationTarget->"C",RuntimeOptions->"Speed"];
negQ[num_]:= Block[{},If[num< 0,True,False]];
hexToRGB=RGBColor@@(IntegerDigits[#~StringDrop~1~FromDigits~16,256,3]/255.)&;
standardGrowthRate:=10^-5;
HexagonArea[r_]:= Block[{},(2*Sqrt[3])r^2];
HexagonalDiameter[n_, box_]:=Sqrt[2/((n/PolyArea[box]) Sqrt[3])];
(* This is the main function that moves the particle centroids! *)
Hexaticize[d_,dr_]:=d+RandomVariate[NormalDistribution[0.0,dr],{Length[d],Length[d[[1]]]}];

(* Boundary Conditions - Solving Boxes and Applying Virtual Particles *)
AutoBoxTransform[indata2D_]:=Block[{b,ba,data2D},
data2D=indata2D[[All,1;;2]];
b=rectangleBindingBox[Sequence@@(EuclideanDistance[Max[#],Min[#]]&/@Transpose[data2D])];
b=TranslationTransform[-RegionCentroid[Polygon[b]]][b];
ba=TranslationTransform[RegionCentroid[ConvexHullMesh[data2D[[;;,1;;2]]]]][(1.+(1/Sqrt[3./Sqrt[2.]]/Sqrt[Length[data2D]] )) b];
ba];
MinMaxBox[inPoints_ ] :=Block[{dh,minX,minY,maxX,maxY},

minX=Min[inPoints[[;;,1]]];
minY=Min[inPoints[[;;,2]]];
maxX=Max[inPoints[[;;,1]]];
maxY=Max[inPoints[[;;,2]]];

dh= ((maxX+minX) +(maxY+minY))/((maxX - minX) +(maxY- minY));
If[maxX <1||maxY<1, dh=dh-1];
{{minX,minY}-{dh,dh},{minX,maxY}+{-dh,dh},{maxX,maxY}+{dh,dh},{maxX,minY}+{dh,-dh}}
];
Options[ApplyPeriodicBoundary]= {SetDistance->1,ReturnDim->2};
ApplyPeriodicBoundary[inData2d_,box_, OptionsPattern[] ]:=Block[{dx= OptionValue[SetDistance],initPoints,pbcPoints,xLow,xHi,yLow,yHi,FullPbcPoints,np,xmin,xmax,ymin,ymax,SetDistance,Dim,ReturnDim=OptionValue[ReturnDim],data2d,dataInsideBox},


(* Reject any particles outside the box - for numerical accuracy *)
dataInsideBox=Pick[inData2d,
MapThread[inPolyQ2[box,#1,#2]&,Transpose[inData2d[[All,1;;2]]]]];

np=Length[dataInsideBox];

If[np!=Length[inData2d],Print["Warning! Data lies outside box region and will be clipped! (ApplyPeriodicBoundary[])"];];

(* Apply 3d scheme to 2d data - include zero rotation of particles *)
Dim = Last[Dimensions[inData2d]];
If[Dim==2,data2d=PadLeft[# ,3,0.,1] &/@dataInsideBox ];
If[Dim== 3,data2d=dataInsideBox[[;;,1;;Dim]];];

(* Periodic basis vectors assigned from box *)
xmin=Min[box[[All,1]]  ];
xmax=Max[box[[All,1]]  ];
ymin=Min[box[[All,2]]  ];
ymax=Max[box[[All,2]]  ];


(* Extented distance outside the box to include pbc effects *)
If[dx<0,dx=Sqrt[PolyArea[box]/np]];

xLow=xmin+dx;
xHi=xmax - dx;
yLow=ymin + dx;
yHi=ymax - dx;

(* Routines to append pbc virtual particles outside the box *)
pbcPoints={};

 Do[If[data2d[[x,1]]<=xLow,AppendTo[pbcPoints,data2d[[x,1;;3]]+{xmax,ymin,0.}   ]] ,{x,1,np}];
 Do[If[data2d[[x,2]]<=yLow,AppendTo[pbcPoints,data2d[[x,1;;3]]+{xmin,ymax,0.}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]>= xHi,AppendTo[pbcPoints,data2d[[x,1;;3]]-{xmax,ymin,0.}   ]] ,{x,1,np}];
 Do[If[data2d[[x,2]]>=yHi,AppendTo[pbcPoints,data2d[[x,1;;3]]-{xmin,ymax,0.}   ]] ,{x,1,np}];

 Do[If[data2d[[x,1]]<=xLow && data2d[[x,2]]<= yLow,AppendTo[pbcPoints,data2d[[x,1;;3]]+{xmax,ymax,0.}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]>= xHi && data2d[[x,2]]>=  yHi,AppendTo[pbcPoints,data2d[[x,1;;3]]-{xmax,ymax,0.}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]<=xLow && data2d[[x,2]]>=  yHi,AppendTo[pbcPoints,data2d[[x,1;;3]]+{xmax,-ymax,0.}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]>= xHi&& data2d[[x,2]]<= yLow,AppendTo[pbcPoints,data2d[[x,1;;3]]+{-xmax,ymax,0.}   ]] ,{x,1,np}];

Flatten[{data2d[[All,1;;ReturnDim]],pbcPoints[[All,1;;ReturnDim]]},1]
];

Options[ApplyImageChargeBoundary]= {SetDistance->1,ReturnDim->2};
ApplyImageChargeBoundary[inData2d_,box_,OptionsPattern[]]:=Block[{dx=OptionValue[SetDistance],Dim=OptionValue[ReturnDim],icbPoints,FullIcbPoints,np,xmin,xmax,ymin,ymax,bMinX,bMaxX,bMinY,bMaxY,SetDistance,dataInsideBox,data2d},

(* Reject any particles outside the box - for numerical accuracy *)
dataInsideBox=Pick[inData2d,
MapThread[inPolyQ2[box,#1,#2]&,Transpose[inData2d[[All,1;;2]]]]];
np=Length[dataInsideBox];

If[np!=Length[inData2d],Print["Warning! Data lies outside box region and will be clipped! (ApplyImageChargeBoundary[])"];];

icbPoints ={};
np=Length[dataInsideBox];

(* Basis vectors for reflection trasformation at wall *)
xmin=Min[box[[All,1]]  ];
xmax=Max[box[[All,1]]  ];
ymin=Min[box[[All,2]]  ];
ymax=Max[box[[All,2]]  ];

bMinX=xmin+  dx;
bMaxX=xmax -  dx;
bMinY=ymin +  dx;
bMaxY=ymax -  dx;

icbPoints=Flatten[ Reap[
MapThread[
(*-------------- Center Left *)
(If[#1<=  bMinX,Sow[{xmin+(xmin-#1),#2}  ]] ;
(*-------------- Center Right *)
If[#1>= bMaxX,Sow[{xmax+(xmax-#1),#2}    ]] ;
(*-------------- Bottom Center *)
If[#2<=bMinY,Sow[{#1,ymin+(ymin-#2)}   ]] ;
(*-------------- Top Center *)
If[#2>=bMaxY,Sow[{#1,ymax+(ymax-#2)}   ]] ;

(*-------------- Bottom Left *)
If[#1<=bMinX &&#2<= bMinY,Sow[{xmin-(#1-xmin),ymin-(#2-ymin)}  ]] ;
(*-------------- Top Right *)
If[#1>= bMaxX && #2>=  bMaxY,Sow[{xmax+(xmax-#1),ymax+(ymax-#2)}  ]] ;
 (*-------------- Top Left *)
If[#1<=bMinX && #2>=  bMaxY,Sow[{xmin-(#1-xmin),ymax+(ymax-#2)}  ]] ;
(*-------------- Bottom Right *)
If[#1>= bMaxX&& #2<= bMinY,Sow[{xmax+(xmax-#1),ymin-(#2-ymin)}  ]];)& ,Transpose@dataInsideBox];][[2]],1];

FullIcbPoints=Flatten[{dataInsideBox[[;;,1;;2]],icbPoints},1]
];
ApplyGeneralPBC[data2d_,box_, numNN___]:=Block[{dx,pbcPoints,bmin,bmax,FullPbcPoints,np,xmin,xmax,ymin,ymax,Nnn},
pbcPoints ={};
np=Length[data2d];

xmin=Min[box[[All,1]]  ];
xmax=Max[box[[All,1]]  ];
ymin=Min[box[[All,2]]  ];
ymax=Max[box[[All,2]]  ];

dx= Sqrt[2./ (Sqrt[3.]    (np /  PolyArea[box])   )];
If[numNN=!=Null,Nnn=numNN];
If[numNN===Null,Nnn= 1.4 ];

If[ymax>= xmax, bmin= ymin+ Nnn dx ,bmin= xmin+Nnn dx ];
If[ymax>= xmax, bmax=ymax-Nnn dx,bmax=xmax-Nnn dx];


 Do[If[data2d[[x,1]]<=bmin,AppendTo[pbcPoints,data2d[[x,1;;2]]+{xmax,ymin}   ]] ,{x,1,np}];
 Do[If[data2d[[x,2]]<=bmin,AppendTo[pbcPoints,data2d[[x,1;;2]]+{xmin,ymax}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]>= bmax,AppendTo[pbcPoints,data2d[[x,1;;2]]-{xmax,ymin}   ]] ,{x,1,np}];
 Do[If[data2d[[x,2]]>=bmax,AppendTo[pbcPoints,data2d[[x,1;;2]]-{xmin,ymax}   ]] ,{x,1,np}];

 Do[If[data2d[[x,1]]<=bmin && data2d[[x,2]]<= bmin,AppendTo[pbcPoints,data2d[[x,1;;2]]+{xmax,ymax}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]>= bmax && data2d[[x,2]]>=  bmax,AppendTo[pbcPoints,data2d[[x,1;;2]]-{xmax,ymax}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]<=bmin && data2d[[x,2]]>=  bmax,AppendTo[pbcPoints,data2d[[x,1;;2]]+{xmax,-ymax}   ]] ,{x,1,np}];
 Do[If[data2d[[x,1]]>= bmax&& data2d[[x,2]]<= bmin,AppendTo[pbcPoints,data2d[[x,1;;2]]+{-xmax,ymax}   ]] ,{x,1,np}];

FullPbcPoints=Flatten[{data2d[[;;,1;;2]],pbcPoints},1]

];

(* Building Lattices - Hexagonal and Square *)
Options[SquareLattice]={n->400,box-> closedBindingBox};
SetOptions[SquareLattice,n->400,box-> closedBindingBox];
SquareLattice[OptionsPattern[]]:=Block[
{ \[CapitalDelta],intensity,xmin,ymin, xmax,ymax,square,rot,f,\[Phi],\[Theta],b,ba,
nO= OptionValue[n],
boxO=OptionValue[box]
},
xmin=Min[boxO[[All,1]]  ];
xmax=Max[boxO[[All,1]]  ];
ymin=Min[boxO[[All,2]]  ];
ymax=Max[boxO[[All,2]]  ];


(*intensity=n/((xmax-xmin)*(ymax-ymin));*)
intensity= nO/ Area[ Polygon[boxO]];
\[CapitalDelta]=Sqrt[1./(intensity)];

square=(\[CapitalDelta])Flatten[Table[{x,y}, {x,0.5,Floor@Sqrt@nO},{y,0.5,Floor@Sqrt@nO}],1];

Pick[square,(inPolyQ2[boxO,#[[1]],#[[2]]  ]&/@  square)]

(*square*)
];
Options[HexagonalLattice]={n->400,box-> closedBindingBox,radius->1.,periodic->False};
HexagonalLattice[OptionsPattern[]]:=Block[
{ \[CapitalDelta],x,y,pa,pb,xmin,ymin, xmax,ymax,hex,hexIn,hexPBC,rot,f,\[Phi],\[Theta],i=0,intensity,bigbox,
nO= OptionValue[n],
boxO=OptionValue[box],
radius=OptionValue[radius],
periodic=OptionValue[periodic]
},


xmin=Min[boxO[[All,1]]  ];
xmax=Max[boxO[[All,1]]  ];
ymin=Min[boxO[[All,2]]  ];
ymax=Max[boxO[[All,2]]  ];

intensity= nO / Area[ Polygon[boxO]];
\[CapitalDelta]=Sqrt[2./(intensity*Sqrt[3.])];


If[periodic==True,

bigbox={{xmin-Abs[xmax],ymin-Abs[ymax]},{xmax+Abs[xmax],ymin-Abs[ymax]},
{xmax+Abs[xmax],ymax+Abs[ymax]},{xmin-Abs[xmax],ymax+Abs[ymax]}};

(* Oblique Section of Hexagonal *)
x=Table[a,{a,xmin-Abs[xmax],xmax+Abs[xmax], \[CapitalDelta]}];
y=Table[a,{a,ymin-Abs[ymax],ymax+Abs[ymax],Sqrt[3.] \[CapitalDelta]}];
pa=Tuples[{x,y}];

(* Shifted Section *)
x=Table[a,{a,xmin+(\[CapitalDelta]/2.)-Abs[xmax],xmax+Abs[xmax], \[CapitalDelta]}];
y=Table[(\[CapitalDelta] Sqrt[3.]/2.)+(\[CapitalDelta] Sqrt[3.])a  -Abs[xmax],{a,ymin,Ceiling[Length[x/2]]}];
pb=Tuples[{x,y}];

(* Assign Hexagonal Pattern *)
hex=Flatten[{pa,pb},1];
hex=Select[hex,inPolyQ2[bigbox,#[[1]],#[[2]] ]==True&];
hex=Sort[hex, inPolyQ2[boxO,#[[1]],#[[2]] ]==True& ];

];


If[periodic==False,


(* Oblique Section of Hexagonal *)
x=Table[a,{a,xmin,xmax, \[CapitalDelta]}];
y=Table[a,{a,ymin,ymax,Sqrt[3.] \[CapitalDelta]}];
pa=Tuples[{x,y}];

(* Shifted Section *)
x=Table[a,{a,xmin+(\[CapitalDelta]/2.),xmax, \[CapitalDelta]}];
y=Table[(\[CapitalDelta] Sqrt[3.]/2.)+(\[CapitalDelta] Sqrt[3.])a,{a,ymin,Ceiling[Length[x]/2]}];
pb=Tuples[{x,y}];

(* Assign Hexagonal Pattern *)
hex=Flatten[{pa,pb},1];

(* Redo If there is some kind of mis-alignment at the top *)
While[Length[hex]< nO,
i++;
x=Table[a,{a,xmin+(\[CapitalDelta]/2.),xmax+((\[CapitalDelta]/2.)), \[CapitalDelta]}];
y=Table[(\[CapitalDelta] Sqrt[3.]/2.)+(\[CapitalDelta] Sqrt[3.])a,{a,ymin,Ceiling[Length[x]/2]+i}];
pb=Tuples[{x,y}];hex=Flatten[{pa,pb},1];
];

(* Solve Optimization for Rotation of Lattice to obtain Best Estimate of points inside box *)
If[nO<200,
rot=RegionCentroid[Polygon[boxO]];

f[in\[Phi]_Real]:=
Abs[nO-Length[ 
Pick[hex,(inPolyQ2[boxO,#[[1]],#[[2]] ]&/@  (RotationTransform[in\[Phi],rot][hex]) ) ]]];

\[Phi]= NArgMin[f[\[Theta]],\[Theta]];

(* Returns the Lattice  *)
Pick[hex,(inPolyQ2[boxO,#[[1]],#[[2]]  ]&/@  (RotationTransform[\[Phi],rot][hex]))]
,hex]
(*hex*)
];

Pick[hex,(inPolyQ2[boxO,#[[1]],#[[2]]  ]&/@  hex)]

];

(* Pair Correlation Function Routines *)
(*http://forums.wolfram.com/mathgroup/archive/2009/Apr/msg00533.html*)
distancesWithEdgeCorrections=Compile[{{ll,_Real,2},{np,_Integer}},
Sort[Flatten[Table[If[k<j,
Sqrt[# . #]&[(ll[[k]]-ll[[j]])],-1]
,{j,Length[ll]},{k,np}]]]
,"RuntimeOptions"->"Speed",CompilationTarget->"C",Parallelization->True];
(*http://forums.wolfram.com/mathgroup/archive/2009/Apr/msg00533.html*)
radialDistributionC=Compile[{{dlist,_Real,1},{r,_Real},{eps,_Real}},Catch[Module[{len=Length[dlist],bot=r-eps,top=r+eps,lo=1,mid,hi},hi=len;
If[dlist[[lo]]>top||dlist[[hi]]<bot,Throw[0]];
mid=Ceiling[(hi+lo)/2.];
While[dlist[[lo]]<bot&&mid>lo,If[dlist[[mid]]<bot,lo=mid;mid=Ceiling[(hi+lo)/2.],mid=Floor[(mid+lo)/2.];]];
mid=Ceiling[(hi+lo)/2.];
While[dlist[[hi]]>top&&mid<hi,If[dlist[[mid]]>top,hi=mid;mid=Floor[(hi+lo)/2.],mid=Ceiling[(mid+hi)/2.];]];
Round[(hi-lo+1)/2]]],"RuntimeOptions"->"Speed",CompilationTarget->"C",Parallelization->True];
Options[PairCorrelationFunction]= { box->{},boundary->"trunc",lattice->"none",shells->4.25,distance-> {}};
SetOptions[PairCorrelationFunction, box->{},boundary->"trunc",lattice->"none",shells->4.25,distance-> {}];
PairCorrelationFunction[data2d_, OptionsPattern[] ]:=Block[{d, dr,density,pcf,ensemblePCF,bins,spacing,confData,n,nconfs,rList,volume,td,truncBox,
autobox,autodistance,maxdist=Infinity,singlePCF,xBoxLength,yBoxLength,
boxO= OptionValue[box],
edgeO= OptionValue[boundary],
latticeO=OptionValue[lattice],
shellsO=OptionValue[shells],
distanceO=OptionValue[distance]
},


(* If only one configuration, make into ensemble of size=1 *)
Off[TensorRank::rect];


If[TensorRank[data2d]==2,confData={data2d};,confData=data2d;];

On[TensorRank::rect]; 

nconfs=Length[confData];
If[boxO==={},autobox=True,autobox=False];
If[distanceO==={},autodistance=True,autodistance=False];


Do[
(*        Default  Options             *)
(* -----------------------------------*)
(* Box Not Set - Auto Select Container *)
If[autobox==True,
boxO=AutoBoxTransform[confData[[conf]]];
n=Length[confData[[conf]]];
,
n= Length[Sort[confData[[conf,All,1;;2]],inPolyQ2[boxO,#[[1]],#[[2]] ]==True&]];
];
spacing =1./(  Sqrt[n]);


(* Distance not set - Auto Select Shells *)
If[ autodistance==True||distanceO<= 0,
(*Print["d"];*)
 distanceO=Sqrt[PolyArea@boxO]  spacing shellsO;
(*Print[distanceO \[LessEqual]  maxdist];*)
(*If[distanceO \[LessEqual]  maxdist, maxdist= distanceO;Print[maxdist];];*)
];

dr=(( distanceO  spacing /(2shellsO)));
(* -----------------------------------*)

(* Create List with normalized bins between configurations *)
rList=Table[r,{r,dr,( distanceO ),dr}];
If[Max[rList] <   maxdist,maxdist= Max[rList];(*Print[ Max[rList]];*)];
volume=PolyArea[boxO];




(* Selection for Edge Corrections *)
Which[

(* Use Periodic Hexagonal Lattice *)
ToUpperCase[latticeO]==ToUpperCase["hex"]||ToUpperCase[latticeO]==ToUpperCase["square"],
n=Length[Select[confData[[conf]],inPolyQ2[boxO,#[[1]],#[[2]] ]==True&]];
td=confData[[conf,All,1;;2]];,


(* Periodic Boundaries  *)
 ToUpperCase[edgeO]==ToUpperCase["periodic"] || ToUpperCase[edgeO]==ToUpperCase["pbc"],
td=ApplyPeriodicBoundary[confData[[conf,All,1;;2]],boxO,SetDistance->(distanceO dr/2),ReturnDim->2];,

(* Image Charge Boundaries  *)
ToUpperCase[edgeO]==ToUpperCase["image"]||ToUpperCase[edgeO]==ToUpperCase["hard"],
td=ApplyImageChargeBoundary[confData[[conf,All,1;;2]],boxO,SetDistance->(distanceO dr/2)];,

(* Apply -NO- Edge Corrections *)
ToUpperCase[edgeO]==ToUpperCase["none"]|| ToUpperCase[edgeO]==ToUpperCase["NoEdge"],
td=confData[[conf,All,1;;2]];,

(* Apply -NO- Edge Corrections *)
ToUpperCase[edgeO]==ToUpperCase["trunc"]|| ToUpperCase[edgeO]==ToUpperCase["Truncated"],

{xBoxLength,yBoxLength}=(EuclideanDistance@@@Partition[boxO,2,1])[[1;;2]];
truncBox=First[ScalingTransform[{( 2*distanceO-xBoxLength)/xBoxLength,(2*distanceO-yBoxLength)/yBoxLength}, RegionCentroid[Polygon[#]]][#]&/@{boxO}];


n=Length[Select[confData[[conf]],inPolyQ2[truncBox,#[[1]],#[[2]] ]==True&]];
volume=PolyArea[truncBox];
td=Sort[confData[[conf,All,1;;2]],inPolyQ2[truncBox,#[[1]],#[[2]] ]==True&];

];


d=Select[distancesWithEdgeCorrections[td,n ],#>0.0&];
pcf=Table[(volume) radialDistributionC[d,r,dr]/ ( Pi (n(n-1)) r  dr),{r,dr,( distanceO   ),dr}];

singlePCF=Interpolation[Prepend[Transpose[{rList,pcf}],{0.,First[pcf]}],InterpolationOrder->1];
If[conf==1, ensemblePCF=Interpolation[Prepend[Transpose[{rList,0.0pcf}],{0.,0.0}],InterpolationOrder->1];];
ensemblePCF=Interpolation[Table[{x,ensemblePCF[x]+singlePCF[x]},{x,0.,maxdist,maxdist/1024.}],InterpolationOrder->1];

,{conf,1,nconfs}];

(* Create Average of the Pair Correlation Functions *)
Table[{x,ensemblePCF[x]/nconfs},{x,0.,maxdist,maxdist/1024.}]

];
InterpolationFunctionDifference[reference_, diffFunct_ ,xMax___]:= Block[{interpdRef,interpdFunction,xMin,funct,dif,dx=1024,differencedFunction,interpolatedReference,interpolatedComparing,interpolationNumber=1,rms},
(* Interpolates the function to be subtracted from reference *)
interpdFunction=Interpolation[diffFunct,InterpolationOrder->interpolationNumber];
interpdRef= Interpolation[reference,InterpolationOrder->interpolationNumber];
(*interpdFunction=Interpolation[reference];*)

(* Builds the difference function at the x-positions of reference *)
(* Solves this function at the x-positions of the reference *)
xMin=Max[{Min@reference[[;;,1]],Min@diffFunct[[;;,1]]  } ];

If[xMax===Null,
funct= Table[{reference[[x,1]],interpdFunction[reference[[x,1]]]-reference[[x,2]]},{x,Range@Length@diffFunct[[;;,1]]}];
,

differencedFunction= Table[{x,interpdFunction[x]-interpdRef[x]},{x,xMin,xMax,xMax/dx }];
interpolatedReference=Table[{x,interpdRef[x]},{x,xMin,xMax,xMax/dx }];
interpolatedComparing=Table[{x,interpdFunction[x]},{x,xMin,xMax,xMax/dx }];
rms=RootMeanSquare[differencedFunction[[;;,2]]];
];

{rms,differencedFunction,interpolatedReference,interpolatedComparing}

];

findBadPoints[vmesh_, vinp_]:=Block[{allpolysin,allpolys,pncell,testPs},

allpolysin =  MeshCells[vmesh,2][[All,1]];

allpolys = coordFromPolygon[MeshCoordinates[vmesh],allpolysin] ;



pncell = Table[  If[inPolyQ2[allpolys[[polg]],ps[[1]],ps[[2]]], polg, Unevaluated[Sequence[]]]          ,{ps,vinp},{polg,Range@Length@ allpolys} ];testPs = Table[   (DisjointQ[pncell[[;;pin-1]], {pncell[[pin]]}] )     ,{pin, Range@Length@pncell}];
Return[testPs]
]

cellNum[pinV_, cellinV_] := Block[{al},
Do[ If[ inPolyQ2[cellinV[[al]], pinV[[1]], pinV[[2]] ]==True, Return[al]  ]  ,{al, Length@cellinV}]
]

pcell[pinV_, cellinV_]:= Block[ {al},
	Do[If[inPolyQ2[cellinV, pinV[[al,1]], pinV[[al,2]] ], Return[al]   ], {al, Length@pinV}]
]
coordFromPolygon[fullVert_List,polyl_List] := Table[ fullVert[[z]], {z, polyl}]


Options[PlotPairCorrelationFunction]={box-> {},boundary-> "trunc",lattice-> "",shells-> 4,distance->{} , normalized->False,absgr-> False,stacked->True, xLabel->"Distance (r)" ,names-> {}, boots-> 25, colours->{}, varNames->True, chartQ->True, save ->False};
SetOptions[PlotPairCorrelationFunction,box-> {} , boundary->"trunc" ,lattice-> "",shells-> 4,distance->{} ,normalized->False ,absgr-> False,stacked->True, xLabel->"Distance (r)"  ,names-> {},boots-> 25, colours->{}, varNames->True, chartQ->True, save->False];
PlotPairCorrelationFunction[inData2d_, OptionsPattern[]]:=
Block[{
xyDataList,xyData,xyLattice,r,dr,xyDataDisplaced,xyLatticeDisplaced,pcfData,pcfLattice,hexBox,
paddingA,paddingB,bstyle,normalizedRef, normalizedFunc, rmsMetric,differenceOfBoth,inRef,inFunc,maxDelta,minDelta,errorGrid,
nfuncts,gridLines,latticePeaks,colouring,grswLg,polys,lmsize,fullData,fullNames,diffRMSmetrics,diffLists,interpFullDataN,tempDelta,maxR,difPlots,stackedPlots,Func,funcNameList,returnedPlot,
ref,refName,func,multDatType,
rmsList,
pcfDataList,pcfLatticeList,pcfResidualList,pcfFirstPeakList,
xMaxSaved,dataExpectedDiameterList,
npointsDataList,npointsLatticeList,outCol,
passedOptions,optionNames,optionNamesO,latticeName=" ",
boxO=OptionValue[box],
boundaryO = OptionValue[boundary],
shellsO=OptionValue[shells],
distanceO= OptionValue[distance],
normalizedO= OptionValue[normalized],
stackedO= OptionValue[stacked],
namesO=OptionValue[names],
xLabelO=OptionValue[xLabel],
bootsO=OptionValue[boots],
latticeO=OptionValue[lattice],
absgrO=OptionValue[absgr],
colourO=OptionValue[colours],
varNamesO = OptionValue[varNames],
saveO = OptionValue[save]
},
xMaxSaved=Infinity;

(* Smoothed *)
(* Remove any non-positional data that might be passed when called *)
optionNames={box,boundary,lattice,shells,distance};
optionNamesO:= {boxO,boundaryO,latticeO,shellsO,distanceO};


Off[TensorRank::rect];
multDatType=True;
(*  Autodetect if more than one dataset is input *)
If[TensorRank[inData2d]==2,xyDataList={inData2d}; multDatType=False;, xyDataList=inData2d; multDatType=True;];

On[TensorRank::rect]; 


nfuncts= Length[xyDataList];



If[namesO==={}, If[ varNamesO == False , namesO=Table["Data: "<>ToString[x],{x,nfuncts}]; , 


If[ Length@((Trace@inData2d)[[1]])==2, namesO = (Trace@inData2d)[[All,1]][[1;; Length[Trace@inData2d]  -1 ]]     ]; ];


If[Length@((Trace@inData2d)[[1]])==1, namesO = Table[ ToString@((Trace@inData2d)[[1]]) <> ToString@pp, {pp, nfuncts}    ]];


If[varNamesO == True, namesO = Table[ ToString[namesO[[pp]]], {pp, Length@namesO}]];
Which[Length[namesO]==0,namesO={namesO};,Length[namesO]<  nfuncts , Print["Number of Names NOT EQUAL to Number of Data Sets!"];
];
];


(* Initialize Arrays for PCFs  *)
rmsList=ConstantArray[{},nfuncts];
pcfDataList=ConstantArray[{},nfuncts];
pcfLatticeList=ConstantArray[{},nfuncts];
pcfResidualList=ConstantArray[{},nfuncts];
dataExpectedDiameterList=ConstantArray[{},nfuncts];
pcfFirstPeakList = ConstantArray[{},nfuncts];
npointsDataList=ConstantArray[{},nfuncts];
npointsLatticeList=ConstantArray[{},nfuncts];

Do[
xyData=xyDataList[[nf]];
If[Length[xyData[[1]]]==3,xyData=xyData[[All,1;;2]]];

If[OptionValue[box]==={},
boxO=AutoBoxTransform[xyData]; 
(*hexBox=(Sqrt[PolyArea[MinMaxBox[xyData]]])closedBindingBox;*)
hexBox= (Sqrt[PolyArea[boxO]]) closedBindingBox ; 
,
hexBox= (Sqrt[PolyArea[boxO]]) closedBindingBox ; ];


If[bootsO<0 ,bootsO=Abs[bootsO];];


(* Find the Hexatic Diameter and Lattice Disorder Parameter - For Smoothing *)
{r,dr}=ExpectedDiameterFromLattice[xyData,lattice->latticeO];

If[dr<=0, Print["The lattice expected radius uncertainty, was estimated at 0. This is likely due to all voronoi cells having the same area. The uncertainty is set to 1% of calculated value "]; dr = 0.01*r; ];

dataExpectedDiameterList[[nf]]={r,dr};



(* Choose the Reference Lattice - square or (default) hexagonal *)
If[latticeO=="square",
xyLattice=SquareLattice[n->Length[xyData], box->hexBox];,
xyLattice=HexagonalLattice[n->Length[xyData], box->hexBox];
];




npointsDataList[[nf]]=Length[xyData];
npointsLatticeList[[nf]]=Length[xyLattice];

(* Generate boots0 number of datasets that are slightly offset around original and Select only the points that are inside the window -may have been randomly displaced outside box *)
xyDataDisplaced=Table[Select[Hexaticize[xyData,Sqrt@dr],inPolyQ2[boxO,#[[1]],#[[2]] ]==True&],{x,bootsO}];
xyLatticeDisplaced=Table[Select[Hexaticize[xyLattice,dr],inPolyQ2[hexBox,#[[1]],#[[2]] ]==True&],{x,bootsO}];



(* Calculate the pcf for all the displaced data sets (nBoots)  *)
passedOptions =Sequence@@MapThread[  #1-> #2(*OptionValue[#2]*)&  ,{optionNames, optionNamesO}];
(*Print[passedOptions];*)
pcfData=PairCorrelationFunction[xyDataDisplaced,passedOptions];
pcfFirstPeakList[[nf]]=FirstPeakValue[pcfData];


passedOptions =Sequence@@(MapThread[  #1-> #2(*OptionValue[#2]*)&  ,{optionNames, optionNamesO}]/.boxO->hexBox);
(*Print[passedOptions];*)


pcfLattice=PairCorrelationFunction[xyLatticeDisplaced,passedOptions];


(* Normalize the X-Axis by the Expected Diameter from the Lattice  *)
If[ normalizedO==True,
pcfData[[All,1]]=pcfData[[All,1]]/dataExpectedDiameterList[[nf,1]];
pcfLattice[[All,1]]=pcfLattice[[All,1]]/(ExpectedDiameterFromLattice[xyLattice,lattice-> latticeO][[1]]);
];

(* Normalize the Y-Axis by the Maximum Value of the g(r) (i.e. remove Absolute value of g(r))  *)
If[absgrO==False,
pcfData[[All,2]]=pcfData[[All,2]]/Max[pcfData[[All,2]]];
pcfLattice[[All,2]]=pcfLattice[[All,2]]/Max[pcfLattice[[All,2]]];
];


(* Save the Smallest Distance to Plot To  *)
(*Print[xMaxSaved];*)
If[#< xMaxSaved,xMaxSaved=# ]&@Min[{ Max[pcfLattice[[All,1]]],Max[pcfData[[All,1]]]}];
(*Print[xMaxSaved];*)

{rmsMetric,differenceOfBoth,inRef,inFunc} = (InterpolationFunctionDifference[pcfLattice,pcfData,#]&@Min[{ Max[pcfLattice[[All,1]]],Max[pcfData[[All,1]]]}]);

rmsList[[nf]]=rmsMetric;
pcfDataList[[nf]] = pcfData;
(*Print[pcfData];*)
pcfLatticeList[[nf]] = pcfLattice;
pcfResidualList[[nf]]=differenceOfBoth;


,{nf,  nfuncts}];
(* ============================================================================================== *)
(* ============================================================================================== *)



(*{{left,right},{bottom,top}}*)
paddingA={{60,10},{0,0}};
paddingB={{60,10},{50,5}};
bstyle={FontFamily->"Arial",FontSize->14};
maxDelta=0;minDelta=1000;



If[ToUpperCase[xLabelO]==  ToUpperCase["Normalized Radius"],
gridLines=2{1.,1.73205,2.,2.64575,3.,3.4641,3.60555,4.,4.3589,4.58258,5.,5.19615,5.2915,5.56776,6.,6.08276,6.245,6.55744,6.9282,7.,7.2111,7.54983,7.81025,7.93725,8.,8.18535,8.544,8.66025,8.7178,8.88819,9.,9.16515,9.53939,10.};
];



If[latticeO=="hex"||latticeO=="" || latticeO == "hexagonal",
(*ToUpperCase[xLabel]\[Equal]  ToUpperCase["Normalized Diameter"],*)
If[normalizedO==True,
xLabelO= "Normalized Hexagonal Diameter";];
latticePeaks=N@Sort[DeleteDuplicates@Flatten@Table[Sqrt@{m^2+n m+n^2},{m,0,7},{n,0,7}],Less];
latticeName = "Hexagonal";
];



If[latticeO=="square",
(*ToUpperCase[xLabel]\[Equal]  ToUpperCase["Normalized Diameter"],*)
If[normalizedO==True,
xLabelO= "Normalized Square Diameter"; ];
latticeName = "Square";
latticePeaks=N@Sort[DeleteDuplicates@Flatten@Table[Sqrt@{m^2+n^2},{m,0,7},{n,0,7}],Less];
];
gridLines=latticePeaks;



(* ============================================================================================== *)
(* ============================================================================================== *)


If[stackedO==False,

returnedPlot=Table[
maxDelta=(-Infinity);
minDelta=(Infinity);
If[Max[pcfResidualList[[nf,All,2]]]>maxDelta,maxDelta=Max[pcfResidualList[[nf,All,2]]]  ];
If[Min[pcfResidualList[[nf,All,2]]]<minDelta,minDelta=Min[pcfResidualList[[nf,All,2]]]  ];
errorGrid=Round[{maxDelta/2,minDelta/2},0.01];

If[nfuncts==1, grswLg =namesO[[1]] <> " Simulated " <> latticeName <> " Lattice", grswLg = Unevaluated[Sequence[]] ];

(*If[latticeO\[NotEqual] "hex" ||latticeO\[NotEqual] "square",gridLines=Table[ x FirstPeakValue[pcfDataList[[nf]]],{x,0,10,0.5}];];*)
If[normalizedO==False,gridLines=dataExpectedDiameterList[[nf,1]] latticePeaks; (*xLabelO= OptionValue[xLabel];*)];

(* Plot Residual Spectrum at the Top *)
 Grid[{
{ListPlot[pcfResidualList[[nf]],
Joined->True,
Filling->Axis,
FrameLabel->{"","\[CapitalDelta]"},
RotateLabel->False,
ImagePadding->paddingA,
PlotLegends->{SwatchLegend[{"|\!\(\*SubscriptBox[\(\[CapitalDelta]\), \(RMS\)]\)| = "<>
ToString[NumberForm[rmsList[[nf]],3]]}]},
Axes->{True,True},
Frame->True,
FrameTicks->{{errorGrid,False},{False,False}},
FrameTicksStyle->Directive[Black,Thin,10],
PlotRange->{{0,xMaxSaved},{1.25 minDelta,1.25maxDelta}},
AspectRatio->1/(4GoldenRatio),
ImageSize->400,
BaseStyle->bstyle,
PlotStyle->Black,
GridLines->{gridLines,{minDelta/2.,maxDelta/2.}}
]},
(*  Plots the Overlay of Reference and Subtracting Function *)
Quiet@{ListPlot[
{pcfDataList[[nf]],pcfLatticeList[[nf]]},
Joined->True,
Filling->Axis,
PlotRange->{{0,xMaxSaved},Automatic},
PlotStyle->{{Thick,RGBColor[0.545,0.0,0.8]},{Dashed,RGBColor[0.1333,0.5451,0.1333]}},PlotLegends->{ namesO[[nf]] , grswLg (*<>"\n r="<>ToString[NumberForm[#[[1]],3]]<>If[latticeO\[Equal]"square",
("Squaratic Disorder: \[Sigma]="<>ToString[NumberForm[#[[2]],3]]),("Hexatic Disorder: \[Sigma]="<>ToString[NumberForm[#[[2]],3]])]*)}(*&/@{dataExpectedDiameterList[[nf]]}*),
ImagePadding->paddingB,
FrameLabel->{ToString[xLabelO],If[absgrO===True,"g(r)  (A.u.)","g(r)"]},
Axes->False,Frame->{True,True,True,True},
ImageSize->400,
AspectRatio->1.15/(GoldenRatio),
BaseStyle->bstyle,
GridLines->{gridLines,Automatic}
]
,SpanFromLeft}}]

,{nf , nfuncts}];


];




(* Stacked Metric Differences  *)
(* ============================================================================================== *)


If[stackedO==True,




bstyle={FontFamily->"Arial",FontSize->16};

(*colours={Red,Blue,Green,Magenta,Orange, Cyan,Yellow,LightBlue,LightGreen,LightRed};*)




If[SameQ[colourO,{}], colouring=RegularPolygonColourScheme,colouring = colourO;];


If[nfuncts>Length@colouring, Print["There are less colours than data sets. The colours have therefore been looped. "]; colouring = Table[ colouring[[ Mod[a, Length@colouring]+1  ]] ,{a, nfuncts}];  ];



polys=Table[Graphics[{
EdgeForm[{Black}],Directive[{colouring[[x]],Opacity[0.5]}],Polygon[standardPolygon[40]]
}],{x,Range[nfuncts]}]; 


lmsize=26-((nfuncts+1));
(*maxR=0.;*)




(*-----------------------------------------------------*)


difPlots=Column[
Table[

maxDelta=-Infinity;
minDelta=Infinity;
If[Max[pcfResidualList[[nf,All,2]]]>maxDelta,maxDelta=Max[pcfResidualList[[nf,All,2]]]  ];
If[Min[pcfResidualList[[nf,All,2]]]<minDelta,minDelta=Min[pcfResidualList[[nf,All,2]]]  ];
errorGrid=Round[{maxDelta/2,minDelta/2},0.01];
If[normalizedO==False,gridLines=Min@dataExpectedDiameterList[[All,1]] latticePeaks; ];(*xLabelO= OptionValue[xLabel]; *)



ListPlot[pcfResidualList[[nf]],
Joined->True,
Filling->Axis,
FillingStyle->Directive[{Opacity[0.3],colouring[[nf]]}],
FrameLabel->{"","\[CapitalDelta]"},
RotateLabel->False,
FrameStyle->Thick,

FrameTicks->{{errorGrid,False},{False,False}},
FrameTicksStyle->Directive[Black,Thin,10],
PlotLegends->SwatchLegend[{"|\!\(\*SubscriptBox[\(\[CapitalDelta]\), \(RMS\)]\)| = "<>
ToString[NumberForm[rmsList[[nf]]   ,3]]},LegendMarkers->polys[[nf]],LegendMarkerSize->{{lmsize/2.,lmsize/2.}}],
PlotRange->{{0,xMaxSaved},{1.15 minDelta,1.15 maxDelta}},
ImagePadding->paddingA,
Axes->{True,False},
Frame->{True,True,True,True},
AspectRatio-> 1./( (nfuncts+2) GoldenRatio ),
ImageSize->400,
GridLines->{gridLines,{minDelta/2.,maxDelta/2.}},
BaseStyle->bstyle,
PlotStyle->Black],{nf,(Range[nfuncts])}]
,Spacings->0
];


(*-----------------------------------------------------*)
If[absgrO == True ,maxR  = Table[  Max[ Max[pcfDataList[[a, All,2]]] , Max[pcfLatticeList[[a, All,2]]] ] ,{a,2,nfuncts}]   ];
If[absgrO == True, maxR=  Table[ If[a<nfuncts, Total[  Take[maxR, -(Length@maxR - a+1)]  ],0  ],  {a,nfuncts}]    ];


If[nfuncts==1, grswLg =Placed[ SwatchLegend[{(namesO[[1]] <> " Simulated " <> latticeName <> " Lattice" )}&/@{{1,1}},LegendMarkers->Graphics[{
EdgeForm[{Black}],Directive[{Gray,Opacity[0.7]}],Polygon[standardPolygon[40]]
}],LegendMarkerSize->{{lmsize,lmsize}}],After], grswLg = Unevaluated[Sequence[]]];

If[normalizedO==False,gridLines=Min@dataExpectedDiameterList[[All,1]] latticePeaks; (*xLabelO= OptionValue[xLabel]; *)];


stackedPlots=Show[
Table[


(*If[nf== 2,maxR=Max[{Max[pcfLatticeList[[1,All,2]]],Max[pcfDataList[[1,All,2]]]}]];*)
(*If[nf>=  2, maxR=maxR+Max[{Max[pcfLatticeList[[nf-1,All,2]]],Max[pcfDataList[[nf-1,All,2]]]}]]; *)


ListPlot[ 
{pcfLatticeList[[nf]]+Table[{0,If[absgrO==False,(nfuncts-nf),maxR[[nf]] ]},{a,Range@Length[pcfLatticeList[[nf]]]}],
pcfDataList[[nf]]+Table[{0,If[absgrO==False,(nfuncts-nf),maxR[[nf]] ]},{a,Range@Length[pcfDataList[[nf]]]}]}
,
Joined->True,
Filling->{1-> {If[absgrO==False,(nfuncts-nf),maxR[[nf]]],Gray}, 2->{{1},{GrayLevel[0.7],Directive[{Opacity[0.4],White}]}},2-> {If[absgrO==False,(nfuncts-nf),maxR[[nf]]],Directive[{Opacity[0.7],colouring[[nf]]}]}(*1\[Rule]{{2},{None,Red}}*)},
(*FillingStyle\[Rule]({Directive[{Opacity[0.4],colouring[[nf]]}]}(*&/@{0.1,0.4}*)),*)
PlotRange->{{0,xMaxSaved},Full},
PlotStyle->{Directive[{Thin,Black}],Directive[{Thick,Black}]},
PlotLegends->{SwatchLegend[{(namesO[[nf]](*<>"\n r="<>ToString[NumberForm[#[[1]],3]]<>"\n"<>ToString@If[latticeO\[Equal]"square",
("Squaratic Disorder: \[Sigma]="<>ToString[NumberForm[#[[2]],3]]),("Hexatic Disorder: \[Sigma]="<>ToString[NumberForm[#[[2]],3]])]*)     )}&/@{(*Reverse[*)dataExpectedDiameterList(*]*)[[nf]]},LegendMarkers-> polys[[nf]],LegendMarkerSize->{{lmsize,lmsize}}],grswLg},


(*PlotLegends\[Rule]{ ("Data: r="<>ToString[NumberForm[#[[1]],3]]),If[latticeO\[Equal]"square",
("Squaratic Lattice: \[Sigma]="<>ToString[NumberForm[#[[2]],3]]),("Hexatic Lattice: \[Sigma]="<>ToString[NumberForm[#[[2]],3]])]}&/@{dataExpectedDiameterList[[nf]]},  *)
ImagePadding->paddingB,
FrameLabel->{ToString[xLabelO],If[absgrO==False,"Pair Correlation   g(r)  A.u.","Pair Correlation  g(r)"]},
FrameStyle->Directive[Thick,Black],
Axes->False,
Frame->{True,True,True,True},
FrameTicks->{True,True,False,False},
FrameTicksStyle->Directive[Black,Thin],
ImageSize->400,
AspectRatio->1/(  GoldenRatio),
GridLines->{gridLines,Automatic},
BaseStyle->bstyle]

,{nf,( Range[nfuncts])}]
,PlotRange-> {{0,xMaxSaved},Automatic}
];

returnedPlot = Column[{difPlots,stackedPlots},Spacings->0];
];



If[latticeO==""||latticeO=="hex" || latticeO == "Hexagonal",latticeO="Hexatic"];
If[latticeO=="square" || latticeO == "Square",latticeO="Squaratic"];
outCol = {returnedPlot};


If[OptionValue[chartQ], AppendTo[outCol,TableForm[
Transpose@{ pcfFirstPeakList    , dataExpectedDiameterList[[;;,1]], dataExpectedDiameterList[[;;,2]], npointsDataList,npointsLatticeList  }
,TableHeadings->{namesO,{ "First Peak\ng(r)","Expected Spacing\n("<>ToString[latticeO]<>" Lattice)","Expected Disorder\n("<>ToString[latticeO]<>" Lattice)", "Particles\n(data)","Particles\n(lattice)"}}]    ]   ] ;

If[saveO==True,
If[stackedO==True,Export[NotebookDirectory[]<>namesO[[1]]<>"_Resid"<>".png",difPlots]; Export[NotebookDirectory[]<>namesO[[1]]<>"_CorrelationFunctionPlot.png", stackedPlots];  ];
If[stackedO==False, Do[Export[NotebookDirectory[]<>namesO[[zz]]<>"_CorrelationFunction"<>ToString[zz]<>".png", returnedPlot[[zz]]],{zz,Length@returnedPlot}]]
];


Return[Column[outCol,Spacings->{0,3}  ] ];

];

(* Order Metrics Based on Pair Correlation Function *)
(* Options[TranslationalOrderParameter]= {diameter\[Rule]1 , density\[Rule]1.};*)
TranslationalOrderParameter[pcf_, {rMin_,rMax_},\[Rho]_]:= Block[{r,gr,hr,\[Tau],interpolatedPFC,density,d, diameter,distance},

diameter=1;
r=pcf[[All,1]];
gr=pcf[[All,2]];
hr=Abs[(gr-1.)];
interpolatedPFC=Interpolation[Transpose[{Sqrt[\[Rho]](r),hr}],InterpolationOrder->1];
(*Print[ListLinePlot[interpolatedPFC]];*)
distance = diameter*Sqrt[\[Rho]];
(1./(rMax - distance))*NIntegrate[interpolatedPFC[dr],{dr, distance,distance+rMax},MaxRecursion->12]
];
TranslationalNumberOfNeighbours[pcf_,{xMin_,xMax_},\[Rho]_]:= Block[{r,gr,hr,tempPCF,interpolatedPFC,xr},

r=pcf[[All,1]];
gr=pcf[[All,2]];
hr=Abs[(gr-1.)];
interpolatedPFC=Interpolation[Transpose[{r,hr}],InterpolationOrder->1];
(2. Pi / \[Rho])*NIntegrate[dr * interpolatedPFC[dr],{dr,xMin,xMax}]
];
TranslationalExcessEntropy[pcf_,{xMin_,xMax_},\[Rho]_]:= Block[{nonZeroPCF,s2,interpolatedPFC,xr,r,gr},

nonZeroPCF=Select[pcf,Part[#,2]>0&];
r=nonZeroPCF[[All,1]];
gr=nonZeroPCF[[All,2]];
s2=Log[#]*# -( #-1.)  &/@gr ;
interpolatedPFC=Interpolation[Transpose[{r,s2}],InterpolationOrder->1];
 (\[Rho])*NIntegrate[ interpolatedPFC[dr],{dr,First[r],xMax}]
];

(* Diffraction Routines - ADO implementation and ImagePeriodogram *)
ImageDiffractionPattern = Function[{img},Block[{data,nRow,nCol,nChannel,d,fw,fudgeFactor,abs2,imgsizescreen,plotrange},

data=ImageData[img];(*get data*)
{nRow,nCol,nChannel}=Dimensions[data];
d=data[[All,All,1]];(*get channel 2 to FFT but center it first*)
d=d*(-1)^Table[i+j,{i,nRow},{j,nCol}];
fw=Fourier[d]; (*make FFT,and look at spectrum and phase*)
fudgeFactor=10.0;(*adjust for better viewing as needed*)
(*abs=fudgeFactor*Log[1+Abs@fw];*)
abs2=fudgeFactor*Log[Abs@fw];
imgsizescreen=450;
plotrange={{nRow/3,2nRow/3},{nCol/3,2nCol/3}};

ArrayPlot[abs2,ImageSize->imgsizescreen,PlotRange->plotrange,ColorFunction->"GrayTones",ColorFunctionScaling->False,Axes->True,FrameTicks->Automatic]
](* Block *)
];
Options[DiffractionPatternFromPosition]= {annulus-> True};
SetOptions[DiffractionPatternFromPosition,annulus->True ];
DiffractionPatternFromPosition[data2d_, OptionsPattern[]] := Block[{data,nRow,nCol,nChannel,d,fw,fudgeFactor,abs2,imgsizescreen,plotrange,img,extractAnnulus,fft,p,pp,ppp,annulusO=OptionValue[annulus]},


img=ListPlot[data2d[[All,1;;2]],Frame->False,Axes->False,FrameTicks->None,AxesOrigin->{0,0},AspectRatio->1,PlotRange->Full,PlotStyle->{Black,PointSize[Medium]},(*PlotStyle\[Rule]PointSize[(*Large*)(*Large*)(*10 pcSize[Length@data2d]*)],*)Background->None];
(*Print[img];*)


(*Print[10 pcSize[Length@data2d]];*)
(*Print[Length[data2d]];*)
data=ImageData[img];(*get data*)
{nRow,nCol,nChannel}=Dimensions[data];
d=data[[All,All,1]];(*get channel 2 to FFT but center it first*)
d=d*(-1)^Table[i+j,{i,nRow},{j,nCol}];
fw=Fourier[d]; (*make FFT,and look at spectrum and phase*)
fudgeFactor=10.0;(*adjust for better viewing as needed*)
(*abs=fudgeFactor*Log[1+Abs@fw];*)
abs2=fudgeFactor*Log[Abs@fw];
imgsizescreen=450;
plotrange={{nRow/3,2nRow/3},{nCol/3,2nCol/3}};
(*plotrange={{0,nRow},{0,nCol}};*)
plotrange={{nRow/4,3nRow/4},{nCol/4,3nCol/4}};
fft=ArrayPlot[abs2,ImageSize->imgsizescreen,PlotRange->plotrange,ColorFunction->"SunsetColors"(*"GrayTones"*),ColorFunctionScaling->False,Axes->False,FrameTicks->False,Frame->False];

If[annulusO==True,

extractAnnulus[image_,{r1_,r2_}]:=Module[{mask},mask=Graphics[{White,Disk[{0,0},r2],Black,Disk[{0,0},r1]},PlotRange->1,Background->Black,ImageSize->ImageDimensions[image]];
ImageAdd[ImageMultiply[image,mask],ColorNegate[mask]]];

p=(ImagePeriodogram@(Binarize@Show@Graphics[{PointSize[1.5/AverageFirstNeighbourDistance[data2d[[All,1;;2]]]],Black,Point[data2d[[All,1;;2]]]}]));

pp=(Binarize[#,0.96FindThreshold[#,Method->"Mean"]]&/@(ColorConvert[#,"Grayscale"]/@ImageCrop[extractAnnulus[#,{0,0.25}]]&/@{p}));
fft=First@(extractAnnulus[#,{0,0.9}]&/@Blur/@ColorNegate/@pp);
];

fft

];

(* Solving Intermolecular Distances *)
AverageFirstNeighbourDistance[data2d_,numberOfNN___]:=Block[{nn},
If[numberOfNN===Null,nn=2,nn=(numberOfNN+1)];
Mean@Mean[
Table[
EuclideanDistance[
Nearest[data2d[[;;,1;;2]],data2d[[#,1;;2]],x][[x]],data2d[[#,1;;2]]]&/@Range[Length[data2d]],
{x,2,nn}]]
];
FirstPeakValue[xy_]:=xy[[ First@First@ Position[xy[[;;,2]],Max[xy[[;;,2]]] ],1]];
Options[ExpectedHexagonalDiameter]={ boundary->"NOEDGE",box->{} };
SetOptions[ExpectedHexagonalDiameter, boundary->"NOEDGE",box->{} ];
ExpectedHexagonalDiameter[data2d_, OptionsPattern[]]:=Block[{v,areaDistribution,r,
edgeO=OptionValue[boundary],
boxO=OptionValue[box]},

r[area_]:=Sqrt[area/(6 Tan[Pi/6])];

v=VoronoiCells[data2d,box->boxO,edge->edgeO, full-> False];
areaDistribution=PolyArea/@v;
{2 Mean[r@areaDistribution], Variance[r@areaDistribution]}
];
Options[ExpectedDiameterFromLattice]={ boundary->"NOEDGE",box->{} ,lattice->"hex"};
SetOptions[ExpectedDiameterFromLattice, boundary->"NOEDGE",box->{},lattice->"hex" ];
ExpectedDiameterFromLattice[data2d_, OptionsPattern[]]:=Block[{v,areaDistribution,r,rHex,rSqr,
edgeO=OptionValue[boundary],
boxO=OptionValue[box],
latticeO=OptionValue[lattice]
},
rHex[area_]:=Sqrt[area/(6 Tan[Pi/6])];
rSqr[area_]:=Sqrt[area]/2.;

v=VoronoiCells[data2d,box->boxO,edge->edgeO, full-> False];
areaDistribution=PolyArea/@v;
If[latticeO=="square",r=rSqr[areaDistribution];, r=rHex[areaDistribution]; ];

{2 Mean[r], Variance[r]}
];

(* Voronoi Tessellation Routines *)
Options[VoronoiCells]= {box->{},edge->"NOEDGE", full->False ,fast->True,debug->False};
SetOptions[VoronoiCells,box->{},edge->"NOEDGE",full->False ,fast->True,debug->False];
VoronoiCells[data2Din_, OptionsPattern[]]:=Block[{data2D,pbcData,bigBox,npoints,voronoiVertexNumber,voronoiVertexCoord,vor,vorBox,totaltime,return,fullreturn,g,mcells,lengthMcells,vorCellPoints,rangeData,plotPointsList,boxMinMaxX,boxMinMaxY,dh,
del,delConnectors,delaunayNearestNeighbours
,commonVertex,sameTestPoints,sameTestVor,insideIndex,
boxO=OptionValue[box],
edgeO=OptionValue[edge],
fullO=OptionValue[full],
fastO=OptionValue[fast],
debugO=OptionValue[debug]
},


(* Remove any non-positional data that might be passed when called *)
If[Length[data2Din[[1]]]==3,data2D=data2Din[[All,1;;2]],data2D=data2Din[[All,1;;2]]];

(*Print[box==={}];*)
(* Autodetect box *)
If[boxO==={},
If[debugO===True,Print["Autodetect box"];];
boxO=AutoBoxTransform[data2D]];
(*Print[box];*)


(* Maximum values for the edges - translation vectors *)
boxMinMaxX={Min[boxO[[All,1]]],Max[boxO[[All,1]]]};
boxMinMaxY={Min[boxO[[All,2]]],Max[boxO[[All,2]]]};

(* Spacing of hexagonal lattice with same intensity *)
If[fastO=== True,
dh=3 Sqrt[2./ (Sqrt[3.]    (Length[data2D] /  PolyArea[boxO])   )];,
dh=Max[Differences/@{boxMinMaxX,boxMinMaxY}];
];


(*ToUpperCase[type]==="HARD"|| ToUpperCase[type]==="IMAGE" ||ToUpperCase[type]==="NOEDGE",*)

(* Apply proper boundary conditions before solving Voronoi cells *)
If[   ToUpperCase[edgeO]==="PBC"|| ToUpperCase[edgeO]==="PERIODIC",
(* Periodic Boundary Conditions *)
If[debugO===True,Print["Applying Periodic Boundaries."];];
pbcData=ApplyPeriodicBoundary[data2D,boxO,SetDistance-> dh];
,
(* Hard Square Box *)
(* Default to : No-Edge *)
If[debugO===True,Print["Applying Image Boundaries"];];
pbcData=ApplyImageChargeBoundary[data2D,boxO,SetDistance-> dh];
];


(* Collect the number of original data points  *)
npoints = Length[Intersection[data2D,pbcData,SameTest->(EuclideanDistance[#1,#2]<= 10.^-5.&)]];

(* Define Parameters for clipping the Voronoi cell *)
bigBox=AutoBoxTransform[pbcData];
vorBox={{Min[bigBox[[All,1]]],Max[bigBox[[All,1]]]},{Min[bigBox[[All,2]]],Max[bigBox[[All,2]]]}};


(* Extract Voronoi coordinates and neighbours from M.v10 functions *)
If[debugO===True,Print["Starting Voronoi Calculations"];];
vor=VoronoiMesh[pbcData,vorBox];
voronoiVertexCoord=MeshCoordinates[vor];
mcells=MeshCells[vor,2];
lengthMcells=Length[mcells];
voronoiVertexNumber=Table[{x,mcells[[x]][[1]]},{x,1,lengthMcells}];
vorCellPoints=Table[voronoiVertexCoord[[voronoiVertexNumber[[x,2]]]],{x,lengthMcells}];
rangeData=Range[Length[pbcData]];


(* Sorts the Voronoi polygon list to match the particle list *)
voronoiVertexNumber=Sort[
Table[{
Do[
If[inPolyQ2[vorCellPoints[[b]],pbcData[[a,1]],pbcData[[a,2]]],g=a;Break[];];
,{a,rangeData}];rangeData=DeleteCases[rangeData,g];

g,Insert[voronoiVertexNumber[[b,2]],voronoiVertexNumber[[b,2,1]],-1]
}
,{b,lengthMcells}]
];


(* Remove duplicated Voronoi vertices  *)
return=Table[
(DeleteDuplicates[voronoiVertexCoord[[x]],Equal[#1,#2]&]),{x,voronoiVertexNumber[[All,2]]}];
return=PadRight[#,Length[#]+1,{#[[1]]}]&/@return;


delConnectors={1};
If[debugO===True,Print["Voronoi Done: Delaunay NeighboursQ"];];
If[fullO==True ||ToUpperCase[edgeO]==="NOEDGE" || ToUpperCase[edgeO]==="TRUNC" ,

(* M.v10 - Delaunay Neighbour Routine *)
(* debug: time test = pass *)
del=DelaunayMesh[pbcData];
delConnectors=MeshCells[del,1][[All,1]];

(*Print["You are Reading Here"];*)
(* Collect and sort neighbouring adjacent voronoi cells *)
(*  debug: time test = fail 1.31s *)
delaunayNearestNeighbours= Table[
If[x[[1]]==obj , x[[2]],
If[x[[2]]==obj ,x[[1]],
Unevaluated[Sequence[]]   ]],{obj,Length@pbcData},{x,delConnectors}];


(* 1.85s *)
(*Print[AbsoluteTiming[
delaunayNearestNeighbours= ParallelTable[Reap[
If[#[[1]]\[Equal]obj , Sow[#[[2]]],
If[#[[2]]\[Equal]obj ,Sow[#[[1]]]   ]] &/@delConnectors ;][[2,1]],{obj,Range@Length@pbcData}];
]];*)


(* Collect the vertices that are common between adjacent voronoi cells *)
(*  Debug:  time test = 0.4 s *)
commonVertex=Table[Intersection[return[[j]],
return[[x]],SameTest->(EuclideanDistance[#1,#2]<= 10.^-5.&)],{j,Length@pbcData},{x,delaunayNearestNeighbours[[j]]}];



(* Remove all neighbours if they only share a single vertex *)
(*  Debug:  time test = 0.4 s *)
Do[
sameTestPoints=(Length/@commonVertex[[j]]) /.{1->False,2->True};
delaunayNearestNeighbours[[j]]=Pick[delaunayNearestNeighbours[[j]],sameTestPoints];
commonVertex[[j]]=Pick[commonVertex[[j]],sameTestPoints];
,{j,Length@pbcData}];
];


(* Remove outside particles, edge voronoi, and edge delanay neighbours if noedge is selected *)
If[ToUpperCase[edgeO]==="NOEDGE" || ToUpperCase[edgeO]==="TRUNC",

(* all particles inside box are removed from list *)
sameTestPoints =(!inPolyQ2[boxO,#[[1]],#[[2]]])&/@pbcData;

(* Delaunay neighbour list with outside particles removed *)
insideIndex=Pick[Range[Length@pbcData],sameTestPoints];

(* Pick voronoi with virtual particles outside the box *)
(* If particle has neighbour that is in the outside list, it is also removed *)

(*Print[Length@return == Length@delaunayNearestNeighbours]; *)

(*return = return[[;;Length[delaunayNearestNeighbours]]];*)
return=Pick[return,DisjointQ[#,insideIndex]&/@delaunayNearestNeighbours];
;
pbcData={pbcData,DisjointQ[#,insideIndex]&/@delaunayNearestNeighbours} ;
,
(* If boundary = pbc or image, return normal cells *)
return=return[[;;npoints]];
];


(*  Complete Voronoi data returned *)
If[debugO===True,Print["Final Return Voronoi"];];
If[fullO=== True,{return  ,pbcData ,delaunayNearestNeighbours,commonVertex},return]

];
Options[\!\(TraditionalForm\`PlotVoronoiTessellations\)]= {type->"bop",symmetry->6,cells->{},typedata-> Null ,points->True,scalebar->False, stats->"Count", boundary->"NOEDGE",box->{} ,raster-> False,binsize->0.1, colourBinned->True, colourGrad->{purpleBlend,greenBlend}, save->False};
SetOptions[PlotVoronoiTessellations,type->"bop",symmetry->6,cells->{},typedata-> Null ,points->True,scalebar->False, stats->"Count", boundary->"NOEDGE",box->{},raster->False,binsize->0.1,colourBinned->True,colourGrad->{purpleBlend,greenBlend}, save->False ];
PlotVoronoiTessellations[data2Din_, OptionsPattern[] ]:=Block[
{data2D,npoints,vor,hexagonalSpacing,nnDist,hexagonalVoronoi,colourBins,voronoiColour,polygonColour,polyAreaColourTable,returnVor,xmin,ymin, xmax,ymax,intensity,\[CapitalDelta],histstats,histtype,sym,bopdata,boptrue=False,scalebargraphic,r,\[Theta],r\[Theta],vorCenters,nvor,m,renderVor,extraGraphics,l2d,dupecheck,savenames,
typeO=OptionValue[type],
symmetryO=OptionValue[symmetry],
cellsO=OptionValue[cells], (*Never used*)
typedataO=OptionValue[typedata], (*Never used*)
pointsO=OptionValue[points],
scalebarO=OptionValue[scalebar],
statsO=OptionValue[stats],
boundaryO= OptionValue[boundary],
boxO=OptionValue[box],
rasterO=OptionValue[raster],
saveO = OptionValue[save]
},




extraGraphics={};
If[Length[data2Din[[1]]]==3,data2D=data2Din[[All,1;;2]],data2D=data2Din];
l2d = Length[data2D];


data2D = DeleteCases[data2D,{"",""}];
data2D = DeleteCases[data2D,{}];
(*data2D = DeleteCases[data2D,{,}];*)

If[Length@data2D <l2d, Print["There were some empty entries in your dataset (points with no coordinates at all). This is usually due to the csv file having initialized empty rows. These were deleted for futher computation."]];

l2d = Length@data2D;

data2D = DeleteDuplicates[data2D];



If[Length@data2D < l2d, Print["Duplicates of points were found in the dataset and were removed."]; dupecheck = True; l2d = Length@data2D;  Show[ListPlot[ data2Din,PlotMarkers->{Automatic,8}], ListPlot[data2D, PlotMarkers->{"\[Star]",12}]] SwatchLegend[{Blue,Red},{"Original Data Set","Data Used in Calculation"}]     , dupecheck = False];


data2D = Pick[data2D, findBadPoints[VoronoiMesh[data2D],data2D]];

If[Length@data2D!=l2d, Print["The voronoi mesh was generated to have more than 1 particle in the same mesh. One of these points was deleted. The bar graph may not be representative of the actual order."]];


npoints = Length@data2D;


(* Auto-detect a box. Can be set manually using Option box\[Rule]polygon *)
If[TypeBondOrderQ[typeO]==False,
If[boxO=={},
boxO=AutoBoxTransform[data2D];
vor=VoronoiCells[data2D,edge-> boundaryO,full->False,fast->True];
,
vor=VoronoiCells[data2D,box->boxO,edge-> boundaryO(*, full\[Rule]True*)];
];
];

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*~~~~~~~ Voronoi Colours ~~~~~~~~~~~~~~~~~*)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
If[TypeVoronoiQ[typeO],
voronoiColour=AssignVoronoiColour[vor];
scalebargraphic=ScaleBars["voronoi"];
];


(*=======================================*)
(* Bond Order Parameter Colour Scheme   *)
(*=======================================*)

If[TypeBondOrderQ[typeO],

(* Automatically calculates the (voronoi) bond order *)
boxO=AutoBoxTransform[data2D];

vor=VoronoiCells[data2D,box->boxO,edge-> boundaryO, full->True];


polyAreaColourTable=BondOrderParameter[data2D, symmetryO,box-> boxO,passVor->vor,edge-> boundaryO];(*/BOPexp[symmetryO];*)


vor=vor[[1]];

If[Length[vor]!= Length@polyAreaColourTable, Print["For some reason the number of colours generated was not the same as the number of Voronoi cells. This should never happen?"]];
colourBins={};

voronoiColour={};


If[OptionValue[colourBinned]==True,

colourBins = binGen[OptionValue[binsize]];(* HistogramList[polyAreaColourTable, {0.0,1.1,OptionValue[binsize]}][[1]] ;*)

voronoiColour = vorColourer[ #, colourGrad->OptionValue[colourGrad],binDstr->colourBins] &/@polyAreaColourTable;


];


If[OptionValue[colourBinned ] ==False,
voronoiColour= vorColourer[#, colourGrad->OptionValue[colourGrad], bind->False]&/@polyAreaColourTable[[;;Length[vor]]]; ];





If[Max[polyAreaColourTable]>1,scalebargraphic=ScaleBars["bop",sym->symmetryO, bopOps->{OptionValue[colourBinned],OptionValue[colourGrad],colourBins}];, scalebargraphic=ScaleBars["bop",sym->symmetryO,bopOps->{OptionValue[colourBinned],OptionValue[colourGrad],colourBins}];];

boptrue=True;
polyAreaColourTable=polyAreaColourTable[[;;Length[vor]]]
(*extraGraphics=Flatten[Graphics[{Opacity[.9999],Black,Line[#]}]&/@vor,1]*)
];


(*=============================================*)
(*  Coordination Number Colour Scheme         *)
(*=============================================*)
If[TypeCoordinationQ[typeO],

(* Pass to colour scheme *)
voronoiColour=CoordinationNumberColourScheme[Length[#]]&/@vor;
(*extraGraphics=Flatten[Graphics[{Opacity[1-.9999],White,PointSize[Small],Point[#]}]&/@vor,1];*)
scalebargraphic=ScaleBars["coord",minmax->( {Min[Length/@vor],Max[Length/@vor]}-1)];
];

(*==============================================*)
(* Clustering Vector Colour Acheme(unfinished) *)
(*==============================================*)
If[TypeAngularDirectionQ[typeO],

nvor=Length[vor];
vorCenters=RegionCentroid[Polygon[#]]&/@vor;
\[Theta][obj_,nn_]:=If[negQ[(PadRight[nn-obj,3]\[Cross]{1.,0.,0.})[[3]]],Pi+VectorAngle[{1.,0.},nn-obj],VectorAngle[{1.,0.},nn-obj]];
r[obj_,nn_]:=EuclideanDistance[obj,nn];
r\[Theta][obj_,nn_]:=r[obj,nn]{ Cos[\[Theta][obj,nn]], Sin[\[Theta][obj,nn]]};
m=Max[Table[r[vorCenters[[x]],data2D[[x]]],{x,nvor}] ];

voronoiColour=Directive[{BjornAngleScheme[
\[Theta][vorCenters[[#]],data2D[[#]] ]+Pi/4.,symmetryO],Opacity[(Abs[(r[vorCenters[[#]],data2D[[#]] ])]/m)
]}]&/@Range[nvor];
scalebargraphic=ScaleBars["angle",sym->symmetryO]
];


(* Show only Voronoi *)

renderVor=Show[{Table[Graphics[{If[boptrue==True&&pointsO==True,EdgeForm[{Thin,Black}],Unevaluated[Sequence[]]],FaceForm[voronoiColour[[x]]],Polygon[vor[[x]]]}],{x,Length[vor]}]
}];


(* Include/Exclude boundary lines and Voronoi points   *)
If[pointsO==False,
returnVor=renderVor;

, (* Show Points and Box *)


returnVor=Show[
{Graphics[{Black, Line[boxO]}],
renderVor,
Graphics[{Black, Point[data2D]}]}];
];


(* Reset variables for proper passing *)
returnVor={returnVor};

(* Calculate the histograms of Voronoi Cells  *)
If[statsO == "Count" || statsO ==  "Probability",  
(*  Pass additional Options if BOP is calculated  *)
If[boptrue==True,
AppendTo[returnVor,VoronoiHistogram[vor, type->typeO,stats->statsO,  sym->symmetryO,bopdata-> polyAreaColourTable, binsize->OptionValue[binsize], colourBinned->OptionValue[colourBinned ], colSch->OptionValue[colourGrad]]];,
AppendTo[returnVor,VoronoiHistogram[vor, type->typeO,stats->statsO]];
];,Print["The stats option was input incorrectly. The available options are `Count' and `Probability' "];
];


(* Add a ScaleBar from Options *)
If[scalebarO==True,
returnVor[[1]]= Labeled[returnVor[[1]],scalebargraphic,Right]
(*AppendTo[returnVor,scalebargraphic]; *)];

If[saveO == True,
savenames = ToString@((Trace@data2Din)[[1]]);

If[Length@returnVor==3,  savenames = { savenames<>"VoronoiTessellation", savenames<>"Tessellation Scalebar", savenames<>"Histogram"     } ];
If[Length@returnVor==2,  savenames = { savenames<>"VoronoiTessellation", savenames<>"Histogram"     } ];
If[Length@returnVor==1, savenames = {savenames<>"VoronoiTessellation"}];
Do[ Export[NotebookDirectory[]<>savenames[[zz]]<>".png", returnVor[[zz]]],{zz,Length@returnVor}];

];

(* Final Return of VoronoiTessellation + Histogram  *)
If[rasterO==True,returnVor=Rasterize/@returnVor];
If[Length[returnVor]==1,returnVor[[1]],returnVor]

];
Options[VoronoiHistogram]= {type->"vor",stats->"Probability",scalebar->True,sym->1,bopdata->Null, binsize->0.1,colourBinned->True,colSch->{purpleBlend,greenBlend}};
SetOptions[VoronoiHistogram,type->"vor",stats->"Probability",scalebar->True,sym->1,bopdata->Null, binsize->0.1,colourBinned->True,colSch->{purpleBlend,greenBlend}];
VoronoiHistogram[inVoronoiCells_, OptionsPattern[]]:=Block[{vDistrib,
typeO= OptionValue[type],
statsO= OptionValue[stats],
scalebarO= OptionValue[scalebar],
bopO= OptionValue[bopdata],
symO=OptionValue[sym],
binsizeO = OptionValue[binsize],
h,hlist,maxh,mh,offset,histo,npoints,intensity,\[CapitalDelta],hexagonalVoronoi,vorAreas,
coordN,HistCoordColours,
placedNumber},



(*--------------------------------*VORONOI*--------------------------------*)
If[TypeVoronoiQ[typeO],

npoints=Length[inVoronoiCells];
intensity=npoints/Total[PolyArea[ # ]&/@inVoronoiCells];
\[CapitalDelta]=Sqrt[2./(intensity*Sqrt[3.])];
hexagonalVoronoi = HexagonArea[\[CapitalDelta]/2];
vorAreas=Abs[PolyArea[ # ]-hexagonalVoronoi]/hexagonalVoronoi&/@inVoronoiCells;

h=Table[
If[a>0.3,0.31,a]
,{a,vorAreas }];
hlist=HistogramList[h,{0.,0.35,0.05},statsO];
maxh=Position[hlist,Max[hlist[[2,;;]]  ]][[1,2]];
mh=hlist[[2,maxh]]*1.15;
offset=Position[hlist[[2]],SelectFirst[hlist[[2]],#> 0.0&]][[1,1]];
If[statsO=="Probability",placedNumber=Placed[NumberForm[#,{5,3}],Above]&];
If[statsO=="Count",placedNumber=Placed[NumberForm[#,{5,0}],Above]&];

(*--------------------------------*)
histo=Histogram[h*100,{5},statsO,
ChartStyle-> VoronoiCellColourScheme[[offset;;7]],
LabelingFunction->(placedNumber),
PlotRange->{100*{0.,0.35},{0.,mh}},
Frame->True,
Axes->False,
GridLines->{Table[(mh/10) x,{x,10}],Automatic},
FrameTicks->{{ Automatic,True},{100*Table[0.05x,{x,0,7}],True}},
FrameLabel->{Style["Local Area Deviation from Hexagonal (%)\n"<>"Total Voronoi Tiles: "<>ToString[NumberForm[Length[vorAreas],{5,0}]],Black,FontSize->15],Style[statsO,Black,FontSize->15]},
BaseStyle->{FontFamily->"Arial",FontSize->14
}
];
];



(*--------------------------------*COORDINATION*--------------------------------*)
If[TypeCoordinationQ[typeO],

coordN=  Table[(Length[x]-1),{x,inVoronoiCells}];
HistCoordColours= CoordinationNumberColourScheme[#]&/@(1+Range[Max[coordN]]);

hlist=HistogramList[coordN,{1,Max[coordN],1},statsO];
maxh=Position[hlist,Max[hlist[[2,;;]]  ]][[1,2]];
mh=hlist[[2,maxh]]*1.15;(* <<-----*)
offset= Min[coordN];(*Position[hlist[[2]],SelectFirst[hlist[[2]],#> 0.0&]][[1,1]];*)
If[statsO=="Probability",placedNumber=Placed[NumberForm[#,{5,3}],Above]&];
If[statsO=="Count",placedNumber=Placed[NumberForm[#,{5,0}],Above]&];

(*--------------------------------*)
(*coordN,{1}*)

vDistrib= Table[a+0.5,{a,Min[coordN]-1,Max[coordN]}];
(*
Print[vDistrib];
Print[Min[coordN]];*)

histo=Histogram[coordN,{vDistrib},statsO,Axes->{True,False,False,False},Frame->{True,True,True,True},FrameLabel->{Style["Local Voronoi Coordination \nMean Number of Tile Sides: "<>ToString[NumberForm[N@Mean[coordN],{5,2}]]<>" \[PlusMinus] "<>ToString[NumberForm[N@StandardDeviation[coordN],{5,2}]],Black,FontSize->15],Style[statsO,Black,FontSize->15]},
BaseStyle->{FontFamily->"Arial",FontSize->14,Black},
ChartStyle->HistCoordColours[[offset;;]],
(*GridLines->{Table[x,{x,Max[coordN]}],Automatic},*)
(*PlotRange\[Rule]{{Min[#]+1,Max[#]},{0.,mh}}&@HistogramList[coordN,{1}][[1]],*)
(* <<-----*)PlotRange->{Automatic,{0.,mh}},
LabelingFunction->(placedNumber)
(*FrameTicks-> {{Automatic,Automatic},{Table[{x+0.5,x},{x,Min[coordN],Max[coordN],1}],None}}*)
];
];


(*--------------------------------*BOND ORDER*--------------------------------*)
If[TypeBondOrderQ[typeO],


hlist=HistogramList[bopO,{binGen[binsizeO]},statsO];



maxh=Position[hlist,Max[hlist[[2,;;]]  ]][[1,2]];
mh=hlist[[2,maxh]]*1.15;
offset= {1, Length@hlist[[1]]};

offset[[1]]=Position[hlist[[2]],SelectFirst[hlist[[2]],#> 0.0&]][[1,1]];

offset[[2]] = Length@hlist[[2]]- Position[Reverse@hlist[[2]],SelectFirst[Reverse@hlist[[2]], #>0.0&] ][[1,1]] +1;

hlist[[1]] = hlist[[1]][[offset[[1]];;offset[[2]]+1]];



HistCoordColours= vorColourer[#, colourGrad->OptionValue[colSch], bind->OptionValue[colourBinned], binDstr ->  hlist[[1]] ]&/@Table[ Mean[hlist[[1, a;;a+1]] ], {a, Length@hlist[[1]]-1}  ]; 





If[statsO=="Probability",placedNumber=Placed[NumberForm[#,{5,3}],Above]&];
If[statsO=="Count",placedNumber=Placed[NumberForm[#,{5,0}],Above]&];

(*--------------------------------*)
histo=Histogram[bopO,{hlist[[1]]},statsO,Axes->False,Frame->True,FrameLabel->{Style["Normalized Local Bond Order Parameter\n Mean (\!\(\*SubscriptBox[\(q\), \(i\)]\)/q"<>ToString[symO]<>"): "<>ToString[NumberForm[N@Mean[bopO],{5,3}]]<>" \[PlusMinus] "<>ToString[NumberForm[N@StandardDeviation[bopO],{5,3}]],Black,FontSize->15],Style[statsO,Black,FontSize->15]},
BaseStyle->{FontFamily->"Arial",FontSize->14,Black},
ChartStyle->HistCoordColours(*[[offset;;]]*),
GridLines->{Table[0.1x,{x,11}],Automatic},
PlotRange->{Automatic,{0.,mh}},
FrameTicks->{{Automatic,Automatic},{Table[x,{x,0.0,1.1,0.10}],Automatic}},LabelingFunction->(placedNumber)];
];


(* Return the histogram *)
histo
];


(* Bond Order and Angular Analysis *)
Options[BondOrderParameter]={passVor->True,fast->True,edge->"NOEDGE",vorbop->True,box-> {},normalized->False,debug->False};
SetOptions[BondOrderParameter,passVor->True,fast->True,edge->"NOEDGE",vorbop->True,box-> {},normalized->False,debug->False];
BondOrderParameter[unBoundedData2D_, whichL_, OptionsPattern[]] := Block[{Ylm,\[Theta]Invariant,BondOrderCalc,bondOrder,particlePickList,
vor,fullPoints,delaunayNearestNeighbours,npoints,commonVertices,commonVertex,angles,particleIndicies,vorFacetWeight,
passVor=OptionValue[passVor],
fastO=OptionValue[fast],
edgeO=OptionValue[edge],
vorbop=OptionValue[vorbop],
imagebox=OptionValue[box],
normalizedO=OptionValue[normalized],
debug=OptionValue[debug]
},
(*===================================================================*)
Ylm[l_,m_,\[Phi]_]:=SphericalHarmonicY[l,m,\[Pi]/2.,\[Phi]];
(*===================================================================*)
\[Theta]Invariant[obj2d_,nn2d_]:=If[NonPositive[(PadRight[obj2d,3]\[Cross]PadRight[nn2d,3])[[3]]],2.Pi-VectorAngle[obj2d,nn2d-obj2d],VectorAngle[obj2d,nn2d-obj2d]];
(*===================================================================*)
BondOrderCalc[nnAngles_,vorFacets_,L_]:= \[Sqrt](((4. \[Pi])/(2. L+1.))NSum[((Abs[Total[Table[vorFacets[[nn]]   Ylm[L,M,nnAngles[[nn]]],{nn,Length[nnAngles]}]]])^2),{M,-L,L,1}]);
(*===================================================================*)

npoints=Length[unBoundedData2D];

(* Remove boundary BOP's if "noedge"*)
(* If clipping happened, return the new list of 2d points as well *)
If[passVor=!= True,
If[debug===True,Print["Voronoi calculation was skipped."];];
{vor,fullPoints,delaunayNearestNeighbours,commonVertices}=passVor;
,
{vor,fullPoints,delaunayNearestNeighbours,commonVertices}=VoronoiCells[unBoundedData2D,box->imagebox,edge-> edgeO,full->True,fast->fastO];
];


If[ToUpperCase[edgeO]==="NOEDGE",particleIndicies= Pick[Range[Length[fullPoints[[1]]]],fullPoints[[2]]];
particlePickList=fullPoints[[2]];
fullPoints=fullPoints[[1]];
,
particleIndicies=Range[npoints]  ];


(* Table of Bond Order Parameters  *)
bondOrder=Table[

commonVertex=commonVertices[[particle]];

If[vorbop==True,
vorFacetWeight=MapThread[EuclideanDistance[#1,#2]&,Transpose[commonVertex]];
vorFacetWeight=Divide[vorFacetWeight,Total[vorFacetWeight]];
,
vorFacetWeight = Table[1./Length[commonVertex],{x,Length[commonVertex]}];
];


angles=\[Theta]Invariant[fullPoints[[particle,1;;2]],#]&/@fullPoints[[delaunayNearestNeighbours[[particle]],1;;2]]/.{Indeterminate-> 10.^-8};

BondOrderCalc[angles,vorFacetWeight,whichL]

,{particle,Length@fullPoints}];

	
(*bondOrder[[particleIndicies]]*)
If[normalizedO===True, bondOrder[[particleIndicies]],  bondOrder[[particleIndicies]]/ BOPexp[whichL]]


];

Options[CoordinationNeighbourCloud]={passVor->{},fast->True,coord-> 1,mod->1,poly->{} ,simstepList->{},growthR->{}};
SetOptions[CoordinationNeighbourCloud,passVor->{},fast->True,coord->1,mod->1,poly->{} ,simstepList->{},growthR->{}];
CoordinationNeighbourCloud[DataEnsemble_,OptionsPattern[]]:=Block[{simlist,data,inData,distanceAndAngle,nconfs,scale,fullPointsAroundPoly,showpoly,modAngleVar,pcPoints,pc,dens,pcWhite,viewScale=4.2
,vor,fullPoints,fullPointsVor,delaunayNearestNeighbours,commonVertices,AngleSubtract,selectedParticlesIndex,t,coordColour,commonDirection,
passVor=OptionValue[passVor],
fast=OptionValue[fast],
coord=OptionValue[coord],
mod=OptionValue[mod],
poly=OptionValue[poly] ,
simstepList=OptionValue[simstepList] ,
growthR=OptionValue[growthR] 
},


(* Default Settings *)
If[growthR==={},growthR=AverageFirstNeighbourDistance[DataEnsemble,coord]/2];
If[simstepList==={},simstepList=1];
If[poly==={},poly=standardPolygon[40]];


If[mod<1,mod=1];
If[coord<3, coordColour = 6+1, coordColour=coord+1 ];

(*===================================================================*)
AngleSubtract[refPoint2D_,toPoint2D_]:=
If[negQ[(PadRight[(refPoint2D*{-1.,0.}),3]\[Cross]PadRight[toPoint2D-refPoint2D,3])[[3]]],2Pi-VectorAngle[(refPoint2D*{-1.,0.}),toPoint2D-refPoint2D],VectorAngle[(refPoint2D*{-1.,0.}),toPoint2D-refPoint2D]];
(*===================================================================*)
modAngleVar[\[Theta]_,sym_]:= Mod[\[Theta],2.Pi/sym];
(*===================================================================*)

If[TensorRank[DataEnsemble]==2,
inData={DataEnsemble};simlist={simstepList};,
inData=DataEnsemble;simlist=simstepList;];
nconfs=Length[inData];


(* Take in VoronoiCells[ full\[Rule]True ] *)
(*  *)
(* Subtract distance R to particle from origin from {x,y,0} *)
(* Subtract same distance R to delneighbour to origin {x,y,0} *)
(* Calculate angle from x-axis to neighbour  *)
(* Combine[ For All Neighbours [ Subtract angle of NN ]]  *)

(*Print[passVor==={}];*)
If[passVor==={},
(*Print[inData[[1]]];*)
{vor,fullPointsVor,delaunayNearestNeighbours,commonVertices}=VoronoiCells[inData[[1]],full->True];
(*Print["b"];*)
,
Print["Skipping Voronoi Calculation - input as (passVor->VoronoiCells[full->True])"];
{vor,fullPointsVor,delaunayNearestNeighbours,commonVertices}=passVor;
];



fullPoints=Transpose@{fullPointsVor[[1,;;,1]],fullPointsVor[[1,;;,2]],0*fullPointsVor[[1,;;,1]]};
selectedParticlesIndex={};
selectedParticlesIndex=Pick[Range[Length[fullPointsVor[[1]]]],fullPointsVor[[2]]];
scale=simlist[[1(*conf*)]] growthR;

(* Return List of Centered coordinates *)
(* {R{x,y}, {} *)


(* Rigid Neighbour Bond-Structure { {r,\[Theta]}  }  *)
distanceAndAngle=Table[
(* Distance from central particle to neighbours *)

(* Neighbour bond structure for particle *)
({EuclideanDistance[   fullPoints[[particle,1;;2]],   fullPoints[[# ,1;;2]]  ],
(* Rotate the particle to common direction - (x-axis) *)
modAngleVar[AngleSubtract[ fullPoints[[particle,1;;2]],   fullPoints[[# ,1;;2]]]- If[coord<3,fullPoints[[#,3]],0.0]   ,mod]
}&/@ delaunayNearestNeighbours[[particle]])

,{particle,selectedParticlesIndex}
];


(* Rotate Bond-Structures to a common orientation *)
fullPointsAroundPoly=Flatten[Table[
((#[[1]]/scale)
{Cos[#[[2]]  +sym\[Theta] + If[coord<3,0.0,(*(Pi/2)*)-x]   ],Sin[#[[2]]+sym\[Theta] + If[coord<3,0.0,(*(Pi/2)*)-x]]})&/@distanceAndAngle[[p]]
,{sym\[Theta],0.,2Pi-(2Pi/mod),(2Pi/mod)}
,{p,Select[Range@Length@distanceAndAngle,If[coord<3,True&,Length[distanceAndAngle[[#]]]==coord&]]}
,{x,distanceAndAngle[[p,;;,2]] }  ],3];


distanceAndAngle=Flatten[distanceAndAngle,1];
pcPoints=fullPointsAroundPoly;


showpoly=Graphics[{EdgeForm[{Black,Thick}],CoordinationNumberColourScheme[coordColour],Polygon[poly]}];

pc=Show[{ListPlot[pcPoints,AspectRatio->Automatic,Axes->False,PlotStyle->Directive[{Opacity[0.15],CoordinationNumberColourScheme[coordColour]}],PlotRange->(viewScale{{-1,1},{-1,1}})],showpoly}];

pcWhite=Show[{ListPlot[pcPoints,AspectRatio->Automatic,Axes->False,PlotStyle->Directive[{Opacity[0.25],White(*CoordinationNumberColourScheme[sym+1]*)}],PlotRange->(viewScale{{-1,1},{-1,1}})],showpoly}];
dens=Show[{SmoothDensityHistogram[pcPoints,Automatic,"PDF",(*PlotRange\[Rule]Automatic,*)PlotRange->(viewScale{{-1,1},{-1,1}}),AxesOrigin->{0,0},AspectRatio->Automatic,Frame->False,ColorFunction->"DeepSeaColors"],showpoly}];


{pc,dens,Show[{dens,pcWhite}]}

];
Options[BondOrderPointCloud]={mod\[Theta]->1,poly->{} ,box-> {},simlist->{}, growthR->{} ,debug->False};
SetOptions[BondOrderPointCloud,mod\[Theta]->1,poly-> {},box-> {}, simlist->{},growthR->{},debug->False];
BondOrderPointCloud[DataEnsemble_ ,OptionsPattern[]]:=Block[{data,inData,distanceAndAngle,nconfs,scale,fullPointsAroundPoly,showpoly,modAngleVar,pcPoints,pc,dens,pcWhite,
mod\[Theta]=OptionValue[mod\[Theta]],
poly= OptionValue[poly],
boxO=OptionValue[box],
simlist= OptionValue[simlist],
growthR = OptionValue[growthR],
debug=OptionValue[debug]
},


If[simlist==={},simlist=1];
If[poly==={}, poly = standardPolygon[40];];
If[growthR ==={},growthR=1];

If[TensorRank[DataEnsemble]==2,
inData={DataEnsemble};simlist={simlist};,
inData=DataEnsemble;simlist=simlist;];
nconfs=Length[inData];


If[boxO==={}, boxO= AutoBoxTransform[inData[[1]]];];



fullPointsAroundPoly=Table[
data= inData[[conf]];
scale=simlist[[conf]] growthR ;


(*distanceAndAngle=Table[
{EuclideanDistance[   data2Dfull[[particle,1;;2]],   data2Dfull[[# ,1;;2]]  ],
(*AngleSubtract[ data2Dfull[[particle,1;;2]],   data2Dfull[[# ,1;;2]]]- data2Dfull[[#,3]]}  *)
 modAngleVar[AngleSubtract[ data2Dfull[[particle,1;;2]],   data2Dfull[[# ,1;;2]]]- data2Dfull[[#,3]],symmetry]}  &/@ dNeighs[[particle,2]],{particle,1,npoints}];*)

distanceAndAngle=Flatten[PbcDelaunayNeighbours[data,boxO, mod\[Theta]],1];
Table[
Table[(distanceAndAngle[[x,1]]
{Cos[distanceAndAngle[[x,2]]+ang],Sin[distanceAndAngle[[x,2]]+ang]}),
{x,1,Length[distanceAndAngle]}] . {{1./scale,0.},{0.,1./scale}}
,{ang,0.,2.Pi-(2. Pi /mod\[Theta]),2. Pi /mod\[Theta]}]
,{conf,1,nconfs}];



showpoly=Graphics[{EdgeForm[{Black,Thick}],Orange,Polygon[poly]}];


pcPoints=Flatten[fullPointsAroundPoly,2];
pc=Show[{ListPlot[pcPoints,AspectRatio->Automatic,Axes->False,PlotStyle->Blue],showpoly}];
pcWhite=Show[{ListPlot[pcPoints,AspectRatio->Automatic,Axes->False,PlotStyle->White],showpoly}];
dens=Show[{SmoothDensityHistogram[pcPoints,(*PlotRange\[Rule]2.6{{-1,1},{-1,1}},*)PlotRange->Automatic,AspectRatio->Automatic,Frame->False,ColorFunction->"DeepSeaColors"],showpoly}];


{pc,dens,Show[{dens,pcWhite}]}

];
PbcDelaunayNeighbours= Function[{unBoundedData2D,imagebox,symmetry},Block[{npoints,box,dh,bondOrder,Ylm,negQ,data2D,data2Dfull,vor,del,delConnectors,vorBox,voronoiCenters,voronoiVertexCoord,voronoiVertexNumber,sharedVoronoiPoint,nonNeighbours,voronoiCellLengths,BondOrderCalc,delaunayNearestNeighbours,fullNpoints,\[Theta]Invariant,mRange, distanceAndAngles,dNeighs,AngleSubtract,modAngleVar},

(*===================================================================*)
AngleSubtract[refPoint2D_,toPoint2D_]:=
(*refPoint2D+refPoint2D*(2,1);
(refPoint2D+toPoint2D)-refPoint2D
*)
If[negQ[(PadRight[(refPoint2D*{-1,0}),3]\[Cross]PadRight[toPoint2D-refPoint2D,3])[[3]]],2Pi-VectorAngle[(refPoint2D*{-1,0}),toPoint2D-refPoint2D],VectorAngle[(refPoint2D*{-1,0}),toPoint2D-refPoint2D]];
(*===================================================================*)
modAngleVar[\[Theta]_,sym_]:= Mod[\[Theta],2.Pi/sym];
(*===================================================================*)

(*data2Dfull=ApplyLimitedPBC[unBoundedData2D,imagebox];*)
data2Dfull=ApplyPeriodicBoundary[unBoundedData2D,imagebox ,ReturnDim->3];
npoints= Length[unBoundedData2D];

data2D=data2Dfull[[All,1;;2]];
fullNpoints= Length[data2D];


dh=Sqrt[2./ (Sqrt[3.]    (npoints /  PolyArea[imagebox])   )];
box=AutoBoxTransform[data2Dfull];

vorBox={{Min[box[[;;,1]]],Max[box[[;;,1]]]},
{Min[box[[;;,2]]],Max[box[[;;,2]]]}};
(*===================================================================*)
(* --------------------------------- *)

vor=VoronoiMesh[data2D,vorBox];
voronoiVertexCoord=MeshCoordinates[vor];



voronoiVertexNumber=Table[{x,MeshCells[vor,2][[x]][[1]]},{x,1,Length[MeshCells[vor,2]]}];
voronoiVertexNumber=Sort@Table[{Flatten@Table[
If[
inPolyQ2[voronoiVertexCoord[[voronoiVertexNumber[[b,2]]]],data2D[[a,1]],data2D[[a,2]]]
,a,{}],{a,Range[Length[data2D]]}],MeshCells[vor,2][[b]][[1]]},{b,1,Length[MeshCells[vor,2]]}];


del=DelaunayMesh[data2D];
delConnectors=MeshCells[del,1];



delaunayNearestNeighbours=Table[{y,Flatten[Table[If[delConnectors[[x,1,1]]==y,delConnectors[[x,1,2]],If[delConnectors[[x,1,2]]==y,delConnectors[[x,1,1]],{}]]
,{x,1,Length[delConnectors]}]]},{y,1,Max[delConnectors[[;;,1]]]}];


Do[
nonNeighbours={};

Do[

sharedVoronoiPoint=DeleteDuplicates@Flatten[Table[Cases[voronoiVertexNumber[[neighbourParticle,2]],voronoiVertexNumber[[constantParticle,2,x]]],{x,Length[voronoiVertexNumber[[constantParticle,2]]]}]];

If[Length[sharedVoronoiPoint]<=1,
nonNeighbours=PadRight[nonNeighbours,1,neighbourParticle];
Break[];];
,{neighbourParticle,delaunayNearestNeighbours[[constantParticle,2]]}];

If[nonNeighbours!= {},
delaunayNearestNeighbours[[constantParticle,2]]=DeleteCases[delaunayNearestNeighbours[[constantParticle,2]], {___,nonNeighbours,___} ];];
,{constantParticle,Range[fullNpoints]}];


dNeighs=delaunayNearestNeighbours[[;;npoints]];



distanceAndAngles=Table[
{EuclideanDistance[   data2Dfull[[particle,1;;2]],   data2Dfull[[# ,1;;2]]  ],
AngleSubtract[ data2Dfull[[particle,1;;2]],   data2Dfull[[# ,1;;2]]]- data2Dfull[[#,3]]}  
(* modAngleVar[AngleSubtract[ data2Dfull[[particle,1;;2]],   data2Dfull[[# ,1;;2]]]- data2Dfull[[#,3]],symmetry]}*)  &/@ dNeighs[[particle,2]],{particle,1,npoints}]
]];

(*  END OF (Experiment-based) OPTIMIZED FUNCTIONS *)


(*  Start of GranSimTools Functions *)
Options[PlotFinalConfiguration]= {polygon->{},inflate-> 0,box->{}, boundary->"PBC",symmetry->6,type->"colour",mod-> 1,colour-> Gray,FFT->False,full-> False,fast->True};
SetOptions[PlotFinalConfiguration,polygon->{},inflate-> 0,box->{}, boundary->"PBC",symmetry->6,type->"colour",mod-> 1,colour-> Gray ,FFT->False,full-> False,fast->True];
PlotFinalConfiguration[Rcurrent_,OptionsPattern[]]:=
Block[{npoints,PlottedPolys,placedPolygons,polyColour,boxColour,ExtraLines,ExtraInset,vor,
polygonO=OptionValue[polygon],
inflateO=OptionValue[inflate],
boxO=OptionValue[box],
boundaryO=OptionValue[boundary],
typeO=OptionValue[type],
symmetryO=OptionValue[symmetry],
modO=OptionValue[mod],
colourO=OptionValue[colour],
fftO=OptionValue[FFT],
fullO=OptionValue[full],
fastO=OptionValue[fast]
},

npoints = Length[Rcurrent];
If[ Length[Rcurrent[[1]]]==4,Max@Rcurrent[[All,4]];  ];

If[polygonO==={}, polygonO=standardPolygon[100]];
If[boxO==={},boxO=AutoBoxTransform[Rcurrent]];
If[inflateO===0,inflateO=AverageFirstNeighbourDistance[Rcurrent]/2];
If[colourO=== {},colourO=GrayLevel[0.7]];

(* Inflate Polygons To Correct Size *)
placedPolygons=Inflate[Rcurrent,polygonO,inflateO,1.,1.]; 


(* --------- Colour Selectors ----------- *)
Which[
ToUpperCase[typeO]==ToUpperCase["COLOUR"]||typeO==={},
polyColour=Table[colourO,{x,Range@npoints}];
boxColour=Graphics[{EdgeForm[{Black}],White,Opacity[0],Polygon[boxO]}];
(* Default colour code to mimic AFM images *)
,
ToUpperCase[typeO]==ToUpperCase["STM"],
polyColour=Table[RGBColor[{208,127,41}/256],{x,Range@npoints}];
boxColour=Graphics[{EdgeForm[{Black}],RGBColor[{92,10,12}/256],Polygon[boxO]}];
,
(* Voronoi Type  *)
TypeVoronoiQ[typeO],
vor=VoronoiCells[Rcurrent[[;;,1;;2]],box-> boxO,edge-> boundaryO,full->False,fast-> fastO];
polyColour=AssignVoronoiColour[vor];
boxColour=Graphics[{EdgeForm[{Black}],White,Polygon[boxO]}];
,
(*  Angle Type  *)
TypeAngularDirectionQ[typeO],
polyColour=(BjornAngleScheme[#,modO]&/@Rcurrent[[;;,3]]);
boxColour=Graphics[{EdgeForm[{Black}],White,Polygon[boxO]}];
,
(*  Coordination Type  *)
TypeCoordinationQ[typeO],
vor=VoronoiCells[Rcurrent[[;;,1;;2]],box-> boxO,edge-> boundaryO,full->fullO,fast-> fastO];
polyColour=AssignCoordinationColour[vor];
boxColour=Graphics[{EdgeForm[{Black}],White,Polygon[boxO]}];
,
(*  Bond Order Type*)
TypeBondOrderQ[typeO],
vor=VoronoiCells[Rcurrent[[;;,1;;2]],box->boxO,edge-> boundaryO, full->True,fast-> fastO];
polyColour=BondOrderParameter[Rcurrent[[;;,1;;2]], symmetryO,box-> boxO,passVor->vor,edge-> boundaryO];
vor=vor[[1]];
(*BOP RED COLOUR RGBColor[147/255,3/255,46/255]*)
polyColour=BondOrderColourScheme[#,symmetryO]&/@polyColour[[;;Length[vor]]];
boxColour=Graphics[{EdgeForm[{Black}],RGBColor[117/255,176/255,156/255]   ,Polygon[boxO]}]

];

(* --------- Plot Polygons ----------- *)
PlottedPolys= Table[
Graphics[{
{EdgeForm[{Black}],polyColour[[n]],Polygon[placedPolygons[[n]]]}
}],{n,1,npoints}];


(*ExtraInset=Show[{(*DiffractionInset[Rcurrent,box],*)RotationInset[Rcurrent,box,typeArg]}];*)

Show[{boxColour,PlottedPolys}]
];

(* Planar Statistics - Confined Data to Square Box 1x1 *)
(*  ------ ---- ----- LOADS IN REQUIRED PROCEDURES   *)
(*  Point Clouds           *) 
(* point size depends on the number of particles and samples *)
pcSize=Function[{npoints},Block[{psize=0.},
	If[npoints<= 9,  psize=0.0100]; 
	If[9<npoints <= 16,psize=0.0060];
	If[16<npoints<= 25,psize=0.0040]; 
	If[25 <npoints<= 36,psize=0.0025];
	If[36<npoints <= 64,psize=0.0015];
	If[64<  npoints<=  81,     psize=0.0011];
	If[81 <  npoints<1024,psize=0.0001];
	If[ npoints>= 1024,psize=0.00005];  
psize]];
Options[PlotPointCloud]={colour->BjornColourScheme[[1]],box-> closedBindingBox,split-> False };
SetOptions[PlotPointCloud,colour-> BjornColourScheme[[1]],box->closedBindingBox,split-> False ];

PlotPointCloud[data2d_, OptionsPattern[]]:=Block[{pointcloud,planardensity,plotdata,pointsize, 
colourO=OptionValue[colour],
boxO=OptionValue[box],
splitO=OptionValue[split]
},


pointsize=pcSize[Length[data2d[[1,All,All]]]];
plotdata=Flatten[data2d[[All,All,1;;2]],1];

pointcloud= Graphics[{colourO,PointSize[ pointsize],Point[plotdata],Black,Line[boxO]}];

If[splitO==True,

If[boxO[[1]]==boxO[[-1]],boxO=Most@boxO];
planardensity= Graphics[{
Texture[PlotPlanarDensity[data2d,view->2 ,scalebar-> False]],Opacity[0.85],Polygon[Most@boxO,VertexTextureCoordinates->Most@boxO]
}];
pointcloud= Show[{pointcloud,planardensity}];
];

(* return value *)
pointcloud
];
Options[PlotPlanarDensity]={box-> closedBindingBox,view->2, scalebar-> True,symmetric-> False,bandwidth->0.,rescale-> 0.};
SetOptions[PlotPlanarDensity,box->closedBindingBox,view->2,scalebar-> True,symmetric-> False ,bandwidth-> 0.,rescale-> 0.];

PlotPlanarDensity[data2d_,OptionsPattern[]]:=Block[{plotdata,plot,colF,colFbar,
boxO=OptionValue[box],
viewO=OptionValue[view],
scalebarO=OptionValue[scalebar],
symmetricO=OptionValue[symmetric],
bandwidthO=OptionValue[bandwidth],
rescaleO=OptionValue[rescale]
},


colF=Function[{height},ColorData["Rainbow"][(height)/rescaleO]];
colFbar=Function[{height},ColorData["Rainbow"][(height)]];

plotdata=Flatten[data2d[[All,All,1;;2]],1];

If[viewO!= 2&&viewO!= 3,viewO=2];
If[symmetricO==True,plotdata=Flatten[ApplyBoxSymmetryToEnsemble[data2d],2];] ;
If[bandwidthO<= 0.,bandwidthO=pcSize[Length[plotdata]]];
If[rescaleO <=   0.0,rescaleO=1.];


(* Code to force the colour scale of the bar (maxTempValue)*)
(* ColorFunctionScaling\[Rule]False, *)
(* ColorFunction\[Rule]Function[{height},ColorData["Rainbow"][height/(maxTempValue)]]  *)
(* PlotLegends\[Rule]Placed[{Automatic,{0,(maxTempValue)}},Below] *)

Which[
viewO == 2,
plot=SmoothDensityHistogram[
plotdata,Automatic,"PDF",
Frame->False,
FrameTicks->None,
AxesOrigin->{0,0},
AspectRatio->1,

Background->None,
PlotRangePadding->None,
ImagePadding->None,

ColorFunctionScaling->True,
ColorFunction->colF,
PlotLegends->Placed[BarLegend[{colFbar,{0,rescaleO}},LegendLabel-> "PDF"],Below]

];,

viewO==3,
plot=SmoothHistogram3D[plotdata,bandwidthO,"PDF",AxesOrigin->{0,0},AspectRatio->1,PlotRange->{{0,1},{0,1}},PlotRangePadding->None,ColorFunction->"Rainbow",Background->None,Mesh->None,PlotLegends->Placed[{Automatic,{0,1.}},Below] ];
];

If[scalebarO == True,plot,plot[[1]] ]
];
Options[PlotRegistryMap]={box-> closedBindingBox,view->2, F-> True,symmetric-> False};
SetOptions[PlotRegistryMap,box->closedBindingBox,view->2,symmetric-> False] ;

PlotRegistryMap[data2d_,OptionsPattern[]] :=Block[{plotdata,plot,
boxO=OptionValue[box],
viewO=OptionValue[view],
scalebarO=OptionValue[scalebar],
symmetricO=OptionValue[symmetric]
},

plotdata=Flatten[data2d[[All,All,1;;2]],1];

If[viewO!= 2&&viewO!= 3,viewO=2];
If[symmetricO==True,plotdata=Flatten[ApplyBoxSymmetryToEnsemble[data2d],2];] ;

Which[viewO == 2,
plot=DensityHistogram[plotdata,"Scott","ProbabilityDensity",
(*"FreedmanDiaconis",{"Log","PDF"},*)
Frame->False,
FrameTicks->False,
AspectRatio->1,
ColorFunction->Function[{height},
ColorData["Rainbow"][height]],
Background->None,
PlotRangePadding->None,
ImagePadding->None,
PerformanceGoal->"Quality",
ChartLegends->Placed[BarLegend[Automatic,LegendLabel-> "ProbabilityDensity"],Below]
];

Show[{Graphics[Line[boxO]],If[scalebarO == True,plot,plot[[1]] ]}]

,
viewO==3,
Histogram3D[plotdata,"Scott","ProbabilityDensity",PlotRange->{{0,1},{0,1},All},PlotRangePadding->None,Axes->{False,False,True},LabelStyle->Directive[FontFamily->"Helvetica",Black,Larger],Boxed->True,ViewPoint->{Pi,Pi/2,2},PerformanceGoal->"Speed",ChartElements->Graphics3D[Cylinder[]],ColorFunction->Function[{height},ColorData["Rainbow"][height]],Background->None]
]

];

(* Overlap Network - Routines *)
(*  Inflates the Polygons to OverlapScale % times bigger  *)
(*  ------------------------------------------------------  *)

OverlapNetwork[pointPattern2D_,polygon_, Grow_,GrowthRate_,inflateRatio_,box_, Args___]:=
Block[{polys, nearestN,finalOverlapPolys,finalPolyAreas,numNear,overlapByPoly,npoints,maxLine,fullLinesToNearestN,finalSize,finalPlot,centerPoints, lineColour},
If[Length[Grow]==0,finalSize=Grow,finalSize=First[Grow]];
polys = Inflate[pointPattern2D, polygon, finalSize,GrowthRate,inflateRatio];
nearestN=FindNearestForOverlap[pointPattern2D];
finalOverlapPolys=PolygonsThatOverlap[polys,nearestN];
(*finalPolyAreas =PolyArea[finalOverlapPolys[[#]]]&/@ Range[Length[finalOverlapPolys]];*)
finalPolyAreas=Table[PolyArea[finalOverlapPolys[[x]]],{x,1,Length[finalOverlapPolys],1}];

If[Args===Null,lineColour=Green,lineColour=Args];

numNear=Length[nearestN[[1]]];
npoints=Length[pointPattern2D];
overlapByPoly={};
overlapByPoly = Table[{finalPolyAreas[[(x numNear)+#]]}&/@Range[numNear],{x,0,npoints-1,1}];

(*overlapByPoly = Table[{PolyArea[finalOverlapPolys[[(x numNear)+#]]]}&/@Range[numNear],{x,0,npoints-1,1}];*)

maxLine=PolyArea[polygon* GrowthRate*finalSize];


fullLinesToNearestN = Table[
If[Part[overlapByPoly[[const,#]],1]!=0.0,
Graphics[{Thickness[0.035Part[overlapByPoly[[const,#]],1]/maxLine],
lineColour,Line[{pointPattern2D[[const,1;;2]],pointPattern2D[[nearestN[[const,#]],1;;2]]}]}],
Unevaluated[Sequence[]]
]&/@Range[numNear]
,{const,1,npoints}];


centerPoints = Table[Graphics[{Black,Point[pointPattern2D[[const,1;;2]]]  }],{const,1,npoints}];


(*If[Args===Null,finalPlot = Show[{PlotFinalConfiguration[pointPattern2D,polygon, finalSize,GrowthRate,box,"automatic"],fullLinesToNearestN,centerPoints}]; ];

If[Args=!=Null,
If[ToUpperCase[Args[[1]]]\[Equal]"BOPVORONOI",
finalPlot = Show[{PlotVoronoiBOPcells[pointPattern2D,bindingBox, Args[[2]], "pbc" ],fullLinesToNearestN,centerPoints}];
];
];*)

finalPlot = Show[{fullLinesToNearestN,centerPoints}]

];
(*  Inflates the Polygons to OverlapScale % times bigger  *)
(*  ------------------------------------------------------  *)
Inflate=Compile[{{Rcurrent,_Real,2},{poly,_Real,2},{SimStep,_Real},{GrowthRate,_Real},{OverlapScale,_Real}},
Block[{inflatedPolys,rot,OverlapScaleSize,M},

OverlapScaleSize = SimStep*GrowthRate*OverlapScale;

inflatedPolys=Table[
rot = Rcurrent[[n,3]];
M={{Cos[rot], -Sin[rot]},{Sin[rot],Cos[rot]}};
Table[
((M . poly[[j]])*OverlapScaleSize)+{Rcurrent[[n,1]],Rcurrent[[n,2]]}
,{j,1,Length[poly],1}]
,{n,1,Length[Rcurrent],1}];

inflatedPolys
] ,RuntimeOptions->"Speed",CompilationTarget->"C"];
(*  Uses "Nearest" to finds the closest N=18 particles  *)
(*  ------------------------------------------------------  *)
FindNearestForOverlap= Compile[{{testdata,_Real,2}},
Block[{NN,maxNN,pointPattern},
If[Length[testdata]<19,maxNN=Length[testdata],maxNN=19(*Length[testdata]*)];

pointPattern=testdata[[All,1;;2]];
Table[
NN =Nearest[pointPattern,pointPattern[[x]],maxNN];
Drop[Flatten[Position[pointPattern,NN[[#]]]&/@Range[maxNN]],1]
,{x,1,Length[pointPattern]}]
],RuntimeOptions->"Speed",CompilationTarget->"C"];
(*  Sorts any 2D points by anngle, to create a proper polygon  *)
(*  ----------------------------------------------------------  *)
sortByAngle = Function[{data2D},
Block[{base,angle,remain,s,path,pos},
(**)
pos=Position[data2D, Max[data2D[[;;,2]]]];
base=data2D[[pos[[1,1]]]];
(*
base=data2D[[Random[Integer,{1,Length[data2D]}]]];
*)
(*base=data2D[[1]];*)

angle[a_List,b_List]:=Apply[ArcTan,(b-a)];
remain=Complement[data2D,{base}];
s=Sort[remain,angle[base,#1]<= angle[base,#2]&];
Join[{base},s,{base}]
]
];

(*  Takes a list of Inflated Polygons and Nearest Neighbours  *)
(*    to build a list of polygons that are the overlap        *)
(*  ----------------------------------------------------------  *)
PolygonsThatOverlap=Function[{inflatedPolys,NN},

Block[{const,var,np,npolySides,InsidePointTruth,
overlappedPoints,overlappedPolygons},

(* Initialization *)
overlappedPoints={};
overlappedPolygons={};
npolySides=Length[inflatedPolys[[1]]];

(* LOOP over the number of particles - constant *)
For[const=1,const<= Length[NN],const++,
(*  LOOP over all Nearest Neighbours - variable *)
Do[
overlappedPoints={};
(* Go over all points of constant poly and then var poly *)
(* Add the points into one object *)
(*  LOOP over all variable polygon points *)

For[np=1,np<= npolySides,np++,
(* If this point is inside the Inflated Constant Polygon*)
InsidePointTruth=False;
InsidePointTruth =inPolyQ2[inflatedPolys[[const]],inflatedPolys[[var,np,1]],inflatedPolys[[var,np,2]] ];
If[InsidePointTruth ==True, 
AppendTo[overlappedPoints,inflatedPolys[[var,np]] ]
]];

(*  LOOP over all constant polygon points *)
For[np=1,np<= npolySides,np++,
(* If this point is inside the Inflated Constant Polygon*)
InsidePointTruth=False;
InsidePointTruth =inPolyQ2[inflatedPolys[[var]],inflatedPolys[[const,np,1]],inflatedPolys[[const,np,2]] ];
If[InsidePointTruth ==True, 
AppendTo[overlappedPoints,inflatedPolys[[const,np]] ]
]];

(* Adds a "zero" area polygon if No Overlap is detected *)
If[overlappedPoints!={},
AppendTo[overlappedPolygons,sortByAngle[overlappedPoints]],AppendTo[overlappedPolygons,{{0,0},{0,0},{0,0}}]];

,{var, NN[[const]]}];];

overlappedPolygons
]];

(* Visualizations of the Angular Orientation of Particles *)
RotationWheel[Ensemble_,Args___]:=Block[{modAng=1,data,\[Phi],\[Theta],max,len,prob,maxAng,circ,box,r ,assignAngleColour,angleColours},
If[TensorRank[Ensemble]==2,data={Ensemble},data=Ensemble];

If[Args===Null,
\[Phi]=(*Sort@*)Flatten[data[[;;,;;,3]]];
(*findOpac=(2len/modAngle^2)^-(1/modAngle);*)
,
modAng={Args}[[1]];
\[Phi]=(*Sort@*)Mod[Flatten[data[[;;,;;,3]]],2Pi/modAng]
];


{\[Theta],prob}=HistogramList[\[Phi],Automatic,"Probability"];
(*{\[Theta],prob}=HistogramList[\[Phi],{Pi/Length[\[Phi]]},"Probability"];*)


len=Length[prob];
max=Max[prob];
maxAng=2Pi/modAng;
circ=N@Flatten[{{{0,0}},Table[{Cos[x],Sin[x]},{x,0,maxAng,maxAng/100}]},1];
r=Max[EuclideanDistance[RegionCentroid[Polygon[circ]], circ[[#]]]&/@Range[Length[circ]]];
(*Graphics[Table[{ ColorData["Rainbow"][(max-prob[[x]])/max],Line[{{0,0},prob[[x]]{Cos[\[Theta][[x]]],Sin[\[Theta][[x]]]}}]},
{x,Range@len}]]*)



assignAngleColour[angleList_,rot_]:=Block[{angBin,col},
angBin=Table[n/rot ,{n,(Pi/2)/2,2Pi,Pi/2}];
col={
RGBColor[{255,51,51}/256],(*bjorn red*)
Yellow,
(*RGBColor[0.545,0.0,0.8],*) (*bjorn orange*)
RGBColor[{0,153,51}/256], (*bjorn green*)
(*RGBColor[147/255,3/255,46/255]*)
(*Yellow*)
Blue
(*RGBColor[{51,51,153}/256]*)(*bjorn blue*)
};


Table[
Which[
angleList[[x]]<= angBin[[1]]|| angleList[[x]]>  angBin[[4]],
col[[1]],
(*bjorn red*)
angleList[[x]]<=  angBin[[2]]&& angleList[[x]]>  angBin[[1]],
col[[2]],
(*bjorn purple*)
(*bjorn green*)
angleList[[x]]<=  angBin[[3]]&& angleList[[x]]> angBin[[2]],
col[[3]],
(*bjorn green*)
angleList[[x]]<=  angBin[[4]]&& angleList[[x]]> angBin[[3]],
col[[4]](*bjorn blue*)
],{x,Range[Length[angleList]]}]
];

angleColours=assignAngleColour[\[Theta],modAng];

box=Table[RegionCentroid[Polygon[circ]]+r{Cos[x],Sin[x]},{x,(Pi/4)+(Pi/2Range[4])}];

Show[{
Graphics[{White,Polygon[box]}],
Graphics[{EdgeForm[{Black,Thick}],GrayLevel[0.97],Polygon[circ]},PlotRange->All],
Graphics[Table[{ Thick,angleColours[[x]],Line[{{0,0},prob[[x]]/max{Cos[\[Theta][[x]]],Sin[\[Theta][[x]]]}}]},{x,Range@len}]]
}]

];
RotationHistogram[Ensemble_,Args___]:=Block[{data,\[Theta],modAngle=3,xFrameText},

If[TensorRank[Ensemble]==2,data={Ensemble},data=Ensemble];

If[Args===Null,
\[Theta]=Sort@Flatten[data[[;;,;;,3]]];
xFrameText="Angle of Particles: (\[Theta])";,
modAngle={Args}[[1]];
\[Theta]=Mod[Sort@Flatten[data[[;;,;;,3]]],2Pi/modAngle];
xFrameText="Angle of Particles: Mod(\[Theta], 2\[Pi]/"<>ToString[modAngle]<>")";
];


Histogram[\[Theta],"Wand","Probability",Axes->False,Frame->True,FrameTicks->{{Automatic,None},{(Pi/(2modAngle))(Range[(4modAngle)+1]-1),None}},ColorFunction->Function[{height},ColorData["Rainbow"][height]],PlotTheme->"Detailed",FrameLabel->{xFrameText,"Probability"}]
];


(* Density Calculations - Expectation Values and Distributions *)
PackingFraction[Rcurrent_,poly_,Grow_,GrowthRate_,box_]:=Block[{TempPolygonPoints,rot,M,Centeroid,OverlapScaleSize,np,FullPolyPoints,j,SimStep},
np=Length[Rcurrent];
j=Length[poly];
If[Length[Grow]==0,SimStep=Grow,SimStep=First[Grow]];
OverlapScaleSize=(GrowthRate) *SimStep;
FullPolyPoints=Table[
rot = Rcurrent[[n,3]];
M={{Cos[rot], -Sin[rot]},{Sin[rot],Cos[rot]}};
Centeroid={Rcurrent[[n,1]],Rcurrent[[n,2]]};
Table[
((M . poly[[i]])*OverlapScaleSize)+Centeroid,
{i,1,j}]
,{n,1,np}
];
Total[PolyArea[FullPolyPoints[[#]]]&/@Range[np]]/ PolyArea[box]
];
FindMaxProbabilityFromList[pf_]:=Block[{d,pos,interp,maxPF ,max,min,std},
d=SmoothKernelDistribution[pf,"Scott"];
std=StandardDeviation[pf];
min=(Min[pf]-std);
max=(Max[pf]+std);

interp=Table[{x,PDF[d,x]},{x,min,max,((max-min)/10000.)}];

pos=Position[interp[[;;,2]],Max[interp[[;;,2]]  ]][[1]];

maxPF=interp[[pos]][[1,1]];

{interp[[pos]][[1,1]],
Position[pf,Nearest[pf,maxPF][[1]]][[1]][[1]]}

(*If[args=!=Null,
If[ToUpperCase[ToString[args]]\[Equal]ToUpperCase["All"],
{interp[[pos]][[1,1]],
Flatten@Position[pf,Nearest[pf,maxPF][[1]]]}
,
If[ToUpperCase[ToString[args]]\[Equal]ToUpperCase["Random"],
{interp[[pos]][[1,1]],
RandomChoice@Flatten@
Position[pf,Nearest[pf,maxPF][[1]]]}

]]
,*)

];
Options[AreaFractionDistribution]={names->{},colours-> RegularPolygonColourScheme};
SetOptions[AreaFractionDistribution,names->{},colours-> RegularPolygonColourScheme ];
AreaFractionDistribution [pf_, OptionsPattern[]]:=Block[{labelsO= OptionValue[names],coloursO= OptionValue[colours]},
DistributionChart[pf,
ChartStyle->coloursO,
AxesStyle->Directive[Black,15],
(*PlotRange\[Rule]{{0.5,Length[pf]+.5},{Min[pf],Max[pf]}},*)
GridLines->Automatic,
(*GridLines\[Rule]{Automatic, Table[Pi/4 +(0.05 x),{x,-6,6}]}*)
(*GridLines\[Rule]{{0.5,(2+Pi)/2,(Pi+1+2Pi-1.05)/2,(2Pi+2Pi+1.1)/2,(2Pi+2.1+3Pi+0.05)/2,(3Pi+1.1+3Pi+2.2)/2,4Pi+.7},{(*Pi/4.*)}},*)
If[labelsO=!= {},ChartLegends->labelsO, ChartLegends->Table["Data: "<>ToString[x],{x,Length[pf]}]],

FrameLabel->{"Ensembles of Molecules","Area Fraction (\[Phi])"},
FrameStyle->Directive[Thick],
BaseStyle->{FontFamily->"Arial",FontSize->14}
]
];
ParticleDensityDistribution[pf_,pfNames_,args___]:=Block[{npf,smoothPF,pfEns,colours,bstyle,polys,lmsize},
(*If[TensorRank[pf]\[Equal]1,
npf=1;pfEns={pf};,
npf=Length[pf];pfEns=pf;];
Print[TensorRank[pf]];
Print[npf];*)

If[Length[pf]!= Length[pfNames],Throw[$Failed]];
npf=Length[pf];pfEns=pf;

colours={
Black,
RGBColor[0.545,0.0,0.8],
Red,
RGBColor[0.5529,0.7137,0.0],
RGBColor[126/256,232/256,250/256],
RGBColor[128/256,255/256,114/256],

RGBColor[229/256,140/256,138/256],
RGBColor[7/256,80/256,20/256],
RGBColor[36/256,34/256,127/256]
};

colours = hexToRGB[#] &/@{"#000000","#A054B7","#C42021","#61C6EC","#A7FFA4","#FDE756","#F68658","#D8B2DE"};


(*colours=Table[ColorData["Rainbow",x],{x,0,1,1/Length[pf]}];*)
bstyle={FontFamily->"Helvetica",FontSize->14};

(*If[args===Null,colours=colours;,colours=args;];*)
If[args===Null,colours=colours;,
polys=Table[
Graphics[{
EdgeForm[{Black}],
(*EdgeForm[Directive[Thin,Dashed,Black]],*)
Directive[{colours[[x]],Opacity[0.5]}],
(*colours[[x]],*)
Polygon[args[[x]]]
}
],
{x,Range@Length[args]}];];

Show[Table[

SmoothHistogram[pfEns[[ens]],
Automatic,"PDF",
Filling->Axis,
PlotStyle->{colours[[ens]],Thick},
Filling->Axis,
FillingStyle->Directive[{Opacity[0.2],colours[[ens]]}] ,
PerformanceGoal->"Quality",
FrameTicks->True,
Frame->{True,False,False,False},
Axes-> False,
BaseStyle->bstyle,
If[args===Null,
PlotLegends->SwatchLegend[{Directive[{Opacity[0.5],colours[[ens]]}]},{pfNames[[ens]]} ],
lmsize=50-(5Length[polys]);
PlotLegends->SwatchLegend[{pfNames[[ens]]},LegendMarkers-> polys[[ens]],LegendMarkerSize->{{lmsize,lmsize}}]
]
,
FrameLabel->{"Particle Density"},
PlotRange->All

 ],


{ens,Range[npf]}],PlotRange->All] 

];

(* Pattern Matching Routines - Optimized for 1x1 Box *)
LoneNormCalc=Compile[{{pattern,_Real,2},{testCase,_Real,2}},
Block[{},
Norm[pattern-testCase]/Length[pattern]
],CompilationTarget->"C",RuntimeOptions->"Speed"];
QuickDistanceSimilarityCheckQ=Function[{pattern,testCases,simThreshold},
Block[{nCases=Length[testCases],allSim,maxSim},
(*simThreshold=1./(100*Sqrt[Length[pattern]])*)
allSim=Table[ LoneNormCalc[pattern,testCases[[i]] ],{i,1,nCases}];
maxSim=Min[allSim];
If[  maxSim<=  simThreshold,testCases[[  First[Position[allSim,maxSim ]  ]    ]],
False
]
]];
QuickDistanceSimilarityCheckQuiet=Function[{pattern,testCases,simThreshold},
Block[{nCases=Length[testCases],allSim,maxSim},
(*simThreshold=3./(100*Sqrt[Length[pattern]])*)
allSim=Table[ LoneNormCalc[pattern,testCases[[i]] ],{i,1,nCases}];
maxSim=Min[allSim];
If[  maxSim<=  simThreshold,True,
False
]
]];
SimNormCalc=Compile[{{pattern,_Real,2},{testCase,_Real,2}},
Block[{},
1.-2. * (Norm[pattern-testCase]/(Norm[pattern]+Norm[testCase ]))
],CompilationTarget->"C",RuntimeOptions->"Speed"];
QuickSimilarityMax=Function[{pattern,testCases},
Block[{nCases=Length[testCases],allSim,maxSim},
allSim=Table[ SimNormCalc[pattern,testCases[[i]] ],{i,1,nCases}];
maxSim=Max[allSim];
(*allSim=Table[ LoneNormCalc[pattern,testCases[[i]] ],{i,1,nCases}];*)
(*maxSim=Min[allSim];*)
First[Position[allSim,maxSim ]  ]
]];
QuickSimilarityCheckQ=Function[{pattern,testCases,simThreshold},
Block[{nCases=Length[testCases],allSim,maxSim},
allSim=Table[ SimNormCalc[pattern,testCases[[i]] ],{i,1,nCases}];
maxSim=Max[allSim];
If[Length[Position[allSim,maxSim ]] >1,Max[Table[ SimNormCalc[pattern,testCases[[i]] ],{i,Position[allSim,maxSim ]}]]];
If[  maxSim>= simThreshold,testCases[[  First[Position[allSim,maxSim ]  ]    ]],
False
]
]];
QuickSimilarityCheckQuiet=Function[{pattern,testCases,simThreshold},
Block[{nCases=Length[testCases],allSim,maxSim},
allSim=Table[ SimNormCalc[pattern,testCases[[i]] ],{i,1,nCases}];
maxSim=Max[allSim];
If[  maxSim>= simThreshold,True,
False
]
]];
ApplyBoxSymmetryToEnsemble[Ensemble_,modNumber___]:=(*Function[{Ensemble,modNumber},*)Block[{
nconfs=Length[Ensemble],
npoints=Length[Ensemble[[1]] ],
(*  Initialize the Symmetry Rotations *)
nsym=8,
rx=ReflectionTransform[{1,0},{1/2,1/2}],
ry=ReflectionTransform[{0,1},{1/2,1/2}],
rxy=ReflectionTransform[{-1,1},{1/2,1/2}],
ryx=ReflectionTransform[{1,1},{1/2,1/2}],
c4=RotationTransform[2Pi/4],
s41,
data2d,
t,
modAngle,
angleList={},
rotationModNumber
},

(*Print[modNumber===Null];*)
If[modNumber===Null,rotationModNumber=0,
Which[Length[modNumber]==0,rotationModNumber=modNumber,
Length[modNumber]> 0,rotationModNumber=modNumber[[1]]];
]
;

(*Print[Length[modNumber]];*)
(*Print[rotationModNumber];*)
(*Print[modNumber\[Equal]Null];*)
(* Error Checking *)
If[
(*If: it is only one config being passed:*)
TensorRank[Ensemble]==2,
data2d={Ensemble[[All,1;;2]]};
nconfs=1;
npoints=Length[data2d[[1]] ];
If[Max[data2d]> 1,Return[] ];
,
(*If: it is only one config being passed:*)
data2d=Ensemble[[All,All,1;;2]];
angleList =Ensemble[[All,All,3]];

If[Max[data2d]> 1,Return[] ];
];
(*Print[Length@angleList];*)
(*Print[nconfs];*)
(*notation:a=angle mod (2 pi/n) where n is the number of sides in the polygon;b is the new angle*)
(*Step zero is to recast all angles as mod (2 pi/n)*)

(*b=rot(pi/2):a=pi/2+a*)
(*m(yy):a=pi-a*)
(*m(xx):a=2pi-a*)
(*m(xy):a=pi/2-a*)
(*m(yx):a=3 pi/2-a*)

s41=Table[{1,0},{i,1,npoints}];
t=Table[{},{configurations,1,nconfs},{symmetries,1,nsym}];

(* Apply the Rotations for 1x1 Box *)
Do[
t[[i,1]]=c4[  data2d[[i]]  ]      +s41;
t[[i,2]]=c4[  t[[i,1]]  ]+s41;
t[[i,3]]=c4[  t[[i,2]]  ]+s41;
t[[i,4]]=rx[ data2d[[i]]   ];
t[[i,5]]=ry[  data2d[[i]]  ];
t[[i,6]]=rxy[  data2d[[i]]  ];
t[[i,7]]=ryx[  data2d[[i]]  ]; 
t[[i,8]]=data2d[[i]];     ,{i,1,nconfs}];

(*Print["Successful Sym Rot"];*)
(*Print[Length[angleList]];*)
(*Print[Length[angleList[[1]]]];*)
(*Print[angleList];*)
(* Apply the Rotations for 1x1 Box *)
(*Only Use Rotation if it is specified in header*)

If[rotationModNumber!=0,
modAngle[\[Theta]_]:= Mod[\[Theta],2.Pi/rotationModNumber];
Do[
(* Rotation:  Pi/2 *)
t[[i,1]]=Join[ t[[i,1]],List/@Flatten@Transpose@modAngle[{angleList[[i]]} + (Pi/2)],2];
t[[i,2]]=Join[t[[i,2]],List/@Flatten@Transpose@modAngle[{angleList[[i]]} + (Pi)],2];
t[[i,3]]=Join[t[[i,3]],List/@Flatten@Transpose@modAngle[{angleList[[i]]}+ (3 Pi/2)],2];
t[[i,4]]=Join[t[[i,4]],List/@Flatten@Transpose@modAngle[{Pi- angleList[[i]]}  ],2];
t[[i,5]]=Join[t[[i,5]],List/@Flatten@Transpose@modAngle[{2Pi-angleList[[i]]} ],2];
t[[i,6]]=Join[t[[i,6]],List/@Flatten@Transpose@modAngle[{(Pi/2)- angleList[[i]]}],2];
t[[i,7]]=Join[t[[i,7]],List/@Flatten@Transpose@modAngle[{(3Pi/2)- angleList[[i]] }],2];
t[[i,8]]=Join[t[[i,8]],List/@Flatten@Transpose@modAngle[{angleList[[i]]} ],2];   

,{i,1,nconfs}];
];

(* Sort the Ensemble Rotations*)
Do[  Do[  t[[i,j]] = ADOsort[ t[[i,j]] ],{j,1,nsym}],{i,1,nconfs}];



(* Return a new ensemble of applied Box Symmetries *)
t
];
(*,CompilationTarget\[Rule]"C",RuntimeOptions->"Speed"*)(*];*)

ADOsort=Function[{data2d},
If[TensorRank[data2d]<= 2 && Length[data2d[[1]]]<3,
 Sort[data2d,Norm[#1]*#1[[2]]/#1[[1]] <Norm[#2]*#2[[2]]/#2[[1]] &],
Sort[data2d,Norm[#1[[1;;2]]]*#1[[2]]/#1[[1]] <Norm[#2[[1;;2]]]*#2[[2]]/#2[[1]] &]
]
];
SortParcelPositions[Ensemble_,parcels_]:=Block[{
data2d,t,
nconfs=Length[Ensemble],
nparc=Length[parcels],
pickedSample
},

t= ApplyBoxSymmetryToEnsemble[Ensemble,Null];
(*Print["Successful Sym"];*)
data2d = Table[{},{x,1,nconfs}  ];

Do[
pickedSample=t[[First@parcels[[parcelNumber]],8  ]];
(* =================================================================*)
Do[
data2d[[j]] = Flatten[t[[j,QuickSimilarityMax[pickedSample, t[[ j ,All ]]  ]]],1];
,{j,parcels[[parcelNumber]]}];
(* =================================================================*)

,{parcelNumber, Range[nparc] } ];

data2d
];(* End Function *)
ParcelBySimilarity[Ensemble_,ConfidenceRange_,SymRot___]:=Block[{data2d,

nconfs=Length[Ensemble],
npoints=Length[  Ensemble[[1]] ],

nsym,rx,ry,rxy,ryx,c4,s41,t,samples,
pattern,listofparcels,listofsamples,parcelsample,candidates,usamples,counter,pickedSample,delta0,delta1,allsim,j,i,
simPattern,
uParcels,ParcelsToCheck,confsInPatternParcel,confsInTestParcel
},

t= ApplyBoxSymmetryToEnsemble[Ensemble,SymRot];
(*Print["Successful Sym"];*)
data2d = Table[{},{x,1,nconfs}  ];


(* Clean the variables *)
pattern=.;
listofparcels=Table[{},{x,1,nconfs}];
samples={};
listofsamples=Table[{},{x,1,nconfs}];
parcelsample=Table[{},{x,1,nconfs}];


(* Size of database depends on whether external configurations have been added or not *)
(* Change the Range to reflect a Randomized Sample set  *)
samples=Range[nconfs];

(* Define the number of samples solved *)


While[Length[samples]>0,

(* Reset the list of samples *)
(* Change list of samples to reflect those not solved yet *)

candidates=Table[{},{x,Range[Max[samples]]}];
usamples=Table[{},{x,Range[Max[samples]]}];

(* Picks out the sample number from the randomized list *)
pickedSample=First[samples];

(* Removes picked sample from the list *)
samples=Rest[samples];

usamples[[pickedSample]]=pickedSample;

(* =================================================================*)
(* =================================================================*)

(* Distance and order parameters for comparisions *)
Do[

(* Loop over all Symmetry from box *)
(* =================================================================*)
Do[

pattern=t[[pickedSample,sym]];
simPattern=QuickSimilarityCheckQuiet[pattern, t[[ j ,All ]] ,ConfidenceRange ];
(*simPattern=QuickDistanceSimilarityCheckQuiet[pattern, t[[ j ,All ]] ,ConfidenceRange ];*)


(* Using Return True/False *)
If[simPattern,usamples[[j]]=j;Break[];  ];

,{sym, Range[8]}];
(* =================================================================*)
,{j,samples}];

(*DeleteCases of empty initialization*)
usamples=DeleteCases[usamples,{}];
listofparcels[[pickedSample]]=Flatten[usamples];

Do[ samples=Delete[samples,Flatten[Position[samples,j]]],{j,usamples}];

(* Randomize the cleaned database indexes *)
samples=RandomSample[samples];
];

(* Sort Parcels *)
(* Final:  Returns the list of parcels  *)
listofparcels=Sort[DeleteCases[listofparcels,{}],Length[#1]>Length[#2]&]

];(* End Function *)

End[]
EndPackage[]
