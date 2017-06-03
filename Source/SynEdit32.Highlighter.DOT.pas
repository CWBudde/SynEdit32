{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterDOT.pas, released 2002-11-30.
Description: DOT Syntax Parser/Highlighter
The initial author of this file is nissl (nissl@tiscali.it, nissl@mammuth.it)
Unicode translation by Maël Hörz.
Copyright (c) 2002, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterDOT.pas,v 1.3.2.7 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides a ATT DOT highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it))
@created(november 2002)
@lastmod(2002-11-30)
The SynHighlighterDOT unit provides SynEdit with a DOT Graph Drawing (.dot) highlighter.
The highlighter formats DOT source code ref.: http://www.research.att.com/sw/tools/graphviz/.
}

unit SynEdit32.Highlighter.DOT;

{$I SynEdit.Inc}

interface

uses
  Windows, Controls, Graphics, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (
    tkArrowHead,
    tkAttribute,
    tkComment,
    tkDirections,
    tkIdentifier,
    tkKey,
    tkNull,
    tkShape,
    tkSpace,
    tkString,
    tkUnknown,
    tkValue,
    tkSymbol);

  TRangeState = (rsUnKnown, rsCStyleComment, rsString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterDOT = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..786] of TIdentFuncTableFunc;
    FArrowHeadAttri: TSynEdit32HighlighterAttributes;
    FAttributeAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDirectionsAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FShapeAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FValueAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAll(Index: Integer): TtkTokenKind;
    function FuncAppendix(Index: Integer): TtkTokenKind;
    function FuncArrowhead(Index: Integer): TtkTokenKind;
    function FuncArrowsize(Index: Integer): TtkTokenKind;
    function FuncArrowtail(Index: Integer): TtkTokenKind;
    function FuncAuto(Index: Integer): TtkTokenKind;
    function FuncBack(Index: Integer): TtkTokenKind;
    function FuncBgcolor(Index: Integer): TtkTokenKind;
    function FuncBold(Index: Integer): TtkTokenKind;
    function FuncBoth(Index: Integer): TtkTokenKind;
    function FuncBottomlabel(Index: Integer): TtkTokenKind;
    function FuncBox(Index: Integer): TtkTokenKind;
    function FuncCenter(Index: Integer): TtkTokenKind;
    function FuncCircle(Index: Integer): TtkTokenKind;
    function FuncClusterrank(Index: Integer): TtkTokenKind;
    function FuncColor(Index: Integer): TtkTokenKind;
    function FuncComment(Index: Integer): TtkTokenKind;
    function FuncCompound(Index: Integer): TtkTokenKind;
    function FuncConcentrate(Index: Integer): TtkTokenKind;
    function FuncConstraint(Index: Integer): TtkTokenKind;
    function FuncDecorate(Index: Integer): TtkTokenKind;
    function FuncDiamond(Index: Integer): TtkTokenKind;
    function FuncDigraph(Index: Integer): TtkTokenKind;
    function FuncDir(Index: Integer): TtkTokenKind;
    function FuncDistortion(Index: Integer): TtkTokenKind;
    function FuncDot(Index: Integer): TtkTokenKind;
    function FuncDotted(Index: Integer): TtkTokenKind;
    function FuncDoublecircle(Index: Integer): TtkTokenKind;
    function FuncDoubleoctagon(Index: Integer): TtkTokenKind;
    function FuncE(Index: Integer): TtkTokenKind;
    function FuncEdge(Index: Integer): TtkTokenKind;
    function FuncEgg(Index: Integer): TtkTokenKind;
    function FuncEllipse(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFill(Index: Integer): TtkTokenKind;
    function FuncFillcolor(Index: Integer): TtkTokenKind;
    function FuncFilled(Index: Integer): TtkTokenKind;
    function FuncFixedsize(Index: Integer): TtkTokenKind;
    function FuncFontcolor(Index: Integer): TtkTokenKind;
    function FuncFontname(Index: Integer): TtkTokenKind;
    function FuncFontpath(Index: Integer): TtkTokenKind;
    function FuncFontsize(Index: Integer): TtkTokenKind;
    function FuncForward(Index: Integer): TtkTokenKind;
    function FuncGlobal(Index: Integer): TtkTokenKind;
    function FuncGraph(Index: Integer): TtkTokenKind;
    function FuncGroup(Index: Integer): TtkTokenKind;
    function FuncHeadlabel(Index: Integer): TtkTokenKind;
    function FuncHeadport(Index: Integer): TtkTokenKind;
    function FuncHeadurl(Index: Integer): TtkTokenKind;
    function FuncHeight(Index: Integer): TtkTokenKind;
    function FuncHexagon(Index: Integer): TtkTokenKind;
    function FuncHouse(Index: Integer): TtkTokenKind;
    function FuncId(Index: Integer): TtkTokenKind;
    function FuncInv(Index: Integer): TtkTokenKind;
    function FuncInvdot(Index: Integer): TtkTokenKind;
    function FuncInvhouse(Index: Integer): TtkTokenKind;
    function FuncInvodot(Index: Integer): TtkTokenKind;
    function FuncInvtrapezium(Index: Integer): TtkTokenKind;
    function FuncInvtriangle(Index: Integer): TtkTokenKind;
    function FuncLabel(Index: Integer): TtkTokenKind;
    function FuncLabelangle(Index: Integer): TtkTokenKind;
    function FuncLabeldistance(Index: Integer): TtkTokenKind;
    function FuncLabelfloat(Index: Integer): TtkTokenKind;
    function FuncLabelfontcolor(Index: Integer): TtkTokenKind;
    function FuncLabelfontname(Index: Integer): TtkTokenKind;
    function FuncLabelfontsize(Index: Integer): TtkTokenKind;
    function FuncLabeljust(Index: Integer): TtkTokenKind;
    function FuncLabelloc(Index: Integer): TtkTokenKind;
    function FuncLayer(Index: Integer): TtkTokenKind;
    function FuncLayers(Index: Integer): TtkTokenKind;
    function FuncLhead(Index: Integer): TtkTokenKind;
    function FuncLtail(Index: Integer): TtkTokenKind;
    function FuncMargin(Index: Integer): TtkTokenKind;
    function FuncMax(Index: Integer): TtkTokenKind;
    function FuncMcircle(Index: Integer): TtkTokenKind;
    function FuncMclimit(Index: Integer): TtkTokenKind;
    function FuncMdiamond(Index: Integer): TtkTokenKind;
    function FuncMerged(Index: Integer): TtkTokenKind;
    function FuncMin(Index: Integer): TtkTokenKind;
    function FuncMinimum(Index: Integer): TtkTokenKind;
    function FuncMinlen(Index: Integer): TtkTokenKind;
    function FuncMrecord(Index: Integer): TtkTokenKind;
    function FuncMsquare(Index: Integer): TtkTokenKind;
    function FuncMultiples(Index: Integer): TtkTokenKind;
    function FuncN(Index: Integer): TtkTokenKind;
    function FuncNe(Index: Integer): TtkTokenKind;
    function FuncNode(Index: Integer): TtkTokenKind;
    function FuncNodesep(Index: Integer): TtkTokenKind;
    function FuncNone(Index: Integer): TtkTokenKind;
    function FuncNormal(Index: Integer): TtkTokenKind;
    function FuncNslimit(Index: Integer): TtkTokenKind;
    function FuncNw(Index: Integer): TtkTokenKind;
    function FuncOctagon(Index: Integer): TtkTokenKind;
    function FuncOdot(Index: Integer): TtkTokenKind;
    function FuncOnto(Index: Integer): TtkTokenKind;
    function FuncOrdering(Index: Integer): TtkTokenKind;
    function FuncOrientation(Index: Integer): TtkTokenKind;
    function FuncPage(Index: Integer): TtkTokenKind;
    function FuncPagedir(Index: Integer): TtkTokenKind;
    function FuncParallelogram(Index: Integer): TtkTokenKind;
    function FuncPeripheries(Index: Integer): TtkTokenKind;
    function FuncPlaintext(Index: Integer): TtkTokenKind;
    function FuncPoint(Index: Integer): TtkTokenKind;
    function FuncPolygon(Index: Integer): TtkTokenKind;
    function FuncQuantum(Index: Integer): TtkTokenKind;
    function FuncRank(Index: Integer): TtkTokenKind;
    function FuncRankdir(Index: Integer): TtkTokenKind;
    function FuncRanksep(Index: Integer): TtkTokenKind;
    function FuncRatio(Index: Integer): TtkTokenKind;
    function FuncRecord(Index: Integer): TtkTokenKind;
    function FuncRegular(Index: Integer): TtkTokenKind;
    function FuncRemincross(Index: Integer): TtkTokenKind;
    function FuncRotate(Index: Integer): TtkTokenKind;
    function FuncS(Index: Integer): TtkTokenKind;
    function FuncSame(Index: Integer): TtkTokenKind;
    function FuncSamehead(Index: Integer): TtkTokenKind;
    function FuncSametail(Index: Integer): TtkTokenKind;
    function FuncSamplepoints(Index: Integer): TtkTokenKind;
    function FuncSe(Index: Integer): TtkTokenKind;
    function FuncSearchsize(Index: Integer): TtkTokenKind;
    function FuncSection(Index: Integer): TtkTokenKind;
    function FuncShape(Index: Integer): TtkTokenKind;
    function FuncShapefile(Index: Integer): TtkTokenKind;
    function FuncSides(Index: Integer): TtkTokenKind;
    function FuncSink(Index: Integer): TtkTokenKind;
    function FuncSize(Index: Integer): TtkTokenKind;
    function FuncSkew(Index: Integer): TtkTokenKind;
    function FuncSource(Index: Integer): TtkTokenKind;
    function FuncStrict(Index: Integer): TtkTokenKind;
    function FuncStyle(Index: Integer): TtkTokenKind;
    function FuncSubgraph(Index: Integer): TtkTokenKind;
    function FuncSw(Index: Integer): TtkTokenKind;
    function FuncTaillabel(Index: Integer): TtkTokenKind;
    function FuncTailport(Index: Integer): TtkTokenKind;
    function FuncTailurl(Index: Integer): TtkTokenKind;
    function FuncToplabel(Index: Integer): TtkTokenKind;
    function FuncTrapezium(Index: Integer): TtkTokenKind;
    function FuncTriangle(Index: Integer): TtkTokenKind;
    function FuncTripleoctagon(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncUrl(Index: Integer): TtkTokenKind;
    function FuncW(Index: Integer): TtkTokenKind;
    function FuncWeight(Index: Integer): TtkTokenKind;
    function FuncWhen(Index: Integer): TtkTokenKind;
    function FuncWidth(Index: Integer): TtkTokenKind;
    function FuncZ(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure IdentProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CStyleCommentOpenProc;
    procedure CStyleCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure DirectionsProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
     function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property ArrowHeadAttri: TSynEdit32HighlighterAttributes read FArrowHeadAttri write FArrowHeadAttri;
    property AttributeAttri: TSynEdit32HighlighterAttributes read FAttributeAttri write FAttributeAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property DirectionsAttri: TSynEdit32HighlighterAttributes read FDirectionsAttri write FDirectionsAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property ShapeAttri: TSynEdit32HighlighterAttributes read FShapeAttri write FShapeAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property ValueAttri: TSynEdit32HighlighterAttributes read FValueAttri write FValueAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..145] of UnicodeString = (
    'all', 'appendix', 'arrowhead', 'arrowsize', 'arrowtail', 'auto', 'back', 
    'bgcolor', 'bold', 'both', 'bottomlabel', 'box', 'center', 'circle', 
    'clusterrank', 'color', 'comment', 'compound', 'concentrate', 'constraint', 
    'decorate', 'diamond', 'digraph', 'dir', 'distortion', 'dot', 'dotted', 
    'doublecircle', 'doubleoctagon', 'e', 'edge', 'egg', 'ellipse', 'false', 
    'fill', 'fillcolor', 'filled', 'fixedsize', 'fontcolor', 'fontname', 
    'fontpath', 'fontsize', 'forward', 'global', 'graph', 'group', 'headlabel', 
    'headport', 'headurl', 'height', 'hexagon', 'house', 'id', 'inv', 'invdot', 
    'invhouse', 'invodot', 'invtrapezium', 'invtriangle', 'label', 'labelangle', 
    'labeldistance', 'labelfloat', 'labelfontcolor', 'labelfontname', 
    'labelfontsize', 'labeljust', 'labelloc', 'layer', 'layers', 'lhead', 
    'ltail', 'margin', 'max', 'mcircle', 'mclimit', 'mdiamond', 'merged', 'min', 
    'minimum', 'minlen', 'mrecord', 'msquare', 'multiples', 'n', 'ne', 'node', 
    'nodesep', 'none', 'normal', 'nslimit', 'nw', 'octagon', 'odot', 'onto', 
    'ordering', 'orientation', 'page', 'pagedir', 'parallelogram', 
    'peripheries', 'plaintext', 'point', 'polygon', 'quantum', 'rank', 
    'rankdir', 'ranksep', 'ratio', 'record', 'regular', 'remincross', 'rotate', 
    's', 'same', 'samehead', 'sametail', 'samplepoints', 'se', 'searchsize', 
    'section', 'shape', 'shapefile', 'sides', 'sink', 'size', 'skew', 'source', 
    'strict', 'style', 'subgraph', 'sw', 'taillabel', 'tailport', 'tailurl', 
    'toplabel', 'trapezium', 'triangle', 'tripleoctagon', 'true', 'url', 'w', 
    'weight', 'when', 'width', 'z' 
  );

  KeyIndices: array[0..786] of Integer = (
    -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 141, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, 50, -1, -1, -1, -1, -1, 
    -1, -1, -1, 40, -1, -1, -1, -1, 4, -1, -1, -1, -1, 90, -1, 3, -1, 110, 86, 
    -1, -1, 49, 23, -1, 92, -1, -1, -1, 15, -1, 122, -1, -1, 28, -1, 78, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 85, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 140, -1, 103, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, 7, -1, 0, 
    -1, -1, 97, -1, -1, -1, -1, -1, 43, -1, -1, -1, 131, -1, -1, -1, 5, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, 10, -1, 
    47, 68, -1, 132, -1, -1, 52, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, 
    -1, -1, 64, -1, -1, 124, -1, -1, -1, -1, -1, -1, 87, -1, -1, -1, 12, -1, 84, 
    -1, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 42, -1, 38, -1, -1, -1, 143, -1, -1, -1, 145, 
    106, -1, 127, -1, -1, -1, 99, 75, -1, -1, 102, -1, 58, -1, -1, 56, -1, -1, 
    -1, -1, 9, -1, -1, -1, -1, -1, 22, -1, 73, -1, -1, -1, 17, -1, 54, 112, -1, 
    -1, -1, -1, -1, -1, -1, 113, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 96, -1, 
    21, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 69, 116, -1, -1, 32, -1, 
    -1, -1, -1, -1, -1, -1, 16, -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, -1, 
    -1, 71, -1, -1, -1, -1, -1, -1, -1, -1, -1, 137, -1, -1, 117, -1, -1, -1, 
    -1, -1, -1, -1, -1, 111, 93, -1, -1, -1, -1, 108, -1, -1, 119, -1, -1, -1, 
    -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, -1, -1, -1, 76, -1, -1, -1, 
    -1, -1, -1, -1, 77, -1, -1, 104, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 26, -1, -1, -1, 79, -1, 19, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 39, -1, -1, -1, 115, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 109, 35, -1, -1, 70, -1, -1, 57, -1, 72, -1, 
    -1, 83, -1, -1, -1, -1, 130, -1, -1, -1, 18, -1, 118, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 81, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 61, 
    37, 1, -1, -1, -1, -1, 138, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 14, -1, -1, 8, -1, -1, -1, 125, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 91, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, -1, -1, 
    95, -1, -1, -1, -1, 136, -1, -1, 20, -1, 62, -1, -1, -1, -1, 134, -1, -1, 
    -1, 63, -1, -1, -1, 121, 80, -1, -1, -1, -1, -1, -1, 135, -1, -1, 120, -1, 
    -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, 
    -1, 24, -1, -1, 139, 67, -1, -1, 59, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, 
    -1, 128, 34, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, 114, -1, -1, -1, -1, 
    -1, -1, -1, 55, -1, -1, 94, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 41, -1, -1, -1, -1, -1, -1, -1, 44, -1, 
    -1, -1, -1, -1, 74, -1, 51, 144, -1, -1, 82, 98, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 100, 66, -1, 25, -1, -1, -1, 45, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, 
    6, 105, -1, -1, 133, 123, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 107, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, -1, -1, -1 
  );

{$Q-}
function TSynEdit32HighlighterDOT.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 63 + Ord(Str^) * 331;
    Inc(Str);
  end;
  Result := Result mod 787;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterDOT.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynEdit32HighlighterDOT.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[132] := FuncAll;
  FIdentFuncTable[509] := FuncAppendix;
  FIdentFuncTable[188] := FuncArrowhead;
  FIdentFuncTable[72] := FuncArrowsize;
  FIdentFuncTable[65] := FuncArrowtail;
  FIdentFuncTable[149] := FuncAuto;
  FIdentFuncTable[752] := FuncBack;
  FIdentFuncTable[130] := FuncBgcolor;
  FIdentFuncTable[536] := FuncBold;
  FIdentFuncTable[266] := FuncBoth;
  FIdentFuncTable[169] := FuncBottomlabel;
  FIdentFuncTable[4] := FuncBox;
  FIdentFuncTable[206] := FuncCenter;
  FIdentFuncTable[666] := FuncCircle;
  FIdentFuncTable[533] := FuncClusterrank;
  FIdentFuncTable[85] := FuncColor;
  FIdentFuncTable[327] := FuncComment;
  FIdentFuncTable[278] := FuncCompound;
  FIdentFuncTable[481] := FuncConcentrate;
  FIdentFuncTable[425] := FuncConstraint;
  FIdentFuncTable[573] := FuncDecorate;
  FIdentFuncTable[302] := FuncDiamond;
  FIdentFuncTable[272] := FuncDigraph;
  FIdentFuncTable[79] := FuncDir;
  FIdentFuncTable[621] := FuncDistortion;
  FIdentFuncTable[726] := FuncDot;
  FIdentFuncTable[419] := FuncDotted;
  FIdentFuncTable[104] := FuncDoublecircle;
  FIdentFuncTable[90] := FuncDoubleoctagon;
  FIdentFuncTable[377] := FuncE;
  FIdentFuncTable[783] := FuncEdge;
  FIdentFuncTable[614] := FuncEgg;
  FIdentFuncTable[319] := FuncEllipse;
  FIdentFuncTable[409] := FuncFalse;
  FIdentFuncTable[641] := FuncFill;
  FIdentFuncTable[461] := FuncFillcolor;
  FIdentFuncTable[631] := FuncFilled;
  FIdentFuncTable[508] := FuncFixedsize;
  FIdentFuncTable[237] := FuncFontcolor;
  FIdentFuncTable[435] := FuncFontname;
  FIdentFuncTable[60] := FuncFontpath;
  FIdentFuncTable[685] := FuncFontsize;
  FIdentFuncTable[235] := FuncForward;
  FIdentFuncTable[141] := FuncGlobal;
  FIdentFuncTable[693] := FuncGraph;
  FIdentFuncTable[730] := FuncGroup;
  FIdentFuncTable[212] := FuncHeadlabel;
  FIdentFuncTable[171] := FuncHeadport;
  FIdentFuncTable[749] := FuncHeadurl;
  FIdentFuncTable[78] := FuncHeight;
  FIdentFuncTable[51] := FuncHexagon;
  FIdentFuncTable[701] := FuncHouse;
  FIdentFuncTable[177] := FuncId;
  FIdentFuncTable[603] := FuncInv;
  FIdentFuncTable[280] := FuncInvdot;
  FIdentFuncTable[660] := FuncInvhouse;
  FIdentFuncTable[261] := FuncInvodot;
  FIdentFuncTable[467] := FuncInvtrapezium;
  FIdentFuncTable[258] := FuncInvtriangle;
  FIdentFuncTable[628] := FuncLabel;
  FIdentFuncTable[557] := FuncLabelangle;
  FIdentFuncTable[507] := FuncLabeldistance;
  FIdentFuncTable[575] := FuncLabelfloat;
  FIdentFuncTable[584] := FuncLabelfontcolor;
  FIdentFuncTable[192] := FuncLabelfontname;
  FIdentFuncTable[650] := FuncLabelfontsize;
  FIdentFuncTable[724] := FuncLabeljust;
  FIdentFuncTable[625] := FuncLabelloc;
  FIdentFuncTable[172] := FuncLayer;
  FIdentFuncTable[315] := FuncLayers;
  FIdentFuncTable[464] := FuncLhead;
  FIdentFuncTable[341] := FuncLtail;
  FIdentFuncTable[469] := FuncMargin;
  FIdentFuncTable[274] := FuncMax;
  FIdentFuncTable[699] := FuncMcircle;
  FIdentFuncTable[253] := FuncMclimit;
  FIdentFuncTable[391] := FuncMdiamond;
  FIdentFuncTable[399] := FuncMerged;
  FIdentFuncTable[92] := FuncMin;
  FIdentFuncTable[423] := FuncMinimum;
  FIdentFuncTable[589] := FuncMinlen;
  FIdentFuncTable[493] := FuncMrecord;
  FIdentFuncTable[705] := FuncMsquare;
  FIdentFuncTable[472] := FuncMultiples;
  FIdentFuncTable[208] := FuncN;
  FIdentFuncTable[102] := FuncNe;
  FIdentFuncTable[75] := FuncNode;
  FIdentFuncTable[202] := FuncNodesep;
  FIdentFuncTable[50] := FuncNone;
  FIdentFuncTable[386] := FuncNormal;
  FIdentFuncTable[70] := FuncNslimit;
  FIdentFuncTable[551] := FuncNw;
  FIdentFuncTable[81] := FuncOctagon;
  FIdentFuncTable[364] := FuncOdot;
  FIdentFuncTable[663] := FuncOnto;
  FIdentFuncTable[565] := FuncOrdering;
  FIdentFuncTable[300] := FuncOrientation;
  FIdentFuncTable[135] := FuncPage;
  FIdentFuncTable[706] := FuncPagedir;
  FIdentFuncTable[252] := FuncParallelogram;
  FIdentFuncTable[723] := FuncPeripheries;
  FIdentFuncTable[167] := FuncPlaintext;
  FIdentFuncTable[256] := FuncPoint;
  FIdentFuncTable[117] := FuncPolygon;
  FIdentFuncTable[402] := FuncQuantum;
  FIdentFuncTable[753] := FuncRank;
  FIdentFuncTable[246] := FuncRankdir;
  FIdentFuncTable[773] := FuncRanksep;
  FIdentFuncTable[369] := FuncRatio;
  FIdentFuncTable[460] := FuncRecord;
  FIdentFuncTable[74] := FuncRegular;
  FIdentFuncTable[363] := FuncRemincross;
  FIdentFuncTable[281] := FuncRotate;
  FIdentFuncTable[289] := FuncS;
  FIdentFuncTable[652] := FuncSame;
  FIdentFuncTable[439] := FuncSamehead;
  FIdentFuncTable[316] := FuncSametail;
  FIdentFuncTable[354] := FuncSamplepoints;
  FIdentFuncTable[483] := FuncSe;
  FIdentFuncTable[372] := FuncSearchsize;
  FIdentFuncTable[599] := FuncSection;
  FIdentFuncTable[588] := FuncShape;
  FIdentFuncTable[87] := FuncShapefile;
  FIdentFuncTable[757] := FuncSides;
  FIdentFuncTable[195] := FuncSink;
  FIdentFuncTable[540] := FuncSize;
  FIdentFuncTable[333] := FuncSkew;
  FIdentFuncTable[248] := FuncSource;
  FIdentFuncTable[640] := FuncStrict;
  FIdentFuncTable[520] := FuncStyle;
  FIdentFuncTable[477] := FuncSubgraph;
  FIdentFuncTable[145] := FuncSw;
  FIdentFuncTable[174] := FuncTaillabel;
  FIdentFuncTable[756] := FuncTailport;
  FIdentFuncTable[580] := FuncTailurl;
  FIdentFuncTable[596] := FuncToplabel;
  FIdentFuncTable[570] := FuncTrapezium;
  FIdentFuncTable[351] := FuncTriangle;
  FIdentFuncTable[514] := FuncTripleoctagon;
  FIdentFuncTable[624] := FuncTrue;
  FIdentFuncTable[115] := FuncUrl;
  FIdentFuncTable[39] := FuncW;
  FIdentFuncTable[128] := FuncWeight;
  FIdentFuncTable[241] := FuncWhen;
  FIdentFuncTable[702] := FuncWidth;
  FIdentFuncTable[245] := FuncZ;
end;

function TSynEdit32HighlighterDOT.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncAll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncAppendix(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncArrowhead(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncArrowsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncArrowtail(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncAuto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncBack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncBgcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncBold(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncBoth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncBottomlabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncBox(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncCenter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncCircle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncClusterrank(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncColor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncComment(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncCompound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncConcentrate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncConstraint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDecorate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDiamond(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDigraph(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDistortion(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDotted(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDoublecircle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncDoubleoctagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncE(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncEdge(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncEgg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncEllipse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFill(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFillcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFilled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue  // TODO: ANSI source isn't clear if tkValue or tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFixedsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFontcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFontname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFontpath(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncFontsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncForward(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncGlobal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncGraph(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncGroup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncHeadlabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncHeadport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncHeadurl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncHeight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncHexagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncHouse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncId(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncInv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncInvdot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncInvhouse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncInvodot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncInvtrapezium(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncInvtriangle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabelangle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabeldistance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabelfloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabelfontcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabelfontname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabelfontsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabeljust(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLabelloc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLayer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute  // TODO: ANSI source isn't clear if tkAttribute or tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLayers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute  // TODO: ANSI source isn't clear if tkAttribute or tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLhead(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncLtail(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMargin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMax(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMcircle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMclimit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMdiamond(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMerged(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMinimum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMinlen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMrecord(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMsquare(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncMultiples(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncN(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncNe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncNode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncNodesep(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncNone(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncNormal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncNslimit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncNw(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncOctagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncOdot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncOnto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncOrdering(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncOrientation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncPage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncPagedir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncParallelogram(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncPeripheries(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncPlaintext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncPoint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncPolygon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncQuantum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRank(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRankdir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRanksep(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRatio(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRecord(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRegular(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRemincross(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncRotate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncS(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSame(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSamehead(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSametail(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSamplepoints(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSearchsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSection(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncShape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncShapefile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSides(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSkew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSource(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncStrict(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncStyle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSubgraph(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncSw(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncTaillabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncTailport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncTailurl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncToplabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncTrapezium(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncTriangle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncTripleoctagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncUrl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncW(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncWeight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncWhen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncWidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDOT.FuncZ(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

procedure TSynEdit32HighlighterDOT.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterDOT.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterDOT.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterDOT.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterDOT.DirectionsProc;
begin
  Inc(FRun);
  if (FLine[FRun] = '>') or (FLine[FRun] = '-') then
  begin
    FTokenID := tkDirections;
    Inc(FRun);
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterDOT.CStyleCommentOpenProc;
begin
  Inc(FRun);
  if FLine[FRun] = '/' then
  begin
    FTokenID := tkComment;
    Inc(FRun, 2);
    while not IsLineEnd(FRun) do Inc(FRun);
    Exit;
  end;
  if FLine[FRun] = '*' then
  begin
    FRange := rsCStyleComment;
    CStyleCommentProc;
    FTokenID := tkComment;
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynEdit32HighlighterDOT.CStyleCommentProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
        begin
          Inc(FRun, 2);
          FRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(FRun) then
          Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end;

procedure TSynEdit32HighlighterDOT.StringOpenProc;
begin
  Inc(FRun);
  FRange := rsString;
  StringProc;
  FTokenID := tkString;
end;

procedure TSynEdit32HighlighterDOT.StringProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[FRun] = '''' then
    begin
      Inc(FRun, 1);
      FRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

constructor TSynEdit32HighlighterDOT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FArrowHeadAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrArrowHead, SYNS_FriendlyAttrArrowHead);
  FArrowHeadAttri.Foreground := clRed;
  AddAttribute(FArrowHeadAttri);

  FAttributeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAttribute, SYNS_FriendlyAttrAttribute);
  AddAttribute(FAttributeAttri);

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  AddAttribute(FCommentAttri);

  FDirectionsAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDirections, SYNS_FriendlyAttrDirections);
  FDirectionsAttri.Style := [fsBold];
  FDirectionsAttri.Foreground := clYellow;
  AddAttribute(FDirectionsAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FShapeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrShape, SYNS_FriendlyAttrShape);
  FShapeAttri.Style := [fsBold];
  FShapeAttri.Foreground := clRed;
  AddAttribute(FShapeAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FValueAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  FValueAttri.Style := [fsItalic];
  FValueAttri.Foreground := clRed;
  AddAttribute(FValueAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  FSymbolAttri.Foreground := clGreen;
  AddAttribute(FSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterDOT;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterDOT.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterDOT.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterDOT.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterDOT.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsCStyleComment: CStyleCommentProc;
  else
    begin
      FRange := rsUnknown;
      case FLine[FRun] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '/': CStyleCommentOpenProc;
        '-': DirectionsProc;
        '''': StringOpenProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=': SymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynEdit32HighlighterDOT.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterDOT.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result :=
    '--,->,all,appendix,arrowhead,arrowsize,arrowtail,auto,back,bgcolor,bo' +
    'ld,both,bottomlabel,box,center,circle,clusterrank,color,comment,compou' +
    'nd,concentrate,constraint,decorate,diamond,digraph,dir,distortion,dot,' +
    'dotted,doublecircle,doubleoctagon,e,edge,egg,ellipse,false,fill,fillco' +
    'lor,filled,fixedsize,fontcolor,fontname,fontpath,fontsize,forward,glob' +
    'al,graph,group,headlabel,headport,headURL,height,hexagon,house,id,inv,' +
    'invdot,invhouse,invodot,invtrapezium,invtriangle,label,labelangle,labe' +
    'ldistance,labelfloat,labelfontcolor,labelfontname,labelfontsize,labelj' +
    'ust,labelloc,layer,layers,lhead,ltail,margin,max,mcircle,mclimit,mdiam' +
    'ond,merged,min,minimum,minlen,mrecord,msquare,multiples,n,ne,node,node' +
    'sep,none,normal,nslimit,nw,octagon,odot,onto,ordering,orientation,page' +
    ',pagedir,parallelogram,peripheries,plaintext,point,polygon,quantum,ran' +
    'k,rankdir,ranksep,ratio,record,regular,remincross,rotate,s,same,samehe' +
    'ad,sametail,samplepoints,se,searchsize,section,shape,shapefile,sides,s' +
    'ink,size,skew,source,strict,style,subgraph,sw,taillabel,tailport,tailU' +
    'RL,toplabel,trapezium,triangle,tripleoctagon,true,url,w,weight,when,wi' +
    'dth,z';
end;

function TSynEdit32HighlighterDOT.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterDOT.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkArrowHead: Result := FArrowHeadAttri;
    tkAttribute: Result := FAttributeAttri;
    tkComment: Result := FCommentAttri;
    tkDirections: Result := FDirectionsAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkShape: Result := FShapeAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkValue: Result := FValueAttri;
    tkUnknown: Result := FIdentifierAttri;
    tkSymbol: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterDOT.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterDOT.GetSampleSource: UnicodeString;
begin
  Result :=
    '// ATT DOT Graphic description language'#13#10 +
    'digraph asde91 {'#13#10 +
    '  ranksep=.75; size = "7.5,7.5";'#13#10 +
    '  {'#13#10 +
    '      node [shape=plaintext, fontsize=16];'#13#10 +
    '      /* the time-line graph */'#13#10 +
    '      past -> 1978 -> 1980 -> 1982 -> 1983 -> 1985 -> 1986 ->'#13#10 +
    '      1987 -> 1988 -> 1989 -> 1990 -> "future";'#13#10 +
    '      /* ancestor programs */'#13#10 +
    '      "Bourne sh"; "make"; "SCCS"; "yacc"; "cron"; "Reiser cpp";'#13#10 +
    '      "Cshell"; "emacs"; "build"; "vi"; "<curses>"; "RCS"; "C*";'#13#10 +
    '  }'#13#10 +
    '      { rank = same;'#13#10 +
    '      "Software IS"; "Configuration Mgt"; "Architecture & Libraries";'#13#10 +
    '      "Process";'#13#10 +
    '  };'#13#10 +
    '    node [shape=box];'#13#10 +
    '    { rank = same; "past"; "SCCS"; "make"; "Bourne sh"; "yacc"; "cron"; }'#13#10 +
    '    { rank = same; 1978; "Reiser cpp"; "Cshell"; }'#13#10 +
    '    { rank = same; 1980; "build"; "emacs"; "vi"; }'#13#10 +
    '    { rank = same; 1982; "RCS"; "<curses>"; "IMX"; "SYNED"; }'#13#10 +
    '    { rank = same; 1983; "ksh"; "IFS"; "TTU"; }'#13#10 +
    '    { rank = same; 1985; "nmake"; "Peggy"; }'#13#10 +
    '    { rank = same; 1986; "C*"; "ncpp"; "ksh-i"; "<curses-i>"; "PG2"; }'#13#10 +
    '    { rank = same; 1987; "Ansi cpp"; "nmake 2.0"; "3D File System"; "fdelta";'#13#10 +
    '        "DAG"; "CSAS";}'#13#10 +
    '    { rank = same; 1988; "CIA"; "SBCS"; "ksh-88"; "PEGASUS/PML"; "PAX";'#13#10 +
    '        "backtalk"; }'#13#10 +
    '    { rank = same; 1989; "CIA++"; "APP"; "SHIP"; "DataShare"; "ryacc";'#13#10 +
    '        "Mosaic"; }'#13#10 +
    '    { rank = same; 1990; "libft"; "CoShell"; "DIA"; "IFS-i"; "kyacc"; "sfio";'#13#10 +
    '        "yeast"; "ML-X"; "DOT"; }'#13#10 +
    '    { rank = same; "future"; "Adv. Software Technology"; }'#13#10 +
    '    "PEGASUS/PML" -> "ML-X";'#13#10 +
    '    "SCCS" -> "nmake";'#13#10 +
    '    "SCCS" -> "3D File System";'#13#10 +
    '    "SCCS" -> "RCS";'#13#10 +
    '    "make" -> "nmake";'#13#10 +
    '    "make" -> "build";'#13#10 +
    '}';
end;

function TSynEdit32HighlighterDOT.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterDOT;
end;

function TSynEdit32HighlighterDOT.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynEdit32HighlighterDOT.GetLanguageName: string;
begin
  Result := SYNS_LangDOT;
end;

procedure TSynEdit32HighlighterDOT.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterDOT.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterDOT.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

class function TSynEdit32HighlighterDOT.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangDOT;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterDOT);
end.
