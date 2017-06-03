{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterEiffel.pas, released 2004-03-08.
Description: Eiffel Syntax Parser/Highlighter
The initial author of this file is Massimo Maria Ghisalberti (nissl).
Unicode translation by Maël Hörz.
Copyright (c) 2004, all rights reserved.

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

$Id: SynHighlighterEiffel.pas,v 1.3.2.8 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an Eiffel highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it, nissl@linee.it - www.linee.it)
@created(03-08-2004)
@lastmod(03-08-2004)
The SynHighlighterEiffel unit provides SynEdit with an Eiffel highlighter.
}

unit SynEdit32.Highlighter.Eiffel;

{$I SynEdit.Inc}

interface

uses
  Graphics, SysUtils, Classes,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode;

type
  TtkTokenKind = (
    tkBasicTypes,
    tkComment,
    tkIdentifier,
    tkKey,
    tkLace,
    tkNull,
    tkOperatorAndSymbols,
    tkPredefined,
    tkResultValue,
    tkSpace,
    tkString,
    tkUnknown);

  TRangeState = (rsUnKnown, rsEiffelComment, rsString, rsOperatorAndSymbolProc);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterEiffel = class(TSynEdit32CustomHighlighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..502] of TIdentFuncTableFunc;
    fBasicTypesAttri: TSynEdit32HighlighterAttributes;
    fCommentAttri: TSynEdit32HighlighterAttributes;
    fIdentifierAttri: TSynEdit32HighlighterAttributes;
    fKeyAttri: TSynEdit32HighlighterAttributes;
    fLaceAttri: TSynEdit32HighlighterAttributes;
    fOperatorAndSymbolsAttri: TSynEdit32HighlighterAttributes;
    fPredefinedAttri: TSynEdit32HighlighterAttributes;
    fResultValueAttri: TSynEdit32HighlighterAttributes;
    fSpaceAttri: TSynEdit32HighlighterAttributes;
    fStringAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function OperatorFunc(Index: Integer): TtkTokenKind;
    function Func37u(Index: Integer): TtkTokenKind;
    function FuncAdapt(Index: Integer): TtkTokenKind;
    function FuncAlias(Index: Integer): TtkTokenKind;
    function FuncAll(Index: Integer): TtkTokenKind;
    function FuncAnd(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncAs(Index: Integer): TtkTokenKind;
    function FuncAssertion(Index: Integer): TtkTokenKind;
    function FuncBit(Index: Integer): TtkTokenKind;
    function FuncBoolean(Index: Integer): TtkTokenKind;
    function FuncCharacter(Index: Integer): TtkTokenKind;
    function FuncCheck(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncCluster(Index: Integer): TtkTokenKind;
    function FuncColon(Index: Integer): TtkTokenKind;
    function FuncComma(Index: Integer): TtkTokenKind;
    function FuncCreation(Index: Integer): TtkTokenKind;
    function FuncCurrent(Index: Integer): TtkTokenKind;
    function FuncDebug(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDeferred(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDouble(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncElseif(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncEnsure(Index: Integer): TtkTokenKind;
    function FuncExclude(Index: Integer): TtkTokenKind;
    function FuncExecutable(Index: Integer): TtkTokenKind;
    function FuncExpanded(Index: Integer): TtkTokenKind;
    function FuncExport(Index: Integer): TtkTokenKind;
    function FuncExternal(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFeature(Index: Integer): TtkTokenKind;
    function FuncFrom(Index: Integer): TtkTokenKind;
    function FuncFrozen(Index: Integer): TtkTokenKind;
    function FuncGenerate(Index: Integer): TtkTokenKind;
    function FuncIdentifier(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncIgnore(Index: Integer): TtkTokenKind;
    function FuncImplies(Index: Integer): TtkTokenKind;
    function FuncInclude(Index: Integer): TtkTokenKind;
    function FuncInclude95path(Index: Integer): TtkTokenKind;
    function FuncIndexing(Index: Integer): TtkTokenKind;
    function FuncInfix(Index: Integer): TtkTokenKind;
    function FuncInherit(Index: Integer): TtkTokenKind;
    function FuncInspect(Index: Integer): TtkTokenKind;
    function FuncInteger(Index: Integer): TtkTokenKind;
    function FuncInvariant(Index: Integer): TtkTokenKind;
    function FuncIs(Index: Integer): TtkTokenKind;
    function FuncLike(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLoop(Index: Integer): TtkTokenKind;
    function FuncMake(Index: Integer): TtkTokenKind;
    function FuncNo(Index: Integer): TtkTokenKind;
    function FuncNot(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncObsolete(Index: Integer): TtkTokenKind;
    function FuncOld(Index: Integer): TtkTokenKind;
    function FuncOnce(Index: Integer): TtkTokenKind;
    function FuncOptimize(Index: Integer): TtkTokenKind;
    function FuncOption(Index: Integer): TtkTokenKind;
    function FuncOr(Index: Integer): TtkTokenKind;
    function FuncPointer(Index: Integer): TtkTokenKind;
    function FuncPrecompiled(Index: Integer): TtkTokenKind;
    function FuncPrecursor(Index: Integer): TtkTokenKind;
    function FuncPrefix(Index: Integer): TtkTokenKind;
    function FuncReal(Index: Integer): TtkTokenKind;
    function FuncRedefine(Index: Integer): TtkTokenKind;
    function FuncRename(Index: Integer): TtkTokenKind;
    function FuncRequire(Index: Integer): TtkTokenKind;
    function FuncRescue(Index: Integer): TtkTokenKind;
    function FuncResult(Index: Integer): TtkTokenKind;
    function FuncRetry(Index: Integer): TtkTokenKind;
    function FuncRoot(Index: Integer): TtkTokenKind;
    function FuncSelect(Index: Integer): TtkTokenKind;
    function FuncSeparate(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStrip(Index: Integer): TtkTokenKind;
    function FuncSystem(Index: Integer): TtkTokenKind;
    function FuncThen(Index: Integer): TtkTokenKind;
    function FuncTrace(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncUndefine(Index: Integer): TtkTokenKind;
    function FuncUnique(Index: Integer): TtkTokenKind;
    function FuncUntil(Index: Integer): TtkTokenKind;
    function FuncUse(Index: Integer): TtkTokenKind;
    function FuncVariant(Index: Integer): TtkTokenKind;
    function FuncVisible(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncWhen(Index: Integer): TtkTokenKind;
    function FuncXor(Index: Integer): TtkTokenKind;
    function FuncYes(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure IdentProc;
    procedure InitIdent;
    procedure OperatorAndSymbolProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure EiffelCommentOpenProc;
    procedure EiffelCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
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
    procedure Next; override;
    function IsOperatorChar(AChar: WideChar): Boolean;
  published
    property BasicTypesAttri: TSynEdit32HighlighterAttributes read fBasicTypesAttri write fBasicTypesAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read fKeyAttri write fKeyAttri;
    property LaceAttri: TSynEdit32HighlighterAttributes read fLaceAttri write fLaceAttri;
    property OperatorAndSymbolsAttri: TSynEdit32HighlighterAttributes read fOperatorAndSymbolsAttri write fOperatorAndSymbolsAttri;
    property PredefinedAttri: TSynEdit32HighlighterAttributes read fPredefinedAttri write fPredefinedAttri;
    property ResultValueAttri: TSynEdit32HighlighterAttributes read fResultValueAttri write fResultValueAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read fStringAttri write fStringAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..118] of UnicodeString = (
    '-', '!', '#', '$', '%u', '&', '(', ')', '*', '.', '/', '//', '/=', ':', 
    ':=', ';', '@', '[', '\\', ']', '^', '|', '+', '<', '<>', '=', '>', 'adapt', 
    'alias', 'all', 'and', 'array', 'as', 'assertion', 'bit', 'boolean', 
    'character', 'check', 'class', 'cluster', 'colon', 'comma', 'creation', 
    'current', 'debug', 'default', 'deferred', 'do', 'double', 'else', 'elseif', 
    'end', 'ensure', 'exclude', 'executable', 'expanded', 'export', 'external', 
    'false', 'feature', 'from', 'frozen', 'generate', 'identifier', 'if', 
    'ignore', 'implies', 'include', 'include_path', 'indexing', 'infix', 
    'inherit', 'inspect', 'integer', 'invariant', 'is', 'like', 'local', 'loop', 
    'make', 'no', 'not', 'object', 'obsolete', 'old', 'once', 'optimize', 
    'option', 'or', 'pointer', 'precompiled', 'precursor', 'prefix', 'real', 
    'redefine', 'rename', 'require', 'rescue', 'result', 'retry', 'root', 
    'select', 'separate', 'string', 'strip', 'system', 'then', 'trace', 'true', 
    'undefine', 'unique', 'until', 'use', 'variant', 'visible', 'void', 'when', 
    'xor', 'yes' 
  );

  KeyIndices: array[0..502] of Integer = (
    -1, 49, -1, -1, -1, 97, 69, 85, -1, -1, -1, 106, -1, -1, 37, -1, -1, 63, -1, 
    92, -1, -1, -1, -1, 108, 82, 16, -1, -1, -1, -1, -1, 86, -1, 0, -1, -1, 66, 
    -1, -1, -1, -1, 91, 98, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, 13, -1, 
    -1, -1, -1, -1, -1, -1, 61, -1, -1, -1, -1, -1, -1, -1, 76, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 58, -1, -1, -1, -1, 110, -1, 1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, -1, -1, 9, 
    -1, -1, -1, -1, -1, -1, 68, 88, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 15, 105, -1, -1, -1, 51, -1, -1, 6, -1, 96, -1, -1, 17, -1, -1, 55, -1, 
    -1, -1, -1, -1, 117, -1, -1, -1, 77, -1, -1, -1, -1, -1, -1, 56, -1, -1, -1, 
    -1, 62, -1, 59, -1, -1, -1, -1, -1, -1, 79, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 83, 10, 95, -1, 113, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 65, 18, 23, -1, -1, -1, 35, -1, -1, -1, 7, -1, -1, 32, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, 90, -1, 103, -1, -1, 80, -1, 
    -1, -1, -1, 2, -1, 34, -1, -1, -1, -1, -1, -1, 41, -1, 27, 112, -1, -1, -1, 
    33, -1, 44, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, -1, 93, -1, -1, -1, 8, 46, 102, -1, 
    -1, 19, 87, -1, -1, -1, -1, 43, -1, -1, -1, -1, -1, -1, -1, 84, 53, -1, -1, 
    -1, 71, -1, -1, 11, -1, 3, 107, 67, -1, 64, 47, -1, -1, -1, -1, -1, 24, -1, 
    -1, -1, 114, -1, -1, -1, 116, -1, -1, -1, -1, 81, 75, -1, -1, -1, -1, -1, 
    -1, -1, 100, -1, -1, -1, -1, -1, 54, -1, -1, 26, 115, -1, -1, -1, -1, -1, 
    78, 22, 36, -1, 74, -1, 20, -1, -1, 42, -1, 99, -1, -1, -1, -1, -1, -1, -1, 
    73, -1, 52, -1, -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, 60, -1, 4, 94, -1, 
    -1, 40, -1, -1, 39, -1, -1, -1, -1, 45, -1, 12, -1, -1, -1, 72, 38, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 28, 48, -1, -1, -1, -1, -1, 101, -1, 118, 
    -1, -1, 57, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, 
    -1, -1, -1, -1, -1, 70, -1, 89, -1, -1, 111, -1 
  );

{$Q-}
function TSynEdit32HighlighterEiffel.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) or IsOperatorChar(Str^) do
  begin
    Result := Result * 543 + Ord(Str^) * 79;
    Inc(Str);
  end;
  Result := Result mod 503;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterEiffel.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynEdit32HighlighterEiffel.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[34] := OperatorFunc;
  fIdentFuncTable[92] := OperatorFunc;
  fIdentFuncTable[250] := OperatorFunc;
  fIdentFuncTable[329] := OperatorFunc;
  fIdentFuncTable[413] := Func37u;
  fIdentFuncTable[487] := OperatorFunc;
  fIdentFuncTable[142] := OperatorFunc;
  fIdentFuncTable[221] := OperatorFunc;
  fIdentFuncTable[300] := OperatorFunc;
  fIdentFuncTable[113] := OperatorFunc;
  fIdentFuncTable[192] := OperatorFunc;
  fIdentFuncTable[327] := OperatorFunc;
  fIdentFuncTable[427] := OperatorFunc;
  fIdentFuncTable[55] := OperatorFunc;
  fIdentFuncTable[480] := OperatorFunc;
  fIdentFuncTable[134] := OperatorFunc;
  fIdentFuncTable[26] := OperatorFunc;
  fIdentFuncTable[147] := OperatorFunc;
  fIdentFuncTable[212] := OperatorFunc;
  fIdentFuncTable[305] := OperatorFunc;
  fIdentFuncTable[384] := OperatorFunc;
  fIdentFuncTable[239] := OperatorFunc;
  fIdentFuncTable[379] := OperatorFunc;
  fIdentFuncTable[213] := OperatorFunc;
  fIdentFuncTable[340] := OperatorFunc;
  fIdentFuncTable[292] := OperatorFunc;
  fIdentFuncTable[371] := OperatorFunc;
  fIdentFuncTable[261] := FuncAdapt;
  fIdentFuncTable[462] := FuncAlias;
  fIdentFuncTable[402] := FuncAll;
  fIdentFuncTable[54] := FuncAnd;
  fIdentFuncTable[105] := FuncArray;
  fIdentFuncTable[224] := FuncAs;
  fIdentFuncTable[266] := FuncAssertion;
  fIdentFuncTable[252] := FuncBit;
  fIdentFuncTable[217] := FuncBoolean;
  fIdentFuncTable[380] := FuncCharacter;
  fIdentFuncTable[14] := FuncCheck;
  fIdentFuncTable[432] := FuncClass;
  fIdentFuncTable[420] := FuncCluster;
  fIdentFuncTable[417] := FuncColon;
  fIdentFuncTable[259] := FuncComma;
  fIdentFuncTable[387] := FuncCreation;
  fIdentFuncTable[311] := FuncCurrent;
  fIdentFuncTable[268] := FuncDebug;
  fIdentFuncTable[425] := FuncDefault;
  fIdentFuncTable[301] := FuncDeferred;
  fIdentFuncTable[334] := FuncDo;
  fIdentFuncTable[463] := FuncDouble;
  fIdentFuncTable[1] := FuncElse;
  fIdentFuncTable[270] := FuncElseif;
  fIdentFuncTable[139] := FuncEnd;
  fIdentFuncTable[399] := FuncEnsure;
  fIdentFuncTable[320] := FuncExclude;
  fIdentFuncTable[368] := FuncExecutable;
  fIdentFuncTable[150] := FuncExpanded;
  fIdentFuncTable[167] := FuncExport;
  fIdentFuncTable[474] := FuncExternal;
  fIdentFuncTable[85] := FuncFalse;
  fIdentFuncTable[174] := FuncFeature;
  fIdentFuncTable[411] := FuncFrom;
  fIdentFuncTable[63] := FuncFrozen;
  fIdentFuncTable[172] := FuncGenerate;
  fIdentFuncTable[17] := FuncIdentifier;
  fIdentFuncTable[333] := FuncIf;
  fIdentFuncTable[211] := FuncIgnore;
  fIdentFuncTable[37] := FuncImplies;
  fIdentFuncTable[331] := FuncInclude;
  fIdentFuncTable[120] := FuncInclude95path;
  fIdentFuncTable[6] := FuncIndexing;
  fIdentFuncTable[496] := FuncInfix;
  fIdentFuncTable[324] := FuncInherit;
  fIdentFuncTable[431] := FuncInspect;
  fIdentFuncTable[397] := FuncInteger;
  fIdentFuncTable[382] := FuncInvariant;
  fIdentFuncTable[354] := FuncIs;
  fIdentFuncTable[71] := FuncLike;
  fIdentFuncTable[160] := FuncLocal;
  fIdentFuncTable[378] := FuncLoop;
  fIdentFuncTable[181] := FuncMake;
  fIdentFuncTable[245] := FuncNo;
  fIdentFuncTable[353] := FuncNot;
  fIdentFuncTable[25] := FuncObject;
  fIdentFuncTable[191] := FuncObsolete;
  fIdentFuncTable[319] := FuncOld;
  fIdentFuncTable[7] := FuncOnce;
  fIdentFuncTable[32] := FuncOptimize;
  fIdentFuncTable[306] := FuncOption;
  fIdentFuncTable[121] := FuncOr;
  fIdentFuncTable[498] := FuncPointer;
  fIdentFuncTable[240] := FuncPrecompiled;
  fIdentFuncTable[42] := FuncPrecursor;
  fIdentFuncTable[19] := FuncPrefix;
  fIdentFuncTable[296] := FuncReal;
  fIdentFuncTable[414] := FuncRedefine;
  fIdentFuncTable[193] := FuncRename;
  fIdentFuncTable[144] := FuncRequire;
  fIdentFuncTable[5] := FuncRescue;
  fIdentFuncTable[43] := FuncResult;
  fIdentFuncTable[389] := FuncRetry;
  fIdentFuncTable[362] := FuncRoot;
  fIdentFuncTable[469] := FuncSelect;
  fIdentFuncTable[302] := FuncSeparate;
  fIdentFuncTable[242] := FuncString;
  fIdentFuncTable[282] := FuncStrip;
  fIdentFuncTable[135] := FuncSystem;
  fIdentFuncTable[11] := FuncThen;
  fIdentFuncTable[330] := FuncTrace;
  fIdentFuncTable[24] := FuncTrue;
  fIdentFuncTable[452] := FuncUndefine;
  fIdentFuncTable[90] := FuncUnique;
  fIdentFuncTable[501] := FuncUntil;
  fIdentFuncTable[262] := FuncUse;
  fIdentFuncTable[195] := FuncVariant;
  fIdentFuncTable[344] := FuncVisible;
  fIdentFuncTable[372] := FuncVoid;
  fIdentFuncTable[348] := FuncWhen;
  fIdentFuncTable[156] := FuncXor;
  fIdentFuncTable[471] := FuncYes;
end;

function TSynEdit32HighlighterEiffel.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.OperatorFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperatorAndSymbols
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.Func37u(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncAdapt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncAlias(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncAll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncAnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncAs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncAssertion(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncBit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncBoolean(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncCharacter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncCheck(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncCluster(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncColon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncComma(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncCreation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncCurrent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncDebug(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncDeferred(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncDouble(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncElseif(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncEnsure(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncExclude(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncExecutable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncExpanded(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncExport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncExternal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncFeature(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncFrom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncFrozen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncGenerate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncIdentifier(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncIgnore(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncImplies(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncInclude(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncInclude95path(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncIndexing(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncInfix(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncInherit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncInspect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncInteger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncInvariant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncIs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncLike(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncLoop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncMake(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncNo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncNot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncObsolete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncOld(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncOnce(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncOptimize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncOption(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncOr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncPointer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncPrecompiled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncPrecursor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncPrefix(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncReal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncRedefine(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncRename(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncRequire(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncRescue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncResult(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkResultValue
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncRetry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncRoot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncSelect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncSeparate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncStrip(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncSystem(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncThen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncTrace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncUndefine(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncUnique(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncUntil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncUse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncVariant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncVisible(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncWhen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncXor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterEiffel.FuncYes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

procedure TSynEdit32HighlighterEiffel.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterEiffel.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterEiffel.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterEiffel.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterEiffel.OperatorAndSymbolProc;
begin
  FTokenID := tkIdentifier;
  if FLine[FRun] = #33 then
    begin
      fRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(FRun);
      Exit;
    end;
  if CharInSet(FLine[FRun], [#35..#44]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(FRun);
      Exit;
    end;
  if CharInSet(FLine[FRun], [#46..#47]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(FRun);
      Exit;
    end;
  if CharInSet(FLine[FRun], [#58..#64]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(FRun);
      Exit;
    end;
  if CharInSet(FLine[FRun], [#91..#96]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(FRun);
      Exit;
    end;
  if CharInSet(FLine[FRun], [#123..#127]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(FRun);
      Exit;
    end;
end;

procedure TSynEdit32HighlighterEiffel.EiffelCommentOpenProc;
begin
  Inc(FRun);
  if (FLine[FRun - 1] = '-') and (FLine[FRun] = '-') then
    begin
      fRange := rsEiffelComment;
      EiffelCommentProc;
      FTokenID := tkComment;
    end
  else
    FTokenID := tkOperatorAndSymbols;
end;

procedure TSynEdit32HighlighterEiffel.EiffelCommentProc;
begin
  FTokenID := tkComment;
  repeat
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterEiffel.StringOpenProc;
begin
  Inc(FRun);
  fRange := rsString;
  StringProc;
  FTokenID := tkString;
end;

procedure TSynEdit32HighlighterEiffel.StringProc;
begin
  FTokenID := tkString;
  repeat
    if (FLine[FRun] = '"') then
      begin
        Inc(FRun, 1);
        fRange := rsUnKnown;
        Break;
      end;
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

constructor TSynEdit32HighlighterEiffel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  fBasicTypesAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrBasicTypes, SYNS_FriendlyAttrBasicTypes);
  fBasicTypesAttri.Style := [fsBold];
  fBasicTypesAttri.Foreground := clBlue;
  AddAttribute(fBasicTypesAttri);

  fCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clTeal;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  fIdentifierAttri.Foreground := clMaroon;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fLaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrLace, SYNS_FriendlyAttrLace);
  fLaceAttri.Style := [fsBold];
  fLaceAttri.Foreground := clNavy;
  AddAttribute(fLaceAttri);

  fOperatorAndSymbolsAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrOperatorAndSymbols, SYNS_FriendlyAttrOperatorAndSymbols);
  fOperatorAndSymbolsAttri.Style := [fsBold];
  fOperatorAndSymbolsAttri.Foreground := clOlive;
  AddAttribute(fOperatorAndSymbolsAttri);

  fPredefinedAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPredefined, SYNS_FriendlyAttrPredefined);
  fPredefinedAttri.Style := [fsBold];
  fPredefinedAttri.Foreground := clRed;
  AddAttribute(fPredefinedAttri);

  fResultValueAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrResultValue, SYNS_FriendlyAttrResultValue);
  fResultValueAttri.Style := [fsBold];
  fResultValueAttri.Foreground := clPurple;
  AddAttribute(fResultValueAttri);

  fSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Style := [fsItalic];
  fStringAttri.Foreground := clGray;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterEiffel;
  fRange := rsUnknown;
end;

procedure TSynEdit32HighlighterEiffel.IdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterEiffel.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterEiffel.Next;
begin
  fTokenPos := FRun;
  fRange := rsUnknown;
  case FLine[FRun] of
    #33, #35..#44, #46..#47, #58..#64, #91..#96, #123..#127: OperatorAndSymbolProc;
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    '-': EiffelCommentOpenProc;
    '"': StringOpenProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    'A'..'Z', 'a'..'z': IdentProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynEdit32HighlighterEiffel.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterEiffel.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result :=
    '-,!,#,$,%U,&,(,),*,.,/,//,/=,:,:=,;,@,[,\\,],^,|,+,<,<>,=,>,adapt,ali' +
    'as,all,and,Array,as,assertion,BIT,boolean,character,check,class,cluste' +
    'r,colon,comma,creation,current,debug,default,deferred,do,double,else,e' +
    'lseif,end,ensure,exclude,executable,expanded,export,external,false,fea' +
    'ture,from,frozen,generate,identifier,if,ignore,implies,include,include' +
    '_path,indexing,infix,inherit,inspect,integer,invariant,is,like,local,l' +
    'oop,make,no,not,object,obsolete,old,once,optimize,option,or,pointer,pr' +
    'ecompiled,precursor,prefix,real,redefine,rename,require,rescue,result,' +
    'retry,root,select,separate,string,strip,system,then,trace,true,undefin' +
    'e,unique,until,use,variant,visible,void,when,xor,yes';
end;

function TSynEdit32HighlighterEiffel.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterEiffel.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkBasicTypes: Result := fBasicTypesAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkLace: Result := fLaceAttri;
    tkOperatorAndSymbols: Result := fOperatorAndSymbolsAttri;
    tkPredefined: Result := fPredefinedAttri;
    tkResultValue: Result := fResultValueAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterEiffel.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterEiffel.GetSampleSource: UnicodeString;
begin
  Result := '-- Eiffel sample source from SmartEiffel'#13#10 +
    'class FIBONACCI'#13#10 +
    '-- Eiffel comment'#13#10 +
    'creation make'#13#10 +
    #13#10 +
    'feature'#13#10 +
    #13#10 +
    '   make is'#13#10 +
    '      do'#13#10 +
    '         if argument_count /= 1 or else'#13#10 +
    '            not argument(1).is_integer'#13#10 +
    '          then'#13#10 +
    '            io.put_string("Usage: ");'#13#10 +
    '            io.put_string(argument(0));'#13#10 +
    '            io.put_string(" <Integer_value>%N");'#13#10 +
    '            die_with_code(exit_failure_code);'#13#10 +
    '         end;'#13#10 +
    '         io.put_integer(fibonacci(argument(1).to_integer));'#13#10 +
    '         io.put_new_line;'#13#10 +
    '      end;'#13#10 +
    '   -- Eiffel comment'#13#10 +
    '   fibonacci(i: INTEGER): INTEGER is'#13#10 +
    '      require -- Eiffel comment'#13#10 +
    '         i >= 0'#13#10 +
    '      do'#13#10 +
    '         if i = 0 then'#13#10 +
    '            Result := 1;'#13#10 +
    '         elseif i = 1 then'#13#10 +
    '            Result := 1;'#13#10 +
    '         else'#13#10 +
    '            Result := fibonacci(i - 1) + fibonacci(i - 2) ;'#13#10 +
    '         end;'#13#10 +
    '      end;'#13#10 +
    #13#10 +
    'end';
end;

function TSynEdit32HighlighterEiffel.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterEiffel;
end;

class function TSynEdit32HighlighterEiffel.GetLanguageName: string;
begin
  Result := SYNS_LangEiffel;
end;

procedure TSynEdit32HighlighterEiffel.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynEdit32HighlighterEiffel.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynEdit32HighlighterEiffel.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynEdit32HighlighterEiffel.IsOperatorChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '-', '!', '#', '$', '%', '&', '(', ')', '*', '.', '/',
    ':', ';', '@', '[', '\', ']', '^', '|', '+', '<', '=', '>':
      Result := True
    else
      Result := False;
  end;
end;

class function TSynEdit32HighlighterEiffel.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangEiffel;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterEiffel);

end.
