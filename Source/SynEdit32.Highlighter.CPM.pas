{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCPM.pas, released 2001-08-14.
The Initial Author of this file is Pieter Polak.
Unicode translation by Maël Hörz.
All Rights Reserved.

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

$Id: SynHighlighterCPM.pas,v 1.16.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.CPM;

{$I SynEdit.Inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

Type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkSpace,
    tkSQLKey,
    tkString,
    tkSymbol,
    tkSpecialVar,
    tkSystem,
    tkVariable,
    tkNumber,
    tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TRangeState = (rsBraceComment, rsUnKnown);

type
  TSynEdit32HighlighterCPM = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FCommentLevel: Integer;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..796] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FSQLKeyAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FSpecialVarAttri: TSynEdit32HighlighterAttributes;
    FSystemAttri: TSynEdit32HighlighterAttributes;
    FVariableAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAllentities(Index: Integer): TtkTokenKind;
    function FuncAllproducts(Index: Integer): TtkTokenKind;
    function FuncAllproperties(Index: Integer): TtkTokenKind;
    function FuncAllqualityproperties(Index: Integer): TtkTokenKind;
    function FuncAllsuppliers(Index: Integer): TtkTokenKind;
    function FuncAssign(Index: Integer): TtkTokenKind;
    function FuncBegin(Index: Integer): TtkTokenKind;
    function FuncBlock(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCategory(Index: Integer): TtkTokenKind;
    function FuncCenterstr(Index: Integer): TtkTokenKind;
    function FuncCharreplacestr(Index: Integer): TtkTokenKind;
    function FuncCharrlenstr(Index: Integer): TtkTokenKind;
    function FuncCharrllenstr(Index: Integer): TtkTokenKind;
    function FuncChr(Index: Integer): TtkTokenKind;
    function FuncClient(Index: Integer): TtkTokenKind;
    function FuncConstants(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncCopyfile(Index: Integer): TtkTokenKind;
    function FuncCountry(Index: Integer): TtkTokenKind;
    function FuncDecr(Index: Integer): TtkTokenKind;
    function FuncDefinition(Index: Integer): TtkTokenKind;
    function FuncDistinct_execute(Index: Integer): TtkTokenKind;
    function FuncDivide(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEmptysheet(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncEntitycode(Index: Integer): TtkTokenKind;
    function FuncEqualstring(Index: Integer): TtkTokenKind;
    function FuncEqualvalue(Index: Integer): TtkTokenKind;
    function FuncExecute(Index: Integer): TtkTokenKind;
    function FuncFileappend(Index: Integer): TtkTokenKind;
    function FuncFileassign(Index: Integer): TtkTokenKind;
    function FuncFileclose(Index: Integer): TtkTokenKind;
    function FuncFilecopy(Index: Integer): TtkTokenKind;
    function FuncFiledate(Index: Integer): TtkTokenKind;
    function FuncFiledelete(Index: Integer): TtkTokenKind;
    function FuncFileend(Index: Integer): TtkTokenKind;
    function FuncFileexists(Index: Integer): TtkTokenKind;
    function FuncFilereadln(Index: Integer): TtkTokenKind;
    function FuncFilereset(Index: Integer): TtkTokenKind;
    function FuncFilerewrite(Index: Integer): TtkTokenKind;
    function FuncFilesize(Index: Integer): TtkTokenKind;
    function FuncFilesort(Index: Integer): TtkTokenKind;
    function FuncFiletime(Index: Integer): TtkTokenKind;
    function FuncFilewriteln(Index: Integer): TtkTokenKind;
    function FuncFilterstr(Index: Integer): TtkTokenKind;
    function FuncFirstinstance(Index: Integer): TtkTokenKind;
    function FuncFlow(Index: Integer): TtkTokenKind;
    function FuncFold(Index: Integer): TtkTokenKind;
    function FuncForeign(Index: Integer): TtkTokenKind;
    function FuncGlobalconstants(Index: Integer): TtkTokenKind;
    function FuncGlobals(Index: Integer): TtkTokenKind;
    function FuncGlobalvariables(Index: Integer): TtkTokenKind;
    function FuncGroupdown(Index: Integer): TtkTokenKind;
    function FuncGroupfooter(Index: Integer): TtkTokenKind;
    function FuncGroupheader(Index: Integer): TtkTokenKind;
    function FuncGroupkey(Index: Integer): TtkTokenKind;
    function FuncGroupup(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncInclude(Index: Integer): TtkTokenKind;
    function FuncIncr(Index: Integer): TtkTokenKind;
    function FuncLanguage(Index: Integer): TtkTokenKind;
    function FuncLastinstance(Index: Integer): TtkTokenKind;
    function FuncLeftstr(Index: Integer): TtkTokenKind;
    function FuncLength(Index: Integer): TtkTokenKind;
    function FuncLlenstr(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLocasestr(Index: Integer): TtkTokenKind;
    function FuncLoop(Index: Integer): TtkTokenKind;
    function FuncLowerlevelstoo(Index: Integer): TtkTokenKind;
    function FuncLtrunc(Index: Integer): TtkTokenKind;
    function FuncMatching(Index: Integer): TtkTokenKind;
    function FuncMember(Index: Integer): TtkTokenKind;
    function FuncMerge(Index: Integer): TtkTokenKind;
    function FuncMessagedlg(Index: Integer): TtkTokenKind;
    function FuncMetaflow(Index: Integer): TtkTokenKind;
    function FuncMidstr(Index: Integer): TtkTokenKind;
    function FuncMultiply(Index: Integer): TtkTokenKind;
    function FuncNextinstance(Index: Integer): TtkTokenKind;
    function FuncNextrepeatinstance(Index: Integer): TtkTokenKind;
    function FuncOf(Index: Integer): TtkTokenKind;
    function FuncOptions(Index: Integer): TtkTokenKind;
    function FuncOrganisation(Index: Integer): TtkTokenKind;
    function FuncOutput(Index: Integer): TtkTokenKind;
    function FuncParam(Index: Integer): TtkTokenKind;
    function FuncParent(Index: Integer): TtkTokenKind;
    function FuncParseInc(Index: Integer): TtkTokenKind;
    function FuncPdriver(Index: Integer): TtkTokenKind;
    function FuncPrevinstance(Index: Integer): TtkTokenKind;
    function FuncPrevrepeatinstance(Index: Integer): TtkTokenKind;
    function FuncPrinter(Index: Integer): TtkTokenKind;
    function FuncPrintfile(Index: Integer): TtkTokenKind;
    function FuncPropertygroup(Index: Integer): TtkTokenKind;
    function FuncRastr(Index: Integer): TtkTokenKind;
    function FuncRaval(Index: Integer): TtkTokenKind;
    function FuncReadinstance(Index: Integer): TtkTokenKind;
    function FuncReadrepeatinstance(Index: Integer): TtkTokenKind;
    function FuncRepeat(Index: Integer): TtkTokenKind;
    function FuncRepeatcount(Index: Integer): TtkTokenKind;
    function FuncReportlevel(Index: Integer): TtkTokenKind;
    function FuncRightstr(Index: Integer): TtkTokenKind;
    function FuncRlenstr(Index: Integer): TtkTokenKind;
    function FuncRoot(Index: Integer): TtkTokenKind;
    function FuncRound(Index: Integer): TtkTokenKind;
    function FuncShowmessage(Index: Integer): TtkTokenKind;
    function FuncSkipemtpty(Index: Integer): TtkTokenKind;
    function FuncSortdown(Index: Integer): TtkTokenKind;
    function FuncSortkey(Index: Integer): TtkTokenKind;
    function FuncSortup(Index: Integer): TtkTokenKind;
    function FuncSql_add(Index: Integer): TtkTokenKind;
    function FuncSql_asfloat(Index: Integer): TtkTokenKind;
    function FuncSql_asstring(Index: Integer): TtkTokenKind;
    function FuncSql_create(Index: Integer): TtkTokenKind;
    function FuncSql_dump(Index: Integer): TtkTokenKind;
    function FuncSql_eof(Index: Integer): TtkTokenKind;
    function FuncSql_execute(Index: Integer): TtkTokenKind;
    function FuncSql_free(Index: Integer): TtkTokenKind;
    function FuncSql_mladd(Index: Integer): TtkTokenKind;
    function FuncSql_mlmultiadd(Index: Integer): TtkTokenKind;
    function FuncSql_next(Index: Integer): TtkTokenKind;
    function FuncSql_setvar(Index: Integer): TtkTokenKind;
    function FuncSqr(Index: Integer): TtkTokenKind;
    function FuncStripstr(Index: Integer): TtkTokenKind;
    function FuncStroptions(Index: Integer): TtkTokenKind;
    function FuncStrpos(Index: Integer): TtkTokenKind;
    function FuncSubtract(Index: Integer): TtkTokenKind;
    function FuncSum(Index: Integer): TtkTokenKind;
    function FuncSupplier(Index: Integer): TtkTokenKind;
    function FuncSuppliesofmembers(Index: Integer): TtkTokenKind;
    function FuncThen(Index: Integer): TtkTokenKind;
    function FuncTrunc(Index: Integer): TtkTokenKind;
    function FuncUpcasestr(Index: Integer): TtkTokenKind;
    function FuncUsedby(Index: Integer): TtkTokenKind;
    function FuncV_date(Index: Integer): TtkTokenKind;
    function FuncV_false(Index: Integer): TtkTokenKind;
    function FuncV_nonereal(Index: Integer): TtkTokenKind;
    function FuncV_par_language(Index: Integer): TtkTokenKind;
    function FuncV_par_language_count(Index: Integer): TtkTokenKind;
    function FuncV_par_language_fields(Index: Integer): TtkTokenKind;
    function FuncV_time(Index: Integer): TtkTokenKind;
    function FuncV_true(Index: Integer): TtkTokenKind;
    function FuncVariables(Index: Integer): TtkTokenKind;
    function FuncVaroptions(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncZerorlenstr(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure CRProc;
    procedure LFProc;
    procedure SemiColonProc;
    procedure SymbolProc;
    procedure NumberProc;
    procedure BraceOpenProc;
    procedure IdentProc;
    procedure VariableProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure BraceCommentProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property SQLKeyAttri: TSynEdit32HighlighterAttributes read FSQLKeyAttri write FSQLKeyAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpecialVarAttri: TSynEdit32HighlighterAttributes read FSpecialVarAttri write FSpecialVarAttri;
    property SystemAttri: TSynEdit32HighlighterAttributes read FSystemAttri write FSystemAttri;
    property VariableAttri: TSynEdit32HighlighterAttributes read FVariableAttri write FVariableAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..145] of UnicodeString = (
    'allentities', 'allproducts', 'allproperties', 'allqualityproperties', 
    'allsuppliers', 'assign', 'begin', 'block', 'case', 'category', 'centerstr', 
    'charreplacestr', 'charrlenstr', 'charrllenstr', 'chr', 'client', 
    'constants', 'continue', 'copyfile', 'country', 'decr', 'definition', 
    'distinct_execute', 'divide', 'else', 'emptysheet', 'end', 'entitycode', 
    'equalstring', 'equalvalue', 'execute', 'fileappend', 'fileassign', 
    'fileclose', 'filecopy', 'filedate', 'filedelete', 'fileend', 'fileexists', 
    'filereadln', 'filereset', 'filerewrite', 'filesize', 'filesort', 
    'filetime', 'filewriteln', 'filterstr', 'firstinstance', 'flow', 'fold', 
    'foreign', 'globalconstants', 'globals', 'globalvariables', 'groupdown', 
    'groupfooter', 'groupheader', 'groupkey', 'groupup', 'if', 'include', 
    'incr', 'language', 'lastinstance', 'leftstr', 'length', 'llenstr', 'local', 
    'locasestr', 'loop', 'lowerlevelstoo', 'ltrunc', 'matching', 'member', 
    'merge', 'messagedlg', 'metaflow', 'midstr', 'multiply', 'nextinstance', 
    'nextrepeatinstance', 'of', 'options', 'organisation', 'output', 'param', 
    'parent', 'parseinc', 'pdriver', 'previnstance', 'prevrepeatinstance', 
    'printer', 'printfile', 'propertygroup', 'rastr', 'raval', 'readinstance', 
    'readrepeatinstance', 'repeat', 'repeatcount', 'reportlevel', 'rightstr', 
    'rlenstr', 'root', 'round', 'showmessage', 'skipemtpty', 'sortdown', 
    'sortkey', 'sortup', 'sql_add', 'sql_asfloat', 'sql_asstring', 'sql_create', 
    'sql_dump', 'sql_eof', 'sql_execute', 'sql_free', 'sql_mladd', 
    'sql_mlmultiadd', 'sql_next', 'sql_setvar', 'sqr', 'stripstr', 'stroptions', 
    'strpos', 'subtract', 'sum', 'supplier', 'suppliesofmembers', 'then', 
    'trunc', 'upcasestr', 'usedby', 'v_date', 'v_false', 'v_nonereal', 
    'v_par_language', 'v_par_language_count', 'v_par_language_fields', 'v_time', 
    'v_true', 'variables', 'varoptions', 'while', 'zerorlenstr' 
  );

  KeyIndices: array[0..796] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, 45, -1, 26, -1, -1, -1, -1, -1, 74, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 25, 85, -1, -1, -1, 58, -1, 51, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 4, 43, 30, -1, 54, 127, -1, -1, -1, 136, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 56, 38, -1, 32, -1, -1, -1, -1, -1, -1, 
    -1, 133, 65, -1, 96, -1, -1, -1, 144, -1, -1, -1, -1, -1, -1, -1, -1, 89, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 23, -1, -1, -1, 35, -1, -1, 5, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 75, 41, -1, -1, 36, -1, -1, -1, -1, -1, -1, 143, -1, 
    -1, 105, -1, -1, -1, -1, -1, 86, 142, 99, -1, 131, -1, -1, -1, -1, -1, -1, 
    8, -1, -1, -1, -1, 83, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 53, 27, -1, -1, -1, -1, -1, -1, 102, -1, -1, 
    -1, -1, -1, -1, -1, 2, -1, -1, 28, -1, 24, 141, -1, -1, 101, -1, -1, 134, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 111, -1, 100, -1, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 44, 135, -1, 117, -1, 77, -1, 37, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 69, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 42, 7, -1, 
    109, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, -1, 113, -1, -1, 0, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, -1, 73, 34, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 132, -1, -1, -1, 123, -1, -1, -1, -1, -1, 
    63, -1, 48, -1, -1, -1, -1, -1, -1, -1, -1, -1, 140, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 66, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 84, -1, 
    -1, -1, -1, 95, -1, -1, -1, -1, -1, -1, -1, 71, 138, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 93, 110, -1, -1, 80, -1, -1, 137, -1, -1, -1, 91, -1, 60, -1, 
    -1, 62, -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, 29, -1, 
    -1, 122, -1, -1, -1, -1, 39, -1, 61, -1, -1, -1, -1, -1, 6, -1, -1, -1, -1, 
    -1, -1, 22, 130, -1, -1, -1, -1, -1, 81, -1, 57, -1, -1, 20, 121, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 94, -1, 31, -1, -1, -1, -1, 
    -1, 47, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, 
    -1, -1, 64, -1, -1, 1, -1, 118, -1, -1, -1, -1, -1, -1, 87, 49, -1, -1, -1, 
    -1, -1, 79, -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    46, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    106, -1, 97, -1, 68, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 40, -1, -1, 72, 70, 88, -1, 12, -1, -1, -1, -1, -1, -1, -1, 124, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 114, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 92, 
    -1, -1, 59, -1, -1, -1, -1, -1, 11, -1, -1, 104, -1, -1, -1, -1, -1, -1, -1, 
    18, 78, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 17, -1, 129, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 112, -1, -1, 98, -1, 116, 120, -1, 21, -1, 9, -1, 
    -1, -1, 19, -1, -1, -1, 50, -1, -1, -1, 126, -1, -1, 55, -1, 145, -1, -1, 
    -1, -1, 52, 139, -1, 14, -1, -1, 115, -1, -1, -1, 90, -1, -1, -1, 128, -1, 
    -1, -1, 103, -1, -1, -1, -1, -1, 3, -1, -1, 76, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 16, -1, -1, -1 
  );

{$Q-}
function TSynEdit32HighlighterCPM.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 841 + Ord(Str^) * 268;
    Inc(Str);
  end;
  Result := Result mod 797;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterCPM.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterCPM.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[314] := FuncAllentities;
  FIdentFuncTable[528] := FuncAllproducts;
  FIdentFuncTable[212] := FuncAllproperties;
  FIdentFuncTable[774] := FuncAllqualityproperties;
  FIdentFuncTable[46] := FuncAllsuppliers;
  FIdentFuncTable[127] := FuncAssign;
  FIdentFuncTable[462] := FuncBegin;
  FIdentFuncTable[297] := FuncBlock;
  FIdentFuncTable[169] := FuncCase;
  FIdentFuncTable[728] := FuncCategory;
  FIdentFuncTable[106] := FuncCenterstr;
  FIdentFuncTable[663] := FuncCharreplacestr;
  FIdentFuncTable[607] := FuncCharrlenstr;
  FIdentFuncTable[326] := FuncCharrllenstr;
  FIdentFuncTable[753] := FuncChr;
  FIdentFuncTable[251] := FuncClient;
  FIdentFuncTable[793] := FuncConstants;
  FIdentFuncTable[694] := FuncContinue;
  FIdentFuncTable[674] := FuncCopyfile;
  FIdentFuncTable[732] := FuncCountry;
  FIdentFuncTable[481] := FuncDecr;
  FIdentFuncTable[726] := FuncDefinition;
  FIdentFuncTable[469] := FuncDistinct_execute;
  FIdentFuncTable[120] := FuncDivide;
  FIdentFuncTable[217] := FuncElse;
  FIdentFuncTable[26] := FuncEmptysheet;
  FIdentFuncTable[10] := FuncEnd;
  FIdentFuncTable[197] := FuncEntitycode;
  FIdentFuncTable[215] := FuncEqualstring;
  FIdentFuncTable[446] := FuncEqualvalue;
  FIdentFuncTable[48] := FuncExecute;
  FIdentFuncTable[499] := FuncFileappend;
  FIdentFuncTable[69] := FuncFileassign;
  FIdentFuncTable[521] := FuncFileclose;
  FIdentFuncTable[329] := FuncFilecopy;
  FIdentFuncTable[124] := FuncFiledate;
  FIdentFuncTable[142] := FuncFiledelete;
  FIdentFuncTable[273] := FuncFileend;
  FIdentFuncTable[67] := FuncFileexists;
  FIdentFuncTable[454] := FuncFilereadln;
  FIdentFuncTable[600] := FuncFilereset;
  FIdentFuncTable[139] := FuncFilerewrite;
  FIdentFuncTable[296] := FuncFilesize;
  FIdentFuncTable[47] := FuncFilesort;
  FIdentFuncTable[266] := FuncFiletime;
  FIdentFuncTable[8] := FuncFilewriteln;
  FIdentFuncTable[561] := FuncFilterstr;
  FIdentFuncTable[505] := FuncFirstinstance;
  FIdentFuncTable[356] := FuncFlow;
  FIdentFuncTable[538] := FuncFold;
  FIdentFuncTable[736] := FuncForeign;
  FIdentFuncTable[33] := FuncGlobalconstants;
  FIdentFuncTable[750] := FuncGlobals;
  FIdentFuncTable[196] := FuncGlobalvariables;
  FIdentFuncTable[50] := FuncGroupdown;
  FIdentFuncTable[743] := FuncGroupfooter;
  FIdentFuncTable[66] := FuncGroupheader;
  FIdentFuncTable[478] := FuncGroupkey;
  FIdentFuncTable[31] := FuncGroupup;
  FIdentFuncTable[657] := FuncIf;
  FIdentFuncTable[427] := FuncInclude;
  FIdentFuncTable[456] := FuncIncr;
  FIdentFuncTable[430] := FuncLanguage;
  FIdentFuncTable[354] := FuncLastinstance;
  FIdentFuncTable[525] := FuncLeftstr;
  FIdentFuncTable[78] := FuncLength;
  FIdentFuncTable[379] := FuncLlenstr;
  FIdentFuncTable[177] := FuncLocal;
  FIdentFuncTable[583] := FuncLocasestr;
  FIdentFuncTable[285] := FuncLoop;
  FIdentFuncTable[604] := FuncLowerlevelstoo;
  FIdentFuncTable[403] := FuncLtrunc;
  FIdentFuncTable[603] := FuncMatching;
  FIdentFuncTable[328] := FuncMember;
  FIdentFuncTable[16] := FuncMerge;
  FIdentFuncTable[138] := FuncMessagedlg;
  FIdentFuncTable[777] := FuncMetaflow;
  FIdentFuncTable[271] := FuncMidstr;
  FIdentFuncTable[675] := FuncMultiply;
  FIdentFuncTable[544] := FuncNextinstance;
  FIdentFuncTable[418] := FuncNextrepeatinstance;
  FIdentFuncTable[476] := FuncOf;
  FIdentFuncTable[440] := FuncOptions;
  FIdentFuncTable[174] := FuncOrganisation;
  FIdentFuncTable[390] := FuncOutput;
  FIdentFuncTable[27] := FuncParam;
  FIdentFuncTable[158] := FuncParent;
  FIdentFuncTable[537] := FuncParseinc;
  FIdentFuncTable[605] := FuncPdriver;
  FIdentFuncTable[93] := FuncPrevinstance;
  FIdentFuncTable[760] := FuncPrevrepeatinstance;
  FIdentFuncTable[425] := FuncPrinter;
  FIdentFuncTable[654] := FuncPrintfile;
  FIdentFuncTable[414] := FuncPropertygroup;
  FIdentFuncTable[497] := FuncRastr;
  FIdentFuncTable[395] := FuncRaval;
  FIdentFuncTable[80] := FuncReadinstance;
  FIdentFuncTable[581] := FuncReadrepeatinstance;
  FIdentFuncTable[721] := FuncRepeat;
  FIdentFuncTable[160] := FuncRepeatcount;
  FIdentFuncTable[249] := FuncReportlevel;
  FIdentFuncTable[221] := FuncRightstr;
  FIdentFuncTable[204] := FuncRlenstr;
  FIdentFuncTable[768] := FuncRoot;
  FIdentFuncTable[666] := FuncRound;
  FIdentFuncTable[152] := FuncShowmessage;
  FIdentFuncTable[579] := FuncSkipemtpty;
  FIdentFuncTable[307] := FuncSortdown;
  FIdentFuncTable[508] := FuncSortkey;
  FIdentFuncTable[299] := FuncSortup;
  FIdentFuncTable[415] := FuncSql_add;
  FIdentFuncTable[247] := FuncSql_asfloat;
  FIdentFuncTable[718] := FuncSql_asstring;
  FIdentFuncTable[311] := FuncSql_create;
  FIdentFuncTable[635] := FuncSql_dump;
  FIdentFuncTable[756] := FuncSql_eof;
  FIdentFuncTable[723] := FuncSql_execute;
  FIdentFuncTable[269] := FuncSql_free;
  FIdentFuncTable[530] := FuncSql_mladd;
  FIdentFuncTable[551] := FuncSql_mlmultiadd;
  FIdentFuncTable[724] := FuncSql_next;
  FIdentFuncTable[482] := FuncSql_setvar;
  FIdentFuncTable[449] := FuncSqr;
  FIdentFuncTable[348] := FuncStripstr;
  FIdentFuncTable[615] := FuncStroptions;
  FIdentFuncTable[566] := FuncStrpos;
  FIdentFuncTable[740] := FuncSubtract;
  FIdentFuncTable[51] := FuncSum;
  FIdentFuncTable[764] := FuncSupplier;
  FIdentFuncTable[696] := FuncSuppliesofmembers;
  FIdentFuncTable[470] := FuncThen;
  FIdentFuncTable[162] := FuncTrunc;
  FIdentFuncTable[344] := FuncUpcasestr;
  FIdentFuncTable[77] := FuncUsedby;
  FIdentFuncTable[224] := FuncV_date;
  FIdentFuncTable[267] := FuncV_false;
  FIdentFuncTable[55] := FuncV_nonereal;
  FIdentFuncTable[421] := FuncV_par_language;
  FIdentFuncTable[404] := FuncV_par_language_count;
  FIdentFuncTable[751] := FuncV_par_language_fields;
  FIdentFuncTable[366] := FuncV_time;
  FIdentFuncTable[218] := FuncV_true;
  FIdentFuncTable[159] := FuncVariables;
  FIdentFuncTable[149] := FuncVaroptions;
  FIdentFuncTable[84] := FuncWhile;
  FIdentFuncTable[745] := FuncZerorlenstr;
end;

function TSynEdit32HighlighterCPM.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncAllentities(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncAllproducts(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncAllproperties(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncAllqualityproperties(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncAllsuppliers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncAssign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncBegin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncBlock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCategory(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCenterstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCharreplacestr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCharrlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCharrllenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncChr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncClient(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncConstants(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCopyfile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncCountry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncDecr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncDefinition(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncDistinct_execute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncDivide(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncEmptysheet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncEntitycode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncEqualstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncEqualvalue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncExecute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFileappend(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFileassign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFileclose(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilecopy(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFiledate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFiledelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFileend(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFileexists(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilereadln(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilereset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilerewrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilesize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilesort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFiletime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilewriteln(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFilterstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFirstinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFlow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncFold(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncForeign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGlobalconstants(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGlobals(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGlobalvariables(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGroupdown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGroupfooter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGroupheader(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGroupkey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncGroupup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncInclude(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncIncr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLanguage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLastinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLeftstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLength(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLocasestr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLoop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLowerlevelstoo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncLtrunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncMatching(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncMember(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncMerge(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncMessagedlg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncMetaflow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncMidstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncMultiply(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncNextinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncNextrepeatinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncOf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncOptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncOrganisation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncOutput(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncParam(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncParent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncParseInc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncPdriver(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncPrevinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncPrevrepeatinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncPrinter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncPrintfile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncPropertygroup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRastr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRaval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncReadinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncReadrepeatinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRepeat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRepeatcount(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncReportlevel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRightstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRoot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncRound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncShowmessage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSkipemtpty(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSortdown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSortkey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSortup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_add(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_asfloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_asstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_create(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_dump(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_eof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_execute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_free(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_mladd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_mlmultiadd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_next(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSql_setvar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSqr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncStripstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncStroptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncStrpos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSubtract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSupplier(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncSuppliesofmembers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncThen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncTrunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncUpcasestr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncUsedby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_date(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_false(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_nonereal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_par_language(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_par_language_count(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_par_language_fields(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_time(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncV_true(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncVariables(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncVaroptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterCPM.FuncZerorlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterCPM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clNavy;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clGreen;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FSQLKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSQLKey, SYNS_FriendlyAttrSQLKey);
  FSQLKeyAttri.ForeGround := clTeal;
  FSQLKeyAttri.Style := [fsBold];
  AddAttribute(FSQLKeyAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpecialVarAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpecialVariable, SYNS_FriendlyAttrSpecialVariable);
  FSpecialVarAttri.Style := [fsBold];
  AddAttribute(FSpecialVarAttri);

  FSystemAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  FSystemAttri.Foreground := $000080FF;
  FSystemAttri.Style := [fsBold];
  AddAttribute(FSystemAttri);

  FVariableAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clMaroon;
  AddAttribute(FVariableAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  FCommentLevel := 0;
  fDefaultFilter := SYNS_FilterCPM;
end; { Create }

procedure TSynEdit32HighlighterCPM.BraceOpenProc;
begin
  FRange := rsBraceComment;
  BraceCommentProc;
  FTokenID := tkComment;
end; { BraceOpenProc }

procedure TSynEdit32HighlighterCPM.IdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end; { IdentProc }

procedure TSynEdit32HighlighterCPM.VariableProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  if (FTokenID = tkIdentifier) then
  begin
    if (FLine[FRun + 1] = '_') then
      FTokenID := tkVariable
  end;
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end; { VariableProc }

procedure TSynEdit32HighlighterCPM.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end; { NullProc }

procedure TSynEdit32HighlighterCPM.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end; { SpaceProc }

procedure TSynEdit32HighlighterCPM.StringProc;
begin
  FTokenID := tkString;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = '"');
  if (FLine[FRun] = '"') then
  begin
    Inc(FRun);
    if (FLine[FRun] = '"') then
      Inc(FRun);
  end;
end; { StringProc }

procedure TSynEdit32HighlighterCPM.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end; { UnknownProc }

procedure TSynEdit32HighlighterCPM.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsBraceComment: BraceCommentProc;
  else
    case FLine[FRun] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '"': StringProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_':
        case FLine[FRun] of
          'V', 'v', 'S', 's': VariableProc;
          else
            IdentProc;
        end;
      '{': BraceOpenProc;
      '}', '!', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
      begin
        case FLine[FRun] of
          ';': SemiColonProc;
          else
            SymbolProc;
        end;
      end;
    else
      UnknownProc;
    end;
  end;
  inherited;
end; { Next }

function TSynEdit32HighlighterCPM.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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
end; { GetDefaultAttribute }

function TSynEdit32HighlighterCPM.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end; { GetTokenID }

function TSynEdit32HighlighterCPM.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkSQLKey: Result := FSQLKeyAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpecialVar: Result := FSpecialVarAttri;
    tkSystem: Result := FSystemAttri;
    tkVariable: Result := FVariableAttri; 
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end; { GetTokenAttribute }

function TSynEdit32HighlighterCPM.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end; { GetTokenKind }

class function TSynEdit32HighlighterCPM.GetLanguageName: string;
begin
  Result := SYNS_LangCPM;
end;

procedure TSynEdit32HighlighterCPM.BraceCommentProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if FLine[FRun] = '{' then
          Inc(FCommentLevel)
        else if FLine[FRun] = '}' then
        begin
          Dec(FCommentLevel);
          if (FCommentLevel < 1) then
          begin
            Inc(FRun);
            FRange := rsUnKnown;
            FCommentLevel := 0;
            Break;
          end;
        end;
        Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end; { BraceCommentProc }

procedure TSynEdit32HighlighterCPM.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end; { CRProc }

procedure TSynEdit32HighlighterCPM.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end; { LFProc }

function TSynEdit32HighlighterCPM.GetSampleSource: UnicodeString;
begin
  Result := '{ COAS Product Manager report (RDF) }'#13#10 +
            'PARAM'#13#10 +
            '  LANGUAGE;'#13#10 +
            '  CONTINUE;'#13#10 +
            'END; { Param }'#13#10 +
            #13#10 +
            'GLOBALS'#13#10 +
            '  LANGUAGE = LOCAL;'#13#10 +
            'END; { Globals }'#13#10 +
            #13#10 +
            'DEFINITION BLOCK "MAIN"'#13#10 +
            'VARIABLES'#13#10 +
            '  S_Query = "";'#13#10 +
            '  V_OraErr = -1;'#13#10 +
            '  V_Count;'#13#10 +
            'BEGIN'#13#10 +
            '  ASSIGN(S_Query, "SELECT * FROM DUAL");'#13#10 +
            '  SQL_CREATE(V_OraErr, S_Query);'#13#10 +
            '  ASSIGN(V_Count, V_NoneReal);'#13#10 +
            'END;';
end; { GetSampleSource }

function TSynEdit32HighlighterCPM.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPM;
end; { IsFilterStored }

procedure TSynEdit32HighlighterCPM.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end; { SemiColonProc }

procedure TSynEdit32HighlighterCPM.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(FRun);
  FTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[FRun] of
      '.': if FLine[FRun + 1] = '.' then
             Break;
    end;
    Inc(FRun);
  end;
end; { NumberProc }

procedure TSynEdit32HighlighterCPM.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end; { SymbolProc }

procedure TSynEdit32HighlighterCPM.ResetRange;
begin
  inherited;
  FRange := rsUnknown;
  FCommentLevel := 0;
end; { ResetRange }

procedure TSynEdit32HighlighterCPM.SetRange(Value: Pointer);
var
  AValue: LongInt;
begin
  inherited;
  AValue := Longint(Value);
  FCommentLevel := AValue div $10000;
  FRange := TRangeState(AValue mod $10000);
end; { SetRange }

function TSynEdit32HighlighterCPM.GetRange: Pointer;
begin
  Result := Pointer((FCommentLevel * $10000) + Integer(FRange));
end; { GetRange }

class function TSynEdit32HighlighterCPM.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCPM;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterCPM);
end.
