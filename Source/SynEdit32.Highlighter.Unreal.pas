{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
                                          PP - 2001/10/24:
The Original Code is based on the UnrealSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Dean Harmon.
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

$Id: SynHighlighterUnreal.pas,v 1.17.2.8 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Unreal syntax highlighter for SynEdit)
@author(Dean Harmon)
@created(2000)
@lastmod(2001-06-29)
}

unit SynEdit32.Highlighter.Unreal;

{$I SynEdit.Inc}

interface

uses
  Graphics, Windows, SysUtils, Classes,
  SynEdit32.Highlighter, SynEdit32.Types, SynEdit32.Unicode;

type
  TtkTokenKind = (
    tkComment,
    tkDirective,
    tkIdentifier,
    tkKey,
    tkKey2,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkString2,
    tkSymbol,
    tkUnknown);

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign,
    xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma,
    xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan,
    xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan,
    xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr,
    xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion,
    xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft,
    xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose,
    xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor,
    xtkXorAssign);

  TRangeState = (rsANil, rsAnsiC, rsDirective, rsDirectiveComment, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynEdit32HighlighterUnreal = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..732] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDirecAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FInvalidAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FKey2Attri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FString2Attri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAbstract(Index: Integer): TtkTokenKind;
    function FuncAlways(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncArraycount(Index: Integer): TtkTokenKind;
    function FuncAssert(Index: Integer): TtkTokenKind;
    function FuncAuto(Index: Integer): TtkTokenKind;
    function FuncAutomated(Index: Integer): TtkTokenKind;
    function FuncBool(Index: Integer): TtkTokenKind;
    function FuncBoundingbox(Index: Integer): TtkTokenKind;
    function FuncBoundingvolume(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncButton(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncCache(Index: Integer): TtkTokenKind;
    function FuncCacheexempt(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCatch(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncCoerce(Index: Integer): TtkTokenKind;
    function FuncCollapsecategories(Index: Integer): TtkTokenKind;
    function FuncColor(Index: Integer): TtkTokenKind;
    function FuncConfig(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncCoords(Index: Integer): TtkTokenKind;
    function FuncCpptext(Index: Integer): TtkTokenKind;
    function FuncCross(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDefaultproperties(Index: Integer): TtkTokenKind;
    function FuncDelegate(Index: Integer): TtkTokenKind;
    function FuncDelete(Index: Integer): TtkTokenKind;
    function FuncDependson(Index: Integer): TtkTokenKind;
    function FuncDeprecated(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDontcollapsecategories(Index: Integer): TtkTokenKind;
    function FuncDot(Index: Integer): TtkTokenKind;
    function FuncEach(Index: Integer): TtkTokenKind;
    function FuncEdfindable(Index: Integer): TtkTokenKind;
    function FuncEditconst(Index: Integer): TtkTokenKind;
    function FuncEditconstarray(Index: Integer): TtkTokenKind;
    function FuncEditinline(Index: Integer): TtkTokenKind;
    function FuncEditinlinenew(Index: Integer): TtkTokenKind;
    function FuncEditinlinenotify(Index: Integer): TtkTokenKind;
    function FuncEditinlineuse(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEnum(Index: Integer): TtkTokenKind;
    function FuncEnumcount(Index: Integer): TtkTokenKind;
    function FuncEvent(Index: Integer): TtkTokenKind;
    function FuncExec(Index: Integer): TtkTokenKind;
    function FuncExpands(Index: Integer): TtkTokenKind;
    function FuncExplicit(Index: Integer): TtkTokenKind;
    function FuncExport(Index: Integer): TtkTokenKind;
    function FuncExportstructs(Index: Integer): TtkTokenKind;
    function FuncExtends(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFinal(Index: Integer): TtkTokenKind;
    function FuncFloat(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForeach(Index: Integer): TtkTokenKind;
    function FuncFunction(Index: Integer): TtkTokenKind;
    function FuncGlobal(Index: Integer): TtkTokenKind;
    function FuncGlobalconfig(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncGuid(Index: Integer): TtkTokenKind;
    function FuncHidecategories(Index: Integer): TtkTokenKind;
    function FuncHidedropdown(Index: Integer): TtkTokenKind;
    function FuncHideparent(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncIgnores(Index: Integer): TtkTokenKind;
    function FuncImport(Index: Integer): TtkTokenKind;
    function FuncInit(Index: Integer): TtkTokenKind;
    function FuncInput(Index: Integer): TtkTokenKind;
    function FuncInsert(Index: Integer): TtkTokenKind;
    function FuncInstanced(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncIntrinsic(Index: Integer): TtkTokenKind;
    function FuncInvariant(Index: Integer): TtkTokenKind;
    function FuncIterator(Index: Integer): TtkTokenKind;
    function FuncLatent(Index: Integer): TtkTokenKind;
    function FuncLength(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLocalized(Index: Integer): TtkTokenKind;
    function FuncLong(Index: Integer): TtkTokenKind;
    function FuncMesh(Index: Integer): TtkTokenKind;
    function FuncModel(Index: Integer): TtkTokenKind;
    function FuncMutable(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNative(Index: Integer): TtkTokenKind;
    function FuncNativereplication(Index: Integer): TtkTokenKind;
    function FuncNew(Index: Integer): TtkTokenKind;
    function FuncNoexport(Index: Integer): TtkTokenKind;
    function FuncNone(Index: Integer): TtkTokenKind;
    function FuncNoteditinlinenew(Index: Integer): TtkTokenKind;
    function FuncNotplaceable(Index: Integer): TtkTokenKind;
    function FuncNousercreate(Index: Integer): TtkTokenKind;
    function FuncOperator(Index: Integer): TtkTokenKind;
    function FuncOptional(Index: Integer): TtkTokenKind;
    function FuncOut(Index: Integer): TtkTokenKind;
    function FuncParseconfig(Index: Integer): TtkTokenKind;
    function FuncPerobjectconfig(Index: Integer): TtkTokenKind;
    function FuncPlaceable(Index: Integer): TtkTokenKind;
    function FuncPlane(Index: Integer): TtkTokenKind;
    function FuncPointer(Index: Integer): TtkTokenKind;
    function FuncPostoperator(Index: Integer): TtkTokenKind;
    function FuncPreoperator(Index: Integer): TtkTokenKind;
    function FuncPrivate(Index: Integer): TtkTokenKind;
    function FuncProtected(Index: Integer): TtkTokenKind;
    function FuncRegister(Index: Integer): TtkTokenKind;
    function FuncReliable(Index: Integer): TtkTokenKind;
    function FuncRemove(Index: Integer): TtkTokenKind;
    function FuncReplication(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncRng(Index: Integer): TtkTokenKind;
    function FuncRot(Index: Integer): TtkTokenKind;
    function FuncRotator(Index: Integer): TtkTokenKind;
    function FuncSafereplace(Index: Integer): TtkTokenKind;
    function FuncScale(Index: Integer): TtkTokenKind;
    function FuncScriptconst(Index: Integer): TtkTokenKind;
    function FuncSelf(Index: Integer): TtkTokenKind;
    function FuncShowcategories(Index: Integer): TtkTokenKind;
    function FuncSimulated(Index: Integer): TtkTokenKind;
    function FuncSingular(Index: Integer): TtkTokenKind;
    function FuncSkip(Index: Integer): TtkTokenKind;
    function FuncSound(Index: Integer): TtkTokenKind;
    function FuncState(Index: Integer): TtkTokenKind;
    function FuncStatic(Index: Integer): TtkTokenKind;
    function FuncStop(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStruct(Index: Integer): TtkTokenKind;
    function FuncSuper(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncTexture(Index: Integer): TtkTokenKind;
    function FuncTransient(Index: Integer): TtkTokenKind;
    function FuncTravel(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncUnreliable(Index: Integer): TtkTokenKind;
    function FuncUntil(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function FuncVect(Index: Integer): TtkTokenKind;
    function FuncVector(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncWithin(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure DollarSignProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function GetExtTokenID: TxtkTokenKind;
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirecAttri: TSynEdit32HighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynEdit32HighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property Key2Attri: TSynEdit32HighlighterAttributes read FKey2Attri write FKey2Attri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SingleStringAttri: TSynEdit32HighlighterAttributes read FString2Attri
      write FString2Attri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  Registry, SynEdit32.StrConst;

const
  KeyWords: array[0..142] of UnicodeString = (
    'abstract', 'always', 'array', 'arraycount', 'assert', 'auto', 'automated', 
    'bool', 'boundingbox', 'boundingvolume', 'break', 'button', 'byte', 'cache', 
    'cacheexempt', 'case', 'catch', 'class', 'coerce', 'collapsecategories', 
    'color', 'config', 'const', 'continue', 'coords', 'cpptext', 'cross', 
    'default', 'defaultproperties', 'delegate', 'delete', 'dependson', 
    'deprecated', 'do', 'dontcollapsecategories', 'dot', 'each', 'edfindable', 
    'editconst', 'editconstarray', 'editinline', 'editinlinenew', 
    'editinlinenotify', 'editinlineuse', 'else', 'enum', 'enumcount', 'event', 
    'exec', 'expands', 'explicit', 'export', 'exportstructs', 'extends', 
    'false', 'final', 'float', 'for', 'foreach', 'function', 'global', 
    'globalconfig', 'goto', 'guid', 'hidecategories', 'hidedropdown', 
    'hideparent', 'if', 'ignores', 'import', 'init', 'input', 'insert', 
    'instanced', 'int', 'intrinsic', 'invariant', 'iterator', 'latent', 
    'length', 'local', 'localized', 'long', 'mesh', 'model', 'mutable', 'name', 
    'native', 'nativereplication', 'new', 'noexport', 'none', 
    'noteditinlinenew', 'notplaceable', 'nousercreate', 'operator', 'optional', 
    'out', 'parseconfig', 'perobjectconfig', 'placeable', 'plane', 'pointer', 
    'postoperator', 'preoperator', 'private', 'protected', 'register', 
    'reliable', 'remove', 'replication', 'return', 'rng', 'rot', 'rotator', 
    'safereplace', 'scale', 'scriptconst', 'self', 'showcategories', 
    'simulated', 'singular', 'skip', 'sound', 'state', 'static', 'stop', 
    'string', 'struct', 'super', 'switch', 'texture', 'transient', 'travel', 
    'true', 'unreliable', 'until', 'var', 'vect', 'vector', 'void', 'while', 
    'within' 
  );

  KeyIndices: array[0..732] of Integer = (
    -1, -1, -1, -1, -1, -1, 78, -1, -1, -1, -1, 25, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 79, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 34, -1, -1, -1, 18, -1, -1, -1, -1, -1, 30, 1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, 114, 
    -1, -1, 121, -1, -1, -1, -1, -1, 105, -1, -1, 108, -1, 135, 9, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 117, 33, 109, -1, -1, -1, -1, -1, -1, 90, -1, -1, 
    -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, 19, -1, 
    -1, -1, -1, 81, -1, 82, -1, -1, -1, -1, 40, 15, -1, -1, -1, 52, -1, 80, -1, 
    -1, -1, -1, -1, -1, 136, -1, -1, 61, -1, 113, -1, -1, -1, 83, -1, -1, -1, 
    -1, -1, -1, 27, -1, -1, 133, -1, -1, -1, -1, 62, -1, -1, -1, -1, -1, -1, -1, 
    76, -1, -1, -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, 
    51, -1, -1, -1, -1, 44, -1, 22, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 20, -1, -1, -1, 8, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 96, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, -1, 
    -1, -1, -1, -1, -1, 39, 24, -1, -1, -1, -1, 54, -1, 4, 123, -1, -1, -1, -1, 
    -1, -1, 50, 141, -1, -1, -1, -1, -1, -1, -1, 87, -1, -1, 21, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, 85, -1, 
    -1, -1, -1, -1, 70, -1, 68, 131, -1, -1, 69, -1, -1, -1, -1, -1, 128, 26, 
    -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, 142, -1, -1, 122, -1, 74, -1, -1, 
    -1, -1, -1, -1, -1, 13, -1, -1, -1, -1, 101, 119, -1, -1, 94, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, 89, -1, -1, 0, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 29, -1, -1, 92, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1, 67, -1, -1, 45, -1, 
    116, -1, -1, 132, 28, -1, -1, -1, 31, -1, -1, -1, 77, -1, -1, -1, -1, -1, 
    91, -1, 37, -1, -1, -1, -1, 35, -1, 6, -1, -1, -1, -1, -1, -1, -1, 97, -1, 
    -1, -1, -1, -1, 53, -1, 84, -1, -1, -1, -1, 56, 14, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 23, -1, 107, -1, -1, -1, -1, 98, -1, -1, 75, -1, -1, -1, -1, 
    -1, 88, -1, -1, 103, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, 59, 
    139, 11, 42, -1, -1, 95, -1, -1, -1, -1, -1, 3, -1, -1, -1, 38, -1, -1, -1, 
    -1, -1, -1, -1, -1, 16, -1, 46, -1, -1, -1, -1, -1, 102, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, 41, -1, -1, -1, 
    -1, -1, -1, -1, -1, 48, 64, -1, -1, -1, -1, 86, -1, 58, 43, 72, -1, -1, 66, 
    137, 71, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, -1, -1, -1, -1, 17, 130, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 120, -1, 73, -1, -1, 118, -1, -1, -1, 
    -1, -1, -1, 138, -1, -1, -1, 55, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 10, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, 115, -1, 
    -1, -1, -1, 32, 47, 49, -1, -1, -1, -1, -1, -1, -1, 57, -1, -1, -1, -1, -1, 
    -1, 125, 134, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 99, 12, -1, 127, 
    140, -1, -1 
  );

{$Q-}
function TSynEdit32HighlighterUnreal.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 41 + Ord(Str^) * 701;
    Inc(Str);
  end;
  Result := Result mod 733;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterUnreal.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterUnreal.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[410] := FuncAbstract;
  FIdentFuncTable[71] := FuncAlways;
  FIdentFuncTable[219] := FuncArray;
  FIdentFuncTable[554] := FuncArraycount;
  FIdentFuncTable[294] := FuncAssert;
  FIdentFuncTable[681] := FuncAuto;
  FIdentFuncTable[477] := FuncAutomated;
  FIdentFuncTable[364] := FuncBool;
  FIdentFuncTable[249] := FuncBoundingbox;
  FIdentFuncTable[109] := FuncBoundingvolume;
  FIdentFuncTable[675] := FuncBreak;
  FIdentFuncTable[544] := FuncButton;
  FIdentFuncTable[727] := FuncByte;
  FIdentFuncTable[380] := FuncCache;
  FIdentFuncTable[499] := FuncCacheexempt;
  FIdentFuncTable[160] := FuncCase;
  FIdentFuncTable[567] := FuncCatch;
  FIdentFuncTable[635] := FuncClass;
  FIdentFuncTable[64] := FuncCoerce;
  FIdentFuncTable[147] := FuncCollapsecategories;
  FIdentFuncTable[245] := FuncColor;
  FIdentFuncTable[314] := FuncConfig;
  FIdentFuncTable[231] := FuncConst;
  FIdentFuncTable[510] := FuncContinue;
  FIdentFuncTable[287] := FuncCoords;
  FIdentFuncTable[11] := FuncCpptext;
  FIdentFuncTable[355] := FuncCross;
  FIdentFuncTable[189] := FuncDefault;
  FIdentFuncTable[454] := FuncDefaultproperties;
  FIdentFuncTable[425] := FuncDelegate;
  FIdentFuncTable[70] := FuncDelete;
  FIdentFuncTable[458] := FuncDependson;
  FIdentFuncTable[696] := FuncDeprecated;
  FIdentFuncTable[120] := FuncDo;
  FIdentFuncTable[60] := FuncDontcollapsecategories;
  FIdentFuncTable[475] := FuncDot;
  FIdentFuncTable[49] := FuncEach;
  FIdentFuncTable[470] := FuncEdfindable;
  FIdentFuncTable[558] := FuncEditconst;
  FIdentFuncTable[286] := FuncEditconstarray;
  FIdentFuncTable[159] := FuncEditinline;
  FIdentFuncTable[596] := FuncEditinlinenew;
  FIdentFuncTable[545] := FuncEditinlinenotify;
  FIdentFuncTable[614] := FuncEditinlineuse;
  FIdentFuncTable[229] := FuncElse;
  FIdentFuncTable[448] := FuncEnum;
  FIdentFuncTable[569] := FuncEnumcount;
  FIdentFuncTable[697] := FuncEvent;
  FIdentFuncTable[605] := FuncExec;
  FIdentFuncTable[698] := FuncExpands;
  FIdentFuncTable[302] := FuncExplicit;
  FIdentFuncTable[224] := FuncExport;
  FIdentFuncTable[164] := FuncExportstructs;
  FIdentFuncTable[491] := FuncExtends;
  FIdentFuncTable[292] := FuncFalse;
  FIdentFuncTable[662] := FuncFinal;
  FIdentFuncTable[498] := FuncFloat;
  FIdentFuncTable[706] := FuncFor;
  FIdentFuncTable[613] := FuncForeach;
  FIdentFuncTable[542] := FuncFunction;
  FIdentFuncTable[330] := FuncGlobal;
  FIdentFuncTable[176] := FuncGlobalconfig;
  FIdentFuncTable[197] := FuncGoto;
  FIdentFuncTable[89] := FuncGuid;
  FIdentFuncTable[606] := FuncHidecategories;
  FIdentFuncTable[278] := FuncHidedropdown;
  FIdentFuncTable[618] := FuncHideparent;
  FIdentFuncTable[445] := FuncIf;
  FIdentFuncTable[344] := FuncIgnores;
  FIdentFuncTable[348] := FuncImport;
  FIdentFuncTable[342] := FuncInit;
  FIdentFuncTable[620] := FuncInput;
  FIdentFuncTable[615] := FuncInsert;
  FIdentFuncTable[648] := FuncInstanced;
  FIdentFuncTable[372] := FuncInt;
  FIdentFuncTable[520] := FuncIntrinsic;
  FIdentFuncTable[205] := FuncInvariant;
  FIdentFuncTable[462] := FuncIterator;
  FIdentFuncTable[6] := FuncLatent;
  FIdentFuncTable[24] := FuncLength;
  FIdentFuncTable[166] := FuncLocal;
  FIdentFuncTable[152] := FuncLocalized;
  FIdentFuncTable[154] := FuncLong;
  FIdentFuncTable[182] := FuncMesh;
  FIdentFuncTable[493] := FuncModel;
  FIdentFuncTable[336] := FuncMutable;
  FIdentFuncTable[611] := FuncName;
  FIdentFuncTable[311] := FuncNative;
  FIdentFuncTable[526] := FuncNativereplication;
  FIdentFuncTable[407] := FuncNew;
  FIdentFuncTable[128] := FuncNoexport;
  FIdentFuncTable[468] := FuncNone;
  FIdentFuncTable[428] := FuncNoteditinlinenew;
  FIdentFuncTable[532] := FuncNotplaceable;
  FIdentFuncTable[389] := FuncNousercreate;
  FIdentFuncTable[548] := FuncOperator;
  FIdentFuncTable[265] := FuncOptional;
  FIdentFuncTable[485] := FuncOut;
  FIdentFuncTable[517] := FuncParseconfig;
  FIdentFuncTable[726] := FuncPerobjectconfig;
  FIdentFuncTable[401] := FuncPlaceable;
  FIdentFuncTable[385] := FuncPlane;
  FIdentFuncTable[575] := FuncPointer;
  FIdentFuncTable[529] := FuncPostoperator;
  FIdentFuncTable[33] := FuncPreoperator;
  FIdentFuncTable[103] := FuncPrivate;
  FIdentFuncTable[134] := FuncProtected;
  FIdentFuncTable[512] := FuncRegister;
  FIdentFuncTable[106] := FuncReliable;
  FIdentFuncTable[121] := FuncRemove;
  FIdentFuncTable[253] := FuncReplication;
  FIdentFuncTable[593] := FuncReturn;
  FIdentFuncTable[440] := FuncRng;
  FIdentFuncTable[178] := FuncRot;
  FIdentFuncTable[94] := FuncRotator;
  FIdentFuncTable[691] := FuncSafereplace;
  FIdentFuncTable[450] := FuncScale;
  FIdentFuncTable[119] := FuncScriptconst;
  FIdentFuncTable[651] := FuncSelf;
  FIdentFuncTable[386] := FuncShowcategories;
  FIdentFuncTable[646] := FuncSimulated;
  FIdentFuncTable[97] := FuncSingular;
  FIdentFuncTable[370] := FuncSkip;
  FIdentFuncTable[295] := FuncSound;
  FIdentFuncTable[142] := FuncState;
  FIdentFuncTable[713] := FuncStatic;
  FIdentFuncTable[213] := FuncStop;
  FIdentFuncTable[729] := FuncString;
  FIdentFuncTable[354] := FuncStruct;
  FIdentFuncTable[626] := FuncSuper;
  FIdentFuncTable[636] := FuncSwitch;
  FIdentFuncTable[345] := FuncTexture;
  FIdentFuncTable[453] := FuncTransient;
  FIdentFuncTable[192] := FuncTravel;
  FIdentFuncTable[714] := FuncTrue;
  FIdentFuncTable[108] := FuncUnreliable;
  FIdentFuncTable[173] := FuncUntil;
  FIdentFuncTable[619] := FuncVar;
  FIdentFuncTable[658] := FuncVect;
  FIdentFuncTable[543] := FuncVector;
  FIdentFuncTable[730] := FuncVoid;
  FIdentFuncTable[303] := FuncWhile;
  FIdentFuncTable[367] := FuncWithin;
end;

function TSynEdit32HighlighterUnreal.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncAbstract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncAlways(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncArraycount(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncAssert(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncAuto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncAutomated(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncBool(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncBoundingbox(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncBoundingvolume(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncButton(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCache(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCacheexempt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCoerce(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCollapsecategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncColor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncConfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCoords(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncCpptext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;

end;

function TSynEdit32HighlighterUnreal.FuncCross(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSymbol
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDefaultproperties(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDelegate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDependson(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDeprecated(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDontcollapsecategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncDot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSymbol
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEach(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEdfindable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEditconst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEditconstarray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEditinline(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEditinlinenew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEditinlinenotify(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEditinlineuse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEnum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEnumcount(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncEvent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncExec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncExpands(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncExplicit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncExport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncExportstructs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncExtends(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncFinal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncFloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncForeach(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncFunction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncGlobal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncGlobalconfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncGuid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncHidecategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncHidedropdown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncHideparent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncIgnores(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncImport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncInit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncInput(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncInsert(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncInstanced(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncIntrinsic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncInvariant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncIterator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncLatent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncLength(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncLocalized(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncLong(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncMesh(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncModel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncMutable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncName(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNative(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNativereplication(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNoexport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNone(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNoteditinlinenew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNotplaceable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncNousercreate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncOperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncOptional(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncOut(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncParseconfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncPerobjectconfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncPlaceable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncPlane(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncPointer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncPostoperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncPreoperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncPrivate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncProtected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncRegister(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncReliable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncRemove(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncReplication(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncRng(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncRot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncRotator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncSafereplace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncScale(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncScriptconst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncSelf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncShowcategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncSimulated(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;

end;

function TSynEdit32HighlighterUnreal.FuncSingular(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncSkip(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncSound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncState(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncStatic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncStop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncStruct(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncSuper(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncTexture(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncTransient(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncTravel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncUnreliable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncUntil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncVect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncVector(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterUnreal.FuncWithin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterUnreal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FInvalidAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FInvalidAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FKey2Attri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  FKey2Attri.Style:= [fsBold];
  AddAttribute(FKey2Attri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FString2Attri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSingleString, SYNS_FriendlyAttrSingleString);
  AddAttribute(FString2Attri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FDirecAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  AddAttribute(FDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  fDefaultFilter := SYNS_FilterCPP;
end; { Create }

procedure TSynEdit32HighlighterUnreal.AnsiCProc;
begin
  FTokenID := tkComment;
  case FLine[FRun] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(FRun) do
    case FLine[FRun] of
      '*':
        if FLine[FRun + 1] = '/' then
        begin
          Inc(FRun, 2);
          if FRange = rsDirectiveComment then
            FRange := rsDirective
          else
            FRange := rsUnKnown;
          Break;
        end else
          Inc(FRun);
      #10: Break;
      #13: Break;
    else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterUnreal.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {and assign}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {logical and}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkLogAnd;
      end;
  else                                 {and}
    begin
      Inc(FRun);
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.AsciiCharProc;
begin
  FTokenID := tkString2;
  repeat
    if IsLineEnd(FRun) then Break;
    if FLine[FRun] = #92 then                             {backslash}
        {if we have an escaped single quote it doesn't count}
      if FLine[FRun + 1] = #39 then Inc(FRun);
    Inc(FRun);
  until FLine[FRun] = #39;
  if not IsLineEnd(FRun) then Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.BraceCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynEdit32HighlighterUnreal.BraceOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynEdit32HighlighterUnreal.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun + 1] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.ColonProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    ':':                               {scope resolution operator}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkScopeResolution;
      end;
  else                                 {colon}
    begin
      Inc(FRun);
      FExtTokenID := xtkColon;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynEdit32HighlighterUnreal.DirectiveProc;
begin
  if IsLineEnd(FRun) then
  begin
    if (FRun <= 0) then
      FRange := rsUnknown;
    NextProcedure;
  end
  else
  begin
    FTokenID := tkDirective;
    while TRUE do
      case FLine[FRun] of
        '/': // comment?
          begin
            if FLine[FRun + 1] = '/' then // is end of directive as well
              Break
            else if FLine[FRun + 1] = '*' then
            begin // might be embedded only
              FRange := rsDirectiveComment;
              Break;
            end else
              Inc(FRun);
          end;
        #0, #10, #13:
          begin
            FRange := rsUnknown;
            Break;
          end;
        else Inc(FRun);
      end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.EqualProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {logical equal}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      Inc(FRun);
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.GreaterProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[FRun + 2] = '=' then   {shift right assign}
        begin
          Inc(FRun, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else                           {shift right}
        begin
          Inc(FRun, 2);
          FExtTokenID := xtkShiftRight;
        end;
      end;
  else                                 {greater than}
    begin
      Inc(FRun);
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.LowerProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[FRun + 2] = '=' then   {shift left assign}
        begin
          Inc(FRun, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          Inc(FRun, 2);
          FExtTokenID := xtkShiftLeft;
        end;
      end;
  else                                 {less than}
    begin
      Inc(FRun);
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.MinusProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {subtract assign}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkDecrement;
      end;
    '>':                               {arrow}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkArrow;
      end;
  else                                 {subtract}
    begin
      Inc(FRun);
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.ModSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {mod assign}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkModAssign;
      end;
  else                                 {mod}
    begin
      Inc(FRun);
      FExtTokenID := xtkMod;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.NotSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {not equal}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {not}
    begin
      Inc(FRun);
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X':
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
      '.':
        if FLine[FRun + 1] = '.' then Break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterUnreal.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {or assign}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {logical or}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkLogOr;
      end;
  else                                 {or}
    begin
      Inc(FRun);
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.PlusProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {add assign}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      Inc(FRun);
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.PointProc;
begin
  FTokenID := tkSymbol;
  if (FLine[FRun + 1] = '.') and (FLine[FRun + 2] = '.') then
    begin                              {ellipse}
      Inc(FRun, 3);
      FExtTokenID := xtkEllipse;
    end
  else                                 {point}
    begin
      Inc(FRun);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynEdit32HighlighterUnreal.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  dec(FRoundCount);
end;

procedure TSynEdit32HighlighterUnreal.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  Inc(FRoundCount);
end;

procedure TSynEdit32HighlighterUnreal.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynEdit32HighlighterUnreal.SlashProc;
begin
  case FLine[FRun + 1] of
    '/':                               {c++ style comments}
      begin
        FTokenID := tkComment;
        Inc(FRun, 2);
       while not IsLineEnd(FRun) do Inc(FRun);
      end;
    '*':                               {c style comments}
      begin
        FTokenID := tkComment;
        if FRange <> rsDirectiveComment then
          FRange := rsAnsiC;
        Inc(FRun, 2);
        while not IsLineEnd(FRun) do
          case FLine[FRun] of
            '*':
              if FLine[FRun + 1] = '/' then
              begin
                Inc(FRun, 2);
                if FRange = rsDirectiveComment then
                  FRange := rsDirective
                else
                  FRange := rsUnKnown;
                Break;
              end else Inc(FRun);
            #10, #13:
              begin
                if FRange = rsDirectiveComment then
                  FRange := rsAnsiC;
                Break;
              end;
          else Inc(FRun);
          end;
      end;
    '=':                               {divide assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {divide}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.SquareCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  dec(FSquareCount);
end;

procedure TSynEdit32HighlighterUnreal.SquareOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  Inc(FSquareCount);
end;

procedure TSynEdit32HighlighterUnreal.StarProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
    '=':                               {multiply assign}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {star}
    begin
      Inc(FRun);
      FExtTokenID := xtkStar;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34) then Inc(FRun, 2);
  repeat
    if IsLineEnd(FRun) then Break;
    if FLine[FRun] = #92 then                             {backslash}
        case FLine[FRun + 1] of
          #10: Inc(FRun);               {line continuation character}
          #34: Inc(FRun);               {escaped quote doesn't count}
          #92: Inc(FRun);
        end;
    Inc(FRun);
  until FLine[FRun] = #34;
  if not IsLineEnd(FRun) then Inc(FRun);
end;

procedure TSynEdit32HighlighterUnreal.DollarSignProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
end;


procedure TSynEdit32HighlighterUnreal.TildeProc;
begin
  Inc(FRun);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynEdit32HighlighterUnreal.XOrSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[FRun + 1] of
  	'=':                               {xor assign}
      begin
        Inc(FRun, 2);
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      Inc(FRun);
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynEdit32HighlighterUnreal.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterUnreal.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsAnsiC, rsDirectiveComment: AnsiCProc;
    rsDirective: DirectiveProc;
  else
    begin
      FRange := rsUnknown;
      NextProcedure
    end;
  end;
  inherited;
end;

procedure TSynEdit32HighlighterUnreal.NextProcedure;
begin
  case FLine[FRun] of
    '&': AndSymbolProc;
    #39: AsciiCharProc;
    '}': BraceCloseProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '#': DirectiveProc;
    '=': EqualProc;
    '>': GreaterProc;
    '?': QuestionProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '%': ModSymbolProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '.': PointProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    '*': StarProc;
    #34: StringProc;
    '$', '@': DollarSignProc;
    '~': TildeProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
end;

function TSynEdit32HighlighterUnreal.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterUnreal.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterUnreal.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterUnreal.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;


function TSynEdit32HighlighterUnreal.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPP;
end; { IsFilterStored }


function TSynEdit32HighlighterUnreal.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkKey2: Result := FKey2Attri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkString2: Result := FString2Attri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FInvalidAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterUnreal.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynEdit32HighlighterUnreal.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynEdit32HighlighterUnreal.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterUnreal.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  with TRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Borland\C++Builder') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TSynEdit32HighlighterUnreal.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

{$IFNDEF SYN_CLX}
  function ReadCPPBSettings(settingIndex: integer): boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): boolean;

      function ReadCPPB1(settingTag: string; attri: TSynEdit32HighlighterAttributes; name: string): boolean;
      var
        i: integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
             '\SOFTWARE\Borland\C++Builder\'+settingTag+'\Highlight',name,true);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                 '\Software\Borland\C++Builder\'+settingTag+'\Editor\Highlight',
                 key,false);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1')
          then Result := ReadCPPB1(settingTag,attri,key)
          else Result := ReadCPPB3OrMore(settingTag,attri,key);
      except Result := false; end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynEdit32HighlighterAttributes;
    tmpNumberAttri    : TSynEdit32HighlighterAttributes;
    tmpKeyAttri       : TSynEdit32HighlighterAttributes;
    tmpSymbolAttri    : TSynEdit32HighlighterAttributes;
    tmpCommentAttri   : TSynEdit32HighlighterAttributes;
    tmpIdentifierAttri: TSynEdit32HighlighterAttributes;
    tmpInvalidAttri   : TSynEdit32HighlighterAttributes;
    tmpSpaceAttri     : TSynEdit32HighlighterAttributes;
    tmpDirecAttri     : TSynEdit32HighlighterAttributes;
    sl                 : TStringList;

  begin { ReadCPPBSettings }
    sl := TStringList.Create;
    try
      EnumUserSettings(sl);
      if settingIndex >= sl.Count then Result := false
      else begin
        tmpStringAttri    := TSynEdit32HighlighterAttributes.Create('', '');
        tmpNumberAttri    := TSynEdit32HighlighterAttributes.Create('', '');
        tmpKeyAttri       := TSynEdit32HighlighterAttributes.Create('', '');
        tmpSymbolAttri    := TSynEdit32HighlighterAttributes.Create('', '');
        tmpCommentAttri   := TSynEdit32HighlighterAttributes.Create('', '');
        tmpIdentifierAttri:= TSynEdit32HighlighterAttributes.Create('', '');
        tmpInvalidAttri   := TSynEdit32HighlighterAttributes.Create('', '');
        tmpSpaceAttri     := TSynEdit32HighlighterAttributes.Create('', '');
        tmpDirecAttri     := TSynEdit32HighlighterAttributes.Create('', '');
        tmpStringAttri    .Assign(FStringAttri);
        tmpNumberAttri    .Assign(FNumberAttri);
        tmpKeyAttri       .Assign(FKeyAttri);
        tmpSymbolAttri    .Assign(FSymbolAttri);
        tmpCommentAttri   .Assign(FCommentAttri);
        tmpIdentifierAttri.Assign(FIdentifierAttri);
        tmpInvalidAttri   .Assign(FInvalidAttri);
        tmpSpaceAttri     .Assign(FSpaceAttri);
        tmpDirecAttri     .Assign(FDirecAttri);
        Result := ReadCPPBSetting(sl[settingIndex],FCommentAttri,'Comment')       and
                  ReadCPPBSetting(sl[settingIndex],FIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(sl[settingIndex],FInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(sl[settingIndex],FKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(sl[settingIndex],FNumberAttri,'Integer')        and
                  ReadCPPBSetting(sl[settingIndex],FSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(sl[settingIndex],FStringAttri,'String')         and
                  ReadCPPBSetting(sl[settingIndex],FSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(sl[settingIndex],FDirecAttri,'Preprocessor');
        if not Result then begin
          FStringAttri    .Assign(tmpStringAttri);
          FString2Attri   .Assign(tmpStringAttri);
          FNumberAttri    .Assign(tmpNumberAttri);
          FKeyAttri       .Assign(tmpKeyAttri);
          FKey2Attri      .Assign(tmpKeyAttri);
          FSymbolAttri    .Assign(tmpSymbolAttri);
          FCommentAttri   .Assign(tmpCommentAttri);
          FIdentifierAttri.Assign(tmpIdentifierAttri);
          FInvalidAttri.Assign(tmpInvalidAttri);
          FSpaceAttri     .Assign(tmpSpaceAttri);
          FDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally
      sl.Free;
    end;
  end; { ReadCPPBSettings }
{$ENDIF}

begin
  Result := ReadCPPBSettings(settingIndex);
end; { TSynEdit32HighlighterUnreal.UseUserSettings }

class function TSynEdit32HighlighterUnreal.GetLanguageName: string;
begin
  Result := SYNS_LangUnreal;
end;

class function TSynEdit32HighlighterUnreal.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynEdit32HighlighterUnreal.GetSampleSource: UnicodeString;
begin
  Result := '//----Comment-----------------------------------------------------------'#13#10+
            'class TestObject expands Object native;'#13#10+
            #13#10+
            '#exec MESH    IMPORT     MESH=Something ANIVFILE=MODELS\Something.3D DATAFILE=MODELS\Something.3D X=0 Y=0 Z=0 MLOD=0'#13#10+
            #13#10+
            'var() Sound HitSound;'#13#10+
            'function Cast()'#13#10+
            '{'#13#10+
            '  Super.Cast();'#13#10+
            '  CastTime = 50;'#13#10+
            '  GatherEffect = Spawn( class''SomethingCorona'',,, GetStartLoc(), Pawn(Owner).ViewRotation );'#13#10+
            '  GatherEffect.SetFollowPawn( Pawn(Owner) );'#13#10+
            '}'#13#10+
            #13#10+
            'defaultproperties'#13#10+
            '{'#13#10+
            '  PickupMessage="You have picked up a thing."'#13#10+
            '}';
end;

class function TSynEdit32HighlighterUnreal.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangUnreal;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterUnreal);
end.
