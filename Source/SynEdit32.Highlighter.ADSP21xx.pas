{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterADSP21xx.pas, released 2000-04-17.
The Original Code is based on the wbADSP21xxSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Wynand Breytenbach.
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

$Id: SynHighlighterADSP21xx.pas,v 1.16.2.7 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a ADSP21xx highlighter for SynEdit)
@author(Wynand Breytenbach, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterADSP21xx unit provides a ADSP21xx DSP assembler highlighter for SynEdit.
}

unit SynEdit32.Highlighter.ADSP21xx;

{$I SynEdit32.Inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkCondition, tkIdentifier, tkKey, tkNull, tkNumber,
    tkRegister, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnKnown, rsPascalComment, rsCComment, rsHexNumber,
    rsBinaryNumber, rsInclude);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynEdit32HighlighterADSP21xx = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FIdentFuncTable: array[0..820] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FRegisterAttri: TSynEdit32HighlighterAttributes;
    FConditionAttri: TSynEdit32HighlighterAttributes;
    FNullAttri: TSynEdit32HighlighterAttributes;
    FUnknownAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAbs(Index: Integer): TtkTokenKind;
    function FuncAbstract(Index: Integer): TtkTokenKind;
    function FuncAc(Index: Integer): TtkTokenKind;
    function FuncAf(Index: Integer): TtkTokenKind;
    function FuncAlt95reg(Index: Integer): TtkTokenKind;
    function FuncAnd(Index: Integer): TtkTokenKind;
    function FuncAr(Index: Integer): TtkTokenKind;
    function FuncAr95sat(Index: Integer): TtkTokenKind;
    function FuncAshift(Index: Integer): TtkTokenKind;
    function FuncAstat(Index: Integer): TtkTokenKind;
    function FuncAux(Index: Integer): TtkTokenKind;
    function FuncAv(Index: Integer): TtkTokenKind;
    function FuncAv95latch(Index: Integer): TtkTokenKind;
    function FuncAx0(Index: Integer): TtkTokenKind;
    function FuncAx1(Index: Integer): TtkTokenKind;
    function FuncAy0(Index: Integer): TtkTokenKind;
    function FuncAy1(Index: Integer): TtkTokenKind;
    function FuncB(Index: Integer): TtkTokenKind;
    function FuncBit95rev(Index: Integer): TtkTokenKind;
    function FuncBm(Index: Integer): TtkTokenKind;
    function FuncBoot(Index: Integer): TtkTokenKind;
    function FuncBy(Index: Integer): TtkTokenKind;
    function FuncCache(Index: Integer): TtkTokenKind;
    function FuncCall(Index: Integer): TtkTokenKind;
    function FuncCe(Index: Integer): TtkTokenKind;
    function FuncCirc(Index: Integer): TtkTokenKind;
    function FuncClear(Index: Integer): TtkTokenKind;
    function FuncClr(Index: Integer): TtkTokenKind;
    function FuncClrbit(Index: Integer): TtkTokenKind;
    function FuncCntl(Index: Integer): TtkTokenKind;
    function FuncCntr(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncDefine(Index: Integer): TtkTokenKind;
    function FuncDis(Index: Integer): TtkTokenKind;
    function FuncDivq(Index: Integer): TtkTokenKind;
    function FuncDivs(Index: Integer): TtkTokenKind;
    function FuncDm(Index: Integer): TtkTokenKind;
    function FuncDmovlay(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEmode(Index: Integer): TtkTokenKind;
    function FuncEna(Index: Integer): TtkTokenKind;
    function FuncEndif(Index: Integer): TtkTokenKind;
    function FuncEndmacro(Index: Integer): TtkTokenKind;
    function FuncEndmod(Index: Integer): TtkTokenKind;
    function FuncEntry(Index: Integer): TtkTokenKind;
    function FuncEq(Index: Integer): TtkTokenKind;
    function FuncExp(Index: Integer): TtkTokenKind;
    function FuncExpadj(Index: Integer): TtkTokenKind;
    function FuncExternal(Index: Integer): TtkTokenKind;
    function FuncFl0(Index: Integer): TtkTokenKind;
    function FuncFl1(Index: Integer): TtkTokenKind;
    function FuncFl2(Index: Integer): TtkTokenKind;
    function FuncFlag95in(Index: Integer): TtkTokenKind;
    function FuncFlag95out(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForever(Index: Integer): TtkTokenKind;
    function FuncGe(Index: Integer): TtkTokenKind;
    function FuncGlobal(Index: Integer): TtkTokenKind;
    function FuncGo95mode(Index: Integer): TtkTokenKind;
    function FuncGt(Index: Integer): TtkTokenKind;
    function FuncH(Index: Integer): TtkTokenKind;
    function FuncHi(Index: Integer): TtkTokenKind;
    function FuncI0(Index: Integer): TtkTokenKind;
    function FuncI1(Index: Integer): TtkTokenKind;
    function FuncI2(Index: Integer): TtkTokenKind;
    function FuncI3(Index: Integer): TtkTokenKind;
    function FuncI4(Index: Integer): TtkTokenKind;
    function FuncI5(Index: Integer): TtkTokenKind;
    function FuncI6(Index: Integer): TtkTokenKind;
    function FuncI7(Index: Integer): TtkTokenKind;
    function FuncIcntl(Index: Integer): TtkTokenKind;
    function FuncIdle(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncIfc(Index: Integer): TtkTokenKind;
    function FuncIfdef(Index: Integer): TtkTokenKind;
    function FuncIfndef(Index: Integer): TtkTokenKind;
    function FuncImask(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncInclude(Index: Integer): TtkTokenKind;
    function FuncInit(Index: Integer): TtkTokenKind;
    function FuncIo(Index: Integer): TtkTokenKind;
    function FuncJump(Index: Integer): TtkTokenKind;
    function FuncL0(Index: Integer): TtkTokenKind;
    function FuncL1(Index: Integer): TtkTokenKind;
    function FuncL2(Index: Integer): TtkTokenKind;
    function FuncL3(Index: Integer): TtkTokenKind;
    function FuncL4(Index: Integer): TtkTokenKind;
    function FuncL5(Index: Integer): TtkTokenKind;
    function FuncL6(Index: Integer): TtkTokenKind;
    function FuncL7(Index: Integer): TtkTokenKind;
    function FuncLe(Index: Integer): TtkTokenKind;
    function FuncLo(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLoop(Index: Integer): TtkTokenKind;
    function FuncLshift(Index: Integer): TtkTokenKind;
    function FuncLt(Index: Integer): TtkTokenKind;
    function FuncM95mode(Index: Integer): TtkTokenKind;
    function FuncM0(Index: Integer): TtkTokenKind;
    function FuncM1(Index: Integer): TtkTokenKind;
    function FuncM2(Index: Integer): TtkTokenKind;
    function FuncM3(Index: Integer): TtkTokenKind;
    function FuncM4(Index: Integer): TtkTokenKind;
    function FuncM5(Index: Integer): TtkTokenKind;
    function FuncM6(Index: Integer): TtkTokenKind;
    function FuncM7(Index: Integer): TtkTokenKind;
    function FuncMacro(Index: Integer): TtkTokenKind;
    function FuncMf(Index: Integer): TtkTokenKind;
    function FuncModify(Index: Integer): TtkTokenKind;
    function FuncModule(Index: Integer): TtkTokenKind;
    function FuncMr(Index: Integer): TtkTokenKind;
    function FuncMr0(Index: Integer): TtkTokenKind;
    function FuncMr1(Index: Integer): TtkTokenKind;
    function FuncMr2(Index: Integer): TtkTokenKind;
    function FuncMstat(Index: Integer): TtkTokenKind;
    function FuncMv(Index: Integer): TtkTokenKind;
    function FuncMx0(Index: Integer): TtkTokenKind;
    function FuncMx1(Index: Integer): TtkTokenKind;
    function FuncMy0(Index: Integer): TtkTokenKind;
    function FuncMy1(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNe(Index: Integer): TtkTokenKind;
    function FuncNeg(Index: Integer): TtkTokenKind;
    function FuncNewpage(Index: Integer): TtkTokenKind;
    function FuncNop(Index: Integer): TtkTokenKind;
    function FuncNorm(Index: Integer): TtkTokenKind;
    function FuncNot(Index: Integer): TtkTokenKind;
    function FuncOf(Index: Integer): TtkTokenKind;
    function FuncOr(Index: Integer): TtkTokenKind;
    function FuncPass(Index: Integer): TtkTokenKind;
    function FuncPc(Index: Integer): TtkTokenKind;
    function FuncPm(Index: Integer): TtkTokenKind;
    function FuncPop(Index: Integer): TtkTokenKind;
    function FuncPort(Index: Integer): TtkTokenKind;
    function FuncPush(Index: Integer): TtkTokenKind;
    function FuncRam(Index: Integer): TtkTokenKind;
    function FuncRegbank(Index: Integer): TtkTokenKind;
    function FuncReset(Index: Integer): TtkTokenKind;
    function FuncRnd(Index: Integer): TtkTokenKind;
    function FuncRom(Index: Integer): TtkTokenKind;
    function FuncRti(Index: Integer): TtkTokenKind;
    function FuncRts(Index: Integer): TtkTokenKind;
    function FuncRx0(Index: Integer): TtkTokenKind;
    function FuncRx1(Index: Integer): TtkTokenKind;
    function FuncSat(Index: Integer): TtkTokenKind;
    function FuncSb(Index: Integer): TtkTokenKind;
    function FuncSec95reg(Index: Integer): TtkTokenKind;
    function FuncSeg(Index: Integer): TtkTokenKind;
    function FuncSegment(Index: Integer): TtkTokenKind;
    function FuncSet(Index: Integer): TtkTokenKind;
    function FuncSetbit(Index: Integer): TtkTokenKind;
    function FuncShift(Index: Integer): TtkTokenKind;
    function FuncShl(Index: Integer): TtkTokenKind;
    function FuncShr(Index: Integer): TtkTokenKind;
    function FuncSi(Index: Integer): TtkTokenKind;
    function FuncSr(Index: Integer): TtkTokenKind;
    function FuncSr0(Index: Integer): TtkTokenKind;
    function FuncSr1(Index: Integer): TtkTokenKind;
    function FuncSs(Index: Integer): TtkTokenKind;
    function FuncSstat(Index: Integer): TtkTokenKind;
    function FuncStatic(Index: Integer): TtkTokenKind;
    function FuncSts(Index: Integer): TtkTokenKind;
    function FuncSu(Index: Integer): TtkTokenKind;
    function FuncTest(Index: Integer): TtkTokenKind;
    function FuncTestbit(Index: Integer): TtkTokenKind;
    function FuncTglbit(Index: Integer): TtkTokenKind;
    function FuncTimer(Index: Integer): TtkTokenKind;
    function FuncToggle(Index: Integer): TtkTokenKind;
    function FuncTopofpcstack(Index: Integer): TtkTokenKind;
    function FuncTrap(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncTx0(Index: Integer): TtkTokenKind;
    function FuncTx1(Index: Integer): TtkTokenKind;
    function FuncUndef(Index: Integer): TtkTokenKind;
    function FuncUntil(Index: Integer): TtkTokenKind;
    function FuncUs(Index: Integer): TtkTokenKind;
    function FuncUu(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function FuncXor(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure PascalCommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CCommentProc;
    procedure CRProc;
    procedure ExclamationProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure IncludeCloseProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure BinaryNumber;
    procedure HexNumber;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
  protected
    function IsFilterStored: Boolean; override;
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
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property ConditionAttri: TSynEdit32HighlighterAttributes read FConditionAttri
      write FConditionAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property RegisterAttri: TSynEdit32HighlighterAttributes read FRegisterAttri
      write FRegisterAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  Windows,
  Registry,
  SynEdit32.StrConst;

const
  KeyWords: array[0..178] of UnicodeString = (
    'abs', 'abstract', 'ac', 'af', 'alt_reg', 'and', 'ar', 'ar_sat', 'ashift', 
    'astat', 'aux', 'av', 'av_latch', 'ax0', 'ax1', 'ay0', 'ay1', 'b', 
    'bit_rev', 'bm', 'boot', 'by', 'cache', 'call', 'ce', 'circ', 'clear', 
    'clr', 'clrbit', 'cntl', 'cntr', 'const', 'define', 'dis', 'divq', 'divs', 
    'dm', 'dmovlay', 'do', 'else', 'emode', 'ena', 'endif', 'endmacro', 
    'endmod', 'entry', 'eq', 'exp', 'expadj', 'external', 'fl0', 'fl1', 'fl2', 
    'flag_in', 'flag_out', 'for', 'forever', 'ge', 'global', 'go_mode', 'gt', 
    'h', 'hi', 'i0', 'i1', 'i2', 'i3', 'i4', 'i5', 'i6', 'i7', 'icntl', 'idle', 
    'if', 'ifc', 'ifdef', 'ifndef', 'imask', 'in', 'include', 'init', 'io', 
    'jump', 'l0', 'l1', 'l2', 'l3', 'l4', 'l5', 'l6', 'l7', 'le', 'lo', 'local', 
    'loop', 'lshift', 'lt', 'm_mode', 'm0', 'm1', 'm2', 'm3', 'm4', 'm5', 'm6', 
    'm7', 'macro', 'mf', 'modify', 'module', 'mr', 'mr0', 'mr1', 'mr2', 'mstat', 
    'mv', 'mx0', 'mx1', 'my0', 'my1', 'name', 'ne', 'neg', 'newpage', 'nop', 
    'norm', 'not', 'of', 'or', 'pass', 'pc', 'pm', 'pop', 'port', 'push', 'ram', 
    'regbank', 'reset', 'rnd', 'rom', 'rti', 'rts', 'rx0', 'rx1', 'sat', 'sb', 
    'sec_reg', 'seg', 'segment', 'set', 'setbit', 'shift', 'shl', 'shr', 'si', 
    'sr', 'sr0', 'sr1', 'ss', 'sstat', 'static', 'sts', 'su', 'test', 'testbit', 
    'tglbit', 'timer', 'toggle', 'topofpcstack', 'trap', 'true', 'tx0', 'tx1', 
    'undef', 'until', 'us', 'uu', 'var', 'xor' 
  );

  KeyIndices: array[0..820] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, -1, 67, 
    15, -1, 48, 100, 132, -1, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, 
    152, 93, 155, -1, -1, -1, 70, 62, -1, -1, 103, 0, -1, -1, 10, -1, -1, -1, 
    -1, -1, -1, 171, -1, -1, -1, -1, 120, 162, -1, -1, -1, -1, -1, 82, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 153, -1, -1, -1, 50, 
    -1, -1, -1, -1, -1, -1, 72, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 20, -1, -1, -1, 25, -1, -1, -1, 8, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 156, 83, -1, -1, -1, -1, -1, 77, 106, -1, 45, 27, 
    -1, -1, -1, -1, -1, 7, -1, -1, 43, -1, 74, 14, 174, 73, 86, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 56, -1, -1, -1, -1, 111, -1, -1, 140, -1, 
    -1, -1, 89, -1, -1, -1, -1, 127, -1, -1, -1, 28, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 116, -1, 49, -1, -1, 164, 23, -1, -1, 9, -1, -1, 
    -1, -1, 149, -1, -1, -1, 40, -1, -1, 46, -1, 94, -1, 81, -1, 134, -1, -1, 
    -1, -1, -1, -1, -1, 55, -1, 47, -1, -1, -1, -1, 11, -1, 135, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, -1, -1, -1, 65, 142, -1, 
    -1, 98, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 128, -1, -1, -1, -1, 
    -1, 18, -1, 68, 16, -1, -1, 101, 91, -1, -1, -1, 130, -1, 167, -1, -1, -1, 
    115, -1, -1, -1, -1, 19, 158, -1, 163, -1, -1, -1, -1, -1, 104, -1, -1, -1, 
    -1, -1, -1, -1, 39, -1, 79, 172, -1, -1, -1, -1, 41, -1, 38, 176, 80, -1, 
    -1, -1, 118, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, 
    75, -1, -1, 51, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 138, -1, -1, -1, -1, 
    -1, -1, 42, -1, -1, -1, -1, -1, -1, 58, -1, -1, 136, -1, -1, -1, -1, -1, -1, 
    177, -1, -1, -1, -1, -1, -1, -1, 57, -1, 157, 84, 21, -1, -1, -1, -1, -1, 1, 
    -1, -1, -1, 96, 161, -1, -1, 123, -1, -1, -1, -1, -1, -1, -1, -1, -1, 87, 
    -1, -1, -1, 54, 137, -1, -1, 124, 145, -1, -1, -1, -1, -1, -1, -1, -1, 112, 
    -1, -1, 173, -1, -1, -1, 90, -1, 125, -1, 166, -1, -1, -1, -1, 144, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1, 170, -1, -1, 
    35, -1, -1, -1, -1, -1, -1, -1, 148, -1, 44, -1, -1, -1, -1, 159, -1, -1, 
    -1, -1, -1, 150, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, 
    178, -1, -1, -1, 141, 60, -1, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 66, 143, -1, -1, 99, -1, -1, 97, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 37, -1, -1, 26, -1, -1, 69, -1, -1, -1, 102, -1, -1, 121, -1, 
    -1, -1, 61, 129, 95, -1, -1, -1, 122, -1, 139, -1, -1, 36, 175, -1, -1, -1, 
    -1, -1, 105, -1, -1, -1, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 32, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, 2, -1, -1, 165, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 52, -1, -1, -1, -1, -1, -1, 92, -1, 147, 
    -1, 131, 3, -1, 24, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 168, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, 13, -1, -1, 85, 59, 
    -1, -1, 146, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 88, -1, -1, 107, -1, -1, -1, -1, -1, -1, 160, -1, -1, -1, 
    -1, -1, -1, -1, 113, 151, -1, -1, -1, -1, 53, -1, -1, -1, -1, -1, 34, 29, 
    169, 126, 114, -1, -1, 22, -1, -1, -1, 6, -1, -1, -1, -1, -1, -1, -1, 78, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 154, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 76, -1, -1, -1, -1, -1, 5, 30, -1, -1, -1, -1, -1, -1, 
    64, -1, -1, -1, -1, -1, -1 
  );

{$Q-}
function TSynEdit32HighlighterADSP21xx.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 641 + Ord(Str^) * 282;
    Inc(Str);
  end;
  Result := Result mod 821;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterADSP21xx.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterADSP21xx.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[48] := FuncAbs;
  FIdentFuncTable[426] := FuncAbstract;
  FIdentFuncTable[642] := FuncAc;
  FIdentFuncTable[667] := FuncAf;
  FIdentFuncTable[693] := FuncAlt95reg;
  FIdentFuncTable[806] := FuncAnd;
  FIdentFuncTable[767] := FuncAr;
  FIdentFuncTable[153] := FuncAr95sat;
  FIdentFuncTable[126] := FuncAshift;
  FIdentFuncTable[220] := FuncAstat;
  FIdentFuncTable[51] := FuncAux;
  FIdentFuncTable[253] := FuncAv;
  FIdentFuncTable[99] := FuncAv95latch;
  FIdentFuncTable[698] := FuncAx0;
  FIdentFuncTable[159] := FuncAx1;
  FIdentFuncTable[19] := FuncAy0;
  FIdentFuncTable[301] := FuncAy1;
  FIdentFuncTable[543] := FuncB;
  FIdentFuncTable[298] := FuncBit95rev;
  FIdentFuncTable[320] := FuncBm;
  FIdentFuncTable[118] := FuncBoot;
  FIdentFuncTable[420] := FuncBy;
  FIdentFuncTable[763] := FuncCache;
  FIdentFuncTable[217] := FuncCall;
  FIdentFuncTable[669] := FuncCe;
  FIdentFuncTable[122] := FuncCirc;
  FIdentFuncTable[579] := FuncClear;
  FIdentFuncTable[147] := FuncClr;
  FIdentFuncTable[196] := FuncClrbit;
  FIdentFuncTable[757] := FuncCntl;
  FIdentFuncTable[807] := FuncCntr;
  FIdentFuncTable[525] := FuncConst;
  FIdentFuncTable[629] := FuncDefine;
  FIdentFuncTable[715] := FuncDis;
  FIdentFuncTable[756] := FuncDivq;
  FIdentFuncTable[499] := FuncDivs;
  FIdentFuncTable[604] := FuncDm;
  FIdentFuncTable[576] := FuncDmovlay;
  FIdentFuncTable[347] := FuncDo;
  FIdentFuncTable[337] := FuncElse;
  FIdentFuncTable[229] := FuncEmode;
  FIdentFuncTable[345] := FuncEna;
  FIdentFuncTable[391] := FuncEndif;
  FIdentFuncTable[156] := FuncEndmacro;
  FIdentFuncTable[509] := FuncEndmod;
  FIdentFuncTable[146] := FuncEntry;
  FIdentFuncTable[232] := FuncEq;
  FIdentFuncTable[248] := FuncExp;
  FIdentFuncTable[21] := FuncExpadj;
  FIdentFuncTable[213] := FuncExternal;
  FIdentFuncTable[91] := FuncFl0;
  FIdentFuncTable[373] := FuncFl1;
  FIdentFuncTable[655] := FuncFl2;
  FIdentFuncTable[750] := FuncFlag95in;
  FIdentFuncTable[448] := FuncFlag95out;
  FIdentFuncTable[246] := FuncFor;
  FIdentFuncTable[175] := FuncForever;
  FIdentFuncTable[416] := FuncGe;
  FIdentFuncTable[398] := FuncGlobal;
  FIdentFuncTable[702] := FuncGo95mode;
  FIdentFuncTable[541] := FuncGt;
  FIdentFuncTable[593] := FuncH;
  FIdentFuncTable[44] := FuncHi;
  FIdentFuncTable[532] := FuncI0;
  FIdentFuncTable[814] := FuncI1;
  FIdentFuncTable[275] := FuncI2;
  FIdentFuncTable[557] := FuncI3;
  FIdentFuncTable[18] := FuncI4;
  FIdentFuncTable[300] := FuncI5;
  FIdentFuncTable[582] := FuncI6;
  FIdentFuncTable[43] := FuncI7;
  FIdentFuncTable[369] := FuncIcntl;
  FIdentFuncTable[98] := FuncIdle;
  FIdentFuncTable[161] := FuncIf;
  FIdentFuncTable[158] := FuncIfc;
  FIdentFuncTable[370] := FuncIfdef;
  FIdentFuncTable[800] := FuncIfndef;
  FIdentFuncTable[143] := FuncImask;
  FIdentFuncTable[775] := FuncIn;
  FIdentFuncTable[339] := FuncInclude;
  FIdentFuncTable[349] := FuncInit;
  FIdentFuncTable[236] := FuncIo;
  FIdentFuncTable[70] := FuncJump;
  FIdentFuncTable[137] := FuncL0;
  FIdentFuncTable[419] := FuncL1;
  FIdentFuncTable[701] := FuncL2;
  FIdentFuncTable[162] := FuncL3;
  FIdentFuncTable[444] := FuncL4;
  FIdentFuncTable[726] := FuncL5;
  FIdentFuncTable[187] := FuncL6;
  FIdentFuncTable[469] := FuncL7;
  FIdentFuncTable[305] := FuncLe;
  FIdentFuncTable[662] := FuncLo;
  FIdentFuncTable[38] := FuncLocal;
  FIdentFuncTable[234] := FuncLoop;
  FIdentFuncTable[595] := FuncLshift;
  FIdentFuncTable[430] := FuncLt;
  FIdentFuncTable[564] := FuncM95mode;
  FIdentFuncTable[279] := FuncM0;
  FIdentFuncTable[561] := FuncM1;
  FIdentFuncTable[22] := FuncM2;
  FIdentFuncTable[304] := FuncM3;
  FIdentFuncTable[586] := FuncM4;
  FIdentFuncTable[47] := FuncM5;
  FIdentFuncTable[329] := FuncM6;
  FIdentFuncTable[611] := FuncM7;
  FIdentFuncTable[144] := FuncMacro;
  FIdentFuncTable[729] := FuncMf;
  FIdentFuncTable[617] := FuncModify;
  FIdentFuncTable[268] := FuncModule;
  FIdentFuncTable[8] := FuncMr;
  FIdentFuncTable[180] := FuncMr0;
  FIdentFuncTable[462] := FuncMr1;
  FIdentFuncTable[744] := FuncMr2;
  FIdentFuncTable[760] := FuncMstat;
  FIdentFuncTable[315] := FuncMv;
  FIdentFuncTable[211] := FuncMx0;
  FIdentFuncTable[493] := FuncMx1;
  FIdentFuncTable[353] := FuncMy0;
  FIdentFuncTable[635] := FuncMy1;
  FIdentFuncTable[63] := FuncName;
  FIdentFuncTable[589] := FuncNe;
  FIdentFuncTable[599] := FuncNeg;
  FIdentFuncTable[434] := FuncNewpage;
  FIdentFuncTable[452] := FuncNop;
  FIdentFuncTable[471] := FuncNorm;
  FIdentFuncTable[759] := FuncNot;
  FIdentFuncTable[192] := FuncOf;
  FIdentFuncTable[292] := FuncOr;
  FIdentFuncTable[594] := FuncPass;
  FIdentFuncTable[309] := FuncPc;
  FIdentFuncTable[666] := FuncPm;
  FIdentFuncTable[23] := FuncPop;
  FIdentFuncTable[29] := FuncPort;
  FIdentFuncTable[238] := FuncPush;
  FIdentFuncTable[255] := FuncRam;
  FIdentFuncTable[401] := FuncRegbank;
  FIdentFuncTable[449] := FuncReset;
  FIdentFuncTable[384] := FuncRnd;
  FIdentFuncTable[601] := FuncRom;
  FIdentFuncTable[183] := FuncRti;
  FIdentFuncTable[540] := FuncRts;
  FIdentFuncTable[276] := FuncRx0;
  FIdentFuncTable[558] := FuncRx1;
  FIdentFuncTable[478] := FuncSat;
  FIdentFuncTable[453] := FuncSb;
  FIdentFuncTable[705] := FuncSec95reg;
  FIdentFuncTable[664] := FuncSeg;
  FIdentFuncTable[507] := FuncSegment;
  FIdentFuncTable[225] := FuncSet;
  FIdentFuncTable[520] := FuncSetbit;
  FIdentFuncTable[745] := FuncShift;
  FIdentFuncTable[37] := FuncShl;
  FIdentFuncTable[87] := FuncShr;
  FIdentFuncTable[785] := FuncSi;
  FIdentFuncTable[39] := FuncSr;
  FIdentFuncTable[136] := FuncSr0;
  FIdentFuncTable[418] := FuncSr1;
  FIdentFuncTable[321] := FuncSs;
  FIdentFuncTable[514] := FuncSstat;
  FIdentFuncTable[736] := FuncStatic;
  FIdentFuncTable[431] := FuncSts;
  FIdentFuncTable[64] := FuncSu;
  FIdentFuncTable[323] := FuncTest;
  FIdentFuncTable[216] := FuncTestbit;
  FIdentFuncTable[645] := FuncTglbit;
  FIdentFuncTable[473] := FuncTimer;
  FIdentFuncTable[311] := FuncToggle;
  FIdentFuncTable[683] := FuncTopofpcstack;
  FIdentFuncTable[758] := FuncTrap;
  FIdentFuncTable[496] := FuncTrue;
  FIdentFuncTable[58] := FuncTx0;
  FIdentFuncTable[340] := FuncTx1;
  FIdentFuncTable[465] := FuncUndef;
  FIdentFuncTable[160] := FuncUntil;
  FIdentFuncTable[605] := FuncUs;
  FIdentFuncTable[348] := FuncUu;
  FIdentFuncTable[408] := FuncVar;
  FIdentFuncTable[536] := FuncXor;
end;

function TSynEdit32HighlighterADSP21xx.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynEdit32HighlighterADSP21xx.FuncAbs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAbstract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAlt95reg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAr95sat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAshift(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAstat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAux(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAv95latch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAx0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAx1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAy0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncAy1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncB(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if FLine[FRun + 1] = '#' then
    begin
      Result := tkNumber;
      FRange := rsBinaryNumber;
    end
    else
    begin
      Result := tkIdentifier;
    end
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncBit95rev(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncBm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncBoot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncBy(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncCache(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncCall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncCe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncCirc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncClear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncClr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncClrbit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncCntl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncCntr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncDefine(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncDis(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncDivq(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncDivs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncDm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncDmovlay(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncEmode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncEna(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncEndif(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncEndmacro(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncEndmod(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncEntry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncEq(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncExp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncExpadj(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncExternal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncFl0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncFl1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncFl2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncFlag95in(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncFlag95out(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncForever(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncGe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncGlobal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncGo95mode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncGt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncH(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if FLine[FRun + 1] = '#' then
    begin
      Result := tkNumber;
      FRange := rsHexNumber;
    end
    else
    begin
      Result := tkIdentifier;
    end
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncHi(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI3(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI4(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI5(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI6(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncI7(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIcntl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIdle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIfc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIfdef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIfndef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncImask(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncInclude(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncInit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncIo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncJump(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL3(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL4(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL5(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL6(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncL7(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncLe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncLo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncLoop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncLshift(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncLt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM95mode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM3(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM4(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM5(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM6(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncM7(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMacro(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncModify(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncModule(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMr0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMr1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMr2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMstat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMx0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMx1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMy0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncMy1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncName(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncNe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncNeg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncNewpage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncNop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncNorm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncNot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncOf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncOr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncPass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncPc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncPm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncPop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncPort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncPush(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRam(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRegbank(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncReset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRti(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRts(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRx0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncRx1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSb(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSec95reg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSeg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSegment(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSetbit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncShift(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncShl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncShr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSi(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSr0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSr1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSstat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncStatic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSts(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncSu(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTest(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTestbit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTglbit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTimer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncToggle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTopofpcstack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTrap(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTx0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncTx1(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncUndef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncUntil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncUs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncUu(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterADSP21xx.FuncXor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterADSP21xx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.ForeGround := clTeal;
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.ForeGround := clOlive;
  AddAttribute(FNumberAttri);

  FRegisterAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrRegister, SYNS_FriendlyAttrRegister);
  FRegisterAttri.ForeGround := clBlue;
  AddAttribute(FRegisterAttri);

  FConditionAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrCondition, SYNS_FriendlyAttrCondition);
  FConditionAttri.ForeGround := clFuchsia;
  AddAttribute(FConditionAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FNullAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNull, SYNS_FriendlyAttrNull);
  AddAttribute(FNullAttri);

  FUnknownAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrUnknownWord, SYNS_FriendlyAttrUnknownWord);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FRange := rsUnknown;
  fDefaultFilter := SYNS_FilterADSP21xx;
end;

procedure TSynEdit32HighlighterADSP21xx.BraceCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterADSP21xx.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #39) and (FLine[FRun + 2] = #39) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: break;
    end;
    Inc(FRun);
  until FLine[FRun] = #39;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterADSP21xx.PascalCommentProc;
begin
  FTokenID := tkComment;
  case FLine[FRun] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[FRun] <> #0 do
    case FLine[FRun] of
      '}':
        begin
          FRange := rsUnKnown;
          Inc(FRun);
          break;
        end;
      #10: break;
      #13: break;
      else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterADSP21xx.CCommentProc;
begin
  FTokenID := tkComment;
  case FLine[FRun] of
    #0: begin
          NullProc;
          exit;
        end;
    #10:begin
         LFProc;
         exit;
        end;
    #13:begin
          CRProc;
          exit;
        end;
  end;

  while FLine[FRun] <> #0 do
    case FLine[FRun] of
      '*':
        begin
          if FLine[FRun+1] = '/' then
          begin
            FRange := rsUnknown;
            Inc(FRun, 2);
            break;
          end
          else
            Inc(FRun);
        end;
      #10: break;
      #13: break;
      else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterADSP21xx.BraceOpenProc;
begin
  FTokenID := tkComment;
  FRange := rsPascalComment;
  Inc(FRun);
  while FLine[FRun] <> #0 do
    case FLine[FRun] of
      '}':
        begin
          FRange := rsUnKnown;
          Inc(FRun);
          break;
        end;
      #10: break;
      #13: break;
    else Inc(FRun);
    end;
end;


procedure TSynEdit32HighlighterADSP21xx.IncludeCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterADSP21xx.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
  else
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterADSP21xx.ExclamationProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterADSP21xx.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterADSP21xx.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(FRun);
  FTokenID := tkNumber;
  while IsIntegerChar do Inc(FRun);
end;

procedure TSynEdit32HighlighterADSP21xx.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterADSP21xx.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterADSP21xx.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'A'..'F', 'a'..'f', 'x', 'X', '.':
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
        if FLine[FRun + 1] = '.' then break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterADSP21xx.HexNumber;

  function IsHexChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(FRun);
  FTokenID := tkNumber;
  FRange := rsUnKnown;
  while IsHexChar do
  begin
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterADSP21xx.BinaryNumber;
begin
  Inc(FRun);
  FRange := rsUnKnown;
  while CharInSet(FLine[FRun], ['0'..'1']) do
  begin
    Inc(FRun);
  end;
  if CharInSet(FLine[FRun], ['2'..'9', 'A'..'F', 'a'..'f']) then
  begin
    FTokenID := tkIdentifier
  end
  else
    FTokenID := tkNumber;
end;

procedure TSynEdit32HighlighterADSP21xx.SlashProc;
begin
  if FLine[FRun + 1] = '*' then
  begin
    FTokenID := tkComment;
    FRange := rsCComment;
    Inc(FRun, 2);
    while FLine[FRun] <> #0 do
      case FLine[FRun] of
        '*':  begin
                if FLine[FRun+1] = '/' then
                begin
                  Inc(FRun, 2);
                  FRange := rsUnknown;
                  break;
                end
                else Inc(FRun);
              end;
        #10: break;
        #13: break;
        else Inc(FRun);
      end;
    end
  else
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterADSP21xx.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterADSP21xx.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterADSP21xx.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsPascalComment: PascalCommentProc;
    rsCComment: CCommentProc;
    rsHexNumber: HexNumber;
    rsBinaryNumber: BinaryNumber;
  else
    FRange := rsUnknown;
    case FLine[FRun] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '$': IntegerProc;
      #39: StringProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '{': BraceOpenProc;
      '}': BraceCloseProc;
      '/': SlashProc;
      '>': IncludeCloseProc;
      '!': ExclamationProc;
      else UnknownProc;
    end;
  end;
  inherited;
end;

function TSynEdit32HighlighterADSP21xx.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterADSP21xx.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterADSP21xx.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynEdit32HighlighterADSP21xx.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkRegister: Result := FRegisterAttri;
    tkCondition: Result := FConditionAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterADSP21xx.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynEdit32HighlighterADSP21xx.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterADSP21xx.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynEdit32HighlighterADSP21xx.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  with TRegistry.Create do
  begin
    try
      RootKey := HKEY_CURRENT_USER;
      // we need some method to make the following statement more universal!
      if OpenKeyReadOnly('\SOFTWARE\Wynand\DSPIDE\1.0') then
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

function TSynEdit32HighlighterADSP21xx.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

    {$IFNDEF SYN_CLX}
    function ReadDspIDESetting(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): boolean;
    begin
      try
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Wynand\DspIDE\1.0\Editor\Highlight',key,false);
      except Result := false; end;
    end;
    {$ENDIF}
var
  tmpNumberAttri    : TSynEdit32HighlighterAttributes;
  tmpKeyAttri       : TSynEdit32HighlighterAttributes;
  tmpSymbolAttri    : TSynEdit32HighlighterAttributes;
  tmpCommentAttri   : TSynEdit32HighlighterAttributes;
  tmpConditionAttri : TSynEdit32HighlighterAttributes;
  tmpIdentifierAttri: TSynEdit32HighlighterAttributes;
  tmpSpaceAttri     : TSynEdit32HighlighterAttributes;
  tmpRegisterAttri  : TSynEdit32HighlighterAttributes;
  StrLst            : TStringList;

begin  // UseUserSettings
  StrLst := TStringList.Create;
  try
    EnumUserSettings(StrLst);
    if settingIndex >= StrLst.Count then
      Result := false
    else
    begin
      tmpNumberAttri    := TSynEdit32HighlighterAttributes.Create('', '');
      tmpKeyAttri       := TSynEdit32HighlighterAttributes.Create('', '');
      tmpSymbolAttri    := TSynEdit32HighlighterAttributes.Create('', '');
      tmpCommentAttri   := TSynEdit32HighlighterAttributes.Create('', '');
      tmpConditionAttri := TSynEdit32HighlighterAttributes.Create('', '');
      tmpIdentifierAttri:= TSynEdit32HighlighterAttributes.Create('', '');
      tmpSpaceAttri     := TSynEdit32HighlighterAttributes.Create('', '');
      tmpRegisterAttri  := TSynEdit32HighlighterAttributes.Create('', '');

      tmpNumberAttri    .Assign(FNumberAttri);
      tmpKeyAttri       .Assign(FKeyAttri);
      tmpSymbolAttri    .Assign(FSymbolAttri);
      tmpCommentAttri   .Assign(FCommentAttri);
      tmpConditionAttri .Assign(FConditionAttri);
      tmpIdentifierAttri.Assign(FIdentifierAttri);
      tmpSpaceAttri     .Assign(FSpaceAttri);
      tmpRegisterAttri  .Assign(FRegisterAttri);
      {$IFNDEF SYN_CLX}
      Result := ReadDspIDESetting(StrLst[settingIndex],FCommentAttri,'Comment')       and
                ReadDspIDESetting(StrLst[settingIndex],FIdentifierAttri,'Identifier') and
                ReadDspIDESetting(StrLst[settingIndex],FKeyAttri,'Reserved word')     and
                ReadDspIDESetting(StrLst[settingIndex],FNumberAttri,'BinaryNumber')   and
                ReadDspIDESetting(StrLst[settingIndex],FSpaceAttri,'Whitespace')      and
                ReadDspIDESetting(StrLst[settingIndex],FSymbolAttri,'Symbol')         and
                ReadDspIDESetting(StrLst[settingIndex],FConditionAttri,'Condition')   and
                ReadDspIDESetting(StrLst[settingIndex],FRegisterAttri,'Symbol');
      {$ELSE}
      Result := False;
      {$ENDIF}
      if not Result then
      begin
        FNumberAttri     .Assign(tmpNumberAttri);
        FKeyAttri        .Assign(tmpKeyAttri);
        FSymbolAttri     .Assign(tmpSymbolAttri);
        FCommentAttri    .Assign(tmpCommentAttri);
        FConditionAttri  .Assign(tmpConditionAttri);
        FIdentifierAttri .Assign(tmpIdentifierAttri);
        FSpaceAttri      .Assign(tmpSpaceAttri);
        FConditionAttri  .Assign(tmpConditionAttri);
        FRegisterAttri   .Assign(tmpRegisterAttri);
      end;
      tmpNumberAttri    .Free;
      tmpKeyAttri       .Free;
      tmpSymbolAttri    .Free;
      tmpCommentAttri   .Free;
      tmpConditionAttri .Free;
      tmpIdentifierAttri.Free;
      tmpSpaceAttri     .Free;
      tmpRegisterAttri  .Free;
    end;
  finally StrLst.Free; end;
end;

function TSynEdit32HighlighterADSP21xx.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterADSP21xx;
end;

class function TSynEdit32HighlighterADSP21xx.GetLanguageName: string;
begin
  Result := SYNS_LangADSP21xx;
end;

class function TSynEdit32HighlighterADSP21xx.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

class function TSynEdit32HighlighterADSP21xx.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangADSP21xx;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterADSP21xx);
end.
