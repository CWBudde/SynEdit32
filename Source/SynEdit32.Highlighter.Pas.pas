{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynHighlighterPas.pas,v 1.27.2.10 2009/02/23 15:43:50 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Pascal/Delphi syntax highlighter for SynEdit)
@author(Martin Waldenburg)
@created(1998, converted to SynEdit 2000-04-07)
@lastmod(2004-03-19)
The SynHighlighterPas unit provides SynEdit with a Object Pascal syntax highlighter.
Two extra properties included (DelphiVersion, PackageSource):
  DelphiVersion - Allows you to enable/disable the highlighting of various
                  language enhancements added in the different Delphi versions.
  PackageSource - Allows you to enable/disable the highlighting of package keywords
}

unit SynEdit32.Highlighter.Pas;

{$I SynEdit.Inc}

interface

uses
  Windows,
  Graphics,
  SysUtils,
  Classes,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TDelphiVersion = (dvDelphi1, dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5,
    dvDelphi6, dvDelphi7, dvDelphi8, dvDelphi2005);

const
  LastDelphiVersion = dvDelphi2005;
  BDSVersionPrefix = 'BDS';

type
  TSynEdit32HighlighterPas = class(TSynEdit32CustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FIdentFuncTable: array[0..388] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FCharAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FFloatAttri: TSynEdit32HighlighterAttributes;
    FHexAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FAsmAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDirecAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FDelphiVersion: TDelphiVersion;
    FPackageSource: Boolean;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncAsm(Index: Integer): TtkTokenKind;
    function FuncAutomated(Index: Integer): TtkTokenKind;
    function FuncCdecl(Index: Integer): TtkTokenKind;
    function FuncContains(Index: Integer): TtkTokenKind;
    function FuncDeprecated(Index: Integer): TtkTokenKind;
    function FuncDispid(Index: Integer): TtkTokenKind;
    function FuncDispinterface(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncExports(Index: Integer): TtkTokenKind;
    function FuncFinal(Index: Integer): TtkTokenKind;
    function FuncFinalization(Index: Integer): TtkTokenKind;
    function FuncHelper(Index: Integer): TtkTokenKind;
    function FuncImplements(Index: Integer): TtkTokenKind;
    function FuncIndex(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNodefault(Index: Integer): TtkTokenKind;
    function FuncOperator(Index: Integer): TtkTokenKind;
    function FuncOverload(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncPlatform(Index: Integer): TtkTokenKind;
    function FuncProperty(Index: Integer): TtkTokenKind;
    function FuncRead(Index: Integer): TtkTokenKind;
    function FuncReadonly(Index: Integer): TtkTokenKind;
    function FuncReintroduce(Index: Integer): TtkTokenKind;
    function FuncRequires(Index: Integer): TtkTokenKind;
    function FuncResourcestring(Index: Integer): TtkTokenKind;
    function FuncSafecall(Index: Integer): TtkTokenKind;
    function FuncSealed(Index: Integer): TtkTokenKind;
    function FuncStdcall(Index: Integer): TtkTokenKind;
    function FuncStored(Index: Integer): TtkTokenKind;
    function FuncStringresource(Index: Integer): TtkTokenKind;
    function FuncThreadvar(Index: Integer): TtkTokenKind;
    function FuncWrite(Index: Integer): TtkTokenKind;
    function FuncWriteonly(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure SetPackageSource(const Value: Boolean);
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
      override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function UseUserSettings(VersionIndex: Integer): Boolean; override;
    procedure EnumUserSettings(DelphiVersions: TStrings); override;
  published
    property AsmAttri: TSynEdit32HighlighterAttributes read FAsmAttri write FAsmAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirectiveAttri: TSynEdit32HighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property FloatAttri: TSynEdit32HighlighterAttributes read FFloatAttri
      write FFloatAttri;
    property HexAttri: TSynEdit32HighlighterAttributes read FHexAttri
      write FHexAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property CharAttri: TSynEdit32HighlighterAttributes read FCharAttri
      write FCharAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property DelphiVersion: TDelphiVersion read FDelphiVersion write SetDelphiVersion
      default LastDelphiVersion;
    property PackageSource: Boolean read FPackageSource write SetPackageSource default True;
  end;

implementation

uses
  Registry, SynEdit32.StrConst;

const
  // if the language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..110] of UnicodeString = (
    'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
    'automated', 'begin', 'case', 'cdecl', 'class', 'const', 'constructor',
    'contains', 'default', 'deprecated', 'destructor', 'dispid',
    'dispinterface', 'div', 'do', 'downto', 'dynamic', 'else', 'end', 'except',
    'export', 'exports', 'external', 'far', 'file', 'final', 'finalization',
    'finally', 'for', 'forward', 'function', 'goto', 'helper', 'if',
    'implementation', 'implements', 'in', 'index', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library',
    'message', 'mod', 'name', 'near', 'nil', 'nodefault', 'not', 'object', 'of',
    'on', 'operator', 'or', 'out', 'overload', 'override', 'package', 'packed',
    'pascal', 'platform', 'private', 'procedure', 'program', 'property',
    'protected', 'public', 'published', 'raise', 'read', 'readonly', 'record',
    'register', 'reintroduce', 'repeat', 'requires', 'resourcestring',
    'safecall', 'sealed', 'set', 'shl', 'shr', 'stdcall', 'stored', 'string',
    'stringresource', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'virtual', 'while', 'with', 'write', 'writeonly', 'xor'
  );

  KeyIndices: array[0..388] of Integer = (
    -1, -1, -1, 105, -1, 51, -1, 108, -1, -1, -1, -1, -1, 75, -1, -1, 46, -1,
    -1, 103, -1, -1, -1, -1, 55, -1, -1, -1, -1, 76, -1, -1, 96, 14, -1, 31, 3,
    102, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 25, -1,
    -1, 56, 65, 95, -1, -1, -1, 34, -1, 85, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 22, -1, -1, -1, -1, -1, -1, 80, -1, -1, -1, -1, 50, -1, -1, 109, 98, -1,
    86, -1, 13, -1, -1, -1, 107, -1, -1, 60, -1, 0, 64, -1, -1, -1, -1, 8, 10,
    -1, -1, -1, 67, -1, -1, -1, 74, -1, 17, -1, 73, 69, -1, 68, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 16, -1, -1, 23, 39, -1, 35, 30, -1, -1, -1, 70, -1, 37,
    -1, -1, 89, 71, 84, 72, -1, 29, 40, -1, -1, -1, 32, -1, -1, -1, 94, -1, -1,
    87, -1, -1, -1, -1, -1, -1, 77, -1, -1, -1, -1, -1, -1, 11, 57, 41, 6, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, 97, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 44, 12, -1, -1, 101, -1, 58, -1, -1, -1, 99, -1, -1,
    -1, -1, 53, 20, -1, -1, -1, 36, -1, -1, 63, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, 59,
    -1, 110, -1, 15, -1, 52, -1, -1, -1, -1, 5, 48, -1, -1, -1, 81, -1, 28, -1,
    -1, -1, 2, -1, 1, -1, 106, -1, -1, -1, -1, 90, -1, 83, -1, -1, -1, -1, -1,
    79, -1, -1, 33, 62, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, 88,
    61, 54, -1, 42, -1, -1, -1, 66, -1, -1, -1, 92, 100, -1, -1, -1, -1, -1, 18,
    -1, -1, 26, 47, 38, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, -1, 91, -1, -1, -1, -1, -1, -1, 49, -1, 21, -1, -1, -1, -1, -1, -1, 43,
    -1, 82, -1, 19, 104, -1, -1, -1, -1, -1
  );

{$Q-}
function TSynEdit32HighlighterPas.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 812 + Ord(Str^) * 76;
    Inc(Str);
  end;
  Result := Result mod 389;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterPas.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterPas.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[275] := FuncAsm;
  FIdentFuncTable[41] := FuncAutomated;
  FIdentFuncTable[112] := FuncCdecl;
  FIdentFuncTable[33] := FuncContains;
  FIdentFuncTable[137] := FuncDeprecated;
  FIdentFuncTable[340] := FuncDispid;
  FIdentFuncTable[382] := FuncDispinterface;
  FIdentFuncTable[54] := FuncEnd;
  FIdentFuncTable[282] := FuncExports;
  FIdentFuncTable[163] := FuncFinal;
  FIdentFuncTable[306] := FuncFinalization;
  FIdentFuncTable[141] := FuncHelper;
  FIdentFuncTable[325] := FuncImplements;
  FIdentFuncTable[214] := FuncIndex;
  FIdentFuncTable[323] := FuncName;
  FIdentFuncTable[185] := FuncNodefault;
  FIdentFuncTable[307] := FuncOperator;
  FIdentFuncTable[58] := FuncOverload;
  FIdentFuncTable[116] := FuncPackage;
  FIdentFuncTable[148] := FuncPlatform;
  FIdentFuncTable[120] := FuncProperty;
  FIdentFuncTable[303] := FuncRead;
  FIdentFuncTable[83] := FuncReadonly;
  FIdentFuncTable[297] := FuncReintroduce;
  FIdentFuncTable[65] := FuncRequires;
  FIdentFuncTable[94] := FuncResourcestring;
  FIdentFuncTable[170] := FuncSafecall;
  FIdentFuncTable[321] := FuncSealed;
  FIdentFuncTable[333] := FuncStdcall;
  FIdentFuncTable[348] := FuncStored;
  FIdentFuncTable[59] := FuncStringresource;
  FIdentFuncTable[204] := FuncThreadvar;
  FIdentFuncTable[7] := FuncWrite;
  FIdentFuncTable[91] := FuncWriteonly;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynEdit32HighlighterPas.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynEdit32HighlighterPas.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynEdit32HighlighterPas.FuncAsm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsAsm;
    FAsmStart := True;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncAutomated(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncCdecl(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncContains(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncDeprecated(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi6) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncDispid(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncDispinterface(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsUnknown;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncExports(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsExports;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncFinal(Index: Integer): TtkTokenKind;
begin
 if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncFinalization(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncHelper(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncImplements(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncIndex(Index: Integer): TtkTokenKind;
begin
  if (FRange in [rsProperty, rsExports]) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncName(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsExports) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncNodefault(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncOperator(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncOverload(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncPlatform(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi6) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncProperty(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsProperty;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncRead(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncReadonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncReintroduce(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncRequires(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncResourcestring(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncSafecall(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncSealed(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncStdcall(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncStored(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncStringresource(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncThreadvar(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncWrite(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterPas.FuncWriteonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterPas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  FDelphiVersion := LastDelphiVersion;
  FPackageSource := True;

  FAsmAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(FAsmAttri);
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FDirecAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FDirecAttri.Style:= [fsItalic];
  AddAttribute(FDirecAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FFloatAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(FFloatAttri);
  FHexAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(FHexAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FCharAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(FCharAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  fDefaultFilter := SYNS_FilterPascal;
end;

procedure TSynEdit32HighlighterPas.AddressOpProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '@' then Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.AsciiCharProc;

  function IsAsciiChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '$', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;
  
begin
  FTokenID := tkChar;
  Inc(FRun);
  while IsAsciiChar do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.BorProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      if FRange in [rsDirective, rsDirectiveAsm] then
        FTokenID := tkDirec
      else
        FTokenID := tkComment;
      repeat
        if FLine[FRun] = '}' then
        begin
          Inc(FRun);
          if FRange in [rsBorAsm, rsDirectiveAsm] then
            FRange := rsAsm
          else
            FRange := rsUnKnown;
          break;
        end;
        Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end;

procedure TSynEdit32HighlighterPas.BraceOpenProc;
begin
  if (FLine[FRun + 1] = '$') then
  begin
    if FRange = rsAsm then
      FRange := rsDirectiveAsm
    else
      FRange := rsDirective;
  end
  else
  begin
    if FRange = rsAsm then
      FRange := rsBorAsm
    else
      FRange := rsBor;
  end;
  BorProc;
end;

procedure TSynEdit32HighlighterPas.ColonOrGreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.IdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.IntegerProc;

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
  FTokenID := tkHex;
  while IsIntegerChar do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] = '=') or (FLine[FRun] = '>') then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'e', 'E', '-', '+':
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
        if FLine[FRun + 1] = '.' then
          Break
        else
          FTokenID := tkFloat;
      'e', 'E': FTokenID := tkFloat;
      '-', '+':
        begin
          if FTokenID <> tkFloat then // arithmetic
            Break;
          if (FLine[FRun - 1] <> 'e') and (FLine[FRun - 1] <> 'E') then
            Break; //float, but it ends here
        end;
    end;
    Inc(FRun);
  end;
end; 

procedure TSynEdit32HighlighterPas.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] = '.') or (FLine[FRun - 1] = ')') then
    Inc(FRun);
end; 

procedure TSynEdit32HighlighterPas.AnsiProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[FRun] = '*') and (FLine[FRun + 1] = ')') then begin
        Inc(FRun, 2);
        if FRange = rsAnsiAsm then
          FRange := rsAsm
        else
          FRange := rsUnKnown;
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterPas.RoundOpenProc;
begin
  Inc(FRun);
  case FLine[FRun] of
    '*':
      begin
        Inc(FRun);
        if FRange = rsAsm then
          FRange := rsAnsiAsm
        else
          FRange := rsAnsi;
        FTokenID := tkComment;
        if not IsLineEnd(FRun) then
          AnsiProc;
      end;
    '.':
      begin
        Inc(FRun);
        FTokenID := tkSymbol;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterPas.SemicolonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if FRange in [rsProperty, rsExports] then
    FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterPas.SlashProc;
begin
  Inc(FRun);
  if (FLine[FRun] = '/') and (FDelphiVersion > dvDelphi1) then
  begin
    FTokenID := tkComment;
    repeat
      Inc(FRun);
    until IsLineEnd(FRun);
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPas.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterPas.StringProc;
begin
  FTokenID := tkString;
  Inc(FRun);
  while not IsLineEnd(FRun) do
  begin
    if FLine[FRun] = #39 then begin
      Inc(FRun);
      if FLine[FRun] <> #39 then
        break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterPas.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPas.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterPas.Next;
begin
  FAsmStart := False;
  fTokenPos := FRun;
  case FRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
      BorProc;
    else
      case FLine[FRun] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '#': AsciiCharProc;
        '$': IntegerProc;
        #39: StringProc;
        '0'..'9': NumberProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '{': BraceOpenProc;
        '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
          begin
            case FLine[FRun] of
              '(': RoundOpenProc;
              '.': PointProc;
              ';': SemicolonProc;
              '/': SlashProc;
              ':', '>': ColonOrGreaterProc;
              '<': LowerProc;
              '@': AddressOpProc;
              else
                 SymbolProc;
            end;
          end;
        else
          UnknownProc;
      end;
  end;
  inherited;
end;

function TSynEdit32HighlighterPas.GetDefaultAttribute(Index: Integer):
  TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterPas.GetTokenID: TtkTokenKind;
begin
  if not FAsmStart and (FRange = rsAsm)
    and not (FTokenID in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := FTokenID;
end;

function TSynEdit32HighlighterPas.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := FAsmAttri;
    tkComment: Result := FCommentAttri;
    tkDirec: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkFloat: Result := FFloatAttri;
    tkHex: Result := FHexAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterPas.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynEdit32HighlighterPas.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynEdit32HighlighterPas.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterPas.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynEdit32HighlighterPas.EnumUserSettings(DelphiVersions: TStrings);

  procedure LoadKeyVersions(const Key, Prefix: string);
  var
    Versions: TStringList;
    i: Integer;
  begin
    with TRegistry.Create do
    begin
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(Key) then
        begin
          try
            Versions := TStringList.Create;
            try
              GetKeyNames(Versions);
              for i := 0 to Versions.Count - 1 do
                DelphiVersions.Add(Prefix + Versions[i]);
            finally
              FreeAndNil(Versions);
            end;
          finally
            CloseKey;
          end;
        end;
      finally
        Free;
      end;
    end;
  end;

begin
  { returns the user settings that exist in the registry }
  // See UseUserSettings below where these strings are used
  LoadKeyVersions('\SOFTWARE\Borland\Delphi', '');
  LoadKeyVersions('\SOFTWARE\Borland\BDS', BDSVersionPrefix);
  LoadKeyVersions('\SOFTWARE\CodeGear\BDS', BDSVersionPrefix);
  LoadKeyVersions('\SOFTWARE\Embarcadero\BDS', BDSVersionPrefix);
end;

function TSynEdit32HighlighterPas.UseUserSettings(VersionIndex: Integer): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   True : settings were read and used
//   False: problem reading settings or invalid version specified - old settings
//          were preserved

  function ReadDelphiSettings(settingIndex: Integer): Boolean;

    function ReadDelphiSetting(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): Boolean;
    var
      Version: Currency;
      VersionStr: string;

      function ReadDelphi2Or3(settingTag: string; attri: TSynEdit32HighlighterAttributes; name: string): Boolean;
      var
        i: Integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                '\Software\Borland\Delphi\'+settingTag+'\Highlight',name,True);
      end; { ReadDelphi2Or3 }

      function ReadDelphi4OrMore(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\Delphi\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi4OrMore }

      function ReadDelphi8To2007(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi8OrMore }

      function ReadDelphi2009OrMore(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\CodeGear\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi2009OrMore }

      function ReadDelphiXEOrMore(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Embarcadero\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi2009OrMore }


    begin { ReadDelphiSetting }
      try
        if Pos('BDS', settingTag) = 1 then // BDS product
        begin
          VersionStr := Copy(settingTag, Length(BDSVersionPrefix) + 1, 999);
          Version := 0;
          if not TryStrToCurr(StringReplace(VersionStr, '.', {$IFDEF SYN_COMPILER_15_UP}FormatSettings.{$ENDIF}DecimalSeparator, []), Version) then
          begin
            Result := False;
            Exit;
          end;
          if Version >= 8 then
            Result := ReadDelphiXEOrMore(VersionStr, attri, key)
          else
            if Version >= 6 then
              Result := ReadDelphi2009OrMore(VersionStr, attri, key)
            else
              Result := ReadDelphi8To2007(VersionStr, attri, key);
        end
        else begin // Borland Delphi 7 or earlier
          if (settingTag[1] = '2') or (settingTag[1] = '3')
            then Result := ReadDelphi2Or3(settingTag, attri, key)
            else Result := ReadDelphi4OrMore(settingTag, attri, key);
        end;
      except Result := False; end;
    end; { ReadDelphiSetting }

  var
    tmpAsmAttri, tmpCommentAttri, tmpIdentAttri, tmpKeyAttri, tmpNumberAttri,
    tmpSpaceAttri, tmpStringAttri, tmpSymbolAttri: TSynEdit32HighlighterAttributes;
    iVersions: TStringList;
    iVersionTag: string;
  begin { ReadDelphiSettings }
    iVersions := TStringList.Create;
    try
      EnumUserSettings(iVersions);
      if (settingIndex < 0) or (settingIndex >= iVersions.Count) then
      begin
        Result := False;
        Exit;
      end;
      iVersionTag := iVersions[settingIndex];
    finally
      iVersions.Free;
    end;
    tmpAsmAttri     := TSynEdit32HighlighterAttributes.Create('', '');
    tmpCommentAttri := TSynEdit32HighlighterAttributes.Create('', '');
    tmpIdentAttri   := TSynEdit32HighlighterAttributes.Create('', '');
    tmpKeyAttri     := TSynEdit32HighlighterAttributes.Create('', '');
    tmpNumberAttri  := TSynEdit32HighlighterAttributes.Create('', '');
    tmpSpaceAttri   := TSynEdit32HighlighterAttributes.Create('', '');
    tmpStringAttri  := TSynEdit32HighlighterAttributes.Create('', '');
    tmpSymbolAttri  := TSynEdit32HighlighterAttributes.Create('', '');

    Result := ReadDelphiSetting(iVersionTag, tmpAsmAttri,'Assembler') and
      ReadDelphiSetting(iVersionTag, tmpCommentAttri,'Comment') and
      ReadDelphiSetting(iVersionTag, tmpIdentAttri,'Identifier') and
      ReadDelphiSetting(iVersionTag, tmpKeyAttri,'Reserved word') and
      ReadDelphiSetting(iVersionTag, tmpNumberAttri,'Number') and
      ReadDelphiSetting(iVersionTag, tmpSpaceAttri,'Whitespace') and
      ReadDelphiSetting(iVersionTag, tmpStringAttri,'String') and
      ReadDelphiSetting(iVersionTag, tmpSymbolAttri,'Symbol');

    if Result then
    begin
      FAsmAttri.AssignColorAndStyle(tmpAsmAttri);
      FCharAttri.AssignColorAndStyle(tmpStringAttri); { Delphi lacks Char attribute }
      FCommentAttri.AssignColorAndStyle(tmpCommentAttri);
      FDirecAttri.AssignColorAndStyle(tmpCommentAttri); { Delphi lacks Directive attribute }
      FFloatAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Float attribute }
      FHexAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Hex attribute }
      FIdentifierAttri.AssignColorAndStyle(tmpIdentAttri);
      FKeyAttri.AssignColorAndStyle(tmpKeyAttri);
      FNumberAttri.AssignColorAndStyle(tmpNumberAttri);
      FSpaceAttri.AssignColorAndStyle(tmpSpaceAttri);
      FStringAttri.AssignColorAndStyle(tmpStringAttri);
      FSymbolAttri.AssignColorAndStyle(tmpSymbolAttri);
    end;
    tmpAsmAttri.Free;
    tmpCommentAttri.Free;
    tmpIdentAttri.Free;
    tmpKeyAttri.Free;
    tmpNumberAttri.Free;
    tmpSpaceAttri.Free;
    tmpStringAttri.Free;
    tmpSymbolAttri.Free;
  end;

begin
  Result := ReadDelphiSettings(VersionIndex);
end;

function TSynEdit32HighlighterPas.GetSampleSource: UnicodeString;
begin
  Result :=
    '{ Syntax highlighting }'#13#10 +
    'procedure TForm1.Button1Click(Sender: TObject);'#13#10 +
    'var'#13#10 +
    '  Number, I, X: Integer;'#13#10 +
    'begin'#13#10 +
    '  Number := 123456;'#13#10 +
    '  Caption := ''The Number is'' + #32 + IntToStr(Number);'#13#10 +
    '  for I := 0 to Number do'#13#10 +
    '  begin'#13#10 +
    '    Inc(X);'#13#10 +
    '    Dec(X);'#13#10 +
    '    X := X + 1.0;'#13#10 +
    '    X := X - $5E;'#13#10 +
    '  end;'#13#10 +
    '  {$R+}'#13#10 +
    '  asm'#13#10 +
    '    mov AX, 1234H'#13#10 +
    '    mov Number, AX'#13#10 +
    '  end;'#13#10 +
    '  {$R-}'#13#10 +
    'end;';
end;


class function TSynEdit32HighlighterPas.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynEdit32HighlighterPas.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynEdit32HighlighterPas.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

procedure TSynEdit32HighlighterPas.SetDelphiVersion(const Value: TDelphiVersion);
begin
  if FDelphiVersion <> Value then
  begin
    FDelphiVersion := Value;
    if (FDelphiVersion < dvDelphi3) and FPackageSource then
      FPackageSource := False;
    DefHighlightChange(Self);
  end;
end;

procedure TSynEdit32HighlighterPas.SetPackageSource(const Value: Boolean);
begin
  if FPackageSource <> Value then
  begin
    FPackageSource := Value;
    if FPackageSource and (FDelphiVersion < dvDelphi3) then
      FDelphiVersion := dvDelphi3;
    DefHighlightChange(Self);
  end;
end;

class function TSynEdit32HighlighterPas.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPascal;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterPas);
end.
