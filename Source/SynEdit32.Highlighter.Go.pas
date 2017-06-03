{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: C:\Users\Public\Code\SynEdit\SynGen\Test GO\SynHighlighterGo.pas, released 2017-06-01.
Description: Syntax Parser/Highlighter
The initial author of this file is Christian-W. Budde.
Copyright (c) 2017, all rights reserved.

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

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERGO}
unit SynEdit32.Highlighter.Go;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkFloat,
    tkKey,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

  TRangeState = (rsUnknown, rsSingleString, rsDoubleString, rsExtraString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterGo = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..88] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function FuncBool(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncChan(Index: Integer): TtkTokenKind;
    function FuncComplex128(Index: Integer): TtkTokenKind;
    function FuncComplex64(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDefer(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncFallthrough(Index: Integer): TtkTokenKind;
    function FuncFloat32(Index: Integer): TtkTokenKind;
    function FuncFloat64(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncFunc(Index: Integer): TtkTokenKind;
    function FuncGo(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncImport(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncInt16(Index: Integer): TtkTokenKind;
    function FuncInt32(Index: Integer): TtkTokenKind;
    function FuncInt64(Index: Integer): TtkTokenKind;
    function FuncInt8(Index: Integer): TtkTokenKind;
    function FuncInterface(Index: Integer): TtkTokenKind;
    function FuncMap(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncRange(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncRune(Index: Integer): TtkTokenKind;
    function FuncSelect(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStruct(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncType(Index: Integer): TtkTokenKind;
    function FuncUint(Index: Integer): TtkTokenKind;
    function FuncUint16(Index: Integer): TtkTokenKind;
    function FuncUint32(Index: Integer): TtkTokenKind;
    function FuncUint64(Index: Integer): TtkTokenKind;
    function FuncUint8(Index: Integer): TtkTokenKind;
    function FuncUintptr(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc(Index: Integer): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure BlockCommentProc;
    procedure CRProc;
    procedure LFProc;
    procedure LineCommentProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri write FNumberAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEdit32.StrConst;
{$ENDIF}

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..43] of UnicodeString = (
    'bool', 'break', 'byte', 'case', 'chan', 'complex128', 'complex64', 'const', 
    'continue', 'default', 'defer', 'else', 'fallthrough', 'float32', 'float64', 
    'for', 'func', 'go', 'goto', 'if', 'import', 'int', 'int16', 'int32', 
    'int64', 'int8', 'interface', 'map', 'package', 'range', 'return', 'rune', 
    'select', 'string', 'struct', 'switch', 'type', 'uint', 'uint16', 'uint32', 
    'uint64', 'uint8', 'uintptr', 'var' 
  );

  KeyIndices: array[0..88] of Integer = (
    29, 24, -1, -1, -1, 7, -1, 37, 11, -1, 22, 14, -1, 21, 1, -1, -1, -1, 39, 
    31, -1, -1, -1, 28, -1, -1, 17, -1, -1, -1, 32, 30, 26, 41, 23, -1, 34, -1, 
    -1, -1, -1, -1, -1, 9, 13, 20, -1, 35, -1, 3, -1, -1, 15, -1, -1, 25, -1, 
    27, 4, 6, -1, 5, -1, -1, 10, 2, 16, -1, -1, -1, 43, -1, -1, 8, 40, 36, 33, 
    -1, -1, -1, -1, 18, 19, 38, -1, 42, 12, -1, 0 
  );

procedure TSynEdit32HighlighterGo.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[88] := FuncBool;
  FIdentFuncTable[14] := FuncBreak;
  FIdentFuncTable[65] := FuncByte;
  FIdentFuncTable[49] := FuncCase;
  FIdentFuncTable[58] := FuncChan;
  FIdentFuncTable[61] := FuncComplex128;
  FIdentFuncTable[59] := FuncComplex64;
  FIdentFuncTable[5] := FuncConst;
  FIdentFuncTable[73] := FuncContinue;
  FIdentFuncTable[43] := FuncDefault;
  FIdentFuncTable[64] := FuncDefer;
  FIdentFuncTable[8] := FuncElse;
  FIdentFuncTable[86] := FuncFallthrough;
  FIdentFuncTable[44] := FuncFloat32;
  FIdentFuncTable[11] := FuncFloat64;
  FIdentFuncTable[52] := FuncFor;
  FIdentFuncTable[66] := FuncFunc;
  FIdentFuncTable[26] := FuncGo;
  FIdentFuncTable[81] := FuncGoto;
  FIdentFuncTable[82] := FuncIf;
  FIdentFuncTable[45] := FuncImport;
  FIdentFuncTable[13] := FuncInt;
  FIdentFuncTable[10] := FuncInt16;
  FIdentFuncTable[34] := FuncInt32;
  FIdentFuncTable[1] := FuncInt64;
  FIdentFuncTable[55] := FuncInt8;
  FIdentFuncTable[32] := FuncInterface;
  FIdentFuncTable[57] := FuncMap;
  FIdentFuncTable[23] := FuncPackage;
  FIdentFuncTable[0] := FuncRange;
  FIdentFuncTable[31] := FuncReturn;
  FIdentFuncTable[19] := FuncRune;
  FIdentFuncTable[30] := FuncSelect;
  FIdentFuncTable[76] := FuncString;
  FIdentFuncTable[36] := FuncStruct;
  FIdentFuncTable[47] := FuncSwitch;
  FIdentFuncTable[75] := FuncType;
  FIdentFuncTable[7] := FuncUint;
  FIdentFuncTable[83] := FuncUint16;
  FIdentFuncTable[18] := FuncUint32;
  FIdentFuncTable[74] := FuncUint64;
  FIdentFuncTable[33] := FuncUint8;
  FIdentFuncTable[85] := FuncUintptr;
  FIdentFuncTable[70] := FuncVar;
end;

{$Q-}
function TSynEdit32HighlighterGo.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 345 + Ord(Str^) * 670;
    Inc(Str);
  end;
  Result := Result mod 89;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterGo.FuncBool(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncChan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncComplex128(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncComplex64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncDefer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncFallthrough(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncFloat32(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncFloat64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncGo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncImport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncInt16(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncInt32(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncInt64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncInt8(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncInterface(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncMap(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncRange(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncRune(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncSelect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncStruct(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncType(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncUint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncUint16(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncUint32(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncUint64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncUint8(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncUintptr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterGo.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterGo.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterGo.StringProc;
var
  StringChar: Char;
begin
  FTokenID := tkString;
  StringChar := fLine[FRun];
  repeat
    Inc(FRun);
  until IsLineEnd(FRun) or (fLine[FRun] = StringChar);

  if FLine[FRun] = #34 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterGo.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterGo.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if fLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterGo.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterGo.SlashProc;
begin
  case FLine[FRun + 1] of
    '/':
      begin
        FTokenID := tkComment;
        Inc(FRun, 2);
        while not IsLineEnd(FRun) do Inc(FRun);
      end;
    '*':
      begin
        FTokenID := tkComment;
        Inc(FRun, 2);
        while fLine[FRun] <> #0 do
          case fLine[FRun] of
            '*':
              if fLine[FRun + 1] = '/' then
              begin
                Inc(FRun, 2);
                break;
              end else Inc(FRun);
            #10, #13:
                break;
          else Inc(FRun);
          end;
      end;
    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterGo.BlockCommentProc;
begin
  case fLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (fLine[FRun] = '*') and
           (fLine[FRun + 1] = '/') then
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

procedure TSynEdit32HighlighterGo.LineCommentProc;
begin
  FTokenID := tkComment;
  repeat
//    if  then
    begin
      Inc(FRun, 0);
      FRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

constructor TSynEdit32HighlighterGo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := $666666;
  AddAttribute(FNumberAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := $214195;
  AddAttribute(FKeyAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := $619121;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := $666666;
  AddAttribute(FSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterGo;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterGo.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[FRun] of
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
    case fLine[FRun] of
      '.':
        if fLine[FRun + 1] = '.' then
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

procedure TSynEdit32HighlighterGo.IdentProc;
begin
  FTokenID := IdentKind((fLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(fLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterGo.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterGo.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterGo.Next;
begin
  fTokenPos := FRun;
  case fLine[FRun] of
    #0:
      NullProc;
    #10:
      LFProc;
    #13:
      CRProc;
    '/':
      SlashProc;
    #34:
      StringProc;
    #39:
      StringProc;
    #180:
      StringProc;
    #1..#9, #11, #12, #14..#32:
      SpaceProc;
    '0'..'9':
      NumberProc;
    'A'..'Z', 'a'..'z', '_':
      IdentProc;
    ':', '=', '+', '-', '.', ',':
      SymbolProc;
    '(', ')', '[', ']', '{', '}':
      UnknownProc;
  else
    UnknownProc;
  end;

  inherited;
end;

function TSynEdit32HighlighterGo.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterGo.GetEol: Boolean;
begin
  Result := FRun = fLineLen + 1;
end;

function TSynEdit32HighlighterGo.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := 
    'bool,break,byte,case,chan,complex128,complex64,const,continue,default' +
    ',defer,else,fallthrough,float32,float64,for,func,go,goto,if,import,int' +
    ',int16,int32,int64,int8,interface,map,package,range,return,rune,select' +
    ',string,struct,switch,type,uint,uint16,uint32,uint64,uint8,uintptr,var';
end;

function TSynEdit32HighlighterGo.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterGo.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment:
      Result := FCommentAttri;
    tkIdentifier:
      Result := FIdentifierAttri;
    tkNumber, tkFloat:
      Result := FNumberAttri;
    tkKey:
      Result := FKeyAttri;
    tkSpace:
      Result := FSpaceAttri;
    tkString:
      Result := FStringAttri;
    tkSymbol:
      Result := FSymbolAttri;
    tkUnknown:
      Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterGo.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterGo.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynEdit32HighlighterGo.GetSampleSource: UnicodeString;
begin
  Result := 
    #13#10 +
    '/* Sample source for the go highlighter */'#13#10 +
    #13#10 +
    'package main'#13#10 +
    #13#10 +
    'import "fmt"'#13#10 +
    #13#10 +
    'func main() {'#13#10 +
    '  fmt.Println("hello world")'#13#10+'}';
end;

function TSynEdit32HighlighterGo.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGo;
end;

class function TSynEdit32HighlighterGo.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGo;
end;

class function TSynEdit32HighlighterGo.GetLanguageName: string;
begin
  Result := SYNS_LangGo;
end;

procedure TSynEdit32HighlighterGo.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterGo.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterGo.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynEdit32HighlighterGo);
{$ENDIF}
end.
