{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterST.pas, released 2002-07.
ST stands for Structured Text, and it is part of IEC1131 standard for
programming PLCs.
Author of this file is Ruggero Bandera.
Portions created by Ruggero Bandera are Copyright (C) 2002 Ruggero Bandera.
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

$Id: SynHighlighterST.pas,v 1.9.2.6 2008/09/14 16:25:03 maelh Exp $ by Ruggero Bandera

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.ST;

{$I SynEdit32.Inc}

interface

uses
  Windows,
  Controls,
  Graphics,
  SysUtils,
  Classes,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynEdit32HighlighterST = class(TSynEdit32CustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FIdentFuncTable: array[0..210] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FAsmAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
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
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property AsmAttri: TSynEdit32HighlighterAttributes read FAsmAttri write FAsmAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..74] of UnicodeString = (
    'action', 'and', 'any', 'any_num', 'array', 'at', 'bool', 'by', 'byte', 
    'case', 'configuration', 'constant', 'dint', 'do', 'dword', 'else', 'elsif', 
    'end_action', 'end_case', 'end_configuration', 'end_for', 'end_if', 
    'end_repeat', 'end_resource', 'end_step', 'end_struct', 'end_transition', 
    'end_type', 'end_var', 'end_while', 'exit', 'external', 'finally', 'for', 
    'from', 'function', 'goto', 'if', 'index', 'initial_step', 'initialization', 
    'int', 'label', 'not', 'of', 'on', 'or', 'program', 'real', 'repeat', 
    'resource', 'retain', 'return', 'sint', 'step', 'string', 'struct', 'then', 
    'time', 'to', 'transition', 'type', 'udint', 'uint', 'until', 'usint', 
    'var', 'var_external', 'var_global', 'var_in_out', 'var_input', 
    'var_output', 'while', 'word', 'xor' 
  );

  KeyIndices: array[0..210] of Integer = (
    -1, -1, -1, -1, -1, 55, 39, -1, -1, -1, -1, 51, -1, -1, -1, -1, 57, 49, 4, 
    -1, 17, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, -1, -1, 61, -1, -1, -1, 
    47, -1, -1, -1, 58, 70, 38, -1, -1, 35, -1, -1, -1, 28, 12, -1, -1, -1, -1, 
    -1, -1, 64, -1, -1, 1, -1, -1, 69, 27, 45, -1, 2, -1, -1, -1, 3, 9, -1, 37, 
    13, 63, -1, -1, 8, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, -1, -1, -1, 
    10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 18, 25, 20, -1, 53, 14, -1, -1, -1, 
    0, -1, -1, 26, 41, 42, 62, -1, -1, -1, 66, 21, 36, -1, -1, 30, -1, 73, 22, 
    -1, 16, -1, -1, -1, -1, 74, -1, -1, 23, -1, 29, 50, -1, -1, -1, -1, -1, 68, 
    -1, -1, 19, -1, 15, 11, -1, 48, -1, 72, -1, 43, -1, -1, -1, -1, 67, 31, -1, 
    32, -1, -1, 6, -1, -1, 7, 65, -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, 5, -1, 
    40, 52, 34, -1, -1, -1, -1, -1, -1, -1, 56, -1, -1, 44, 54, -1, 71, 46, 59 
  );

{$Q-}
function TSynEdit32HighlighterST.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 381 + Ord(Str^) * 141;
    Inc(Str);
  end;
  Result := Result mod 211;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterST.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterST.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynEdit32HighlighterST.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynEdit32HighlighterST.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynEdit32HighlighterST.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FAsmAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(FAsmAttri);
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  fDefaultFilter := SYNS_FilterST;
end; { Create }

procedure TSynEdit32HighlighterST.AddressOpProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '@' then Inc(FRun);
end;

procedure TSynEdit32HighlighterST.AsciiCharProc;
begin
  FTokenID := tkString;
  Inc(FRun);
  while CharInSet(FLine[FRun], ['0'..'9']) do Inc(FRun);
end;

procedure TSynEdit32HighlighterST.BorProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      FTokenID := tkComment;
      repeat
        if FLine[FRun] = '}' then begin
          Inc(FRun);
          if FRange = rsBorAsm then
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

procedure TSynEdit32HighlighterST.BraceOpenProc;
begin
  if FRange = rsAsm then
    FRange := rsBorAsm
  else
    FRange := rsBor;
  BorProc;
end;

procedure TSynEdit32HighlighterST.ColonOrGreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterST.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterST.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterST.IntegerProc;

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

procedure TSynEdit32HighlighterST.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterST.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '>']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterST.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterST.NumberProc;

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
      '.':
        if FLine[FRun + 1] = '.' then break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterST.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['.', ')']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterST.AnsiProc;
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

procedure TSynEdit32HighlighterST.RoundOpenProc;
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

procedure TSynEdit32HighlighterST.SemicolonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if FRange = rsProperty then
    FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterST.SlashProc;
begin
  Inc(FRun);
  if FLine[FRun] = '/' then begin
    FTokenID := tkComment;
    repeat
      Inc(FRun);
    until IsLineEnd(FRun);
  end else
    FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterST.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterST.StringProc;
begin
  FTokenID := tkString;
  Inc(FRun);
  while not IsLineEnd(FRun) do
  begin
    if FLine[FRun] = #39 then
    begin
      Inc(FRun);
      if FLine[FRun] <> #39 then
        break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterST.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterST.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterST.Next;
begin
  FAsmStart := False;
  fTokenPos := FRun;
  case FRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm:
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

function TSynEdit32HighlighterST.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterST.GetTokenID: TtkTokenKind;
begin
  if not FAsmStart and (FRange = rsAsm)
    and not (FTokenID in [tkNull, tkComment, tkSpace])
  then
    Result := tkAsm
  else
    Result := FTokenID;
end;

function TSynEdit32HighlighterST.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := FAsmAttri;
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterST.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynEdit32HighlighterST.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynEdit32HighlighterST.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterST.ResetRange;
begin
  FRange:= rsUnknown;
end;

class function TSynEdit32HighlighterST.GetLanguageName: string;
begin
  Result := SYNS_LangST;
end;

function TSynEdit32HighlighterST.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterST;
end;

class function TSynEdit32HighlighterST.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangST;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterST);
end.
