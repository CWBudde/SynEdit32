{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterBaan.pas, released 2000-04-21.
The Original Code is based on the mwBaanSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is "riceball".
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

$Id: SynHighlighterBaan.pas,v 1.13.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Baan syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-04-21)
The SynHighlighterBaan unit provides SynEdit with a Baan syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynEdit32.Highlighter.Baan;

{$I SynEdit.Inc}

interface

uses
  Windows, Messages, Controls, Graphics, Registry,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils, Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterBaan = class(TSynEdit32CustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..460] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDirectiveAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FVariableAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncBrp46open(Index: Integer): TtkTokenKind;
    function FuncDate46num(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure ErectProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
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
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirectiveAttri: TSynEdit32HighlighterAttributes read FDirectiveAttri
      write FDirectiveAttri;
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
    property VariableAttri: TSynEdit32HighlighterAttributes read FVariableAttri
      write FVariableAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..112] of UnicodeString = (
    '__based', '__cdecl', '__declspe', '__except', '__export', '__far', 
    '__fastcal', '__fortran', '__import', '__int16', '__int32', '__int64', 
    '__int8', '__interrup', '__loadds', '__near', '__pascal', '__rtti', 
    '__segment', '__segname', '__self', '__stdcall', '__thread', '__try', 
    '_cdecl', '_export', '_fastcall', '_import', '_pascal', '_stdcall', 'auto', 
    'bool', 'break', 'brp.open', 'case', 'catch', 'cdecl', 'char', 'class', 
    'const', 'continue', 'date.num', 'default', 'defined', 'delete', 'do', 
    'domain', 'double', 'else', 'endif', 'endselect', 'enum', 'explicit', 
    'export', 'extern', 'false', 'fastcall', 'finally', 'float', 'for', 
    'friend', 'from', 'function', 'goto', 'if', 'import', 'inline', 'int', 
    'interrupt', 'long', 'mutable', 'namespace', 'new', 'null', 'operator', 
    'pascal', 'private', 'protected', 'public', 'register', 'reinterpr', 
    'return', 'select', 'selectdo', 'short', 'signed', 'sizeof', 'sql.close', 
    'static', 'static_ca', 'stdcall', 'string', 'strip$', 'struct', 'switch', 
    'table', 'template', 'this', 'throw', 'true', 'try', 'typedef', 'typeid', 
    'typename', 'union', 'unsigned', 'using', 'virtual', 'void', 'volatile', 
    'wchar_t', 'where', 'while' 
  );

  KeyIndices: array[0..460] of Integer = (
    -1, -1, -1, -1, -1, -1, 83, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 3, 33, 26, -1, 78, -1, -1, -1, -1, -1, 5, -1, 14, -1, 27, -1, 92, -1, 
    -1, -1, -1, 42, -1, 77, -1, -1, -1, -1, -1, -1, -1, -1, -1, 61, -1, -1, -1, 
    93, 2, -1, -1, -1, 50, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, 63, -1, 94, 
    -1, -1, 69, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, 44, -1, -1, 
    -1, 110, -1, -1, 51, -1, -1, -1, -1, 56, -1, 32, -1, -1, 109, -1, -1, -1, 
    -1, 16, -1, -1, -1, -1, 23, 88, -1, -1, 10, -1, -1, -1, -1, 67, -1, -1, -1, 
    72, 81, -1, -1, -1, 82, 24, -1, -1, -1, -1, -1, -1, -1, -1, 79, -1, -1, 64, 
    21, 80, -1, -1, 59, 0, -1, -1, -1, 12, -1, -1, 107, -1, 36, -1, -1, -1, -1, 
    31, -1, -1, -1, 62, -1, -1, 112, -1, -1, -1, -1, -1, -1, 7, -1, 106, -1, -1, 
    -1, -1, -1, -1, -1, -1, 52, 104, -1, 18, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 65, -1, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 29, 28, 43, -1, 20, -1, -1, -1, 38, -1, -1, -1, -1, 
    -1, 103, -1, 70, 87, -1, -1, -1, 85, -1, 74, -1, -1, -1, -1, -1, 35, 39, -1, 
    -1, 97, 53, -1, -1, -1, -1, -1, -1, -1, 84, -1, 95, -1, -1, -1, -1, -1, -1, 
    -1, 100, 98, -1, -1, -1, -1, -1, -1, -1, -1, 111, 73, -1, 47, -1, -1, -1, 
    -1, -1, -1, -1, 105, -1, -1, -1, -1, -1, 66, 86, -1, -1, -1, -1, -1, -1, -1, 
    -1, 34, -1, -1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 37, 55, -1, 
    -1, -1, 89, -1, 11, -1, -1, -1, 19, -1, -1, -1, -1, 90, -1, 102, 54, -1, -1, 
    45, -1, -1, 6, 30, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, 8, 22, -1, 
    -1, -1, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, -1, -1, -1, -1, 
    -1, -1, 71, -1, -1, -1, -1, -1, 96, 48, -1, -1, -1, -1, -1, 75, -1, 60, -1, 
    -1, 58, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 17, 4, -1, -1, -1, -1, 
    49, -1, -1, -1, -1, 57, -1, -1, -1, -1, 15, 91, -1, -1, 41, -1, -1, -1, 76, 
    68, -1, -1, -1, 108, -1, -1 
  );

{$Q-}
function TSynEdit32HighlighterBaan.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 838 + Ord(Str^) * 296;
    Inc(Str);
  end;
  Result := Result mod 461;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterBaan.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterBaan.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[21] := FuncBrp46open;
  FIdentFuncTable[449] := FuncDate46num;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynEdit32HighlighterBaan.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterBaan.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynEdit32HighlighterBaan.FuncBrp46open(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterBaan.FuncDate46num(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterBaan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FDirectiveAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  AddAttribute(FDirectiveAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FVariableAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(FVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterBaan;
end;

procedure TSynEdit32HighlighterBaan.AndSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {and assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '&':                               {logical and}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {and}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    case FLine[FRun] of
      #0, #10, #13: break;
    end;
    Inc(FRun);
  until FLine[FRun] = #39;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterBaan.AtSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterBaan.BraceCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.BraceOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
  else
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterBaan.ColonProc;
begin
  case FLine[FRun + 1] of
    ':':                               {scope resolution operator}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {colon}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.DirectiveProc;
begin
  FTokenID := tkDirective;
  repeat
    case FLine[FRun] of
      #0, #10, #13: break;
    end;
    Inc(FRun);
  until FLine[FRun] = #0;
end;

procedure TSynEdit32HighlighterBaan.EqualProc;
begin
  case FLine[FRun + 1] of
    '=':                               {logical equal}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.ErectProc;
begin
  Inc(FRun, 1);                        {Bann Comments}
  FTokenID := tkComment;
  while FLine[FRun] <> #0 do
  begin
    case FLine[FRun] of
      #10, #13: break;
    end; //case
    Inc(FRun);
  end; //while
end;

procedure TSynEdit32HighlighterBaan.GreaterProc;
begin
  case FLine[FRun + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '>':
      begin
        if FLine[FRun + 2] = '=' then   {shift right assign}
          Inc(FRun, 3)
        else                           {shift right}
          Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.IdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterBaan.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterBaan.LowerProc;
begin
  case FLine[FRun + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[FRun + 2] = '=' then   {shift left assign}
          Inc(FRun, 3)
        else                           {shift left}
          Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.MinusProc;
begin
  case FLine[FRun + 1] of
    '=':                               {subtract assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '>':                               {arrow}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.ModSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {mod assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {mod}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.NotSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {not equal}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {not}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterBaan.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F':
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

procedure TSynEdit32HighlighterBaan.PlusProc;
begin
  case FLine[FRun + 1] of
    '=':                               {add assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.SlashProc;
begin
  case FLine[FRun + 1] of
    '=':                               {division assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterBaan.SquareCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.SquareOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.StarProc;
begin
  case FLine[FRun + 1] of
    '=':                               {multiply assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {star}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: break;
      #92:
        if FLine[FRun + 1] = #10 then Inc(FRun);
    end;
    Inc(FRun);
  until FLine[FRun] = #34;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterBaan.TildeProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterBaan.XOrSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {xor assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterBaan.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterBaan.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
    '&': AndSymbolProc;
    #39: AsciiCharProc;
    '@': AtSymbolProc;
    '}': BraceCloseProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '#': DirectiveProc;
    '=': EqualProc;
    '|': ErectProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_', '.', '$': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '%': ModSymbolProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '+': PlusProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    '*': StarProc;
    #34: StringProc;
    '~': TildeProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynEdit32HighlighterBaan.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterBaan.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterBaan.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirectiveAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterBaan.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterBaan.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterBaan;
end;

function TSynEdit32HighlighterBaan.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '.', '$', '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynEdit32HighlighterBaan.GetLanguageName: string;
begin
  Result := SYNS_LangBaan;
end;

class function TSynEdit32HighlighterBaan.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangBaan;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterBaan);
end.
