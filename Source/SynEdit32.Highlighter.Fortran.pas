{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterFortran.pas, released 2000-04-21.
The Original Code is based on the mwFortranSyn.pas file from the
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

$Id: SynHighlighterFortran.pas,v 1.15.2.9 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Fortran syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterFortran unit provides SynEdit with a Fortran syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynEdit32.Highlighter.Fortran;

{$I SynEdit.Inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterFortran = class(TSynEdit32CustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..192] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure ExclamationProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure CommentProc;
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
    procedure Next; override;
  published
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
  KeyWords: array[0..69] of UnicodeString = (
    'allocatable', 'allocate', 'allocated', 'associated', 'call', 'case', 
    'character', 'close', 'common', 'complex', 'contains', 'continue', 'cycle', 
    'data', 'deallocate', 'default', 'define', 'dimension', 'do', 'else', 
    'elseif', 'elsewhere', 'end', 'enddo', 'endif', 'entry', 'equivalence', 
    'exit', 'external', 'forall', 'format', 'function', 'if', 'implicit', 
    'include', 'integer', 'interface', 'logical', 'map', 'module', 'namelist', 
    'nullify', 'open', 'optional', 'parameter', 'pause', 'pointer', 'print', 
    'private', 'program', 'public', 'pure', 'read', 'real', 'record', 'return', 
    'save', 'select', 'stop', 'subroutine', 'target', 'then', 'type', 'union', 
    'use', 'value', 'volatile', 'where', 'while', 'write' 
  );

  KeyIndices: array[0..192] of Integer = (
    8, -1, -1, -1, -1, 11, -1, -1, -1, 31, 2, -1, -1, 59, -1, -1, -1, -1, -1, 
    13, 55, -1, -1, -1, 65, -1, 38, 54, 40, 10, 37, -1, -1, 25, -1, -1, 5, -1, 
    -1, -1, -1, -1, -1, 4, -1, -1, 21, -1, -1, 49, -1, -1, -1, -1, 9, -1, -1, 
    27, -1, 22, -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, 64, -1, -1, 53, 68, -1, 
    34, -1, -1, 69, 30, -1, -1, -1, 32, -1, -1, -1, 19, 16, -1, -1, -1, -1, -1, 
    -1, -1, 62, -1, -1, -1, -1, -1, -1, 36, 60, 14, -1, -1, 66, 29, -1, -1, -1, 
    -1, 24, -1, 67, -1, 15, -1, -1, -1, -1, -1, -1, 44, 35, -1, -1, 46, -1, 17, 
    -1, -1, 28, -1, 56, 61, -1, -1, 63, 45, 18, -1, 0, 20, -1, -1, -1, -1, -1, 
    -1, 42, -1, 50, 3, 58, 52, -1, -1, -1, 51, -1, 48, -1, -1, -1, -1, -1, -1, 
    -1, -1, 12, 23, -1, 26, 1, -1, 41, 43, -1, -1, -1, 33, 7, -1, -1, -1, 47, 
    39, 57, -1 
  );

{$Q-}
function TSynEdit32HighlighterFortran.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 294 + Ord(Str^) * 110;
    Inc(Str);
  end;
  Result := Result mod 193;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterFortran.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterFortran.InitIdent;
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

function TSynEdit32HighlighterFortran.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterFortran.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynEdit32HighlighterFortran.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
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
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterFortran;
end;

procedure TSynEdit32HighlighterFortran.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
    end;
    Inc(FRun);
  until FLine[FRun] = #39;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterFortran.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterFortran.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.EqualProc;
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

procedure TSynEdit32HighlighterFortran.ExclamationProc;
begin
  Inc(FRun, 1);                        {Fortran Comments}
  FTokenID := tkComment;
  while FLine[FRun] <> #0 do
  begin
    case FLine[FRun] of
      #10, #13: Break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterFortran.GreaterProc;
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

procedure TSynEdit32HighlighterFortran.IdentProc;
begin
  if CharInSet(FLine[FRun], ['C', 'c']) and (FRun = 0) then
  begin   //Fortran comments
    Inc(FRun, 1);
    CommentProc;
  end
  else begin
    FTokenID := IdentKind(FLine + FRun);
    Inc(FRun, FStringLen);
    while IsIdentChar(FLine[FRun]) do Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterFortran.LFProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
end;

procedure TSynEdit32HighlighterFortran.LowerProc;
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

procedure TSynEdit32HighlighterFortran.MinusProc;
begin
  {subtract}
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.ModSymbolProc;
begin
  {mod}
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterFortran.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'x', 'X', 'e', 'E', 'f', 'F':
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

procedure TSynEdit32HighlighterFortran.PlusProc;
begin
  {subtract}
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.PointProc;
begin
  if (((SynWideUpperCase(FLine[FRun + 1]) = 'G') and CharInSet(SynWideUpperCase(FLine[FRun + 2])[1], ['E', 'T'])) {.ge. .gt.}
       or ((SynWideUpperCase(FLine[FRun + 1]) = 'L') and CharInSet(SynWideUpperCase(FLine[FRun + 2])[1], ['E', 'T'])) {.le. .lt.}
       or ((SynWideUpperCase(FLine[FRun + 1]) = 'N') and (SynWideUpperCase(FLine[FRun + 2]) = 'E')) {.ne.}
       or ((SynWideUpperCase(FLine[FRun + 1]) = 'E') and (SynWideUpperCase(FLine[FRun + 2]) = 'Q')) {.eq.}
       or ((SynWideUpperCase(FLine[FRun + 1]) = 'O') and (SynWideUpperCase(FLine[FRun + 2]) = 'R'))){.or.}
     and (FLine[FRun + 3] = '.') then
    begin
      Inc(FRun, 4);
      FTokenID := tkSymbol;
    end
  else if (((SynWideUpperCase(FLine[FRun + 1]) = 'A')
              and (SynWideUpperCase(FLine[FRun + 2]) = 'N')
              and (SynWideUpperCase(FLine[FRun + 3]) = 'D'))    {.and.}
           or ((SynWideUpperCase(FLine[FRun + 1]) = 'N')
              and (SynWideUpperCase(FLine[FRun + 2]) = 'O')
              and (SynWideUpperCase(FLine[FRun + 3]) = 'T')))    {.not.}
          and (FLine[FRun + 4] = '.') then
    begin
      Inc(FRun, 5);
      FTokenID := tkSymbol;
    end
  else if (SynWideUpperCase(FLine[FRun + 1]) = 'T')
          and (SynWideUpperCase(FLine[FRun + 2]) = 'R')
          and (SynWideUpperCase(FLine[FRun + 3]) = 'U')
          and (SynWideUpperCase(FLine[FRun + 4]) = 'E')
          and (FLine[FRun + 5] = '.') then  {.true.}
    begin
      Inc(FRun, 6);
      FTokenID := tkSymbol;
    end
  else if (SynWideUpperCase(FLine[FRun + 1]) = 'F')
          and (SynWideUpperCase(FLine[FRun + 2]) = 'A')
          and (SynWideUpperCase(FLine[FRun + 3]) = 'L')
          and (SynWideUpperCase(FLine[FRun + 4]) = 'S')
          and (SynWideUpperCase(FLine[FRun + 5]) = 'E')
          and (FLine[FRun + 6] = '.') then  {.false.}
    begin
      Inc(FRun, 7);
      FTokenID := tkSymbol;
    end
  else                                 {point}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
end;

procedure TSynEdit32HighlighterFortran.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.SlashProc;
begin
  {division}
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterFortran.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterFortran.StarProc;
begin
  if (FRun = 0) then begin   //Fortran comments
    Inc(FRun);
    CommentProc;
  end
  else begin
    {star}
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterFortran.CommentProc;
begin
  FTokenID := tkComment;
  while FLine[FRun] <> #0 do
  begin
    case FLine[FRun] of
      #10, #13: Break;
    end; //case
    Inc(FRun);
  end; //while
end;

procedure TSynEdit32HighlighterFortran.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
      #92:
        if FLine[FRun + 1] = #10 then Inc(FRun);
    end;
    Inc(FRun);
  until FLine[FRun] = #34;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterFortran.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterFortran.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
    #39: AsciiCharProc;
    #13: CRProc;
    ',': CommaProc;
    '=': EqualProc;
    '!': ExclamationProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '%': ModSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '+': PlusProc;
    '.': PointProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '*': StarProc;
    #34: StringProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynEdit32HighlighterFortran.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterFortran.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterFortran.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterFortran.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterFortran.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterFortran;
end;

class function TSynEdit32HighlighterFortran.GetLanguageName: string;
begin
  Result := SYNS_LangFortran;
end;

class function TSynEdit32HighlighterFortran.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangFortran;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterFortran);
end.
