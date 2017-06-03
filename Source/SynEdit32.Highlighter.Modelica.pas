{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterModelica.pas, released 2000-11-09.
The Initial Author of this file is Falko Jens Wagner.
Portions created by Falko Jens Wagner are Copyright 2000 Falko Jens Wagner.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterModelica.pas,v 1.12.2.6 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.Modelica;

{$I SynEdit.inc}

interface

uses
  Graphics, Registry, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsString39, rsString34, rsComment);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterModelica = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..96] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDirectiveAttri: TSynEdit32HighlighterAttributes;
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
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure ColonProc;
    procedure DirectiveProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolProcWithEqual;
    procedure UnknownProc;
    procedure AnsiCProc;
    procedure String34Proc;
    procedure String39Proc;
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
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
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
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..47] of UnicodeString = (
    'algorithm', 'and', 'annotation', 'assert', 'block', 'Boolean', 'class', 
    'connect', 'connector', 'constant', 'der', 'discrete', 'else', 'elseif', 
    'end', 'equation', 'extends', 'external', 'false', 'final', 'flow', 'for', 
    'function', 'if', 'in', 'input', 'Integer', 'loop', 'model', 'nondiscrete', 
    'not', 'or', 'output', 'package', 'parameter', 'partial', 'protected', 
    'public', 'Real', 'record', 'redeclare', 'replaceable', 'terminate', 'then', 
    'true', 'type', 'when', 'while' 
  );

  KeyIndices: array[0..96] of Integer = (
    -1, 8, 41, 46, -1, 21, -1, 30, 5, -1, 45, -1, -1, 23, 7, -1, -1, 17, 15, -1, 
    -1, 10, -1, -1, -1, 3, -1, 18, -1, 28, -1, -1, 47, -1, -1, -1, -1, -1, 39, 
    16, 27, 25, -1, 4, 22, -1, 43, -1, 37, 40, -1, -1, 31, -1, 42, -1, -1, 26, 
    14, 24, 44, -1, -1, -1, -1, 11, 33, 0, -1, -1, -1, -1, 36, 19, -1, 38, -1, 
    32, -1, -1, 29, -1, -1, -1, 6, 35, 12, 1, -1, -1, -1, 20, 34, -1, 13, 9, 2 
  );

{$Q-}
function TSynEdit32HighlighterModelica.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 598 + Ord(Str^) * 127;
    Inc(Str);
  end;
  Result := Result mod 97;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterModelica.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterModelica.InitIdent;
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

function TSynEdit32HighlighterModelica.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterModelica.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynEdit32HighlighterModelica.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

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
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterModelica;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterModelica.AndSymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if CharInSet(FLine[FRun], ['=', '&']) then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.AsciiCharProc;
begin
  FRange := rsString39;
  FTokenID := tkString;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = #39);
  if FLine[FRun] = #39 then
  begin
    FRange := rsUnknown;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterModelica.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.ColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if FLine[FRun] = ':' then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.DirectiveProc;
begin
  FTokenID := tkDirective;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterModelica.GreaterProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  case FLine[FRun] of
    '=': Inc(FRun);
    '>': begin
           Inc(FRun);
           if FLine[FRun] = '=' then
             Inc(FRun);
         end;
  end;
end;

procedure TSynEdit32HighlighterModelica.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.LowerProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  case FLine[FRun] of
    '=': Inc(FRun);
    '<': begin
           Inc(FRun);
           if FLine[FRun] = '=' then
             Inc(FRun);
         end;
  end;
end;

procedure TSynEdit32HighlighterModelica.MinusProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if CharInSet(FLine[FRun], ['=', '-', '>']) then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.NumberProc;

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

procedure TSynEdit32HighlighterModelica.OrSymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if CharInSet(FLine[FRun], ['=', '|']) then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.PlusProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if CharInSet(FLine[FRun], ['=', '+']) then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.PointProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if (FLine[FRun] = '.') and (FLine[FRun + 1] = '.') then
    Inc(FRun, 2);
end;

procedure TSynEdit32HighlighterModelica.SlashProc;
begin
  Inc(FRun);
  case FLine[FRun] of
    '/':
      begin
        FTokenID := tkComment;
        repeat
          Inc(FRun);
        until IsLineEnd(FRun);
      end;
    '*':
      begin
        FRange := rsComment;
        Inc(FRun);
        if IsLineEnd(FRun) then
          FTokenID := tkComment
        else
          AnsiCProc;
      end;
  else
    FTokenID := tkSymbol;
    if FLine[FRun] = '=' then
      Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterModelica.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(FRun);
  until (FLine[FRun] > #32) or IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterModelica.StringProc;
begin
  FRange := rsString34;
  Inc(FRun);
  if IsLineEnd(FRun) then
    FTokenID := tkString
  else
    String34Proc;
end;

procedure TSynEdit32HighlighterModelica.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterModelica.SymbolProcWithEqual;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if FLine[FRun] = '=' then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterModelica.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterModelica.AnsiCProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then begin
        Inc(FRun, 2);
        FRange := rsUnknown;
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterModelica.String39Proc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkString;
    repeat
      if FLine[FRun] = #39 then begin
        Inc(FRun);
        FRange := rsUnknown;
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterModelica.String34Proc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkString;
    repeat
      case FLine[FRun] of
        #34:
          begin
            Inc(FRun);
            FRange := rsUnknown;
            break;
          end;
        #92:
          begin
            Inc(FRun);
            if FLine[FRun] = #34 then
              Inc(FRun);
          end;
      else
        Inc(FRun);
      end;
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterModelica.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsComment: AnsiCProc;
    rsString39: String39Proc;
    rsString34: String34Proc;
  else
    FRange := rsUnknown;
    case FLine[FRun] of
      '&': AndSymbolProc;
      #39: AsciiCharProc;
      #13: CRProc;
      ':': ColonProc;
      '#': DirectiveProc;
      '>': GreaterProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      #10: LFProc;
      '<': LowerProc;
      '-': MinusProc;
      #0: NullProc;
      '0'..'9': NumberProc;
      '|': OrSymbolProc;
      '+': PlusProc;
      '.': PointProc;
      '/': SlashProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      #34: StringProc;
      '~', '[', ']', '@', '{', '}', '(', ')', ';', ',': SymbolProc;
      '*', '^', '=', '%', '!': SymbolProcWithEqual;
      else UnknownProc;
    end;
  end;
  inherited;
end;

function TSynEdit32HighlighterModelica.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterModelica.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterModelica.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterModelica.GetTokenAttribute: TSynEdit32HighlighterAttributes;
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
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterModelica.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterModelica.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterModelica.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterModelica.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterModelica;
end;

class function TSynEdit32HighlighterModelica.GetLanguageName: string;
begin
  Result := SYNS_LangModelica;
end;

class function TSynEdit32HighlighterModelica.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangModelica;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterModelica);
end.
