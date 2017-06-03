{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCAC.pas, released 2000-04-21.
The Original Code is based on the cwCACSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Carlos Wijders.
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

$Id: SynHighlighterCAC.pas,v 1.10.2.8 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a CA-Clipper syntax highlighter for SynEdit)
@author(Carlos Wijders <ctfbs@sr.net>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1998-12-27, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterCAC unit provides SynEdit with a CA-Clipper syntax highlighter.
Thanks to Primoz Gabrijelcic, Andy Jeffries.
}

unit SynEdit32.Highlighter.CAC;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkOperator, tkUnknown);

  TRangeState = (rsANil, rsCStyle, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynEdit32HighlighterCAC = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FOperatorAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FDirecAttri: TSynEdit32HighlighterAttributes;
    FIdentFuncTable: array[0..708] of TIdentFuncTableFunc;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure StarProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure StringProc;
    procedure DirectiveProc;
    procedure UnknownProc;
    procedure CStyleProc;
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
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property OperatorAttri: TSynEdit32HighlighterAttributes read FOperatorAttri
      write FOperatorAttri;
    property DirecAttri: TSynEdit32HighlighterAttributes read FDirecAttri
      write FDirecAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..142] of UnicodeString = (
    'aadd', 'abs', 'and', 'announce', 'asc', 'at', 'average', 'begin', 'bof', 
    'break', 'call', 'cancel', 'cdow', 'chr', 'clear', 'close', 'cmonth', 'col', 
    'commit', 'continue', 'copy', 'count', 'create', 'ctod', 'date', 'day', 
    'declare', 'delete', 'deleted', 'devpos', 'dir', 'display', 'dow', 'dtoc', 
    'dtos', 'eject', 'else', 'elseif', 'empty', 'endcase', 'enddo', 'endif', 
    'eof', 'erase', 'exit', 'exp', 'external', 'fcount', 'field', 'fieldname', 
    'file', 'find', 'flock', 'for', 'found', 'function', 'get', 'go', 'if', 
    'iif', 'index', 'init', 'inkey', 'input', 'int', 'join', 'keyboard', 
    'lastrec', 'len', 'list', 'local', 'locate', 'lock', 'log', 'lower', 
    'ltrim', 'max', 'memvar', 'min', 'month', 'not', 'note', 'or', 'pack', 
    'parameters', 'pcol', 'pcount', 'private', 'procedure', 'prompt', 'prow', 
    'public', 'quit', 'read', 'recall', 'reccount', 'recno', 'reindex', 
    'release', 'rename', 'replace', 'replicate', 'request', 'restore', 'return', 
    'rlock', 'round', 'row', 'rtrim', 'run', 'save', 'say', 'seconds', 'seek', 
    'select', 'sequence', 'setpos', 'skip', 'sort', 'space', 'sqrt', 'static', 
    'store', 'str', 'substr', 'sum', 'text', 'time', 'total', 'transform', 
    'trim', 'type', 'unlock', 'update', 'upper', 'use', 'val', 'valtype', 
    'wait', 'while', 'word', 'year', 'zap' 
  );

  KeyIndices: array[0..708] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 138, 87, 41, 140, 88, -1, -1, -1, 11, 
    -1, -1, -1, 53, -1, -1, -1, -1, 54, -1, 111, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 110, -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, 24, -1, 86, -1, 
    -1, -1, 81, -1, -1, -1, -1, -1, 119, -1, -1, 14, -1, -1, -1, 92, -1, -1, -1, 
    -1, -1, 77, 89, 10, 23, -1, -1, 91, 65, -1, 122, -1, -1, -1, 36, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, -1, 27, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 120, -1, 100, 2, -1, -1, -1, -1, 75, 7, -1, -1, 
    -1, -1, -1, -1, -1, 108, 99, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 137, -1, -1, -1, -1, -1, -1, -1, -1, 50, 30, -1, 
    -1, -1, -1, 83, 116, -1, -1, 134, -1, -1, 69, -1, -1, -1, 109, -1, 76, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, -1, -1, -1, -1, 
    -1, -1, -1, 85, -1, -1, -1, 127, -1, -1, 102, 48, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 90, -1, -1, -1, -1, -1, -1, -1, -1, 74, -1, -1, -1, -1, 133, 
    -1, 57, 113, -1, -1, -1, -1, -1, -1, 43, -1, 33, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 126, -1, 132, -1, -1, -1, -1, -1, -1, -1, 80, -1, -1, -1, 
    58, -1, -1, -1, -1, -1, -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, 98, -1, 49, 
    123, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, 15, -1, -1, -1, 
    -1, -1, -1, -1, -1, 103, -1, -1, -1, -1, -1, 5, 82, -1, -1, -1, -1, -1, 35, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, 72, 
    -1, -1, -1, -1, -1, -1, -1, 19, 63, -1, 52, -1, -1, -1, -1, -1, 34, -1, -1, 
    -1, -1, -1, -1, -1, 13, -1, -1, -1, 105, -1, -1, -1, -1, -1, -1, 39, -1, -1, 
    -1, 118, -1, -1, -1, -1, -1, 121, 3, 115, -1, -1, 64, -1, -1, 60, -1, 114, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 47, -1, -1, -1, -1, -1, 20, -1, -1, 
    62, -1, -1, -1, -1, -1, -1, -1, -1, -1, 135, -1, -1, -1, -1, 22, -1, -1, -1, 
    -1, -1, 55, -1, 68, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 17, 94, 112, -1, 
    -1, -1, -1, 59, -1, -1, 21, -1, -1, 66, -1, -1, -1, -1, -1, 107, 28, -1, -1, 
    -1, -1, -1, -1, -1, 96, -1, -1, -1, 56, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, 
    -1, -1, 9, -1, -1, -1, -1, 104, -1, -1, -1, 42, -1, -1, -1, -1, 79, 18, 70, 
    -1, 26, 25, 32, -1, -1, 0, 37, -1, 40, -1, -1, -1, -1, 73, -1, 97, -1, -1, 
    -1, 67, 128, -1, -1, -1, -1, -1, -1, 136, 16, 12, -1, -1, -1, -1, -1, -1, 
    131, 117, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, -1, -1, 51, -1, 1, -1, 
    -1, -1, -1, -1, 141, -1, 129, -1, 44, -1, -1, 71, -1, 61, -1, -1, -1, -1, 
    -1, -1, -1, 101, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 130, 
    139, -1, -1, -1, -1, -1, 95, -1, -1, -1, 31, -1, -1, 84, 8 
  );

{$Q-}
function TSynEdit32HighlighterCAC.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 123 + Ord(Str^) * 763;
    Inc(Str);
  end;
  Result := Result mod 709;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterCAC.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterCAC.InitIdent;
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

function TSynEdit32HighlighterCAC.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterCAC.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterCAC.Create(AOwner: TComponent);
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
  FOperatorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  AddAttribute(FOperatorAttri);
  FDirecAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FDirecAttri);
  InitIdent;
  SetAttributesOnChange(DefHighlightChange);
  FRange := rsUnknown;
  fDefaultFilter := SYNS_FilterCAClipper;
end;

procedure TSynEdit32HighlighterCAC.CStyleProc;
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

  while FLine[FRun] <> #0 do
    case FLine[FRun] of
      '*':
        if FLine[FRun + 1] = '/' then
        begin
          FRange := rsUnknown;
          Inc(FRun, 2);
          Break;
        end else Inc(FRun);
      #10: Break;
      #13: Break;
    else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterCAC.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
  else Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterCAC.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterCAC.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCAC.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCAC.NumberProc;

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
        if FLine[FRun + 1] = '.' then Break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterCAC.SlashProc;
begin
  case FLine[FRun + 1] of
    '/':
      begin
        Inc(FRun, 2);
        FTokenID := tkComment;
        while FLine[FRun] <> #0 do
        begin
          case FLine[FRun] of
            #10, #13: Break;
          end;
          Inc(FRun);
        end;
      end;
    '*':
      begin
        FTokenID := tkComment;
        FRange := rsCStyle;
        Inc(FRun, 2);
        while FLine[FRun] <> #0 do
          case FLine[FRun] of
            '*':
              if FLine[FRun + 1] = '/' then
              begin
                FRange := rsUnknown;
                Inc(FRun, 2);
                Break;
              end else Inc(FRun);
            #10: Break;
            #13: Break;
          else Inc(FRun);
          end;
      end;
  else
    begin
      Inc(FRun);
      FTokenID := tkOperator;
    end;
  end;
end;

procedure TSynEdit32HighlighterCAC.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterCAC.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkOperator;
end;

procedure TSynEdit32HighlighterCAC.StringProc;
var
  ActiveStr: WideChar;
begin
  FTokenID := tkString;
  ActiveStr := FLine[FRun];
  if ((FLine[FRun + 1] = #39) and (FLine[FRun + 2] = #39)) or
    ((FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34)) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
    end;
    Inc(FRun);
  until (FLine[FRun] = ActiveStr);
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterCAC.DirectiveProc;
begin
  FTokenID := tkDirective;
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
      '/': if FLine[FRun + 1] = '/' then Break;
      #34, #39: Break;
    end;
    Inc(FRun);
  until FLine[FRun] = #0;
end;

procedure TSynEdit32HighlighterCAC.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterCAC.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsCStyle: CStyleProc;
    else
      case FLine[FRun] of
        '@': SymbolProc;
        '&': SymbolProc;
        '{': SymbolProc;
        '}': SymbolProc;
        #13: CRProc;
        ':': SymbolProc;
        ',': SymbolProc;
        '#': DirectiveProc;
        '=': SymbolProc;
        '>': SymbolProc;
        'A'..'Z', 'a'..'z': IdentProc;
        '$': SymbolProc;
        #10: LFProc;
        '<': SymbolProc;
        '-': SymbolProc;
        '!': SymbolProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        '+': SymbolProc;
        '.': SymbolProc;
        '?': SymbolProc;
        ')': SymbolProc;
        '(': SymbolProc;
        ';': SymbolProc;
        '/': SlashProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        ']': SymbolProc;
        '[': SymbolProc;
        '*': StarProc;
        #39, #34: StringProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynEdit32HighlighterCAC.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterCAC.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterCAC.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterCAC.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkDirective: Result := FDirecAttri;
    tkOperator: Result := FOperatorAttri;
    tkUnknown: Result := FOperatorAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterCAC.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterCAC.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterCAC.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterCAC.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCAClipper;
end;

class function TSynEdit32HighlighterCAC.GetLanguageName: string;
begin
  Result := SYNS_LangCAClipper;
end;

procedure TSynEdit32HighlighterCAC.StarProc;
begin
// if FRun is 0 there could be an access violation
  if (FRun = 0) or IsLineEnd(FRun - 1) then
  begin
    FTokenID := tkComment;
    repeat
      Inc(FRun);
    until IsLineEnd(FRun);
  end
  else
  begin
    Inc(FRun);
    FTokenID := tkOperator;
  end;
end;

class function TSynEdit32HighlighterCAC.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCAClipper;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterCAC);
end.
