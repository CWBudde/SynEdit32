{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSML.pas, released 2000-04-17.
The Original Code is based on the dmMLSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is David H. Muir.
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

$Id: SynHighlighterSml.pas,v 1.14.2.6 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides SynEdit with a Standard ML syntax highlighter, with extra options for the standard Basis library.)
@author(David H Muir <dhm@dmsoftware.co.uk>)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterSML.pas unit provides SynEdit text control with a Standard ML highlighter.  Many formatting attributes can
be specified, and there is an option to include extra keywords and operators only found in the Basis library, this option can
be disabled for backwards compatibility with older ML compilers that do not have support for the Basis Library.
}

unit SynEdit32.Highlighter.Sml;

{$I SynEdit.Inc}

interface

uses
  Graphics,
  Registry,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

Type
  TtkTokenKind = (tkCharacter, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkOperator, tkSpace, tkString, tkSymbol, tkSyntaxError, tkUnknown);

  TRangeState = (rsUnknown, rsComment, rsMultilineString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynSMLSyn = class(TSynEdit32CustomHighlighter)
  private
    fBasis: Boolean;
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..70] of TIdentFuncTableFunc;
    fCharacterAttri: TSynEdit32HighlighterAttributes;
    fCommentAttri: TSynEdit32HighlighterAttributes;
    fIdentifierAttri: TSynEdit32HighlighterAttributes;
    fKeyAttri: TSynEdit32HighlighterAttributes;
    fNumberAttri: TSynEdit32HighlighterAttributes;
    fOperatorAttri: TSynEdit32HighlighterAttributes;
    fSpaceAttri: TSynEdit32HighlighterAttributes;
    fStringAttri: TSynEdit32HighlighterAttributes;
    fSymbolAttri: TSynEdit32HighlighterAttributes;
    fSyntaxErrorAttri: TSynEdit32HighlighterAttributes;
    function IsValidMLCharacter: Boolean;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure CRProc;
    procedure CharacterProc;
    procedure ColonProc;
    procedure CommentProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OperatorProc;
    procedure RoundBracketOpenProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure BasisOpProc;
    procedure StringEndProc;
    procedure PoundProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
      override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CharacterAttri: TSynEdit32HighlighterAttributes read fCharacterAttri
      write fCharacterAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property OperatorAttri: TSynEdit32HighlighterAttributes read fOperatorAttri
      write fOperatorAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SyntaxErrorAttri: TSynEdit32HighlighterAttributes read fSyntaxErrorAttri
      write fSyntaxErrorAttri;
    property Basis: Boolean read FBasis write FBasis default True;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..40] of UnicodeString = (
    'abstype', 'and', 'andalso', 'as', 'case', 'datatype', 'do', 'else', 'end', 
    'eqtype', 'exception', 'fn', 'fun', 'functor', 'handle', 'if', 'in', 
    'include', 'infix', 'infixr', 'let', 'local', 'nonfix', 'of', 'op', 'open', 
    'orelse', 'raise', 'rec', 'sharing', 'sig', 'signature', 'struct', 
    'structure', 'then', 'type', 'val', 'where', 'while', 'with', 'withtype' 
  );

  KeyIndices: array[0..70] of Integer = (
    28, -1, -1, -1, 23, 4, 19, -1, -1, 32, 8, 6, -1, 33, 0, -1, 14, -1, 2, -1, 
    -1, 29, 35, -1, -1, -1, -1, 13, -1, -1, 9, -1, 11, 30, 1, -1, 25, 36, -1, 
    -1, -1, 40, -1, 7, -1, 16, 26, 37, -1, 15, 21, -1, 18, 12, 5, -1, -1, 10, 
    22, 27, 34, 17, -1, 20, -1, 39, -1, 3, 38, 31, 24 
  );

{$Q-}
function TSynSMLSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 157 + Ord(Str^) * 35;
    Inc(Str);
  end;
  Result := Result mod 71;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynSMLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynSMLSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
end;

function TSynSMLSyn.IsValidMLCharacter: Boolean;

 function IsABNRTChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      'a', 'b', 'n', 'r', 't':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  ASCIIStr: UnicodeString;
  ASCIICode, Error: Integer;
begin
  Result := False;
  if (FLine[FRun] = '"') then
    if (FRun > 2) and (FLine[FRun - 1] <> '\') and (FLine[FRun - 2] = '"') then
      Result := True
    else if (FRun > 3) and (FLine[FRun - 1] = '\') and (FLine[FRun - 2] = '\')
      and (FLine[FRun - 3] = '"') then
      Result := True
    else if (FRun > 3) and IsABNRTChar(FRun - 1) and
      (FLine[FRun - 2] = '\') and (FLine[FRun - 3] = '"') then
      Result := True
    else if (FRun > 5) and (FLine[FRun - 4] = '\') and (FLine[FRun - 5] = '"') then
    begin
      ASCIIStr := copy(FLine, FRun - 2, 3);
      Val(ASCIIStr, ASCIICode, Error);
      if (Error = 0) and (ASCIICode >= 0) and (ASCIICode <= 255) then
        Result := True
    end
end;

function TSynSMLSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynSMLSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynSMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  fCharacterAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  fCharacterAttri.Foreground := clBlue;
  AddAttribute(fCharacterAttri);
  fCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clGreen;
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clRed;
  AddAttribute(fNumberAttri);
  fOperatorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  fOperatorAttri.Foreground := clMaroon;
  AddAttribute(fOperatorAttri);
  fSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fSyntaxErrorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  fSyntaxErrorAttri.Foreground := clRed;
  fSyntaxErrorAttri.Style := [fsBold];
  AddAttribute(fSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;        
  fDefaultFilter := SYNS_FilterSML;
  Basis := True;
end;

procedure TSynSMLSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
  else
    Inc(FRun);
  end;
end;

procedure TSynSMLSyn.ColonProc;
begin
  Inc(FRun);
  if Basis and (FLine[FRun] = ':') then
  begin
    FTokenID := tkOperator;
    Inc(FRun);
  end
  else FTokenID := tkSymbol;
end;

procedure TSynSMLSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynSMLSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynSMLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynSMLSyn.NumberProc;

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
      '.':  if FLine[FRun + 1] = '.' then break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynSMLSyn.OperatorProc;
begin
  Inc(FRun);
  FTokenID := tkOperator;
end;

procedure TSynSMLSyn.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynSMLSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[FRun] = '\' then
    begin
      case FLine[FRun + 1] of
        '"', '\':
          Inc(FRun);
        #00:
          begin
            Inc(FRun);
            fRange := rsMultilineString;
            Exit;
          end;
      end;
    end;
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = '"');
  if FLine[FRun] = '"' then
    Inc(FRun);
end;

procedure TSynSMLSyn.StringEndProc;
begin
  FTokenID := tkString;

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

  fRange := rsUnknown;

  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
      '\':
        begin
          case FLine[FRun + 1] of
            '"', '\':
              Inc(FRun);
            #00:
              begin
                Inc(FRun);
                fRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      '"': Break;
    end;
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = '"');
  if FLine[FRun] = '"' then
    Inc(FRun);
end;

procedure TSynSMLSyn.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynSMLSyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynSMLSyn.BasisOpProc;
begin
  Inc(FRun);
  if Basis then FTokenID := tkOperator else FTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.PoundProc;
begin
  Inc(FRun);
  if (FLine[FRun] = '"') then
    CharacterProc
  else
    FTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.CharacterProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      repeat
        Inc(FRun);
      until IsLineEnd(FRun) or (FLine[FRun] = '"');

      if IsValidMLCharacter then
        FTokenID := tkCharacter
      else
      begin
        if FLine[FRun] = '"' then Inc(FRun);
        FTokenID := tkSyntaxError;
      end;
    end
  end
end;

procedure TSynSMLSyn.RoundBracketOpenProc;
begin
  Inc(FRun);
  if (FLine[FRun] = '*') then
  begin
    fRange := rsComment;
    CommentProc;
    FTokenID := tkComment;
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.CommentProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[FRun] = '*') and
           (FLine[FRun + 1] = ')') then
        begin
          Inc(FRun, 2);
          fRange := rsUnknown;
          Break;
        end;
        if not IsLineEnd(FRun) then
          Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end;

procedure TSynSMLSyn.Next;
begin
  fTokenPos := FRun;
  case fRange of
    rsComment: CommentProc;
    rsMultilineString: StringEndProc; 
  else
    begin
      fRange := rsUnknown;

      case FLine[FRun] of
        #13: CRProc;
        '#': PoundProc;
        ':': ColonProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        #10: LFProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '"': StringProc;
        '@', '^': BasisOpProc;
        '(': RoundBracketOpenProc;
        '+', '-', '~', '*', '/', '=', '<', '>': OperatorProc;
        ',', '.',  ';': SymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynSMLSyn.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynSMLSyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkCharacter: Result := fCharacterAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkOperator: Result := fOperatorAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSyntaxError: Result := fSyntaxErrorAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynSMLSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynSMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSML;
end;

function TSynSMLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    #39, '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynSMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSML;
end;

function TSynSMLSyn.GetSampleSource: UnicodeString;
begin
  Result := '(* Syntax highlighting *)'#13#10 +
            'load "Real";'#13#10 +
            'fun PrintNumber(x: int) ='#13#10 +
            '  let'#13#10 +
            '    val Number = real(x) / 10.0;'#13#10 +
            '    val Text = "The Number is " ^ Real.toString(~Number) ^ "\n";'#13#10 +
            '  in'#13#10 +
            '    print Text;'#13#10 +
            '    if x = 0 then () else PrintNumber(x-1)'#13#10+
            '  end;' 
end;

procedure TSynSMLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSMLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynSMLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynSMLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangSML;
end;

initialization
  RegisterPlaceableHighlighter(TSynSMLSyn);
end.
