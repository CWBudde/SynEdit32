{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHaskell.pas, released 2001-10-28
The Original Code is based on the SynHighlighterCpp.pas, released 2000-04-10
which in turn was based on the dcjCppSyn.pas file from the mwEdit component
suite by Martin Waldenburg and other developers, the Initial Author of this file
is Michael Trier.
Unicode translation by Ma�l H�rz.
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

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

You may retrieve the latest version of this file from
http://www.ashleybrown.co.uk/synedit/

-------------------------------------------------------------------------------}
{
@abstract(Provides a Haskell syntax highlighter for SynEdit)
@author(Ashley Brown)
@created(2001)
@lastmod(2000-10-26)
The SynHighlighterHaskell unit provides SynEdit with a Haskell syntax highlighter.
Based on SynHighlighterCpp.

http://haskell.org/
http://www.ashleybrown.co.uk/
ashley@ashleybrown.co.uk
}

unit SynEdit32.Highlighter.Haskell;

{$I SynEdit32.Inc}

interface

uses
  Graphics, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

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

  TRangeState = (rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm,
    rsAsmBlock, rsDirective, rsDirectiveComment, rsString34, rsString39);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynEdit32HighlighterHaskell = class(TSynEdit32CustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..28] of TIdentFuncTableFunc;
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
    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
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
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function GetExtTokenID: TxtkTokenKind;
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
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
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
  Windows, Registry,
  SynEdit32.StrConst;

const
  KeyWords: array[0..23] of UnicodeString = (
    'Bool', 'Char', 'class', 'data', 'deriving', 'Double', 'else', 'False', 
    'Float', 'if', 'import', 'in', 'instance', 'Int', 'Integer', 'IO', 'let', 
    'module', 'otherwise', 'String', 'then', 'True', 'type', 'where' 
  );

  KeyIndices: array[0..28] of Integer = (
    2, 23, 10, 16, 7, -1, 22, 8, 14, 17, 5, 4, 11, -1, 1, 9, 12, 0, -1, 6, -1, 
    3, 15, 18, 20, -1, 13, 19, 21 
  );

{$Q-}
function TSynEdit32HighlighterHaskell.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 904 + Ord(Str^) * 779;
    Inc(Str);
  end;
  Result := Result mod 29;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterHaskell.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterHaskell.InitIdent;
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

function TSynEdit32HighlighterHaskell.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterHaskell.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynEdit32HighlighterHaskell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
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
  FDefaultFilter := SYNS_FilterHaskell;
end; { Create }

procedure TSynEdit32HighlighterHaskell.AnsiCProc;
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
          Inc(FRun, 2);
          if FRange = rsAnsiCAsm then
            FRange := rsAsm
          else if FRange = rsAnsiCAsmBlock then
            FRange := rsAsmBlock
          else if FRange = rsDirectiveComment then
            FRange := rsDirective
          else
            FRange := rsUnKnown;
          break;
        end else
          Inc(FRun);
      #10: break;
      #13: break;
    else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterHaskell.AndSymbolProc;
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

procedure TSynEdit32HighlighterHaskell.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[FRun] = '\' then begin
      if CharInSet(FLine[FRun + 1], [#39, '\']) then
        Inc(FRun);
    end;
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = #39);
  if FLine[FRun] = #39 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.AtSymbolProc;
begin
  FTokenID := tkUnknown;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.BraceCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if FRange = rsAsmBlock then FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterHaskell.BraceOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  if FRange = rsAsm then
  begin
    FRange := rsAsmBlock;
    FAsmStart := True;
  end;
end;

procedure TSynEdit32HighlighterHaskell.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun + 1] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.ColonProc;
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

procedure TSynEdit32HighlighterHaskell.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynEdit32HighlighterHaskell.EqualProc;
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

procedure TSynEdit32HighlighterHaskell.GreaterProc;
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

procedure TSynEdit32HighlighterHaskell.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.LowerProc;
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

procedure TSynEdit32HighlighterHaskell.MinusProc;
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
        FTokenID := tkComment;
        Inc(FRun, 2);
        while not IsLineEnd(FRun) do Inc(FRun);
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

procedure TSynEdit32HighlighterHaskell.ModSymbolProc;
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

procedure TSynEdit32HighlighterHaskell.NotSymbolProc;
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

procedure TSynEdit32HighlighterHaskell.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.NumberProc;

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
        if FLine[FRun + 1] = '.' then break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterHaskell.OrSymbolProc;
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

procedure TSynEdit32HighlighterHaskell.PlusProc;
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

procedure TSynEdit32HighlighterHaskell.PointProc;
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

procedure TSynEdit32HighlighterHaskell.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynEdit32HighlighterHaskell.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynEdit32HighlighterHaskell.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if FRange = rsAsm then FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterHaskell.SlashProc;
begin
  case FLine[FRun + 1] of
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

procedure TSynEdit32HighlighterHaskell.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.SquareCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynEdit32HighlighterHaskell.SquareOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynEdit32HighlighterHaskell.StarProc;
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

procedure TSynEdit32HighlighterHaskell.StringProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[FRun] = '\' then begin
      if CharInSet(FLine[FRun + 1], [#34, '\']) then
        Inc(FRun);
    end;
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = #34);
  if FLine[FRun] = #34 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterHaskell.TildeProc;
begin
  Inc(FRun);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynEdit32HighlighterHaskell.XOrSymbolProc;
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

procedure TSynEdit32HighlighterHaskell.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterHaskell.Next;
begin
  FAsmStart := False;
  FTokenPos := FRun;
  case FRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock: AnsiCProc;
  else
    begin
      FRange := rsUnknown;
      case FLine[FRun] of
        '&': AndSymbolProc;
        #39: AsciiCharProc;
        '@': AtSymbolProc;
        '}': BraceCloseProc;
        '{': BraceOpenProc;
        #13: CRProc;
        ':': ColonProc;
        ',': CommaProc;
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
        '~': TildeProc;
        '^': XOrSymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynEdit32HighlighterHaskell.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterHaskell.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterHaskell.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterHaskell.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynEdit32HighlighterHaskell.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterHaskell.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynEdit32HighlighterHaskell.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynEdit32HighlighterHaskell.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterHaskell.EnumUserSettings(settings: TStrings);
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

function TSynEdit32HighlighterHaskell.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterHaskell;
end;

function TSynEdit32HighlighterHaskell.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z', #39:
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynEdit32HighlighterHaskell.GetLanguageName: string;
begin
  Result := SYNS_LangHaskell;
end;

class function TSynEdit32HighlighterHaskell.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynEdit32HighlighterHaskell.GetSampleSource: UnicodeString;
begin
  Result :=
    '-- Haskell Sample Source'#13#10 +
    'tail :: [a] -> [a]'#13#10 +
    'tail (x:xs) = xs'#13#10 + '';
end;

class function TSynEdit32HighlighterHaskell.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangHaskell;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterHaskell);
end.
