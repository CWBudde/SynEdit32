{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCpp.pas, released 2000-04-10.
The Original Code is based on the dcjCppSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
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

$Id: SynHighlighterGWS.pas,v 1.13.2.7 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.GWS;

{ This unit provides a syntax highlighter for GW-TEL Scripts }

{$I SynEdit.Inc}

interface

uses
  Graphics,
  SysUtils,
  Classes,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

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

  TRangeState = (rsAnsiC, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynGWScriptSyn = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..12] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FInvalidAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncBool(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncChar(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
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
    procedure InitIdent;
  protected
    function GetExtTokenID: TxtkTokenKind;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetDefaultAttribute (Index: integer): TSynEdit32HighlighterAttributes; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;

    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property InvalidAttri: TSynEdit32HighlighterAttributes read FInvalidAttri write FInvalidAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..12] of UnicodeString = (
    'bool', 'break', 'char', 'do', 'else', 'false', 'for', 'if', 'int', 
    'return', 'string', 'true', 'while' 
  );

  KeyIndices: array[0..12] of Integer = (
    8, 5, 11, 12, 1, 10, 0, 2, 9, 4, 6, 3, 7 
  );

{$Q-}
function TSynGWScriptSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 797 + Ord(Str^) * 6;
    Inc(Str);
  end;
  Result := Result mod 13;
  FStringLen := Str - FToIdent;
end;
{$Q+}

procedure TSynGWScriptSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[6] := FuncBool;
  FIdentFuncTable[4] := FuncBreak;
  FIdentFuncTable[7] := FuncChar;
  FIdentFuncTable[11] := FuncDo;
  FIdentFuncTable[9] := FuncElse;
  FIdentFuncTable[1] := FuncFalse;
  FIdentFuncTable[10] := FuncFor;
  FIdentFuncTable[12] := FuncIf;
  FIdentFuncTable[0] := FuncInt;
  FIdentFuncTable[8] := FuncReturn;
  FIdentFuncTable[5] := FuncString;
  FIdentFuncTable[2] := FuncTrue;
  FIdentFuncTable[3] := FuncWhile;
end;

function TSynGWScriptSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;


function TSynGWScriptSyn.FuncBool(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncChar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGWScriptSyn.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;


function TSynGWScriptSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

constructor TSynGWScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FInvalidAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);

  AddAttribute(FCommentAttri);
  AddAttribute(FIdentifierAttri);
  AddAttribute(FInvalidAttri);
  AddAttribute(FKeyAttri);
  AddAttribute(FNumberAttri);
  AddAttribute(FSpaceAttri);
  AddAttribute(FStringAttri);
  AddAttribute(FSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange         := rsUnknown;
  fDefaultFilter := SYNS_FilterGWS;
end; { Create }

procedure TSynGWScriptSyn.AnsiCProc;
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
        if FLine[FRun + 1] = '/' then begin
          Inc(FRun, 2);
          FRange := rsUnknown;
          Break;
        end else
          Inc(FRun);
      #10: Break;
      #13: Break;
    else Inc(FRun);
    end;
end;

procedure TSynGWScriptSyn.AndSymbolProc;
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

procedure TSynGWScriptSyn.AsciiCharProc;
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

procedure TSynGWScriptSyn.AtSymbolProc;
begin
  FTokenID := tkUnknown;
  Inc(FRun);
end;

procedure TSynGWScriptSyn.BraceCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
  FRange := rsUnknown;
end;

procedure TSynGWScriptSyn.BraceOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynGWScriptSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun + 1] = #10 then Inc(FRun);
end;

procedure TSynGWScriptSyn.ColonProc;
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

procedure TSynGWScriptSyn.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynGWScriptSyn.EqualProc;
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

procedure TSynGWScriptSyn.GreaterProc;
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

procedure TSynGWScriptSyn.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(FRun);
end;

procedure TSynGWScriptSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynGWScriptSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynGWScriptSyn.LowerProc;
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

procedure TSynGWScriptSyn.MinusProc;
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
        Inc(FRun, 2);
        FExtTokenID := xtkDecrement;
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

procedure TSynGWScriptSyn.ModSymbolProc;
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

procedure TSynGWScriptSyn.NotSymbolProc;
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

procedure TSynGWScriptSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynGWScriptSyn.NumberProc;

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
        if FLine[FRun + 1] = '.' then Break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynGWScriptSyn.OrSymbolProc;
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

procedure TSynGWScriptSyn.PlusProc;
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

procedure TSynGWScriptSyn.PointProc;
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

procedure TSynGWScriptSyn.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynGWScriptSyn.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynGWScriptSyn.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  FRange := rsUnknown;
end;

procedure TSynGWScriptSyn.SlashProc;
begin
  case FLine[FRun + 1] of
    '/':                               {c++ style comments}
      begin
        FTokenID := tkComment;
        Inc(FRun, 2);
       while not IsLineEnd(FRun) do Inc(FRun);
      end;
    '*':                               {c style comments}
      begin
        FTokenID := tkComment;
        FRange := rsAnsiC;
        Inc(FRun, 2);
        while FLine[FRun] <> #0 do
          case FLine[FRun] of
            '*':
              if FLine[FRun + 1] = '/' then
              begin
                Inc(FRun, 2);
                FRange := rsUnknown;
                Break;
              end else Inc(FRun);
            #10: Break;
            #13: Break;
          else Inc(FRun);
          end;
      end;
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

procedure TSynGWScriptSyn.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynGWScriptSyn.SquareCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynGWScriptSyn.SquareOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynGWScriptSyn.StarProc;
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

procedure TSynGWScriptSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
      #92:                             {backslash}
        case FLine[FRun + 1] of
          #34: Inc(FRun);               {escaped quote doesn't count}
          #92: Inc(FRun);               {escaped backslash doesn't count}
        end;
    end;
    Inc(FRun);
  until FLine[FRun] = #34;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynGWScriptSyn.TildeProc;
begin
  Inc(FRun);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynGWScriptSyn.XOrSymbolProc;
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

procedure TSynGWScriptSyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynGWScriptSyn.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsAnsiC : AnsiCProc;
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

function TSynGWScriptSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynGWScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynGWScriptSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynGWScriptSyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FInvalidAttri;
    else Result := nil;
  end;
end;

function TSynGWScriptSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynGWScriptSyn.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynGWScriptSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynGWScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGWS;
end;

class function TSynGWScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGWS;
end;

function TSynGWScriptSyn.GetDefaultAttribute (Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := FKeyAttri;
    SYN_ATTR_STRING     : Result := FStringAttri;
    SYN_ATTR_WHITESPACE : Result := FSpaceAttri;
    SYN_ATTR_SYMBOL     : Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

class function TSynGWScriptSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGWS;
end;

initialization
  RegisterPlaceableHighlighter (TSynGWScriptSyn);
end.
