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

$Id: SynHighlighterCpp.pas,v 1.22.2.9 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a C++ syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1998)
@lastmod(2001-11-21)
The SynHighlighterCpp unit provides SynEdit with a C++ syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynEdit32.Highlighter.Cpp;

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
  TtkTokenKind = (tkAsm, tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown,
    tkChar, tkFloat, tkHex, tkOctal);

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
    rsAsmBlock, rsDirective, rsDirectiveComment, rsMultiLineString,
    rsMultiLineDirective);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynEdit32HighlighterCpp = class(TSynEdit32CustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..342] of TIdentFuncTableFunc;
    FAsmAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDirecAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FInvalidAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FFloatAttri: TSynEdit32HighlighterAttributes;
    FHexAttri: TSynEdit32HighlighterAttributes;
    FOctalAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FCharAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncAsm(Index: Integer): TtkTokenKind;
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
    procedure DirectiveProc;
    procedure DirectiveEndProc;
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
    procedure StringEndProc;
  protected
    function GetExtTokenID: TxtkTokenKind;
    function GetSampleSource: UnicodeString; override;
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
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property AsmAttri: TSynEdit32HighlighterAttributes read FAsmAttri write FAsmAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirecAttri: TSynEdit32HighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynEdit32HighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property FloatAttri: TSynEdit32HighlighterAttributes read FFloatAttri
      write FFloatAttri;
    property HexAttri: TSynEdit32HighlighterAttributes read FHexAttri
      write FHexAttri;
    property OctalAttri: TSynEdit32HighlighterAttributes read FOctalAttri
      write FOctalAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property CharAttri: TSynEdit32HighlighterAttributes read FCharAttri
      write FCharAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  Windows,
  SynEdit32.StrConst;

const
  KeyWords: array[0..94] of UnicodeString = (
    '__asm', '__automated', '__cdecl', '__classid', '__closure', '__declspec', 
    '__dispid', '__except', '__export', '__fastcall', '__finally', '__import', 
    '__int16', '__int32', '__int64', '__int8', '__pascal', '__property', 
    '__published', '__rtti', '__stdcall', '__thread', '__try', '_asm', '_cdecl', 
    '_export', '_fastcall', '_import', '_pascal', '_stdcall', 'asm', 'auto', 
    'bool', 'break', 'case', 'catch', 'cdecl', 'char', 'class', 'const', 
    'const_cast', 'continue', 'default', 'delete', 'do', 'double', 
    'dynamic_cast', 'else', 'enum', 'explicit', 'extern', 'false', 'float', 
    'for', 'friend', 'goto', 'if', 'inline', 'int', 'interface', 'long', 
    'mutable', 'namespace', 'new', 'operator', 'pascal', 'private', 'protected', 
    'public', 'register', 'reinterpret_cast', 'return', 'short', 'signed', 
    'sizeof', 'static', 'static_cast', 'struct', 'switch', 'template', 'this', 
    'throw', 'true', 'try', 'typedef', 'typeid', 'typename', 'union', 
    'unsigned', 'using', 'virtual', 'void', 'volatile', 'wchar_t', 'while' 
  );

  KeyIndices: array[0..342] of Integer = (
    -1, 34, -1, -1, 57, 72, -1, 39, -1, 9, -1, 86, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 88, -1, 12, 66, -1, -1, -1, -1, -1, 42, -1, -1, -1, -1, -1, 56, 51, 
    40, 87, 77, -1, -1, -1, -1, 64, -1, -1, -1, -1, -1, -1, -1, -1, -1, 28, 41, 
    -1, 63, 6, -1, -1, -1, -1, -1, -1, -1, -1, 55, 65, 0, -1, -1, -1, -1, -1, 
    -1, 26, 83, -1, 38, 92, -1, -1, 93, 33, -1, -1, -1, -1, -1, -1, -1, 35, -1, 
    -1, -1, -1, -1, -1, -1, 79, 27, -1, -1, -1, 43, -1, -1, 20, -1, -1, 31, -1, 
    -1, -1, -1, -1, 89, -1, -1, -1, -1, 59, -1, 58, -1, -1, 46, -1, -1, 3, -1, 
    -1, 17, -1, 54, -1, 45, -1, -1, -1, -1, -1, -1, 53, -1, -1, -1, 1, -1, -1, 
    -1, -1, 44, 90, 32, -1, -1, -1, -1, -1, -1, 91, 13, -1, -1, -1, 60, -1, -1, 
    -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, 75, -1, -1, 76, -1, -1, -1, -1, 30, 
    68, 23, 82, -1, 15, -1, -1, 2, -1, 70, -1, -1, -1, 73, 18, -1, -1, -1, -1, 
    -1, 47, 24, 52, 14, 84, -1, -1, -1, -1, -1, 25, -1, -1, -1, 80, 69, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, 19, -1, -1, -1, 
    -1, -1, -1, 74, -1, -1, -1, 29, -1, -1, -1, 67, -1, 7, -1, -1, -1, 50, 61, 
    -1, -1, -1, 4, -1, 94, 85, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    81, -1, -1, -1, -1, -1, 10, 16, -1, -1, 36, 37, -1, -1, -1, 8, -1, 22, -1, 
    -1, -1, -1, 78, 62, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 71, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, 11, -1, 48, 
    -1 
  );

{$Q-}
function TSynEdit32HighlighterCpp.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 179 + Ord(Str^) * 44;
    Inc(Str);
  end;
  Result := Result mod 343;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterCpp.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterCpp.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[70] := FuncAsm;
  FIdentFuncTable[191] := FuncAsm;
  FIdentFuncTable[189] := FuncAsm;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynEdit32HighlighterCpp.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterCpp.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynEdit32HighlighterCpp.FuncAsm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsAsm;
    FAsmStart := True;
  end
  else
    Result := tkIdentifier
end;

constructor TSynEdit32HighlighterCpp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FAsmAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(FAsmAttri);
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FInvalidAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FInvalidAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FCharAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(FCharAttri);
  FFloatAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(FFloatAttri);
  FHexAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(FHexAttri);
  FOctalAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  AddAttribute(FOctalAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FDirecAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  fDefaultFilter := SYNS_FilterCPP;
end;

procedure TSynEdit32HighlighterCpp.AnsiCProc;
begin
  FTokenID := tkComment;
  case FLine[FRun] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
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
          else if (FRange = rsDirectiveComment) and
            not IsLineEnd(FRun) then
              FRange := rsMultiLineDirective
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

procedure TSynEdit32HighlighterCpp.AndSymbolProc;
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

procedure TSynEdit32HighlighterCpp.AsciiCharProc;
begin
  FTokenID := tkChar;
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

procedure TSynEdit32HighlighterCpp.AtSymbolProc;
begin
  FTokenID := tkUnknown;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.BraceCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if FRange = rsAsmBlock then FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterCpp.BraceOpenProc;
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

procedure TSynEdit32HighlighterCpp.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun + 1] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.ColonProc;
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

procedure TSynEdit32HighlighterCpp.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynEdit32HighlighterCpp.DirectiveProc;
begin
  if WideTrim(FLine)[1] <> '#' then // '#' is not first char on the line, treat it as an invalid char
  begin
    FTokenID := tkUnknown;
    Inc(FRun);
    Exit;
  end;
  FTokenID := tkDirective;
  repeat
    if FLine[FRun] = '/' then // comment?
    begin
      if FLine[FRun + 1] = '/' then // is end of directive as well
      begin
        FRange := rsUnknown;
        Exit;
      end
      else
        if FLine[FRun + 1] = '*' then // might be embedded only
        begin
          FRange := rsDirectiveComment;
          Exit;
        end;
    end;
    if (FLine[FRun] = '\') and (FLine[FRun +1 ] = #0) then // a multiline directive
    begin
      Inc(FRun);
      FRange := rsMultiLineDirective;
      Exit;
    end;
    Inc(FRun);
  until IsLineEnd(FRun)
end;

procedure TSynEdit32HighlighterCpp.DirectiveEndProc;
begin
  FTokenID := tkDirective;
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
  FRange := rsUnknown;
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
      '/': // comment?
        begin
          case FLine[FRun + 1] of
            '/': // is end of directive as well
              begin
                FRange := rsUnknown;
                Exit;
              end;
            '*': // might be embedded only
              begin
                FRange := rsDirectiveComment;
                Exit;
              end;
          end;
        end;
      '\': // yet another line?
        begin
          if FLine[FRun + 1] = #0 then
          begin
            Inc(FRun);
            FRange := rsMultiLineDirective;
            Exit;
          end;
        end;
    end;
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterCpp.EqualProc;
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

procedure TSynEdit32HighlighterCpp.GreaterProc;
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

procedure TSynEdit32HighlighterCpp.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.LowerProc;
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

procedure TSynEdit32HighlighterCpp.MinusProc;
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

procedure TSynEdit32HighlighterCpp.ModSymbolProc;
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

procedure TSynEdit32HighlighterCpp.NotSymbolProc;
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

procedure TSynEdit32HighlighterCpp.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.NumberProc;

  function IsNumberChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsDigitPlusMinusChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '+', '-':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexDigit(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsAlphaUncerscore(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      'A'..'Z', 'a'..'z', '_':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  i: Integer;
begin
  idx1 := FRun;
  Inc(FRun);
  FTokenID := tkNumber;
  while IsNumberChar(FRun) do
  begin
    case FLine[FRun] of
      '.':
        if FLine[Succ(FRun)] = '.' then
          Break
        else
          if (FTokenID <> tkHex) then
            FTokenID := tkFloat
          else // invalid
          begin
            FTokenID := tkUnknown;
            Exit;
          end;
      '-', '+':
        begin
          if FTokenID <> tkFloat then // number <> float. an arithmetic operator
            Exit;
          if not CharInSet(FLine[Pred(FRun)], ['e', 'E']) then
            Exit; // number = float, but no exponent. an arithmetic operator
          if not IsDigitPlusMinusChar(Succ(FRun)) then // invalid
          begin
            Inc(FRun);
            FTokenID := tkUnknown;
            Exit;
          end
        end;
      '0'..'7':
        if (FRun = Succ(idx1)) and (FLine[idx1] = '0') then // octal number
          FTokenID := tkOctal;
      '8', '9':
        if (FLine[idx1] = '0') and
           ((FTokenID <> tkHex) and (FTokenID <> tkFloat)) then // invalid octal char
             FTokenID := tkUnknown;
      'a'..'d', 'A'..'D':
        if FTokenID <> tkHex then // invalid char
          Break;
      'e', 'E':
        if (FTokenID <> tkHex) then
          if CharInSet(FLine[Pred(FRun)], ['0'..'9']) then // exponent
          begin
            for i := idx1 to Pred(FRun) do
              if CharInSet(FLine[i], ['e', 'E']) then // too many exponents
              begin
                FTokenID := tkUnknown;
                Exit;
              end;
            if not IsDigitPlusMinusChar(Succ(FRun)) then
              Break
            else
              FTokenID := tkFloat
          end
          else // invalid char
            Break;
      'f', 'F':
        if FTokenID <> tkHex then
        begin
          for i := idx1 to Pred(FRun) do
            if CharInSet(FLine[i], ['f', 'F']) then // declaration syntax error
            begin
              FTokenID := tkUnknown;
              Exit;
            end;
          if FTokenID = tkFloat then
          begin
            if CharInSet(FLine[Pred(FRun)], ['l', 'L']) then // can't mix
              Break;
          end
          else
            FTokenID := tkFloat;
        end;
      'l', 'L':
        begin
          for i := idx1 to FRun - 2 do
            if CharInSet(FLine[i], ['l', 'L']) then // declaration syntax error
            begin
              FTokenID := tkUnknown;
              Exit;
            end;
          if FTokenID = tkFloat then
            if CharInSet(FLine[Pred(FRun)], ['f', 'F']) then // can't mix
              Break;
        end;
      'u', 'U':
        if FTokenID = tkFloat then // not allowed
          Break
        else
          for i := idx1 to Pred(FRun) do
            if CharInSet(FLine[i], ['u', 'U']) then // declaration syntax error
            begin
              FTokenID := tkUnknown;
              Exit;
            end;
      'x', 'X':
        if (FRun = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLine[idx1] = '0') and  // 0x...
           IsHexDigit(Succ(FRun)) then // 0x... must be continued with a number
             FTokenID := tkHex
           else // invalid char
           begin
             if not IsIdentChar(FLine[Succ(FRun)]) and
                CharInSet(FLine[Succ(idx1)], ['x', 'X']) then
             begin
               Inc(FRun); // highlight 'x' too
               FTokenID := tkUnknown;
             end;
             Break;
           end;
    end; // case
    Inc(FRun);
  end; // while
  if IsAlphaUncerscore(FRun) then
    FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterCpp.OrSymbolProc;
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

procedure TSynEdit32HighlighterCpp.PlusProc;
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

procedure TSynEdit32HighlighterCpp.PointProc;
begin
  FTokenID := tkSymbol;
  if (FLine[FRun + 1] = '.') and (FLine[FRun + 2] = '.') then
    begin                              {ellipse}
      Inc(FRun, 3);
      FExtTokenID := xtkEllipse;
    end
  else
    if CharInSet(FLine[FRun + 1], ['0'..'9']) then // float
    begin
      Dec(FRun); // numberproc must see the point
      NumberProc;
    end
  else                                 {point}
    begin
      Inc(FRun);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynEdit32HighlighterCpp.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynEdit32HighlighterCpp.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynEdit32HighlighterCpp.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if FRange = rsAsm then FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterCpp.SlashProc;
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
        if FRange = rsAsm then
          FRange := rsAnsiCAsm
        else if FRange = rsAsmBlock then
          FRange := rsAnsiCAsmBlock
        else if FRange <> rsDirectiveComment then
          FRange := rsAnsiC;
        Inc(FRun, 2);
        while FLine[FRun] <> #0 do
          case FLine[FRun] of
            '*':
              if FLine[FRun + 1] = '/' then
              begin
                Inc(FRun, 2);
                if FRange = rsDirectiveComment then
                  FRange := rsMultiLineDirective
                else if FRange = rsAnsiCAsm then
                  FRange := rsAsm
                else
                  begin
                  if FRange = rsAnsiCAsmBlock then
                    FRange := rsAsmBlock
                  else
                    FRange := rsUnKnown;
                  end;
                break;
              end else Inc(FRun);
            #10, #13:
              begin
                if FRange = rsDirectiveComment then
                  FRange := rsAnsiC;
                break;
              end;
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

procedure TSynEdit32HighlighterCpp.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.SquareCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynEdit32HighlighterCpp.SquareOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynEdit32HighlighterCpp.StarProc;
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

procedure TSynEdit32HighlighterCpp.StringProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[FRun] = '\' then begin
      case FLine[FRun + 1] of
        #34, '\':
          Inc(FRun);
        #00:
          begin
            Inc(FRun);
            FRange := rsMultilineString;
            Exit;
          end;
      end;
    end;
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = #34);
  if FLine[FRun] = #34 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.StringEndProc;
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

  FRange := rsUnknown;

  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
      '\':
        begin
          case FLine[FRun + 1] of
            #34, '\':
              Inc(FRun);
            #00:
              begin
                Inc(FRun);
                FRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      #34: Break;
    end;
    Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = #34);
  if FLine[FRun] = #34 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterCpp.TildeProc;
begin
  Inc(FRun);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynEdit32HighlighterCpp.XOrSymbolProc;
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

procedure TSynEdit32HighlighterCpp.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterCpp.Next;
begin
  FAsmStart := False;
  fTokenPos := FRun;
  case FRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock, rsDirectiveComment: AnsiCProc;
    rsMultiLineDirective: DirectiveEndProc;
    rsMultilineString: StringEndProc;
  else
    begin
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

function TSynEdit32HighlighterCpp.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterCpp.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterCpp.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
  if ((FRange = rsAsm) or (FRange = rsAsmBlock)) and not FAsmStart
    and not (FTokenID in [tkComment, tkSpace, tkNull])
  then
    Result := tkAsm;
end;

function TSynEdit32HighlighterCpp.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynEdit32HighlighterCpp.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  FTokenID := GetTokenID;
  case FTokenID of
    tkAsm: Result := FAsmAttri;
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkFloat: Result := FFloatAttri;
    tkHex: Result := FHexAttri;
    tkOctal: Result := FOctalAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FInvalidAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterCpp.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynEdit32HighlighterCpp.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynEdit32HighlighterCpp.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterCpp.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
{$IFNDEF SYN_CLX}
  with TBetterRegistry.Create do
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
{$ENDIF}
end;

function TSynEdit32HighlighterCpp.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

  {$IFNDEF SYN_CLX}
  function ReadCPPBSettings(settingIndex: integer): boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): boolean;

      function ReadCPPB1(settingTag: string; attri: TSynEdit32HighlighterAttributes; name: string): boolean;
      var
        i: integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
             '\SOFTWARE\Borland\C++Builder\'+settingTag+'\Highlight',name,true);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynEdit32HighlighterAttributes; key: string): boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                 '\Software\Borland\C++Builder\'+settingTag+'\Editor\Highlight',
                 key,false);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1')
          then Result := ReadCPPB1(settingTag,attri,key)
          else Result := ReadCPPB3OrMore(settingTag,attri,key);
      except Result := false; end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynEdit32HighlighterAttributes;
    tmpCharAttri      : TSynEdit32HighlighterAttributes;
    tmpNumberAttri    : TSynEdit32HighlighterAttributes;
    tmpFloatAttri     : TSynEdit32HighlighterAttributes;
    tmpHexAttri       : TSynEdit32HighlighterAttributes;
    tmpOctalAttri     : TSynEdit32HighlighterAttributes;
    tmpKeyAttri       : TSynEdit32HighlighterAttributes;
    tmpSymbolAttri    : TSynEdit32HighlighterAttributes;
    tmpAsmAttri       : TSynEdit32HighlighterAttributes;
    tmpCommentAttri   : TSynEdit32HighlighterAttributes;
    tmpIdentifierAttri: TSynEdit32HighlighterAttributes;
    tmpInvalidAttri   : TSynEdit32HighlighterAttributes;
    tmpSpaceAttri     : TSynEdit32HighlighterAttributes;
    tmpDirecAttri     : TSynEdit32HighlighterAttributes;
    s                 : TStringList;

  begin { ReadCPPBSettings }
    s := TStringList.Create;
    try
      EnumUserSettings(s);
      if settingIndex >= s.Count then Result := false
      else begin
        tmpStringAttri    := TSynEdit32HighlighterAttributes.Create('', '');
        tmpCharAttri      := TSynEdit32HighlighterAttributes.Create('', '');
        tmpNumberAttri    := TSynEdit32HighlighterAttributes.Create('', '');
        tmpFloatAttri     := TSynEdit32HighlighterAttributes.Create('', '');
        tmpHexAttri       := TSynEdit32HighlighterAttributes.Create('', '');
        tmpOctalAttri     := TSynEdit32HighlighterAttributes.Create('', '');
        tmpKeyAttri       := TSynEdit32HighlighterAttributes.Create('', '');
        tmpSymbolAttri    := TSynEdit32HighlighterAttributes.Create('', '');
        tmpAsmAttri       := TSynEdit32HighlighterAttributes.Create('', '');
        tmpCommentAttri   := TSynEdit32HighlighterAttributes.Create('', '');
        tmpIdentifierAttri:= TSynEdit32HighlighterAttributes.Create('', '');
        tmpInvalidAttri   := TSynEdit32HighlighterAttributes.Create('', '');
        tmpSpaceAttri     := TSynEdit32HighlighterAttributes.Create('', '');
        tmpDirecAttri     := TSynEdit32HighlighterAttributes.Create('', '');
        tmpStringAttri    .Assign(FStringAttri);
        tmpCharAttri      .Assign(FCharAttri);
        tmpNumberAttri    .Assign(FNumberAttri);
        tmpFloatAttri     .Assign(FFloatAttri);
        tmpHexAttri       .Assign(FHexAttri);
        tmpOctalAttri     .Assign(FOctalAttri);
        tmpKeyAttri       .Assign(FKeyAttri);
        tmpSymbolAttri    .Assign(FSymbolAttri);
        tmpAsmAttri       .Assign(FAsmAttri);
        tmpCommentAttri   .Assign(FCommentAttri);
        tmpIdentifierAttri.Assign(FIdentifierAttri);
        tmpInvalidAttri   .Assign(FInvalidAttri);
        tmpSpaceAttri     .Assign(FSpaceAttri);
        tmpDirecAttri     .Assign(FDirecAttri);
        if s[settingIndex][1] = '1'
          then Result := ReadCPPBSetting(s[settingIndex],FAsmAttri,'Plain text')
          else Result := ReadCPPBSetting(s[settingIndex],FAsmAttri,'Assembler');
        Result := Result                                                         and
                  ReadCPPBSetting(s[settingIndex],FCommentAttri,'Comment')       and
                  ReadCPPBSetting(s[settingIndex],FIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(s[settingIndex],FInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(s[settingIndex],FKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(s[settingIndex],FNumberAttri,'Integer')        and
                  ReadCPPBSetting(s[settingIndex],FFloatAttri,'Float')           and
                  ReadCPPBSetting(s[settingIndex],FHexAttri,'Hex')               and
                  ReadCPPBSetting(s[settingIndex],FOctalAttri,'Octal')           and
                  ReadCPPBSetting(s[settingIndex],FSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(s[settingIndex],FStringAttri,'String')         and
                  ReadCPPBSetting(s[settingIndex],FCharAttri,'Character')             and
                  ReadCPPBSetting(s[settingIndex],FSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(s[settingIndex],FDirecAttri,'Preprocessor');
        if not Result then begin
          FStringAttri    .Assign(tmpStringAttri);
          FCharAttri      .Assign(tmpCharAttri);
          FNumberAttri    .Assign(tmpNumberAttri);
          FFloatAttri     .Assign(tmpFloatAttri);
          FHexAttri       .Assign(tmpHexAttri);
          FOctalAttri     .Assign(tmpOctalAttri);
          FKeyAttri       .Assign(tmpKeyAttri);
          FSymbolAttri    .Assign(tmpSymbolAttri);
          FAsmAttri       .Assign(tmpAsmAttri);
          FCommentAttri   .Assign(tmpCommentAttri);
          FIdentifierAttri.Assign(tmpIdentifierAttri);
          FInvalidAttri   .Assign(tmpInvalidAttri);
          FSpaceAttri     .Assign(tmpSpaceAttri);
          FDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpCharAttri      .Free;
        tmpNumberAttri    .Free;
        tmpFloatAttri     .Free;
        tmpHexAttri       .Free;
        tmpOctalAttri     .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpAsmAttri       .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally s.Free; end;
  end; { ReadCPPBSettings }
  {$ENDIF}

begin
  {$IFNDEF SYN_CLX}
  Result := ReadCPPBSettings(settingIndex);
  {$ELSE}
  Result := False;
  {$ENDIF}
end; { TSynEdit32HighlighterCpp.UseUserSettings }

function TSynEdit32HighlighterCpp.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPP;
end;

class function TSynEdit32HighlighterCpp.GetLanguageName: string;
begin
  Result := SYNS_LangCPP;
end;

class function TSynEdit32HighlighterCpp.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynEdit32HighlighterCpp.GetSampleSource: UnicodeString;
begin
  Result :=
    '// Syntax Highlighting'#13#10+
    'void __fastcall TForm1::Button1Click(TObject *Sender)'#13#10+
    '{'#13#10+
    '  int number = 123456;'#13#10+
    '  char c = ''a'';'#13#10+
    '  Caption = "The number is " + IntToStr(i);'#13#10+
    '  for (int i = 0; i <= number; i++)'#13#10+
    '  {'#13#10+
    '    x -= 0xff;'#13#10+
    '    x -= 023;'#13#10+
    '    x += 1.0;'#13#10+
    '    x += @; /* illegal character */'#13#10+
    '  }'#13#10+
    '  #ifdef USE_ASM'#13#10+
    '    asm'#13#10+
    '    {'#13#10+
    '      ASM MOV AX, 0x1234'#13#10+
    '      ASM MOV i, AX'#13#10+
    '    }'#13#10+
    '  #endif'#13#10+
    '}';
end;

class function TSynEdit32HighlighterCpp.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCPP;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterCpp);
end.
