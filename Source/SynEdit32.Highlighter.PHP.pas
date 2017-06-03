{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPHP.pas, released 2000-04-21.
The Original Code is based on the wmPHPSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Willo van der Merwe.
"Heredoc" syntax highlighting implementation by Marko Njezic.
Unicode translation by Ma?l H?rz.
PHP5 keywords added by CodehunterWorks.
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

$Id: SynHighlighterPHP.pas,v 1.22.3.0 2012/09/11 16:25:00 codehunterworks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a PHP syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterPHP unit provides SynEdit with a PHP syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynEdit32.Highlighter.PHP;

{$I SynEdit.Inc}

interface

uses
  Graphics,
  Registry,
  SysUtils,
  Classes,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsComment, rsVarExpansion,
    rsHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsComment, rsVarExpansion);
{$ENDIF}

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterPHP = class(TSynEdit32CustomHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength: Byte;
    fHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..255] of TIdentFuncTableFunc;
    fCommentAttri: TSynEdit32HighlighterAttributes;
    fIdentifierAttri: TSynEdit32HighlighterAttributes;
    fKeyAttri: TSynEdit32HighlighterAttributes;
    fNumberAttri: TSynEdit32HighlighterAttributes;
    fSpaceAttri: TSynEdit32HighlighterAttributes;
    fStringAttri: TSynEdit32HighlighterAttributes;
    fSymbolAttri: TSynEdit32HighlighterAttributes;
    fVariableAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
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
    procedure MultiplyProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure PoundProc;
    procedure QuestionProc;
    procedure RemainderSymbolProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StringProc;
    procedure VarExpansionProc;
    procedure TildeProc;
    procedure VariableProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure AnsiCProc;
    procedure String39Proc;
    procedure String34Proc;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
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
    property CommentAttri: TSynEdit32HighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynEdit32HighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
  SynEdit32.MiscProcs,
  SynEdit32.StrConst;

const
  KeyWords: array[0..73] of UnicodeString = (
    '__class__', '__dir__', '__file__', '__function__', '__halt_compiler',
    '__line__', '__method__', '__namespace__', 'abstract', 'and', 'array', 'as',
    'break', 'case', 'catch', 'class', 'clone', 'const', 'continue', 'declare',
    'default', 'die', 'do', 'echo', 'else', 'elseif', 'empty', 'enddeclare',
    'endfor', 'endforeach', 'endif', 'endswitch', 'endwhile', 'eval', 'exit',
    'extends', 'false', 'final', 'for', 'foreach', 'function', 'global', 'goto',
    'if', 'implements', 'include', 'include_once', 'instanceof', 'interface',
    'isset', 'list', 'namespace', 'new', 'null', 'old_function', 'or', 'print',
    'private', 'protected', 'public', 'require', 'require_once', 'return',
    'static', 'switch', 'synedit', 'throw', 'true', 'try', 'unset', 'use',
    'var', 'while', 'xor'
  );

  KeyIndices: array[0..222] of Integer = (
    -1, -1, 69, -1, -1, 1, 19, -1, -1, -1, -1, -1, -1, 35, -1, 17, -1, -1, 53,
    6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, -1, -1, -1,
    52, 36, -1, -1, 66, -1, 62, -1, 38, 15, 44, -1, -1, -1, -1, 32, -1, -1, 24,
    48, -1, -1, 56, 45, 65, 40, -1, -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1,
    -1, 60, -1, -1, -1, -1, -1, 31, 11, -1, 33, 20, 49, -1, -1, -1, 21, -1, -1,
    -1, 54, -1, -1, -1, -1, -1, 29, -1, 64, -1, 23, -1, -1, 14, -1, -1, 42, -1,
    -1, 0, 25, 50, -1, 58, 4, 27, -1, -1, 7, -1, -1, -1, -1, -1, 63, -1, 34, -1,
    -1, -1, -1, -1, -1, -1, -1, 28, 13, 47, 51, -1, -1, 2, -1, 37, -1, -1, 71,
    3, -1, 30, -1, 43, -1, -1, -1, -1, 57, 8, -1, -1, -1, -1, 41, 10, -1, 12,
    72, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, 5, -1, 22, -1, -1, -1, 70,
    9, 18, -1, -1, -1, -1, -1, 59, 26, -1, -1, 16, -1, 68, -1, 61, -1, -1, -1,
    39, -1, -1, -1, -1, -1, -1, -1, -1, 55, -1, -1, -1
  );

{$Q-}
function TSynEdit32HighlighterPHP.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 252 + Ord(Str^) * 595;
    Inc(Str);
  end;
  Result := Result mod 223;
  FStringLen := Str - FToIdent;
end;{$Q+}

function TSynEdit32HighlighterPHP.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterPHP.InitIdent;
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

function TSynEdit32HighlighterPHP.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterPHP.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then begin
    Result := tkKey;
  end else begin
    Result := tkIdentifier;
  end;
end;

constructor TSynEdit32HighlighterPHP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  fCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterPHP;
  fRange := rsUnknown;
end;

procedure TSynEdit32HighlighterPHP.AndSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {and assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '&':                               {conditional and}
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

procedure TSynEdit32HighlighterPHP.AtSymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.BraceCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.BraceOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
  else
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterPHP.ColonProc;
begin
  Inc(FRun);                            {colon - conditional}
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.EqualProc;
begin
  case FLine[FRun + 1] of
    '=':                               {logical equal}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '>':                               {Hash operator}
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

procedure TSynEdit32HighlighterPHP.GreaterProc;
begin
  case FLine[FRun + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '>':
      begin
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

procedure TSynEdit32HighlighterPHP.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.LowerProc;
{$IFDEF SYN_HEREDOC}
var
  i, Len : Integer;
{$ENDIF}
begin
  case FLine[FRun + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '<':
      begin
        FTokenID := tkSymbol;
{$IFDEF SYN_HEREDOC}
        if (FLine[FRun + 2] = '<') and IsIdentChar(FLine[FRun + 3]) then
        begin
          Inc(FRun, 3);

          i := FRun;
          while IsIdentChar(FLine[i]) do Inc(i);
          Len := i - FRun;

          if Len > 255 then
          begin
            FTokenID := tkUnknown;
            Exit;
          end;

          fRange := rsHeredoc;
          fHeredocLength := Len;
          fHeredocChecksum := CalcFCS(FLine[FRun], Len);

          Inc(FRun, Len);
          FTokenID := tkString;
        end
        else
{$ENDIF}
        if FLine[FRun + 2] = '=' then   {shift left assign}
        begin
          Inc(FRun, 3)
        end
        else                           {shift left}
        begin
          Inc(FRun, 2);
        end;
      end;
  else                                 {less than}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterPHP.MinusProc;
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
    '>':                               {Class operator}
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

procedure TSynEdit32HighlighterPHP.MultiplyProc;
begin
  case FLine[FRun + 1] of
    '=':                               {multiply assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {multiply}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterPHP.NotSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {not equal}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {logical complement}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterPHP.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', '-', 'l', 'L', 'x', 'X', 'A'..'F', 'a'..'f':
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

procedure TSynEdit32HighlighterPHP.OrSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {inclusive or assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '|':                               {conditional or}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {inclusive or}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterPHP.PlusProc;
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
  else                                 {add}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterPHP.PointProc;
begin
  Inc(FRun);                            {point}
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.PoundProc;
begin
  repeat
    Inc(FRun);
  until IsLineEnd(FRun);
  FTokenID := tkComment;
end;

procedure TSynEdit32HighlighterPHP.QuestionProc;
begin
  FTokenID := tkSymbol;                {question mark - conditional}
  Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.RemainderSymbolProc;
begin
  case FLine[FRun + 1] of
    '=':                               {remainder assign}
      begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {remainder}
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynEdit32HighlighterPHP.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.SemiColonProc;
begin
  Inc(FRun);                            {semicolon}
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.SlashProc;
begin
  case FLine[FRun + 1] of
    '/':                               {c++ style comments}
      begin
        Inc(FRun, 2);
        FTokenID := tkComment;
        while not IsLineEnd(FRun) do
          Inc(FRun);
      end;
    '*':
      begin
        fRange := rsComment;
        Inc(FRun);
        FTokenID := tkComment;       {c style comment}

        Inc(FRun);
        while not IsLineEnd(FRun) do
          if FLine[FRun] = '*' then
          begin
            if FLine[FRun + 1] = '/' then
            begin
              fRange := rsUnKnown;
              Inc(FRun, 2);
              break;
            end
            else
              Inc(FRun)
          end
          else
            Inc(FRun);
      end;
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

procedure TSynEdit32HighlighterPHP.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.SquareCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.SquareOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.StringProc;

  function IsEscaped: Boolean;
  var
    iFirstSlashPos: Integer;
  begin
    iFirstSlashPos := FRun -1;
    while (iFirstSlashPos > 0) and (FLine[iFirstSlashPos] = '\') do
      Dec(iFirstSlashPos);
    Result := (FRun - iFirstSlashPos + 1) mod 2 <> 0;
  end;

var
  iCloseChar: WideChar;
begin
  if IsLineEnd(FRun) and (fTokenPos = FRun) then
  begin
    NextProcedure;
    Exit;
  end;
  FTokenID := tkString;
  if fRange = rsString39 then
    iCloseChar := #39
  else
    iCloseChar := #34;
  while not IsLineEnd(FRun) do
  begin
    if (FLine[FRun] = iCloseChar) and not IsEscaped then
      break;
    if (FLine[FRun] = '$') and (iCloseChar = '"') and
      ((FLine[FRun + 1] = '{') or IsIdentChar(FLine[FRun + 1])) then
    begin
      if (FRun > 1) and (FLine[FRun -1] = '{') then { complex syntax }
        Dec(FRun);
      if not IsEscaped then
      begin
        { break the token to process the variable }
        fRange := rsVarExpansion;
        Exit;
      end
      else if FLine[FRun] = '{' then
        Inc(FRun); { restore FRun if we previously deincremented it }
    end;
    Inc(FRun);
  end;
  if (FLine[FRun] = iCloseChar) then
    fRange := rsUnKnown;
  if not IsLineEnd(FRun) then Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.VarExpansionProc;
type
  TExpansionSyntax = (esNormal, esComplex, esBrace);
var
  iSyntax: TExpansionSyntax;
  iOpenBraces: integer;
  iOpenBrackets: integer;
  iTempRun: integer;
begin
  fRange := rsString34; { var expansion only occurs in double quoted strings }
  FTokenID := tkVariable;
  if FLine[FRun] = '{' then
  begin
    iSyntax := esComplex;
    Inc(FRun, 2); { skips '{$' }
  end
  else
  begin
    Inc( FRun );
    if FLine[FRun] = '{' then
    begin
      iSyntax := esBrace;
      Inc(FRun);
    end
    else
      iSyntax := esNormal;
  end;
  if iSyntax in [esBrace, esComplex] then
  begin
    iOpenBraces := 1;
    while not IsLineEnd(FRun) do
    begin
      if FLine[FRun] = '}' then
      begin
        Dec(iOpenBraces);
        if iOpenBraces = 0 then
        begin
          Inc(FRun);
          break;
        end;
      end;
      if FLine[FRun] = '{' then
        Inc(iOpenBraces);
      Inc(FRun);
    end;
  end
  else
  begin
    while IsIdentChar(FLine[FRun]) do
      Inc(FRun);
    iOpenBrackets := 0;
    iTempRun := FRun;
    { process arrays and objects }
    while not IsLineEnd(iTempRun) do
    begin
      if FLine[iTempRun] = '[' then
      begin
        Inc( iTempRun );
        if FLine[iTempRun] = #39 then
        begin
          Inc(iTempRun);
          while not IsLineEnd(iTempRun) and (FLine[iTempRun] <> #39) do
            Inc(iTempRun);
          if (FLine[iTempRun] = #39) and (FLine[iTempRun + 1 ] = ']') then
          begin
            Inc(iTempRun, 2);
            FRun := iTempRun;
            continue;
          end
          else
            break;
        end
        else
          Inc(iOpenBrackets);
      end
      else if (FLine[iTempRun] = '-') and (FLine[iTempRun +1] = '>') then
        Inc(iTempRun, 2)
      else
        break;

      if not IsIdentChar(FLine[iTempRun]) then
        break
      else
        repeat
          Inc(iTempRun);
        until not IsIdentChar(FLine[iTempRun]);

      while FLine[iTempRun] = ']' do
      begin
        if iOpenBrackets = 0 then
          break;
        Dec(iOpenBrackets);
        Inc(iTempRun);
      end;
      if iOpenBrackets = 0 then
        FRun := iTempRun;
    end;
  end;
end;

procedure TSynEdit32HighlighterPHP.TildeProc;
begin
  Inc(FRun);                            {bitwise complement}
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterPHP.VariableProc;
begin
  FTokenID := tkVariable;
  Inc(FRun);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.XOrSymbolProc;
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

procedure TSynEdit32HighlighterPHP.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterPHP.AnsiCProc;
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

  while not IsLineEnd(FRun) do
    if FLine[FRun] = '*' then
    begin
      if FLine[FRun + 1] = '/' then
      begin
        Inc(FRun, 2);
        fRange := rsUnKnown;
        break;
      end
      else
        Inc(FRun);
    end
    else
      Inc(FRun);
end;

procedure TSynEdit32HighlighterPHP.String39Proc;
begin
  fRange := rsString39;
  Inc( FRun );
  StringProc;
end;

procedure TSynEdit32HighlighterPHP.String34Proc;
begin
  fRange := rsString34;
  Inc( FRun );
  StringProc;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynEdit32HighlighterPHP.HeredocProc;

  procedure SkipToEOL;
  begin
    case FLine[FRun] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    else
      repeat
        Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;

var
  i: Integer;
begin
  if IsLineEnd(FRun) and (fTokenPos = FRun) then
  begin
    NextProcedure;
    Exit;
  end;
  FTokenID := tkString;

  if FRun = 0 then
  begin
    i := 0;

    while not (IsLineEnd(FLine[i]) or (FLine[i] = ';')) do
    begin
      if i > fHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(i);
    end;

    if i <> fHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[0], i) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      FRun := i;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynEdit32HighlighterPHP.Next;
begin
  fTokenPos := FRun;
  case fRange of
    rsComment: AnsiCProc;
    rsString39, rsString34: StringProc;
    rsVarExpansion: VarExpansionProc;
{$IFDEF SYN_HEREDOC}
    rsHeredoc: HeredocProc;
{$ENDIF}
    else
    begin
      fRange := rsUnknown;
      NextProcedure;
    end;
  end;

  // ensure that one call of Next is enough to reach next token
  if (fOldRun = FRun) and not GetEol then Next;

  inherited;
end;

procedure TSynEdit32HighlighterPHP.NextProcedure;
begin
  case FLine[FRun] of
    '&': AndSymbolProc;
    #39: String39Proc; // single quote
    '@': AtSymbolProc;
    '}': BraceCloseProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '=': EqualProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '*': MultiplyProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '.': PointProc;
    '#': PoundProc;
    '?': QuestionProc;
    '%': RemainderSymbolProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    #34: String34Proc; // double quote
    '~': TildeProc;
    '$': VariableProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
end;

function TSynEdit32HighlighterPHP.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterPHP.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange = rsHeredoc then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynEdit32HighlighterPHP.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterPHP.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterPHP.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterPHP.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynEdit32HighlighterPHP.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  fRange := TRangeState(RangePointer.Range);
  fHeredocLength := 0;
  fHeredocChecksum := 0;
  if fRange = rsHeredoc then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

function TSynEdit32HighlighterPHP.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPHP;
end;

class function TSynEdit32HighlighterPHP.GetLanguageName: string;
begin
  Result := SYNS_LangPHP;
end;

function TSynEdit32HighlighterPHP.GetSampleSource: UnicodeString;
begin
  Result :=
    '// Syntax highlighting'#13#10+
    'function printNumber()'#13#10+
    '{'#13#10+
    '  $number = 1234;'#13#10+
    '  print "The number is $number";'#13#10+
    '  for ($i = 0; $i <= $number; $i++)'#13#10+
    '  {'#13#10+
    '    $x++;'#13#10+
    '    $x--;'#13#10+
    '    $x += 1.0;'#13#10+
    '  }'#13#10+
    '}';
end;

class function TSynEdit32HighlighterPHP.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPHP;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterPHP);
end.
