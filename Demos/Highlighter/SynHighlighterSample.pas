{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: D:\Quellen\Komponenten\SynEdit\Demos\HighlighterDemo\SynHighlighterSample.pas, released 2008-10-25.
Description: 
The initial author of this file is Maël Hörz.
Copyright (c) 2008, all rights reserved.

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

$Id: SynHighlighterSample.pas,v 1.6.2.13 2008/10/25 23:30:31 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterSample;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkSpace,
    tkString,
    tkTest,
    tkUnknown);

  TRangeState = (rsUnKnown, rsBraceComment, rsCStyleComment, rsString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynSampleSyn = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..2] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FTestAttri: TSynEdit32HighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function FuncHello(Index: Integer): TtkTokenKind;
    function FuncSynedit(Index: Integer): TtkTokenKind;
    function FuncWorld(Index: Integer): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc(Index: Integer): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure BraceCommentOpenProc;
    procedure BraceCommentProc;
    procedure CStyleCommentOpenProc;
    procedure CStyleCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property TestAttri: TSynEdit32HighlighterAttributes read FTestAttri write FTestAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

resourcestring
  SYNS_FilterTest = 'All files (*.*)|*.*';
  SYNS_LangTest = 'Test';
  SYNS_FriendlyLangTest = 'Test';
  SYNS_AttrTest = 'Test';
  SYNS_FriendlyAttrTest = 'Test';

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..2] of UnicodeString = (
    'hello', 'synedit', 'world' 
  );

  KeyIndices: array[0..2] of Integer = (
    0, 2, 1 
  );

procedure TSynSampleSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[0] := FuncHello;
  FIdentFuncTable[2] := FuncSynedit;
  FIdentFuncTable[1] := FuncWorld;
end;

{$Q-}
function TSynSampleSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 3 + Ord(Str^);
    inc(Str);
  end;
  Result := Result mod 3;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynSampleSyn.FuncHello(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSampleSyn.FuncSynedit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTest
  else
    Result := tkIdentifier;
end;

function TSynSampleSyn.FuncWorld(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSampleSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynSampleSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynSampleSyn.SpaceProc;
begin
  inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do inc(FRun);
end;

procedure TSynSampleSyn.NullProc;
begin
  FTokenID := tkNull;
  inc(FRun);
end;

procedure TSynSampleSyn.CRProc;
begin
  FTokenID := tkSpace;
  inc(FRun);
  if fLine[FRun] = #10 then
    inc(FRun);
end;

procedure TSynSampleSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(FRun);
end;

procedure TSynSampleSyn.BraceCommentOpenProc;
begin
  Inc(FRun);
  FRange := rsBraceComment;
  FTokenID := tkComment;
end;

procedure TSynSampleSyn.BraceCommentProc;
begin
  case fLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (fLine[FRun] = '}') then
        begin
          Inc(FRun, 1);
          FRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(FRun) then
          Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end;

procedure TSynSampleSyn.CStyleCommentOpenProc;
begin
  Inc(FRun);
  if (fLine[FRun] = '*') then
  begin
    Inc(FRun, 1);
    FRange := rsCStyleComment;
    FTokenID := tkComment;
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynSampleSyn.CStyleCommentProc;
begin
  case fLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (fLine[FRun] = '*') and
           (fLine[FRun + 1] = '/') then
        begin
          Inc(FRun, 2);
          FRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(FRun) then
          Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end;

procedure TSynSampleSyn.StringOpenProc;
begin
  Inc(FRun);
  FRange := rsString;
  StringProc;
  FTokenID := tkString;
end;

procedure TSynSampleSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if (fLine[FRun] = '"') then
    begin
      Inc(FRun, 1);
      FRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

constructor TSynSampleSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  FCommentAttri := TSynEdit32HighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynEdit32HighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FSpaceAttri := TSynEdit32HighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynEdit32HighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clRed;
  AddAttribute(FStringAttri);

  FTestAttri := TSynEdit32HighLighterAttributes.Create(SYNS_AttrTest, SYNS_FriendlyAttrTest);
  FTestAttri.Style := [fsUnderline, fsItalic];
  FTestAttri.Foreground := clBlue;
  FTestAttri.Background := clSilver;
  AddAttribute(FTestAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterTest;
  FRange := rsUnknown;
end;

procedure TSynSampleSyn.IdentProc;
begin
  FTokenID := IdentKind(fLine + FRun);
  inc(FRun, FStringLen);
  while IsIdentChar(fLine[FRun]) do
    Inc(FRun);
end;

procedure TSynSampleSyn.UnknownProc;
begin
  inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynSampleSyn.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsBraceComment: BraceCommentProc;
    rsCStyleComment: CStyleCommentProc;
  else
    case fLine[FRun] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      '{': BraceCommentOpenProc;
      '/': CStyleCommentOpenProc;
      '"': StringOpenProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynSampleSyn.GetDefaultAttribute(Index: Integer): TSynEdit32HighLighterAttributes;
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

function TSynSampleSyn.GetEol: Boolean;
begin
  Result := FRun = fLineLen + 1;
end;

function TSynSampleSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := 
    'Hello,SynEdit,World';
end;

function TSynSampleSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynSampleSyn.GetTokenAttribute: TSynEdit32HighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkTest: Result := FTestAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynSampleSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynSampleSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynSampleSyn.GetSampleSource: UnicodeString;
begin
  Result := 
    '{ Sample source for the demo highlighter }'#13#10 +
    #13#10 +
    'This highlighter will recognize the words Hello and'#13#10 +
    'World as keywords. It will also highlight "Strings".'#13#10 +
    #13#10 +
    'And a special keyword type: SynEdit'#13#10 +
    '/* This style of comments is also highlighted */';
end;

function TSynSampleSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterTest;
end;

class function TSynSampleSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangTest;
end;

class function TSynSampleSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTest;
end;

procedure TSynSampleSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynSampleSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynSampleSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

initialization
  RegisterPlaceableHighlighter(TSynSampleSyn);
end.
