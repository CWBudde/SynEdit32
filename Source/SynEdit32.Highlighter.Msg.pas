{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterMsg.pas, released 2001-10-03.
Description: SynGen Msg file highlighter
The initial author of this file is P.L. Polak.
Copyright (c) 2001, all rights reserved.
Unicode translation by Maël Hörz.

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

$Id: SynHighlighterMsg.pas,v 1.8.2.6 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.Msg;

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
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkSpace,
    tkString,
    tkSymbol,
    tkTerminator,
    tkUnknown);

  TRangeState = (rsUnKnown, rsBraceComment, rsString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynMsgSyn = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..6] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FTerminatorAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncBeginproc(Index: Integer): TtkTokenKind;
    function FuncChars(Index: Integer): TtkTokenKind;
    function FuncEnclosedby(Index: Integer): TtkTokenKind;
    function FuncEndproc(Index: Integer): TtkTokenKind;
    function FuncKeys(Index: Integer): TtkTokenKind;
    function FuncSamplesource(Index: Integer): TtkTokenKind;
    function FuncTokentypes(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure IdentProc;
    procedure SymbolProc;
    procedure TerminatorProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure BraceCommentOpenProc;
    procedure BraceCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property TerminatorAttri: TSynEdit32HighlighterAttributes read FTerminatorAttri write FTerminatorAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..6] of UnicodeString = (
    'beginproc', 'chars', 'enclosedby', 'endproc', 'keys', 'samplesource', 
    'tokentypes' 
  );

  KeyIndices: array[0..6] of Integer = (
    2, 1, 6, 4, 0, 5, 3 
  );

{$Q-}
function TSynMsgSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 182 + Ord(Str^);
    Inc(Str);
  end;
  Result := Result mod 7;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynMsgSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynMsgSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[4] := FuncBeginproc;
  FIdentFuncTable[1] := FuncChars;
  FIdentFuncTable[0] := FuncEnclosedby;
  FIdentFuncTable[6] := FuncEndproc;
  FIdentFuncTable[3] := FuncKeys;
  FIdentFuncTable[5] := FuncSamplesource;
  FIdentFuncTable[2] := FuncTokentypes;
end;

function TSynMsgSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynMsgSyn.FuncBeginproc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynMsgSyn.FuncChars(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynMsgSyn.FuncEnclosedby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynMsgSyn.FuncEndproc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynMsgSyn.FuncKeys(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynMsgSyn.FuncSamplesource(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynMsgSyn.FuncTokentypes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

procedure TSynMsgSyn.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynMsgSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynMsgSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynMsgSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynMsgSyn.BraceCommentOpenProc;
begin
  Inc(FRun);
  FRange := rsBraceComment;
  BraceCommentProc;
  FTokenID := tkComment;
end;

procedure TSynMsgSyn.BraceCommentProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[FRun] = '}') then
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

procedure TSynMsgSyn.StringOpenProc;
begin
  Inc(FRun);
  FRange := rsString;
  StringProc;
  FTokenID := tkString;
end;

procedure TSynMsgSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if (FLine[FRun] = '''') then
    begin
      Inc(FRun, 1);
      FRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

constructor TSynMsgSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FTerminatorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrTerminator, SYNS_FriendlyAttrTerminator);
  AddAttribute(FTerminatorAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterSynGenMsgfiles;
  FRange := rsUnknown;
end;

procedure TSynMsgSyn.IdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynMsgSyn.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynMsgSyn.TerminatorProc;
begin
  Inc(FRun);
  if (FLine[FRun] = '>') and (FLine[FRun + 1] = '<') and (FLine[FRun + 2] = '|') then
  begin
    FTokenID := tkTerminator;
    Inc(FRun, 3);
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynMsgSyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynMsgSyn.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsBraceComment: BraceCommentProc;
  else
    begin
      FRange := rsUnknown;
      case FLine[FRun] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '{': BraceCommentOpenProc;
        '''': StringOpenProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '-', '+', '*', '/', '\', ',', '"', '[', ']', ':', ';': SymbolProc;
        '|': TerminatorProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynMsgSyn.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynMsgSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynMsgSyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkTerminator: Result := FTerminatorAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynMsgSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynMsgSyn.GetSampleSource: UnicodeString;
begin
  Result := 'TSynSampleSyn   {first identifier is the class name }'#13#10 +
            'tk              {second identifier is the prefix }'#13#10 +
            'IdentStart ''a''..''z'':: ''a''..''z''::'#13#10 +
            'KEYS'#13#10 +
            'Sample'#13#10 +
            'Source'#13#10 +
            '|><|';
end;

function TSynMsgSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSynGenMsgfiles;
end;

function TSynMsgSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynMsgSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSynGenMsgfiles;
end;

procedure TSynMsgSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynMsgSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynMsgSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

class function TSynMsgSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangSynGenMsgfiles;
end;

initialization
  RegisterPlaceableHighlighter(TSynMsgSyn);
end.
