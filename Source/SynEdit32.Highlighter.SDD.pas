{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSDD.pas, released 2001-08-20.
The Initial Author of this file is Pieter Polak.
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

$Id: SynHighlighterSDD.pas,v 1.13.2.6 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.SDD;

{$I SynEdit.Inc}

interface

uses
  Windows,
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
    tkDatatype,
    tkNumber,
    tkNull,
    tkSpace,
    tkSymbol,
    tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TRangeState = (rsComment, rsUnKnown);

type
  TSynSDDSyn = class(TSynEdit32CustomHighlighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..36] of TIdentFuncTableFunc;
    fCommentAttri: TSynEdit32HighlighterAttributes;
    fIdentifierAttri: TSynEdit32HighlighterAttributes;
    fKeyAttri: TSynEdit32HighlighterAttributes;
    fDatatypeAttri: TSynEdit32HighlighterAttributes;
    fNumberAttri: TSynEdit32HighlighterAttributes;
    fSpaceAttri: TSynEdit32HighlighterAttributes;
    fSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncBinarydata(Index: Integer): TtkTokenKind;
    function FuncBlock(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncDatabase(Index: Integer): TtkTokenKind;
    function FuncDate(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncEndblock(Index: Integer): TtkTokenKind;
    function FuncInteger(Index: Integer): TtkTokenKind;
    function FuncKeys(Index: Integer): TtkTokenKind;
    function FuncLongint(Index: Integer): TtkTokenKind;
    function FuncMemotext(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncObjects(Index: Integer): TtkTokenKind;
    function FuncOf(Index: Integer): TtkTokenKind;
    function FuncOwner(Index: Integer): TtkTokenKind;
    function FuncPartition(Index: Integer): TtkTokenKind;
    function FuncPartitions(Index: Integer): TtkTokenKind;
    function FuncPrimary(Index: Integer): TtkTokenKind;
    function FuncReal(Index: Integer): TtkTokenKind;
    function FuncSecondary(Index: Integer): TtkTokenKind;
    function FuncSpec(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncSuperblock(Index: Integer): TtkTokenKind;
    function FuncSuperspec(Index: Integer): TtkTokenKind;
    function FuncTime(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure BraceOpenProc;
    procedure BraceCommentProc;
    procedure NumberProc;                                                    
    procedure CRProc;
    procedure LFProc;
    procedure IdentProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure UnknownProc;
    procedure SymbolProc;
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
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read fKeyAttri write fKeyAttri;
    property DatatypeAttri: TSynEdit32HighlighterAttributes read fDatatypeAttri write fDatatypeAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read fNumberAttri write fNumberAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..26] of UnicodeString = (
    'array', 'binarydata', 'block', 'byte', 'database', 'date', 'end',
    'endblock', 'integer', 'keys', 'longint', 'memotext', 'object', 'objects',
    'of', 'owner', 'partition', 'partitions', 'primary', 'real', 'secondary',
    'spec', 'string', 'superblock', 'superspec', 'time', 'var'
  );

  KeyIndices: array[0..36] of Integer = (
    8, 3, 18, 0, 25, 14, 16, 22, 5, 19, 10, 20, -1, -1, 2, 26, -1, 21, -1, 12,
    1, 17, 15, -1, 9, -1, 11, 7, -1, 4, 6, -1, 13, -1, -1, 24, 23
  );

{$Q-}
function TSynSDDSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 813 + Ord(Str^) * 168;
    Inc(Str);
  end;
  Result := Result mod 37;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynSDDSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynSDDSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[3] := FuncArray;
  fIdentFuncTable[20] := FuncBinarydata;
  fIdentFuncTable[14] := FuncBlock;
  fIdentFuncTable[1] := FuncByte;
  fIdentFuncTable[29] := FuncDatabase;
  fIdentFuncTable[8] := FuncDate;
  fIdentFuncTable[30] := FuncEnd;
  fIdentFuncTable[27] := FuncEndblock;
  fIdentFuncTable[0] := FuncInteger;
  fIdentFuncTable[24] := FuncKeys;
  fIdentFuncTable[10] := FuncLongint;
  fIdentFuncTable[26] := FuncMemotext;
  fIdentFuncTable[19] := FuncObject;
  fIdentFuncTable[32] := FuncObjects;
  fIdentFuncTable[5] := FuncOf;
  fIdentFuncTable[22] := FuncOwner;
  fIdentFuncTable[6] := FuncPartition;
  fIdentFuncTable[21] := FuncPartitions;
  fIdentFuncTable[2] := FuncPrimary;
  fIdentFuncTable[9] := FuncReal;
  fIdentFuncTable[11] := FuncSecondary;
  fIdentFuncTable[17] := FuncSpec;
  fIdentFuncTable[7] := FuncString;
  fIdentFuncTable[36] := FuncSuperblock;
  fIdentFuncTable[35] := FuncSuperspec;
  fIdentFuncTable[4] := FuncTime;
  fIdentFuncTable[15] := FuncVar;
end;

function TSynSDDSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynSDDSyn.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncBinarydata(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncBlock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncDatabase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncDate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncEndblock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncInteger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncKeys(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncLongint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncMemotext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncObjects(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncOf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncOwner(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncPartition(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncPartitions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncPrimary(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncReal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSecondary(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSpec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSuperblock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSuperspec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncTime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynSDDSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  fCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clNavy;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clGreen;
  AddAttribute(fKeyAttri);

  fDatatypeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  fDatatypeAttri.Style := [fsBold];
  fDatatypeAttri.Foreground := clTeal;
  AddAttribute(fDatatypeAttri);

  fSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterSDD;
  fRange := rsUnknown;
end; { Create }

procedure TSynSDDSyn.BraceOpenProc;
begin
  fRange := rsComment;
  BraceCommentProc;
  FTokenID := tkComment;
end; { BraceOpenProc }

procedure TSynSDDSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end; { IdentProc }

procedure TSynSDDSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end; { NullProc }

procedure TSynSDDSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(FRun);
  until not CharInSet(FLine[FRun], [#1..#32]);
end; { SpaceProc }

procedure TSynSDDSyn.BraceCommentProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if FLine[FRun] = '}' then
        begin
          Inc(FRun);
          fRange := rsUnKnown;
          Break;
        end;
        Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end; { BraceCommentProc }

procedure TSynSDDSyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end; { UnknownProc }

procedure TSynSDDSyn.Next;
begin
  fTokenPos := FRun;
  case fRange of
    rsComment: BraceCommentProc;
  else
    case FLine[FRun] of
      '{': BraceOpenProc;
      '}', '!', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~': SymbolProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9' : NumberProc;
      #0: NullProc;
      #1..#32: SpaceProc;
      else UnknownProc;
    end;
  end;
  inherited;
end; { Next }

procedure TSynSDDSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end; { CRProc }

procedure TSynSDDSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end; { LFProc }

function TSynSDDSyn.GetSampleSource: UnicodeString;
begin
  Result := '{ Semanta data dictionary }'#13#10 +
            'database Sample.001;'#13#10 +
            'owner = COAS;'#13#10 +
            #13#10 +
            'objects'#13#10 +
            '  Test = object'#13#10 +
            '    Code : string[4];'#13#10 +
            '    Name : string[80];'#13#10 +
            '  end;'#13#10 +
            'keys'#13#10 +
            '  primary Test.Index = [Code];'#13#10 +
            'end.';
end; { GetSampleSource }

function TSynSDDSyn.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end; { GetDefaultAttribute }

function TSynSDDSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end; { GetTokenId }

function TSynSDDSyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkDatatype: Result := fDatatypeAttri;
    tkSpace: Result := fSpaceAttri;
    tkNumber: Result := fNumberAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkSymbol: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end; { GetTokenAttribute }

function TSynSDDSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end; { GetTokenKind }

procedure TSynSDDSyn.ResetRange;
begin
  inherited;
  fRange := rsUnknown;
end; { ResetRange }

procedure TSynSDDSyn.SetRange(Value: Pointer);
begin
  inherited;
  fRange := TRangeState(Value);
end; { SetRange }

function TSynSDDSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end; { GetRange }

class function TSynSDDSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSDD;
end; { GetLanguageName }

procedure TSynSDDSyn.NumberProc;

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
      '.': if FLine[FRun + 1] = '.' then
             Break;
    end;
    Inc(FRun);
  end;
end; { NumberProc }

function TSynSDDSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSDD;
end; { IsFilterStored }

procedure TSynSDDSyn.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

class function TSynSDDSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangSDD;
end;

initialization
  RegisterPlaceableHighlighter(TSynSDDSyn);
end.
