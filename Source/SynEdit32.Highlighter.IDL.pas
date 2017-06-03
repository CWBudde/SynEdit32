{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterIDL.pas, released 2001-10-15.
Description: CORBA IDL Parser/Highlighter
The initial author of this file is P.L. Polak.
Unicode translation by Maël Hörz.
Copyright (c) 2001, all rights reserved.

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

$Id: SynHighlighterIDL.pas,v 1.8.2.7 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.IDL;

{$I SynEdit.Inc}

interface

uses
  Graphics, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

Type
  TtkTokenKind = (
    tkComment,
    tkDatatype,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkPreprocessor,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsString, rsChar);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynIdlSyn = class(TSynEdit32CustomHighlighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..100] of TIdentFuncTableFunc;
    fCommentAttri: TSynEdit32HighlighterAttributes;
    fDatatypeAttri: TSynEdit32HighlighterAttributes;
    fIdentifierAttri: TSynEdit32HighlighterAttributes;
    fKeyAttri: TSynEdit32HighlighterAttributes;
    fNumberAttri: TSynEdit32HighlighterAttributes;
    fPreprocessorAttri: TSynEdit32HighlighterAttributes;
    fSpaceAttri: TSynEdit32HighlighterAttributes;
    fStringAttri: TSynEdit32HighlighterAttributes;
    fSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    procedure IdentProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function FuncAbstract(Index: Integer): TtkTokenKind;
    function FuncAny(Index: Integer): TtkTokenKind;
    function FuncAttribute(Index: Integer): TtkTokenKind;
    function FuncBoolean(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncChar(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncContext(Index: Integer): TtkTokenKind;
    function FuncCustom(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDouble(Index: Integer): TtkTokenKind;
    function FuncEnum(Index: Integer): TtkTokenKind;
    function FuncException(Index: Integer): TtkTokenKind;
    function FuncFactory(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFixed(Index: Integer): TtkTokenKind;
    function FuncFloat(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncInout(Index: Integer): TtkTokenKind;
    function FuncInterface(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLong(Index: Integer): TtkTokenKind;
    function FuncModule(Index: Integer): TtkTokenKind;
    function FuncNative(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncOctet(Index: Integer): TtkTokenKind;
    function FuncOneway(Index: Integer): TtkTokenKind;
    function FuncOut(Index: Integer): TtkTokenKind;
    function FuncPrivate(Index: Integer): TtkTokenKind;
    function FuncPublic(Index: Integer): TtkTokenKind;
    function FuncRaises(Index: Integer): TtkTokenKind;
    function FuncReadonly(Index: Integer): TtkTokenKind;
    function FuncSequence(Index: Integer): TtkTokenKind;
    function FuncShort(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStruct(Index: Integer): TtkTokenKind;
    function FuncSupports(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncTruncatable(Index: Integer): TtkTokenKind;
    function FuncTypedef(Index: Integer): TtkTokenKind;
    function FuncUnion(Index: Integer): TtkTokenKind;
    function FuncUnsigned(Index: Integer): TtkTokenKind;
    function FuncValuebase(Index: Integer): TtkTokenKind;
    function FuncValuetype(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncWchar(Index: Integer): TtkTokenKind;
    function FuncWstring(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CommentOpenProc;
    procedure CommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure CharOpenProc;
    procedure CharProc;
    procedure PreProcessorProc;
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
    property CommentAttri: TSynEdit32HighlighterAttributes read fCommentAttri write fCommentAttri;
    property DatatypeAttri: TSynEdit32HighlighterAttributes read fDatatypeAttri write fDatatypeAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read fNumberAttri write fNumberAttri;
    property PreprocessorAttri: TSynEdit32HighlighterAttributes read fPreprocessorAttri write fPreprocessorAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..47] of UnicodeString = (
    'abstract', 'any', 'attribute', 'boolean', 'case', 'char', 'const', 
    'context', 'custom', 'default', 'double', 'enum', 'exception', 'factory', 
    'FALSE', 'fixed', 'float', 'in', 'inout', 'interface', 'local', 'long', 
    'module', 'native', 'Object', 'octet', 'oneway', 'out', 'private', 'public', 
    'raises', 'readonly', 'sequence', 'short', 'string', 'struct', 'supports', 
    'switch', 'TRUE', 'truncatable', 'typedef', 'union', 'unsigned', 
    'ValueBase', 'valuetype', 'void', 'wchar', 'wstring' 
  );

  KeyIndices: array[0..100] of Integer = (
    5, 19, 17, 7, -1, -1, -1, -1, -1, 15, 18, -1, 37, -1, 24, -1, -1, -1, 44, 
    -1, 11, 31, -1, 25, 33, -1, -1, 42, 39, -1, -1, 36, 46, -1, 27, -1, 43, 28, 
    26, 20, -1, 1, 32, 6, -1, 14, 8, -1, -1, -1, -1, 0, 35, -1, -1, -1, -1, -1, 
    -1, -1, -1, 45, 22, 47, -1, -1, 12, 4, -1, -1, -1, 10, -1, -1, 3, -1, 9, -1, 
    34, 30, 13, -1, 2, 21, 16, -1, 29, 40, -1, -1, -1, -1, -1, -1, -1, 23, -1, 
    38, -1, -1, 41 
  );

{$Q-}
function TSynIdlSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 612 + Ord(Str^) * 199;
    Inc(Str);
  end;
  Result := Result mod 101;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynIdlSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynIdlSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[51] := FuncAbstract;
  fIdentFuncTable[41] := FuncAny;
  fIdentFuncTable[82] := FuncAttribute;
  fIdentFuncTable[74] := FuncBoolean;
  fIdentFuncTable[67] := FuncCase;
  fIdentFuncTable[0] := FuncChar;
  fIdentFuncTable[43] := FuncConst;
  fIdentFuncTable[3] := FuncContext;
  fIdentFuncTable[46] := FuncCustom;
  fIdentFuncTable[76] := FuncDefault;
  fIdentFuncTable[71] := FuncDouble;
  fIdentFuncTable[20] := FuncEnum;
  fIdentFuncTable[66] := FuncException;
  fIdentFuncTable[80] := FuncFactory;
  fIdentFuncTable[45] := FuncFalse;
  fIdentFuncTable[9] := FuncFixed;
  fIdentFuncTable[84] := FuncFloat;
  fIdentFuncTable[2] := FuncIn;
  fIdentFuncTable[10] := FuncInout;
  fIdentFuncTable[1] := FuncInterface;
  fIdentFuncTable[39] := FuncLocal;
  fIdentFuncTable[83] := FuncLong;
  fIdentFuncTable[62] := FuncModule;
  fIdentFuncTable[95] := FuncNative;
  fIdentFuncTable[14] := FuncObject;
  fIdentFuncTable[23] := FuncOctet;
  fIdentFuncTable[38] := FuncOneway;
  fIdentFuncTable[34] := FuncOut;
  fIdentFuncTable[37] := FuncPrivate;
  fIdentFuncTable[86] := FuncPublic;
  fIdentFuncTable[79] := FuncRaises;
  fIdentFuncTable[21] := FuncReadonly;
  fIdentFuncTable[42] := FuncSequence;
  fIdentFuncTable[24] := FuncShort;
  fIdentFuncTable[78] := FuncString;
  fIdentFuncTable[52] := FuncStruct;
  fIdentFuncTable[31] := FuncSupports;
  fIdentFuncTable[12] := FuncSwitch;
  fIdentFuncTable[97] := FuncTrue;
  fIdentFuncTable[28] := FuncTruncatable;
  fIdentFuncTable[87] := FuncTypedef;
  fIdentFuncTable[100] := FuncUnion;
  fIdentFuncTable[27] := FuncUnsigned;
  fIdentFuncTable[36] := FuncValuebase;
  fIdentFuncTable[18] := FuncValuetype;
  fIdentFuncTable[61] := FuncVoid;
  fIdentFuncTable[32] := FuncWchar;
  fIdentFuncTable[63] := FuncWstring;
end;

function TSynIdlSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynIdlSyn.FuncAbstract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncAny(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncAttribute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncBoolean(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncChar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncContext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncCustom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncDouble(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncEnum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncException(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFactory(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFixed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncInout(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncInterface(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncLong(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncModule(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncNative(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncOctet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncOneway(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncOut(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncPrivate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncPublic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncRaises(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncReadonly(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncSequence(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncShort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncStruct(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncSupports(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncTruncatable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncTypedef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncUnion(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncUnsigned(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncValuebase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncValuetype(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncWchar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncWstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

procedure TSynIdlSyn.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynIdlSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynIdlSyn.NumberProc;

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


procedure TSynIdlSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynIdlSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynIdlSyn.CommentOpenProc;
begin
  Inc(FRun);
  if (FLine[FRun] = '*') then
  begin
    fRange := rsComment;
    CommentProc;
    FTokenID := tkComment;
  end
  else if (FLine[FRun] = '/') then
  begin
    while not IsLineEnd(FRun) do
      Inc(FRun);
    FTokenID := tkComment;
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynIdlSyn.CommentProc;
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
           (FLine[FRun + 1] = '/') then
        begin
          Inc(FRun, 2);
          fRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(FRun) then
          Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end;

procedure TSynIdlSyn.StringOpenProc;
begin
  Inc(FRun);
  fRange := rsString;
  StringProc;
  FTokenID := tkString;
end;

procedure TSynIdlSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if (FLine[FRun] = '"') then
    begin
      Inc(FRun);
      fRange := rsUnKnown;
      Break;
    end
    else if (FLine[FRun] = '\') then
      Inc(FRun);
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynIdlSyn.CharOpenProc;
begin
  Inc(FRun);
  fRange := rsChar;
  CharProc;
  FTokenID := tkString;
end;

procedure TSynIdlSyn.CharProc;
begin
  FTokenID := tkString;
  repeat
    if (FLine[FRun] = '''') then
    begin
      Inc(FRun);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(FRun) then
      Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynIdlSyn.PreProcessorProc;

  function IsWhiteChar: Boolean;
  begin
    case FLine[FRun] of
      #0, #9, #10, #13, #32:
        Result := True;
      else
        Result := False;
    end;
  end;

var
  Directive: String;
begin
  Directive := '';
  while not IsWhiteChar do
  begin
    Directive := Directive + FLine[FRun];
    Inc(FRun);
  end;
  if (WideCompareStr(Directive, '#include') = 0) then
    FTokenID := tkPreprocessor
  else if (WideCompareStr(Directive, '#pragma') = 0) then
    FTokenID := tkPreprocessor
  else
    FTokenID := tkIdentifier;
end; { PreProcessorProc }


constructor TSynIdlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  fCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fDatatypeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDatatype, SYNS_FriendlyAttrDatatype);
  fDatatypeAttri.Style := [fsBold];
  fDatatypeAttri.Foreground := clTeal;
  AddAttribute(fDatatypeAttri);

  fIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fPreprocessorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fPreprocessorAttri.Foreground := clRed;
  AddAttribute(fPreprocessorAttri);

  fSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterCORBAIDL;
  fRange := rsUnknown;
end;

procedure TSynIdlSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynIdlSyn.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynIdlSyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynIdlSyn.Next;
begin
  fTokenPos := FRun;
  case fRange of
    rsComment: CommentProc;
  else
    begin
      fRange := rsUnknown;
      case FLine[FRun] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '/': CommentOpenProc;
        '"': StringOpenProc;
        '''': CharOpenProc;
        '#': PreProcessorProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '0'..'9': NumberProc;
        '-', '+', '*', '\', ',', '.', '[', ']', '{', '}', '<', '>', '(', ')',
        '=', '?', ':', ';' : SymbolProc;
      else
        UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynIdlSyn.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynIdlSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynIdlSyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDatatype: Result := fDatatypeAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPreprocessor: Result := fPreprocessorAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynIdlSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynIdlSyn.GetSampleSource: UnicodeString;
begin
  Result := '/* CORBA IDL sample source */'#13#10 +
            '#include <sample.idl>'#13#10 +
            #13#10 +
            'const string TestString = "Hello World";'#13#10 +
            'const long TestLong = 10;'#13#10 +
            #13#10 +
            'module TestModule {'#13#10 +
            '  interface DemoInterface {'#13#10 +
            '    boolean HelloWorld(in string Message);'#13#10 +
            '  }'#13#10 +
            '}';
end;

function TSynIdlSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCORBAIDL;
end;

function TSynIdlSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynIdlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCORBAIDL;
end;

procedure TSynIdlSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynIdlSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynIdlSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynIdlSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCORBAIDL;
end;

initialization
  RegisterPlaceableHighlighter(TSynIdlSyn);
end.
