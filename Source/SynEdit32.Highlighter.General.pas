{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGeneral.pas, released 2000-04-07.
The Original Code is based on the mwGeneralSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions written by Martin Waldenburg are copyright 1999 Martin Waldenburg.
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

$Id: SynHighlighterGeneral.pas,v 1.12 2011/04/14 15:12:54 Egg Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a customizable highlighter for SynEdit)
@author(Martin Waldenburg, converted to SynEdit by Michael Hieke)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterGeneral unit provides a customizable highlighter for SynEdit.
}

unit SynEdit32.Highlighter.General;

{$I SynEdit.inc}

interface

uses
  Windows,
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkPreprocessor, tkSpace, tkString, tkSymbol, tkUnknown);

  TCommentStyle = (csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csBasStyle,
    csCPPStyle);
  TCommentStyles = set of TCommentStyle;

  TRangeState = (rsANil, rsAnsi, rsPasStyle, rsCStyle, rsString, rsUnknown);

  TStringDelim = (sdSingleQuote, sdDoubleQuote, sdSingleAndDoubleQuote);

  TGetTokenAttributeEvent = procedure (attribute : TSynEdit32HighlighterAttributes) of object;

const
   cDefaultIdentChars = '_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                         'abcdefghijklmnopqrstuvwxyz';

type
  TSynEdit32HighlighterGeneral = class(TSynEdit32CustomHighlighter)
  private
    FIdentChars: UnicodeString;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FPreprocessorAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FKeyWords: TUnicodeStrings;
    FComments: TCommentStyles;
    FStringDelim: TStringDelim;
    FDetectPreprocessor: Boolean;
    FOnGetTokenAttribute: TGetTokenAttributeEvent;
    FStringMultiLine : Boolean;
    FStringDelimChar: WideChar;
    procedure AsciiCharProc;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure SetKeyWords(const Value: TUnicodeStrings);
    procedure SetComments(Value: TCommentStyles);
    function GetStringDelim: TStringDelim;
    procedure SetStringDelim(const Value: TStringDelim);
    function GetIdentifierChars: UnicodeString;
    procedure SetIdentifierChars(const Value: UnicodeString);
    function StoreIdentChars : Boolean;
    procedure SetDetectPreprocessor(Value: boolean);
    procedure SetStringMultiLine(const Value: Boolean);
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function IsStringDelim(aChar : WideChar) : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetCharBeforeToken(offset : Integer = -1) : WideChar;
    function GetCharAfterToken(offset : Integer = 1) : WideChar;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    {$IFNDEF SYN_CLX}
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;
    {$ENDIF}
    property OnGetTokenAttribute : TGetTokenAttributeEvent read FOnGetTokenAttribute write FOnGetTokenAttribute;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property Comments: TCommentStyles read FComments write SetComments default [];
    property DetectPreprocessor: boolean read FDetectPreprocessor write SetDetectPreprocessor;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property IdentifierChars: UnicodeString read GetIdentifierChars write SetIdentifierChars stored StoreIdentChars;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property KeyWords: TUnicodeStrings read FKeyWords write SetKeyWords;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri write FNumberAttri;
    property PreprocessorAttri: TSynEdit32HighlighterAttributes read FPreprocessorAttri write FPreprocessorAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property StringDelim: TStringDelim read GetStringDelim write SetStringDelim default sdSingleQuote;
    property StringMultiLine: Boolean read FStringMultiLine write SetStringMultiLine;
  end;

implementation

uses
  Registry, SynEdit32.StrConst;

constructor TSynEdit32HighlighterGeneral.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyWords := TUnicodeStringList.Create;
  TUnicodeStringList(FKeyWords).Sorted := True;
  TUnicodeStringList(FKeyWords).Duplicates := dupIgnore;
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
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FPreprocessorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FPreprocessorAttri);
  SetAttributesOnChange(DefHighlightChange);

  FStringDelim := sdSingleQuote;
  FIdentChars := cDefaultIdentChars;
  FRange := rsUnknown;
end; { Create }

destructor TSynEdit32HighlighterGeneral.Destroy;
begin
  FKeyWords.Free;
  inherited Destroy;
end; { Destroy }

function TSynEdit32HighlighterGeneral.IsIdentChar(AChar: WideChar): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(FIdentChars) do
    if AChar = FIdentChars[i] then
    begin
      Result := True;
      Exit;
    end;
end;

function TSynEdit32HighlighterGeneral.IsKeyword(const AKeyword: UnicodeString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := FKeyWords.Count - 1;
  Result := False;
  Token := SynWideUpperCase(AKeyword);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareText(FKeyWords[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else if Compare < 0 then
      First := I + 1
    else
      Last := I - 1;
  end;
end; { IsKeyWord }

function TSynEdit32HighlighterGeneral.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result := inherited IsWordBreakChar(AChar) and not IsIdentChar(AChar);
end;

procedure TSynEdit32HighlighterGeneral.AnsiProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[FRun] = '*') and (FLine[FRun + 1] = ')') then
      begin
        FRange := rsUnknown;
        Inc(FRun, 2);
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterGeneral.PasStyleProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if FLine[FRun] = '}' then
      begin
        FRange := rsUnknown;
        Inc(FRun);
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterGeneral.CStyleProc;
begin
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
      begin
        FRange := rsUnknown;
        Inc(FRun, 2);
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterGeneral.AsciiCharProc;
begin
  if FDetectPreprocessor then
  begin
    FTokenID := tkPreprocessor;
    repeat
      Inc(FRun);
    until IsLineEnd(FRun);
  end
  else
  begin
    FTokenID := tkString;
    repeat
      Inc(FRun);
    until not CharInSet(FLine[FRun], ['0'..'9']);
  end;
end;

procedure TSynEdit32HighlighterGeneral.BraceOpenProc;
begin
  if csPasStyle in FComments then
  begin
    FTokenID := tkComment;
    FRange := rsPasStyle;
    Inc(FRun);
    while FLine[FRun] <> #0 do
      case FLine[FRun] of
        '}':
          begin
            FRange := rsUnknown;
            Inc(FRun);
            break;
          end;
        #10: break;

        #13: break;
      else
        Inc(FRun);
      end;
  end
  else
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterGeneral.PointCommaProc;
begin
  if (csASmStyle in FComments) or (csBasStyle in FComments) then
  begin
    FTokenID := tkComment;
    FRange := rsUnknown;
    Inc(FRun);
    while FLine[FRun] <> #0 do
    begin
      FTokenID := tkComment;
      Inc(FRun);
    end;
  end
  else
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterGeneral.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterGeneral.IdentProc;
begin
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
  if IsKeyWord(GetToken) then
    FTokenID := tkKey
  else
    FTokenID := tkIdentifier;
end;

procedure TSynEdit32HighlighterGeneral.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(FRun);
  FTokenID := tkNumber;
  while IsIntegerChar do Inc(FRun);
end;

procedure TSynEdit32HighlighterGeneral.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterGeneral.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterGeneral.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'e', 'E', 'x':
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
      'x': begin // handle C style hex numbers
             IntegerProc;
             break;
           end;
      '.':
        if FLine[FRun + 1] = '.' then break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterGeneral.RoundOpenProc;
begin
  Inc(FRun);
  if csAnsiStyle in FComments then
  begin
    case FLine[FRun] of
      '*':
        begin
          FTokenID := tkComment;
          FRange := rsAnsi;
          Inc(FRun);
          while FLine[FRun] <> #0 do
            case FLine[FRun] of
              '*':
                if FLine[FRun + 1] = ')' then
                begin
                  FRange := rsUnknown;
                  Inc(FRun, 2);
                  break;
                end else Inc(FRun);
              #10: break;
              #13: break;
            else Inc(FRun);
            end;
        end;
      '.':
        begin
          Inc(FRun);
          FTokenID := tkSymbol;
        end;
    else
      begin
        FTokenID := tkSymbol;
      end;
    end;
  end else FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterGeneral.SlashProc;
begin
  Inc(FRun);
  case FLine[FRun] of
    '/':
      begin
        if csCPPStyle in FComments then
        begin
          FTokenID := tkComment;
          Inc(FRun);
          while FLine[FRun] <> #0 do
          begin
            case FLine[FRun] of
              #10, #13: break;
            end;
            Inc(FRun);
          end;
        end
        else
          FTokenID := tkSymbol;
      end;
    '*':
      begin
        if csCStyle in FComments then
        begin
          FTokenID := tkComment;
          FRange := rsCStyle;
          Inc(FRun);
          while FLine[FRun] <> #0 do
            case FLine[FRun] of
              '*':
                if FLine[FRun + 1] = '/' then
                begin
                  FRange := rsUnknown;
                  Inc(FRun, 2);
                  break;
                end else Inc(FRun);
              #10, #13:
                break;
              else
                Inc(FRun);
            end;
        end
        else
          FTokenID := tkSymbol;
      end;
    else
      FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterGeneral.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterGeneral.StringProc;
begin
  FTokenID := tkString;
  FRange := rsString;
  if IsStringDelim(FLine[FRun + 1]) and IsStringDelim(FLine[FRun + 2]) then
    Inc(FRun, 2);

  // eventualy store the string delimiter
  if FStringDelimChar = #0 then
    FStringDelimChar := FLine[FRun];

  Inc(FRun);
  while not IsLineEnd(FRun) do
  begin
    if FLine[FRun] = FStringDelimChar then
    begin
      Inc(FRun);
      if FLine[FRun] <> FStringDelimChar then
      begin
        FRange := rsUnknown;
        FStringDelimChar := #0;
        break;
      end;
    end;
    Inc(FRun);
  end;

  if IsLineEnd(FRun) and (not FStringMultiLine) then
  begin
    FRange := rsUnknown;
    FStringDelimChar := #0;
  end;
end;

procedure TSynEdit32HighlighterGeneral.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterGeneral.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
    rsString: StringProc;
  else
    if IsStringDelim(FLine[FRun]) then
      StringProc
    else
      case FLine[FRun] of
        '#':
          AsciiCharProc;
        '{':
          BraceOpenProc;
        ';':
          PointCommaProc;
        #13:
          CRProc;
        'A'..'Z', 'a'..'z', '_':
          IdentProc;
        '$':
          IntegerProc;
        #10:
          LFProc;
        #0:
          NullProc;
        '0'..'9':
          NumberProc;
        '(':
          RoundOpenProc;
        '/':
          SlashProc;
        #1..#9, #11, #12, #14..#32:
          SpaceProc;
        else
          UnknownProc;
      end;
  end;
  inherited;
end;

function TSynEdit32HighlighterGeneral.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterGeneral.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterGeneral.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

// GetCharBeforeToken
//
function TSynEdit32HighlighterGeneral.GetCharBeforeToken(offset: Integer = -1): WideChar;
begin
  if fTokenPos + offset >= 0 then
    Result := FLine[fTokenPos + offset]
  else
    Result := #0;
end;

// GetCharAfterToken
//
function TSynEdit32HighlighterGeneral.GetCharAfterToken(offset: Integer = 1): WideChar;
begin
  Result := FLine[fTokenPos + offset];
end;

function TSynEdit32HighlighterGeneral.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkPreprocessor: Result := FPreprocessorAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
  if Assigned(FOnGetTokenAttribute) then
    FOnGetTokenAttribute(Result);
end;

function TSynEdit32HighlighterGeneral.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterGeneral.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterGeneral.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterGeneral.SetKeyWords(const Value: TUnicodeStrings);
var
  i: Integer;
begin
  if Value <> nil then
  begin
    Value.BeginUpdate;
    for i := 0 to Value.Count - 1 do
      Value[i] := SynWideUpperCase(Value[i]);
    Value.EndUpdate;
  end;

  TUnicodeStringList(FKeyWords).Sorted := False;
  FKeyWords.Assign(Value);
  TUnicodeStringList(FKeyWords).Sorted := True;

  DefHighLightChange(nil);
end;

procedure TSynEdit32HighlighterGeneral.SetComments(Value: TCommentStyles);
begin
  if FComments <> Value then
  begin
    FComments := Value;
    DefHighLightChange(Self);
  end;
end;

class function TSynEdit32HighlighterGeneral.GetLanguageName: string;
begin
  Result := SYNS_LangGeneral;
end;

{$IFNDEF SYN_CLX}
function TSynEdit32HighlighterGeneral.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('KeyWords') then KeyWords.Text:= r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else Result := false;
  finally r.Free; end;
end;

function TSynEdit32HighlighterGeneral.SaveToRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      {$IFNDEF SYN_COMPILER_25_UP}
      Result := true;
      {$ENDIF}
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else Result := false;
  finally r.Free; end;
end;
{$ENDIF}

function TSynEdit32HighlighterGeneral.GetStringDelim: TStringDelim;
begin
  Result := FStringDelim;
end;

procedure TSynEdit32HighlighterGeneral.SetStringDelim(const Value: TStringDelim);
begin
  if FStringDelim <> Value then
  begin
    FStringDelim := Value;
    DefHighLightChange(Self);
  end;
end;

procedure TSynEdit32HighlighterGeneral.SetStringMultiLine(const Value: Boolean);
begin
  if FStringMultiLine <> Value then
  begin
    FStringMultiLine := Value;
    DefHighLightChange(Self);
  end;
end;

function TSynEdit32HighlighterGeneral.GetIdentifierChars: UnicodeString;
begin
  Result := FIdentChars;
end;

procedure TSynEdit32HighlighterGeneral.SetIdentifierChars(const Value: UnicodeString);
begin
  FIdentChars := Value;
end;

function TSynEdit32HighlighterGeneral.StoreIdentChars : Boolean;
begin
  Result := (FIdentChars <> cDefaultIdentChars);
end;

procedure TSynEdit32HighlighterGeneral.SetDetectPreprocessor(Value: boolean);
begin
  if Value <> FDetectPreprocessor then
  begin
    FDetectPreprocessor := Value;
    DefHighlightChange(Self);
  end;
end;

class function TSynEdit32HighlighterGeneral.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGeneral;
end;

// IsStringDelim
//
function TSynEdit32HighlighterGeneral.IsStringDelim(aChar : WideChar): Boolean;
begin
  case FStringDelim of
    sdSingleQuote:
      Result := (aChar = '''');
    sdDoubleQuote:
      Result := (aChar = '"');
  else
    Result := (aChar = '''') or (aChar = '"');
  end;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterGeneral);
end.
