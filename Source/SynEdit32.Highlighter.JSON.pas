{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJSON.pas, released 2015-01-14.
The Initial Author of this file is Christian-W. Budde.
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

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
}

unit SynEdit32.Highlighter.JSON;

{$I SynEdit32.Inc}

interface

uses
  Graphics,
  Registry,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils, Classes;

type
  TtkTokenKind = (tkString, tkReserved, tkNull, tkNumber, tkSpace,
    tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsAttribute, rsObjectValue, rsArrayValue);

type
  TSynEdit32HighlighterJSON = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FReservedAttri: TSynEdit32HighlighterAttributes;
    FAttributeAttri: TSynEdit32HighlighterAttributes;
    FValueAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    procedure CloseArrayProc;
    procedure CloseObjectProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OpenArrayProc;
    procedure OpenObjectProc;
    procedure ReservedWordProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
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
    property AttributeAttri: TSynEdit32HighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
    property ReservedAttri: TSynEdit32HighlighterAttributes read FReservedAttri
      write FReservedAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property ValueAttri: TSynEdit32HighlighterAttributes read FValueAttri
      write FValueAttri;
  end;

implementation

uses
  SynEdit32.StrConst;


{ TSynEdit32HighlighterJSON }

constructor TSynEdit32HighlighterJSON.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  // Attribute
  FAttributeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAttribute,
    SYNS_FriendlyAttrAttribute);
  FAttributeAttri.Foreground := clNavy;
  AddAttribute(FAttributeAttri);

  // reserved words ("true", "false", "null")
  FReservedAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord,
    SYNS_FriendlyAttrReservedWord);
  FReservedAttri.Style := [fsBold];
  AddAttribute(FReservedAttri);

  // numbers
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber,
    SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clRed;
  AddAttribute(FNumberAttri);

  // spaces
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace,
    SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  // symbols
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol,
    SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := clGreen;
  AddAttribute(FSymbolAttri);

  // Value
  FValueAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrValue,
    SYNS_FriendlyAttrValue);
  FValueAttri.Foreground := clBlue;
  AddAttribute(FValueAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterJSON;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterJSON.CloseArrayProc;
begin
  SymbolProc;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterJSON.CloseObjectProc;
begin
  SymbolProc;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterJSON.ColonProc;
begin
  SymbolProc;
  FRange := rsObjectValue;
end;

procedure TSynEdit32HighlighterJSON.CommaProc;
begin
  SymbolProc;
  if FRange = rsObjectValue then
    FRange := rsAttribute;
end;

procedure TSynEdit32HighlighterJSON.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterJSON.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterJSON.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterJSON.NumberProc;

  function ExpectDigit: Boolean;
  begin
    Result := CharInSet(FLine[FRun], ['0'..'9']);
    while CharInSet(FLine[FRun], ['0'..'9']) do
      Inc(FRun);
  end;

begin
  FTokenID := tkNumber;

  if FLine[FRun] = '-' then
    Inc(FRun);

  // ensure that a zero is followed by a dot
  if FLine[FRun] = '0' then
    if FLine[FRun + 1] <> '.' then
    begin
      FTokenID := tkUnknown;
      while (FLine[FRun] <> #32) and not IsLineEnd(FRun) do Inc(FRun);
      Exit;
    end;

  // at least any digit must appear here
  if not ExpectDigit then
  begin
    FTokenID := tkUnknown;
    while (FLine[FRun] <> #32) and not IsLineEnd(FRun) do Inc(FRun);
    Exit;
  end;

  // check for dot
  if FLine[FRun] = '.' then
  begin
    // advance
    Inc(FRun);

    // at least any digit must appear after a dot!
    if not ExpectDigit then
    begin
      FTokenID := tkUnknown;
      while (FLine[FRun] <> #32) and not IsLineEnd(FRun) do Inc(FRun);
      Exit;
    end;
  end;

  // check for an exponent
  if CharInSet(FLine[FRun], ['e', 'E']) then
  begin
    Inc(FRun);

    // allow +/- here
    if CharInSet(FLine[FRun], ['+', '-']) then
      Inc(FRun);

    // at least any digit must appear here
    if not ExpectDigit then
    begin
      FTokenID := tkUnknown;
      while (FLine[FRun] <> #32) and not IsLineEnd(FRun) do Inc(FRun);
      Exit;
    end;
  end;
end;

procedure TSynEdit32HighlighterJSON.OpenArrayProc;
begin
  SymbolProc;
  FRange := rsArrayValue;
end;

procedure TSynEdit32HighlighterJSON.OpenObjectProc;
begin
  SymbolProc;
  FRange := rsAttribute;
end;

procedure TSynEdit32HighlighterJSON.ReservedWordProc;

  procedure SkipToken;
  begin
    while (FLine[FRun] <> #32) and (FLine[FRun] <> ',') and not IsLineEnd(FRun) do
      Inc(FRun);
  end;

begin
  FTokenID := tkUnknown;
  case FLine[FRun] of
    'n':
      if (FLine[FRun + 1] = 'u') and
         (FLine[FRun + 2] = 'l') and
         (FLine[FRun + 3] = 'l') then
      begin
        FTokenID := tkReserved;
        Inc(FRun, 4);
      end
      else
        SkipToken;
    't':
      if (FLine[FRun + 1] = 'r') and
         (FLine[FRun + 2] = 'u') and
         (FLine[FRun + 3] = 'e') then
      begin
        FTokenID := tkReserved;
        Inc(FRun, 4);
      end
      else
        SkipToken;
    'f':
      if (FLine[FRun + 1] = 'a') and
         (FLine[FRun + 2] = 'l') and
         (FLine[FRun + 3] = 's') and
         (FLine[FRun + 4] = 'e') then
      begin
        FTokenID := tkReserved;
        Inc(FRun, 5);
      end
      else
        SkipToken;
    else
      SkipToken;
  end;
end;

procedure TSynEdit32HighlighterJSON.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterJSON.StringProc;

  function IsHex(Digit: AnsiChar): Boolean; overload;
  begin 
    Result := CharInSet(Digit, ['0'..'9', 'A'..'F', 'a'..'f']); 
  end; 

  function IsHex(Digit: WideChar): Boolean; overload;
  begin
    Result := CharInSet(Digit, ['0'..'9', 'A'..'F', 'a'..'f']);
  end;

begin
  FTokenID := tkString;

  repeat
    Inc(FRun);
    case FLine[FRun] of
      '"':
        begin
          Inc(FRun);
          Break;
        end;
      '\':
        case FLine[FRun + 1] of
          '"', '/', '\', 'b', 'f', 'n', 'r', 't':
            Inc(FRun);
          'u':
            begin
              Inc(FRun);
              if not (IsHex(FLine[FRun + 1]) and IsHex(FLine[FRun + 2]) and
                IsHex(FLine[FRun + 3]) and IsHex(FLine[FRun + 4])) then
              begin
                // a 4 hex digit is expected
                FTokenID := tkUnknown;
                while not CharInSet(FLine[FRun], [#32, '"']) and not IsLineEnd(FRun) do
                  Inc(FRun);
                Exit;
              end;
              Inc(FRun, 4);
            end;
        end;
    end;
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterJSON.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterJSON.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterJSON.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
    #0: NullProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #10: LFProc;
    #13: CRProc;
    '0'..'9', '-': NumberProc;
    't',
    'f',
    'n' : ReservedWordProc;
    '"': StringProc;
    ':': ColonProc;
    '{': OpenObjectProc;
    '[': OpenArrayProc;
    '}': CloseObjectProc;
    ']': CloseArrayProc;
    ',' : CommaProc;
    else UnknownProc;
  end;

  inherited;
end;

function TSynEdit32HighlighterJSON.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_KEYWORD: Result := FReservedAttri;
    SYN_ATTR_IDENTIFIER: Result := FAttributeAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    SYN_ATTR_STRING: Result := FValueAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterJSON.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterJSON.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterJSON.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkString:
      if FRange in [rsObjectValue, rsArrayValue] then
        Result := FValueAttri
      else
        Result := FAttributeAttri;
    tkReserved: Result := FReservedAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FAttributeAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterJSON.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterJSON.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterJSON.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterJSON.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterJSON;
end;

class function TSynEdit32HighlighterJSON.GetLanguageName: string;
begin
  Result := SYNS_LangJSON;
end;

function TSynEdit32HighlighterJSON.GetSampleSource: UnicodeString;
begin
  Result :=
    '{'#13#10 +
    '  "firstName": "John",'#13#10 +
    '  "lastName": "Smith",'#13#10 +
    '  "isAlive": true,'#13#10 +
    '  "age": 25,'#13#10 +
    '  "height_cm": 167.6,'#13#10 +
    '  "address": {'#13#10 +
    '    "streetAddress": "21 2nd Street",'#13#10 +
    '    "city": "New York",'#13#10 +
    '    "state": "NY",'#13#10 +
    '    "postalCode": "10021-3100"'#13#10 +
    '  },'#13#10 +
    '  "phoneNumbers": ['#13#10 +
    '    {'#13#10 +
    '      "type": "home",'#13#10 +
    '      "number": "212 555-1234"'#13#10 +
    '    },'#13#10 +
    '    {'#13#10 +
    '      "type": "office",'#13#10 +
    '      "number": "646 555-4567"'#13#10 +
    '    }'#13#10 +
    '  ],'#13#10 +
    '  "face": "\uD83D\uDE02",'#13#10 +
    '  "children": [],'#13#10 +
    '  "spouse": null'#13#10 +
    '}';
end;

class function TSynEdit32HighlighterJSON.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangJSON;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterJSON);
end.
