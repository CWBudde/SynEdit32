{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterM3.pas, released 2000-11-23.
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

$Id: SynHighlighterM3.pas,v 1.11.2.5 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Modula-3 syntax highlighter for SynEdit)
@author(Martin Pley <synedit@pley.de>)
@created(January 2000, converted to SynEdit November 23, 2000)
@lastmod(2000-11-23)
The SynHighlighterM3 unit provides SynEdit with a Modula-3 (.m3) highlighter.
}

unit SynEdit32.Highlighter.M3;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Highlighter.HashEntries,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkPragma,
    tkReserved, tkSpace, tkString, tkSymbol, tkUnknown, tkSyntaxError);

  TTokenRange = (trNone, trComment, trPragma);

  TRangeState = packed record
    case Boolean of
      False: (p: Pointer);
      True: (TokenRange: Word; Level: Word);
    end;

  TSynEdit32HighlighterM3 = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FPragmaAttri: TSynEdit32HighlighterAttributes;
    FReservedAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FSyntaxErrorAttri: TSynEdit32HighlighterAttributes;
    FKeywords: TSynEdit32HashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
    function HashKey(Str: PWideChar): integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymAsciiCharProc;
    procedure SymCommentHelpProc;
    procedure SymCRProc;
    procedure SymIdentProc;
    procedure SymLFProc;
    procedure SymNestedHelperProc(AOpenChar, ACloseChar: WideChar);
    procedure SymNullProc;
    procedure SymNumberProc;
    procedure SymPragmaProc;
    procedure SymPragmaHelpProc;
    procedure SymRoundOpenProc;
    procedure SymSpaceProc;
    procedure SymStringProc;
    procedure SymSymbolProc;
    procedure SymUnknownProc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  public
    property _Keywords: TSynEdit32HashEntryList read FKeywords;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property PragmaAttri: TSynEdit32HighlighterAttributes read FPragmaAttri
      write FPragmaAttri;
    property ReservedAttri: TSynEdit32HighlighterAttributes read FReservedAttri
      write FReservedAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property SyntaxErrorAttri: TSynEdit32HighlighterAttributes read FSyntaxErrorAttri
      write FSyntaxErrorAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  Keywords: UnicodeString =
    'AS,AND,ANY,ARRAY,BEGIN,BITS,BRANDED,BY,CASE,CONST,DIV,DO,ELSE,ELSIF,END,' +
    'EVAL,EXCEPT,EXCEPTION,EXIT,EXPORTS,FINALLY,FOR,FROM,GENERIC,IF,IMPORT,' +
    'IN,INTERFACE,LOCK,LOOP,METHODS,MOD,MODULE,NOT,OBJECT,OF,OR,OVERRIDES,' +
    'PROCEDURE,RAISE,RAISES,READONLY,RECORD,REF,REPEAT,RETURN,REVEAL,ROOT,' +
    'SET,THEN,TO,TRY,TYPE,TYPECASE,UNSAFE,UNTIL,UNTRACED,VALUE,VAR,WHILE,WITH';

  ReservedWords: UnicodeString =
    'ABS,ADDRESS,ADR,ADRSIZE,BITSIZE,BOOLEAN,BYTESIZE,CARDINAL,CEILING,CHAR,' +
    'DEC,DISPOSE,FALSE,FIRST,FLOAT,FLOOR,INC,INTEGER,ISTYPE,LAST,LONGFLOAT,' +
    'LONGREAL,LOOPHOLE,MAX,MIN,MUTEX,NARROW,NEW,NIL,NULL,NUMBER,ORD,REAL,' +
    'REFANY,ROUND,SUBARRAY,TEXT,TRUE,TRUNC,TYPECODE,VAL';

procedure TSynEdit32HighlighterM3.DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

function TSynEdit32HighlighterM3.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 1 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 28 + Ord(Str^) - Ord('0');
      '_': Result := 27;
      else Result := 0;
    end
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
{$IFOPT Q-}
    Result := 7 * Result + GetOrd;
{$ELSE}
    Result := (7 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
  end;
  Result := Result and $FF; // 255
  FStringLen := Str - FToIdent;
end;

function TSynEdit32HighlighterM3.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynEdit32HashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterM3.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FKeywords := TSynEdit32HashEntryList.Create;
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FPragmaAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FPragmaAttri.Style:= [fsBold];
  AddAttribute(FPragmaAttri);
  FReservedAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  AddAttribute(FReservedAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FSyntaxErrorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  FSyntaxErrorAttri.Foreground := clRed;
  AddAttribute(FSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);

  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkReserved), ReservedWords, IsIdentChar, DoAddKeyword);
  FDefaultFilter := SYNS_FilterModula3;
end;

destructor TSynEdit32HighlighterM3.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynEdit32HighlighterM3.SymAsciiCharProc;
begin
  FTokenID := tkString;
  Inc(FRun);
  while not IsLineEnd(FRun) do
  begin
    case FLine[FRun] of
      '\': if FLine[FRun + 1] = #39 then
             Inc(FRun);
      #39: begin
             Inc(FRun);
             if FLine[FRun] <> #39 then
               break;
           end;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterM3.SymCommentHelpProc;
begin
  FTokenID := tkComment;
  SymNestedHelperProc('(', ')');
end;

procedure TSynEdit32HighlighterM3.SymCRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterM3.SymIdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterM3.SymLFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterM3.SymNestedHelperProc(AOpenChar, ACloseChar: WideChar);
begin
  case FLine[FRun] of
     #0: SymNullProc;
    #10: SymLFProc;
    #13: SymCRProc;
  else
    repeat
      if FLine[FRun]= AOpenChar then
      begin
        Inc(FRun);
        if FLine[FRun] = '*' then
        begin
          Inc(FRun);
          Inc(FRange.Level);
        end;
      end
      else if FLine[FRun] = '*' then
      begin
        Inc(FRun);
        if FLine[FRun] = ACloseChar then
        begin
          Inc(FRun);
          if FRange.Level > 0 then
            Dec(FRange.Level);
          if FRange.Level = 0 then
          begin
            FRange.TokenRange := Ord(trNone);
            break
          end;
        end;
      end
      else
        Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterM3.SymNullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterM3.SymNumberProc;
var
  BasedNumber: Boolean;
  MaxDigit: Integer;

  function IsValidDigit(AChar: WideChar): Boolean;
  var
    Digit: Integer;
  begin
    case AChar of
      '0'..'9': Digit := Ord(AChar) - Ord('0');
      'a'..'f': Digit := Ord(AChar) - Ord('a');
      'A'..'F': Digit := Ord(AChar) - Ord('A');
      else Digit := -1;
    end;
    Result := (Digit >= 0) and (Digit <= MaxDigit);
  end;

  function IsExponentChar: Boolean;
  begin
    case FLine[FRun] of
      'd', 'D', 'e', 'E', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;


begin
  FTokenID := tkNumber;
  BasedNumber := False;
  MaxDigit := 9;
  // skip leading zeros, but they can be numbers too
  while FLine[FRun] = '0' do
    Inc(FRun);
  if not IsIdentChar(FLine[FRun]) then
    exit;
  // check for numbers with a base prefix
  if CharInSet(FLine[FRun], ['2'..'9']) and (FLine[FRun + 1] = '_') then
  begin
    BasedNumber := True;
    MaxDigit := Ord(FLine[FRun]) - Ord('0') - 1;
    Inc(FRun, 2);
  end
  else if (FLine[FRun] = '1') and CharInSet(FLine[FRun + 1], ['0'..'6'])
    and (FLine[FRun + 2] = '_') then
  begin
    BasedNumber := True;
    MaxDigit := 10 + Ord(FLine[FRun + 1]) - Ord('0') - 1;
    Inc(FRun, 3);
  end;
  if BasedNumber then
  begin
    // advance over all valid digits, but at least one has to be there
    if IsValidDigit(FLine[FRun]) then
    begin
      repeat
        Inc(FRun);
      until not IsValidDigit(FLine[FRun]);
    end
    else
      FTokenID := tkSyntaxError;
  end
  else
  begin
    // "normal" numbers
    repeat
      Inc(FRun);
    until not CharInSet(FLine[FRun], ['0'..'9']);
    // can include a decimal point and an exponent
    if FLine[FRun] = '.' then
    begin
      Inc(FRun);
      if CharInSet(FLine[FRun], ['0'..'9']) then
      begin
        repeat
          Inc(FRun);
        until not CharInSet(FLine[FRun], ['0'..'9']);
      end
      else
        FTokenID := tkSyntaxError; // must be a number after the '.'
    end;
    // can include an exponent
    if IsExponentChar then
    begin
      Inc(FRun);
      if CharInSet(FLine[FRun], ['+', '-']) then
        Inc(FRun);
      if CharInSet(FLine[FRun], ['0'..'9']) then
      begin
        repeat
          Inc(FRun);
        until not CharInSet(FLine[FRun], ['0'..'9']);
      end
      else // exponent must include a number
        FTokenID := tkSyntaxError;
    end;
  end;
  // it's a syntax error if there are any Identifier chars left
  if IsIdentChar(FLine[FRun]) then
  begin
    FTokenID := tkSyntaxError;
    repeat
      Inc(FRun);
    until not IsIdentChar(FLine[FRun]);
  end;
end;

procedure TSynEdit32HighlighterM3.SymPragmaProc;
begin
  Inc(FRun);
  if FLine[FRun] = '*' then
  begin
    Inc(FRun);
    FRange.TokenRange := Ord(trPragma);
    Inc(FRange.Level);
    if IsLineEnd(FRun) then
      FTokenID := tkPragma
    else
      SymPragmaHelpProc;
  end else
    FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterM3.SymPragmaHelpProc;
begin
  FTokenID := tkPragma;
  SymNestedHelperProc('<', '>');
end;

procedure TSynEdit32HighlighterM3.SymRoundOpenProc;
begin
  Inc(FRun);
  if FLine[FRun] = '*' then
  begin
    Inc(FRun);
    FRange.TokenRange := Ord(trComment);
    Inc(FRange.Level);
    if IsLineEnd(FRun) then
      FTokenID := tkComment
    else
      SymCommentHelpProc;
  end
  else
  begin
    FTokenID := tkSymbol;
    if FLine[FRun] = '.' then
      Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterM3.SymSpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterM3.SymStringProc;
begin
  FTokenID := tkString;
  Inc(FRun);
  while not IsLineEnd(FRun) do
  begin
    case FLine[FRun] of
      #34: begin
             Inc(FRun);
             break;
           end;
      '\': if CharInSet(FLine[FRun + 1], [#34, '\']) then
             Inc(FRun);
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterM3.SymSymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterM3.SymUnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterM3.Next;
begin
  fTokenPos := FRun;
  case TTokenRange(FRange.TokenRange) of
    trComment: SymCommentHelpProc;
    trPragma: SymPragmaHelpProc;
  else
    case FLine[FRun] of
      #39: SymAsciiCharProc;
      #13: SymCRProc;
      'A'..'Z', 'a'..'z', '_': SymIdentProc;
      #10: SymLFProc;
       #0: SymNullProc;
      '0'..'9': SymNumberProc;
      '(': SymRoundOpenProc;
      #1..#9, #11, #12, #14..#32: SymSpaceProc;
      '{','}','|','!', #35..#38, #42..#47, #58, #59, #61..#64, #91..#94, ')': SymSymbolProc;
      '<': SymPragmaProc;
      #34: SymStringProc;
      else SymUnknownProc;
    end;
  end;
  inherited;
end;

function TSynEdit32HighlighterM3.GetDefaultAttribute(Index: integer):
  TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterM3.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterModula3;
end;

class function TSynEdit32HighlighterM3.GetLanguageName: string;
begin
  Result := SYNS_LangModula3;
end;

function TSynEdit32HighlighterM3.GetRange: pointer;
begin
  result := FRange.p;
end;

function TSynEdit32HighlighterM3.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkPragma: Result:= FPragmaAttri;
    tkReserved: Result := FReservedAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSyntaxError: Result := FSyntaxErrorAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterM3.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterM3.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterM3.ResetRange;
begin
  FRange.p := nil;
end;

procedure TSynEdit32HighlighterM3.SetRange(Value: pointer);
begin
  FRange.p := Value;
end;

class function TSynEdit32HighlighterM3.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangModula3;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterM3);
end.
