{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPython.pas, released 2000-06-23.
The Original Code is based on the odPySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Olivier Deckmyn.
Portions created by M.Utku Karatas and Dennis Chuah.
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

$Id: SynHighlighterPython.pas,v 1.18.2.7 2008/09/14 16:25:02 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(A Python language highlighter for SynEdit)
@author(Olivier Deckmyn, converted to SynEdit by David Muir <dhmn@dmsoftware.co.uk>)
@created(unknown, converted to SynEdit on 2000-06-23)
@lastmod(2003-02-13)
The SynHighlighterPython implements a highlighter for Python for the SynEdit projects.
}

unit SynEdit32.Highlighter.Python;

{$I SynEdit.Inc}

interface

uses
  Graphics,
  SysUtils,
  Classes,
  SynEdit32.Highlighter,
  SynEdit32.Types,
  SynEdit32.Unicode;

const
  ALPHA_CHARS = ['_', 'a'..'z', 'A'..'Z'];

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkNonKeyword, tkTrippleQuotedString,
    tkSystemDefined, tkHex, tkOct, tkFloat, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown, rsMultilineString, rsMultilineString2,
                 rsMultilineString3 //this is to indicate if a string is made multiline by backslash char at line end (as in C++ highlighter)
                );

type
  TSynPythonSyn = class(TSynEdit32CustomHighlighter)
  private
    FStringStarter: WideChar;  // used only for rsMultilineString3 stuff
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TUnicodeStringList;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FDocStringAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FHexAttri: TSynEdit32HighlighterAttributes;
    FOctalAttri: TSynEdit32HighlighterAttributes;
    FFloatAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNonKeyAttri: TSynEdit32HighlighterAttributes;
    FSystemAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FErrorAttri: TSynEdit32HighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymbolProc;
    procedure CRProc;
    procedure CommentProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure PreStringProc;
    procedure UnicodeStringProc;
    procedure StringProc;
    procedure String2Proc;
    procedure StringEndProc(EndChar: WideChar);
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    function GetKeywordIdentifiers: TUnicodeStringList;
    property Keywords: TUnicodeStringList read FKeywords;
    property TokenID: TtkTokenKind read FTokenID;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
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
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
    write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
    write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NonKeyAttri: TSynEdit32HighlighterAttributes read FNonKeyAttri
      write FNonKeyAttri;
    property SystemAttri: TSynEdit32HighlighterAttributes read FSystemAttri
      write FSystemAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
    write FNumberAttri;
    property HexAttri: TSynEdit32HighlighterAttributes read FHexAttri
      write FHexAttri;
    property OctalAttri: TSynEdit32HighlighterAttributes read FOctalAttri
      write FOctalAttri;
    property FloatAttri: TSynEdit32HighlighterAttributes read FFloatAttri
      write FFloatAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
    write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
    write FStringAttri;
    property DocStringAttri: TSynEdit32HighlighterAttributes read FDocStringAttri
      write FDocStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
    write FSymbolAttri;
    property ErrorAttri: TSynEdit32HighlighterAttributes read FErrorAttri
      write FErrorAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

var
  GlobalKeywords: TUnicodeStringList;

function TSynPythonSyn.GetKeywordIdentifiers: TUnicodeStringList;
const
  // No need to localise keywords!

  // List of keywords
  KEYWORDCOUNT = 32;
  KEYWORDS: array [1..KEYWORDCOUNT] of UnicodeString =
    (
    'and',
    'as',
    'assert',
    'break',
    'class',
    'continue',
    'def',
    'del',
    'elif',
    'else',
    'except',
    'exec',
    'finally',
    'for',
    'from',
    'global',
    'if',
    'import',
    'in',
    'is',
    'lambda',
    'nonlocal',
    'not',
    'or',
    'pass',
    'print',
    'raise',
    'return',
    'try',
    'while',
    'with',
    'yield'
    );

  // List of non-keyword identifiers
  NONKEYWORDCOUNT = 65;
  NONKEYWORDS: array [1..NONKEYWORDCOUNT] of UnicodeString =
    (
    '__future__',
    '__import__',
    'abs',
    'apply',
    'buffer',
    'callable',
    'chr',
    'cmp',
    'coerce',
    'compile',
    'complex',
    'delattr',
    'dict',
    'dir',
    'divmod',
    'eval',
    'execfile',
    'False',
    'file',
    'filter',
    'float',
    'getattr',
    'globals',
    'hasattr',
    'hash',
    'help',
    'hex',
    'id',
    'input',
    'int',
    'intern',
    'isinstance',
    'issubclass',
    'iter',
    'len',
    'list',
    'locals',
    'long',
    'None',
    'NotImplemented',
    'map',
    'max',
    'min',
    'oct',
    'open',
    'ord',
    'pow',
    'range',
    'raw_input',
    'reduce',
    'reload',
    'repr',
    'round',
    'self',
    'setattr',
    'slice',
    'str',
    'True',
    'tuple',
    'type',
    'unichr',
    'unicode',
    'vars',
    'xrange',
    'zip'
    );
var
  f: Integer;
begin
  if not Assigned (GlobalKeywords) then
  begin
    // Create the string list of keywords - only once
    GlobalKeywords := TUnicodeStringList.Create;

    for f := 1 to KEYWORDCOUNT do
      GlobalKeywords.AddObject(KEYWORDS[f], Pointer(Ord(tkKey)));
    for f := 1 to NONKEYWORDCOUNT do
      GlobalKeywords.AddObject(NONKEYWORDS[f], Pointer(Ord(tkNonKeyword)));
  end; // if
  Result := GlobalKeywords;
end;

function TSynPythonSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  i: Integer;
  temp: PWideChar;
  s: UnicodeString;
begin
  // Extract the identifier out - it is assumed to terminate in a
  //   non-alphanumeric character
  FToIdent := MayBe;
  temp := MayBe;
  while IsIdentChar(temp^) do
    Inc(temp);
  FStringLen := temp - FToIdent;

  // Check to see if it is a keyword
  SetString(s, FToIdent, FStringLen);
  if FKeywords.Find(s, i) then
  begin
    // TUnicodeStringList is not case sensitive!
    if s <> FKeywords[i] then
      i := -1;
  end
  else
    i := -1;

  if i <> -1 then
    Result := TtkTokenKind(FKeywords.Objects[i])

  // Check if it is a system identifier (__*__)
  else if (FStringLen >= 5) and
     (MayBe[0] = '_') and (MayBe[1] = '_') and (MayBe[2] <> '_') and
     (MayBe[FStringLen - 1] = '_') and (MayBe[FStringLen - 2] = '_') and
     (MayBe[FStringLen - 3] <> '_') then
    Result := tkSystemDefined

  // Check for names of class and functions - not optimal
  else if ( (WideCompareStr(Trim(Copy(FLine, 0, Length(FLine) - Length(FToIdent))), 'def')=0)   or (WideCompareStr(Trim(Copy(FLine, 0, Length(FLine) - Length(FToIdent))), 'class')=0) ) then
       Result := tkSystemDefined

  // Else, hey, it is an ordinary FRun-of-the-mill identifier!
  else
    Result := tkIdentifier;
end;
  
constructor TSynPythonSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FKeywords := TUnicodeStringList.Create;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupError;
  FKeywords.Assign (GetKeywordIdentifiers);
  if not FKeywords.Sorted then
  FKeywords.Sort;

  FRange := rsUnknown;
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGray;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FNonKeyAttri := TSynEdit32HighlighterAttributes.Create (SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  FNonKeyAttri.Foreground := clNavy;
  FNonKeyAttri.Style := [fsBold];
  AddAttribute (FNonKeyAttri);
  FSystemAttri := TSynEdit32HighlighterAttributes.Create (SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  FSystemAttri.Style := [fsBold];
  AddAttribute (FSystemAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);
  FHexAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  FHexAttri.Foreground := clBlue;
  AddAttribute(FHexAttri);
  FOctalAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  FOctalAttri.Foreground := clBlue;
  AddAttribute(FOctalAttri);
  FFloatAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  FFloatAttri.Foreground := clBlue;
  AddAttribute(FFloatAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);
  FDocStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_FriendlyAttrDocumentation);
  FDocStringAttri.Foreground := clTeal;
  AddAttribute(FDocStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FErrorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  FErrorAttri.Foreground := clRed;
  AddAttribute(FErrorAttri);
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterPython;
end; { Create }

destructor TSynPythonSyn.Destroy;
begin
  FKeywords.Free;
  inherited;
end;

procedure TSynPythonSyn.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynPythonSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
  else
    Inc(FRun);
  end;
end;

procedure TSynPythonSyn.CommentProc;
begin
  FTokenID := tkComment;
  Inc(FRun);
  while not IsLineEnd(FRun) do
    Inc(FRun);
end;

procedure TSynPythonSyn.GreaterProc;
begin
  case FLine[FRun + 1] of
    '=': begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
  else begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
end;

procedure TSynPythonSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynPythonSyn.LowerProc;
begin
  case FLine[FRun + 1] of
    '=': begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end;
    '>': begin
        Inc(FRun, 2);
        FTokenID := tkSymbol;
      end
  else begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynPythonSyn.NumberProc;
type
  TNumberState =
    (
    nsStart,
    nsDotFound,
    nsFloatNeeded,
    nsHex,
    nsOct,
    nsExpFound
    );

var
  temp: WideChar;
  State: TNumberState;

  function CheckSpecialCases: Boolean;
  begin
    case temp of
      // Look for dot (.)
      '.': begin
        // .45
        if CharInSet(FLine[FRun], ['0'..'9']) then
        begin
          Inc (FRun);
          FTokenID := tkFloat;
          State := nsDotFound;

        // Non-number dot
        end else begin
          // Ellipsis
          if (FLine[FRun] = '.') and (FLine[FRun+1] = '.') then
            Inc (FRun, 2);
          FTokenID := tkSymbol;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      // Look for zero (0)
      '0': begin
        temp := FLine[FRun];
        // 0x123ABC
        if CharInSet(temp, ['x', 'X']) then begin
          Inc (FRun);
          FTokenID := tkHex;
          State := nsHex;
        // 0.45
        end else if temp = '.' then begin
          Inc (FRun);
          State := nsDotFound;
          FTokenID := tkFloat;
        end else if CharInSet(temp, ['0'..'9']) then begin
          Inc (FRun);
          // 0123 or 0123.45
          if CharInSet(temp, ['0'..'7']) then begin
            FTokenID := tkOct;
            State := nsOct;
          // 0899.45
          end else begin
            FTokenID := tkFloat;
            State := nsFloatNeeded;
          end; // if
        end; // if
      end; // ZERO
    end; // case

    Result := True;
  end; // CheckSpecialCases

  function HandleBadNumber: Boolean;
  begin
    Result := False;
    FTokenID := tkUnknown;
    // Ignore all tokens till end of "number"
    while IsIdentChar(FLine[FRun]) or (FLine[FRun] = '.') do
      Inc (FRun);
  end; // HandleBadNumber

  function HandleExponent: Boolean;
  begin
    State := nsExpFound;
    FTokenID := tkFloat;
    // Skip e[+/-]
    if CharInSet(FLine[FRun+1], ['+', '-']) then
      Inc (FRun);
    // Invalid token : 1.0e
    if not CharInSet(FLine[FRun+1], ['0'..'9']) then begin
      Inc (FRun);
      Result := HandleBadNumber;
      Exit;
    end; // if

    Result := True;
  end; // HandleExponent

  function HandleDot: Boolean;
  begin
    // Check for ellipsis
    Result := (FLine[FRun+1] <> '.') or (FLine[FRun+2] <> '.');
    if Result then begin
      State := nsDotFound;
      FTokenID := tkFloat;
    end; // if
  end; // HandleDot

  function CheckStart: Boolean;
  begin
    // 1234
    if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    //123e4
    end else if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (FRun);
      FTokenID := tkFloat;
      Result := False;
    // 123.45
    end else if temp = '.' then begin
      Result := HandleDot;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckStart

  function CheckDotFound: Boolean;
  begin
    // 1.0e4
    if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45
    end else if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (FRun);
      Result := False;
    // 123.45.45: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckDotFound

  function CheckFloatNeeded: Boolean;
  begin
    // 091.0e4
    if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 0912345
    end else if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 09123.45
    end else if temp = '.' then begin
      Result := HandleDot or HandleBadNumber; // Bad octal
    // 09123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (FRun);
      Result := False;
    // End of number (error: Bad oct number) 0912345
    end else begin
      Result := HandleBadNumber;
    end;
  end; // CheckFloatNeeded

  function CheckHex: Boolean;
  begin
    // 0x123ABC
    if CharInSet(temp, ['a'..'f', 'A'..'F', '0'..'9']) then
    begin
      Result := True;
    // 0x123ABCL
    end else if CharInSet(temp, ['l', 'L']) then begin
      Inc (FRun);
      Result := False;
    // 0x123.45: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckHex

  function CheckOct: Boolean;
  begin
    // 012345
    if CharInSet(temp, ['0'..'9']) then begin
      if not CharInSet(temp, ['0'..'7']) then begin
        State := nsFloatNeeded;
        FTokenID := tkFloat;
      end; // if
      Result := True;
    // 012345L
    end else if CharInSet(temp, ['l', 'L']) then begin
      Inc (FRun);
      Result := False;
    // 0123e4
    end else if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 0123j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (FRun);
      FTokenID := tkFloat;
      Result := False;
    // 0123.45
    end else if temp = '.' then begin
      Result := HandleDot;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckOct

  function CheckExpFound: Boolean;
  begin
    // 1e+123
    if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 1e+123j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (FRun);
      Result := False;
    // 1e4.5: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckExpFound

begin
  State := nsStart;
  FTokenID := tkNumber;

  temp := FLine[FRun];
  Inc (FRun);

  // Special cases
  if not CheckSpecialCases then
    Exit;

  // Use a state machine to parse numbers
  while True do begin
    temp := FLine[FRun];

    case State of
      nsStart:
        if not CheckStart then Exit;
      nsDotFound:
        if not CheckDotFound then Exit;
      nsFloatNeeded:
        if not CheckFloatNeeded then Exit;
      nsHex:
        if not CheckHex then Exit;
      nsOct:
        if not CheckOct then Exit;
      nsExpFound:
        if not CheckExpFound then Exit;
    end; // case

    Inc (FRun);
  end; // while
end;

procedure TSynPythonSyn.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynPythonSyn.String2Proc;
var
  fBackslashCount: Integer;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = '"') and (FLine[FRun + 2] = '"') then
  begin
    FTokenID := tkTrippleQuotedString;
    Inc(FRun, 3);

    FRange := rsMultilineString2;
    while FLine[FRun] <> #0 do
    begin
      case FLine[FRun] of

        '\':begin
               { If we're looking at a backslash, and the following character is an
               end quote, and it's preceeded by an odd number of backslashes, then
               it shouldn't mark the end of the string.  If it's preceeded by an
               even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
               if FLine[FRun + 1] = '"' then
                 begin
                   fBackslashCount := 1;

                   while ((FRun > fBackslashCount) and (FLine[FRun - fBackslashCount] = '\')) do
                     fBackslashCount := fBackslashCount + 1;

                   if (fBackslashCount mod 2 = 1) then Inc(FRun)
               end;
               Inc(FRun);
            end;// '\':

        '"':
          if (FLine[FRun + 1] = '"') and (FLine[FRun + 2] = '"') then begin
            FRange := rsUnKnown;
            Inc(FRun, 3);
            exit;
          end else
            Inc(FRun);
        #10: exit;
        #13: exit;
        else
          Inc(FRun);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[FRun] of
      #0, #10, #13:
        begin
          if FLine[FRun-1] = '\' then
          begin
            FStringStarter := '"';
            FRange := rsMultilineString3;
          end;
          Break;
        end;
      {The same backslash stuff above...}
      '\':begin
             if FLine[FRun + 1] = '"' then
               begin
                 fBackslashCount := 1;

                 while ((FRun > fBackslashCount) and (FLine[FRun - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then Inc(FRun)
             end;
             Inc(FRun);
          end;// '\':

      else Inc(FRun);
    end; //case
  until (FLine[FRun] = '"');
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynPythonSyn.PreStringProc;
var
  temp: WideChar;
begin
  // Handle python raw strings
  // r""
  temp := FLine[FRun + 1];
  if temp = '''' then
  begin
    Inc (FRun);
    StringProc;
  end
  else if temp = '"' then
  begin
    Inc (FRun);
    String2Proc;
  end
  else
  begin
    // If not followed by quote char, must be ident
    IdentProc;
  end; // if
end;

procedure TSynPythonSyn.UnicodeStringProc;
begin
  // Handle python raw and unicode strings
  // Valid syntax: u"", or ur""
  if CharInSet(FLine[FRun + 1], ['r', 'R']) and
    CharInSet(FLine[FRun + 2], ['''', '"']) then
  begin
    // for ur, Remove the "u" and...
    Inc (FRun);
  end;
  // delegate to raw strings
  PreStringProc;
end;

procedure TSynPythonSyn.StringProc;
var
  fBackslashCount: Integer;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #39) and (FLine[FRun + 2] = #39) then begin
    FTokenID := tkTrippleQuotedString;
    Inc(FRun, 3);

    FRange:=rsMultilineString;
    while FLine[FRun] <> #0 do begin
      case FLine[FRun] of

        '\': begin
             { If we're looking at a backslash, and the following character is an
             end quote, and it's preceeded by an odd number of backslashes, then
             it shouldn't mark the end of the string.  If it's preceeded by an
             even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
              if FLine[FRun + 1] = #39 then
                begin
                  fBackslashCount := 1;

                  while ((FRun > fBackslashCount) and (FLine[FRun - fBackslashCount] = '\')) do
                    fBackslashCount := fBackslashCount + 1;

                  if (fBackslashCount mod 2 = 1) then Inc(FRun)
              end;
              Inc(FRun);
            end;// '\':

        #39:
          if (FLine[FRun + 1] = #39) and (FLine[FRun + 2] = #39) then begin
            FRange := rsUnKnown;
            Inc(FRun, 3);
            EXIT;
          end else
            Inc(FRun);
        #10: EXIT;
        #13: EXIT;
        else
          Inc(FRun);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[FRun] of
      #0, #10, #13 : begin
        if FLine[FRun-1] = '\' then begin
          FStringStarter := #39;
          FRange := rsMultilineString3;
        end;
        BREAK;
        end;

      {The same backslash stuff above...}
      '\':begin
             if FLine[FRun + 1] = #39 then
               begin
                 fBackslashCount := 1;

                 while ((FRun > fBackslashCount) and (FLine[FRun - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then Inc(FRun)
             end;
             Inc(FRun);
          end;// '\':

      else Inc(FRun);
    end; //case
  until (FLine[FRun] = #39);
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynPythonSyn.StringEndProc(EndChar: WideChar);
var
  fBackslashCount: Integer;
begin
  if FRange = rsMultilineString3 then
    FTokenID := tkString
  else
    FTokenID := tkTrippleQuotedString;

  case FLine[FRun] of
    #0:
      begin
        NullProc;
        EXIT;
      end;
    #10:
      begin
        LFProc;
        EXIT;
    end;
    #13:
      begin
        CRProc;
        EXIT;
      end;
  end;

  if FRange = rsMultilineString3 then begin
    repeat
      if FLine[FRun]=FStringStarter then begin
        Inc(FRun);
        FRange:=rsUnknown;
        EXIT;
      end else if FLine[FRun]='\' then ;  {The same backslash stuff above...}
          begin
             if FLine[FRun + 1] = FStringStarter then
               begin
                 fBackslashCount := 1;

                 while ((FRun > fBackslashCount) and (FLine[FRun - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then Inc(FRun);
             end;
           end;// if FLine[FRun]...

      Inc(FRun);
    until IsLineEnd(FRun);
    if FLine[FRun-1]<>'\' then begin
      FRange:=rsUnknown;
      EXIT;
    end;
  end else
  repeat
    if FLine[FRun] = '\' then
    begin
       if FLine[FRun + 1] = EndChar then
         begin
           fBackslashCount := 1;

           while ((FRun > fBackslashCount) and (FLine[FRun - fBackslashCount] = '\')) do
             fBackslashCount := fBackslashCount + 1;

           if (fBackslashCount mod 2 = 1) then Inc(FRun, 2);
       end;
     end;// if FLine[FRun]...
    if (FLine[FRun]=EndChar) and (FLine[FRun+1]=EndChar) and (FLine[FRun+2]=EndChar) then begin
      Inc(FRun,3);
      FRange:=rsUnknown;
      EXIT;
    end;
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynPythonSyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynPythonSyn.Next;
begin
  fTokenPos := FRun;

  case FRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    rsMultilineString3:
      StringEndProc(FStringStarter);
    else
      case FLine[FRun] of
        '&', '}', '{', ':', ',', ']', '[', '*', '`',
        '^', ')', '(', ';', '/', '=', '-', '+', '!', '\',
        '%', '|', '~' :
          SymbolProc;
        #13: CRProc;
        '#': CommentProc;
        '>': GreaterProc;
        'A'..'Q', 'S', 'T', 'V'..'Z', 'a'..'q', 's', 't', 'v'..'z', '_': IdentProc;
        #10: LFProc;
        '<': LowerProc;
        #0: NullProc;
        '.', '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'r', 'R': PreStringProc;
        'u', 'U': UnicodeStringProc;
        '''': StringProc;
        '"': String2Proc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynPythonSyn.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynPythonSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynPythonSyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNonKeyword: Result := FNonKeyAttri;
    tkSystemDefined: Result := FSystemAttri;
    tkNumber: Result := FNumberAttri;
    tkHex: Result := FHexAttri;
    tkOct: Result := FOctalAttri;
    tkFloat: Result := FFloatAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkTrippleQuotedString: Result := FDocStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynPythonSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynPythonSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynPythonSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPython;
end;

class function TSynPythonSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPython;
end;

function TSynPythonSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '#!/usr/local/bin/python'#13#10 +
    'import string, sys'#13#10 +
    '""" If no arguments were given, print a helpful message """'#13#10 +
    'if len(sys.argv)==1:'#13#10 +
    '    print ''Usage: celsius temp1 temp2 ...'''#13#10 +
    '    sys.exit(0)';
end;

class function TSynPythonSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPython;
end;

initialization
  RegisterPlaceableHighlighter(TSynPythonSyn);
finalization
  GlobalKeywords.Free;
end.
