{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterURI.pas, released 2003-04-10.
The initial author of this file is Ma�l H�rz.
Unicode translation by Ma�l H�rz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterURI.pas,v 1.16.2.9 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an URI syntax highlighter for SynEdit)
@author(Ma�l H�rz)
@created(2003)
@lastmod(2004-03-19)
http://www.mh-net.de.vu

The SynHighlighterURI unit implements an URI syntax highlighter for SynEdit.

Recognition of URIs is based on the information provided in the document
"Uniform Resource Identifiers (URI): Generic Syntax" of "The Internet Society",
that can be found at http://www.ietf.org/rfc/rfc2396.txt.

Also interesting is http://www.freesoft.org/CIE/RFC/1738/33.htm which describes
general URL syntax and major protocols.

these protocols are recognized:
-------------------------------
http://
https://
ftp://
mailto:
news: or news://
nntp://
telnet://
gopher://
prospero://
wais://

as well as commonly used shorthands:
------------------------------------
someone@somewhere.org
www.host.org
}

unit SynEdit32.Highlighter.URI;

{$I SynEdit.Inc}

interface

uses
  Graphics, SysUtils, Classes,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode;

type
  TtkTokenKind = (tkNull, tkSpace, tkFtpLink, tkGopherLink,
    tkHttpLink, tkHttpsLink, tkMailtoLink, tkNewsLink, tkNntpLink,
    tkProsperoLink, tkTelnetLink, tkWaisLink, tkWebLink, tkUnknown, tkNullChar);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Key: Integer): TtkTokenKind of object;

  TAlreadyVisitedURIFunc = function (URI: UnicodeString): Boolean of object;

  TSynURISyn = class(TSynEdit32CustomHighlighter)
  private
    FMayBeProtocol: PWideChar;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..15] of TIdentFuncTableFunc;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FURIAttri: TSynEdit32HighlighterAttributes;
    FVisitedURIAttri: TSynEdit32HighlighterAttributes;
    FAlreadyVisitedURI: TAlreadyVisitedURIFunc;

    function HashKey(Str: PWideChar): Integer;
    procedure InitIdent;

    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure ProtocolProc;
    procedure SpaceProc;
    procedure UnknownProc;

    function AltFunc(Key: Integer): TtkTokenKind;
    function FuncFtp(Key: Integer): TtkTokenKind;
    function FuncGopher(Key: Integer): TtkTokenKind;
    function FuncHttp(Key: Integer): TtkTokenKind;
    function FuncHttps(Key: Integer): TtkTokenKind;
    function FuncMailto(Key: Integer): TtkTokenKind;
    function FuncNews(Key: Integer): TtkTokenKind;
    function FuncNntp(Key: Integer): TtkTokenKind;
    function FuncProspero(Key: Integer): TtkTokenKind;
    function FuncTelnet(Key: Integer): TtkTokenKind;
    function FuncWais(Key: Integer): TtkTokenKind;
    function FuncWeb(Key: Integer): TtkTokenKind;

    function IsAlphaNum(AChar: WideChar): Boolean;
    function IsMark(AChar: WideChar): Boolean;
    function IsReserved(AChar: WideChar): Boolean;
    function IsUnreserved(AChar: WideChar): Boolean;
    function IsURIChar(AChar: WideChar): Boolean;
    function IsNeverAtEnd(AChar: WideChar): Boolean;
    function IsEMailAddressChar(AChar: WideChar): Boolean;
    function IsNeverAtEMailAddressEnd(AChar: WideChar): Boolean;

    function IsValidEmailAddress: Boolean;
    function IsValidURI: Boolean;
    function IsValidWebLink: Boolean;

    procedure SetURIAttri(const Value: TSynEdit32HighlighterAttributes);
    procedure SetVisitedURIAttri(const Value: TSynEdit32HighlighterAttributes);
  protected
    function GetSampleSource: UnicodeString; override;
    function IsCurrentToken(const Token: UnicodeString): Boolean; override;
    function IsFilterStored: Boolean; override;
    procedure SetAlreadyVisitedURIFunc(Value: TAlreadyVisitedURIFunc);
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
      override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property URIAttri: TSynEdit32HighlighterAttributes read FURIAttri write SetURIAttri;
    property VisitedURIAttri: TSynEdit32HighlighterAttributes read FVisitedURIAttri
      write SetVisitedURIAttri;
  end;

const
  SYN_ATTR_URI = 6;
  SYN_ATTR_VISITEDURI = 7;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..15] of UnicodeString = (
    '', 'http://', '', 'https://', 'news:', 'gopher://', '', 'prospero://',
    'news://', 'www', 'nntp://', 'ftp://', 'wais://', '', 'telnet://', 'mailto:'
  );

function TSynURISyn.HashKey(Str: PWideChar): Integer;
begin
  Result := 0;
  while CharInSet(Str^, ['A'..'Z', 'a'..'z']) do
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    Inc(Str);
  end;

  if Str^ = ':' then
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    Inc(Str);
  end;

  if Str^ = '/' then
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    Inc(Str);
  end;

  if Str^ = '/' then
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    Inc(Str);
  end;

  FStringLen := Str - FMayBeProtocol;
end;

procedure TSynURISyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    FIdentFuncTable[i] := AltFunc;
  
  FIdentFuncTable[11] := FuncFtp;
  FIdentFuncTable[5] := FuncGopher;
  FIdentFuncTable[1] := FuncHttp;
  FIdentFuncTable[3] := FuncHttps;
  FIdentFuncTable[15] := FuncMailto;
  FIdentFuncTable[4] := FuncNews;
  FIdentFuncTable[8] := FuncNews;
  FIdentFuncTable[10] := FuncNntp;
  FIdentFuncTable[7] := FuncProspero;
  FIdentFuncTable[14] := FuncTelnet;
  FIdentFuncTable[12] := FuncWais;
  FIdentFuncTable[9] := FuncWeb;
end;

function TSynURISyn.IsCurrentToken(const Token: UnicodeString): Boolean;
var
  I: Integer;
  Temp: PWideChar;
begin
  Temp := FMayBeProtocol;
  if Length(Token) = FStringLen then
  begin
    Result := True;
    for i := 1 to FStringLen do
    begin
      if Temp^ <> Token[i] then
      begin
        Result := False;
        break;
      end;
      Inc(Temp);
    end;
  end
  else
    Result := False;
end;

function TSynURISyn.AltFunc(Key: Integer): TtkTokenKind;
begin
  Result := tkUnknown;
end;

constructor TSynURISyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);

  FURIAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrURI, SYNS_FriendlyAttrURI);
  FURIAttri.Foreground := clBlue;
  FURIAttri.Style := [fsUnderline];
  AddAttribute(FURIAttri);

  FVisitedURIAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVisitedURI, SYNS_FriendlyAttrVisitedURI);
  FVisitedURIAttri.Foreground := clPurple;
  FVisitedURIAttri.Style := [fsUnderline];
  AddAttribute(FVisitedURIAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterURI;
end;

destructor TSynURISyn.Destroy; 
begin
  inherited;
  //the other attributes are automatically freed because of AddAttribute()
  FSpaceAttri.Free;
  FIdentifierAttri.Free;
end;

procedure TSynURISyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynURISyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynURISyn.NullProc;
begin
  if FRun < fLineLen + 1 then
  begin
    Inc(FRun);
    FTokenID := tkNullChar;
  end
  else
    FTokenID := tkNull
end;

procedure TSynURISyn.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynURISyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynURISyn.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
    #13: CRProc;
    #10: LFProc;
    #0: NullProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    'A'..'Z', 'a'..'z': ProtocolProc;
    else
      UnknownProc;
  end;
  inherited;
end;

function TSynURISyn.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_URI: Result := FURIAttri;
    SYN_ATTR_VISITEDURI: Result := FVisitedURIAttri;
  else
    Result := nil;
  end;
end;

function TSynURISyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
var
  Visited: Boolean;
begin
  case GetTokenID of
    tkSpace: Result := FSpaceAttri;
    tkFtpLink, tkGopherLink, tkHttpLink, tkHttpsLink, tkMailtoLink, tkNewsLink,
    tkNntpLink, tkProsperoLink, tkTelnetLink, tkWaisLink, tkWebLink:
    begin
      Visited := False;
      if Assigned(FAlreadyVisitedURI) then
        Visited := FAlreadyVisitedURI(GetToken);
      if Visited then
        Result := FVisitedURIAttri
      else
        Result := FURIAttri;
    end;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynURISyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynURISyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

class function TSynURISyn.GetLanguageName: string;
begin
  Result := SYNS_LangURI;
end;

function TSynURISyn.GetSampleSource: UnicodeString;
begin
  Result := 'Universal Resource Identifier highlighting'#13#10#13#10 +
            'http://www.somewhere.org'#13#10 +
            'ftp://superhost.org/downloads/gems.zip'#13#10 +
            'www.w3c.org'#13#10 +
            'mailto:big@lebowski.edu'#13#10 +
            'douglas@adams.lod'#13#10 +
            'news:comp.lang.pascal.borland';
end;

function TSynURISyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterURI;
end;

function TSynURISyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := SynIsCharAlphaNumeric(AChar);
end;

procedure TSynURISyn.SetAlreadyVisitedURIFunc(Value: TAlreadyVisitedURIFunc);
begin
  FAlreadyVisitedURI := Value;
end;

procedure TSynURISyn.SetURIAttri(const Value: TSynEdit32HighlighterAttributes);
begin
  FURIAttri.Assign(Value);
end;

procedure TSynURISyn.SetVisitedURIAttri(const Value: TSynEdit32HighlighterAttributes);
begin
  FVisitedURIAttri.Assign(Value);
end;

procedure TSynURISyn.ProtocolProc;
var
  Key: Integer;
begin
  if IsValidEmailAddress then
    FTokenID := tkMailtoLink
  else
  begin
    FMayBeProtocol := FLine + FRun;
    Key := HashKey(FMayBeProtocol);
    Inc(FRun, FStringLen);

    if Key <= 15 then
      FTokenID := FIdentFuncTable[Key](Key)
    else
      FTokenID := tkUnknown;
  end;
end;

function TSynURISyn.FuncFtp(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkFtpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncGopher(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkGopherLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncHttp(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkHttpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncHttps(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkHttpsLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncMailto(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkMailtoLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncNews(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkNewsLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncNntp(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkNntpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncProspero(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkProsperoLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncTelnet(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkTelnetLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncWais(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkWaisLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncWeb(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidWebLink then
    Result := tkWebLink
  else
    Result := tkUnknown;
end;


function TSynURISyn.IsAlphaNum(AChar: WideChar): Boolean;
begin
  Result := SynIsCharAlphaNumeric(AChar);
end;

function TSynURISyn.IsMark(AChar: WideChar): Boolean;
begin
  case AChar of
    '-', '_', '.', '!', '~', '*', '''', '(' , ')':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynURISyn.IsReserved(AChar: WideChar): Boolean;
begin
  case AChar of
    ';', '/', '?', ':', '@', '&', '=', '+', '$', ',', '%', '#':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynURISyn.IsUnreserved(AChar: WideChar): Boolean;
begin
  Result := IsAlphaNum(AChar) or IsMark(AChar);
end;

function TSynURISyn.IsURIChar(AChar: WideChar): Boolean;
begin
  Result := IsReserved(AChar) or IsUnreserved(AChar);
end;

function TSynURISyn.IsNeverAtEnd(AChar: WideChar): Boolean;
begin
  Result := (IsMark(AChar) and (AChar <> '''')) or
            (IsReserved(AChar) and (AChar <> '/') and (AChar <> '$'));
end;

function TSynURISyn.IsEMailAddressChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '.', '_', '-', '@':
      Result := True;
    else
      Result := IsAlphaNum(AChar);
  end;
end;

function TSynURISyn.IsNeverAtEMailAddressEnd(AChar: WideChar): Boolean;
begin
  Result := (AChar = '.') or (AChar = '@');
end;

function TSynURISyn.IsValidEmailAddress: Boolean;
var
  StartPos, AtPos, DotPos: Integer;
begin
  StartPos := FRun;

  AtPos := -1;
  DotPos := -1;
  while IsEMailAddressChar(FLine[FRun]) do
  begin
    if FLine[FRun] = '@' then
      AtPos := FRun
    else if FLine[FRun] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (FRun = StartPos) or (DotPos >= 0) and (DotPos = FRun - 1) then
        break
      else
        DotPos := FRun;
    Inc(FRun);
  end;

  while (FRun > StartPos) and (IsNeverAtEMailAddressEnd(FLine[FRun - 1])) do
    dec(FRun);

  while (DotPos >= FRun) or (DotPos > -1) and (FLine[DotPos] <> '.') do
    Dec(DotPos);

  Result := (StartPos < AtPos) and (AtPos < FRun - 1) and (DotPos > AtPos + 1);
  if not Result then FRun := StartPos;
end;

function TSynURISyn.IsValidURI: Boolean;
var
  ProtocolEndPos, DotPos: Integer;

  function IsRelativePath: Boolean;
  begin
    Result := (DotPos - 1 >= 0) and
      ((FLine[DotPos - 1] = '/') and (FLine[DotPos + 2] = '/')) or
      ((FLine[DotPos - 1] = '\') and (FLine[DotPos + 2] = '\'));
  end;

begin
  ProtocolEndPos := FRun;

  DotPos := -1;
  while IsURIChar(FLine[FRun]) do
  begin
    if FLine[FRun] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (DotPos >= 0) and (DotPos = FRun - 1) and not IsRelativePath then
        break
      else
        DotPos := FRun;
    Inc(FRun);
  end;

  while (FRun > ProtocolEndPos) and IsNeverAtEnd(FLine[FRun - 1]) do
    dec(FRun);

  Result := FRun > ProtocolEndPos;
end;

function TSynURISyn.IsValidWebLink: Boolean;
var
  WWWEndPos, DotPos, SecondDotPos: Integer;

  function IsRelativePath: Boolean;
  begin
    Result := (DotPos - 1 >= 0) and
      ((FLine[DotPos - 1] = '/') and (FLine[DotPos + 2] = '/')) or
      ((FLine[DotPos - 1] = '\') and (FLine[DotPos + 2] = '\'));
  end;

begin
  WWWEndPos := FRun;

  DotPos := -1;
  SecondDotPos := -1;
  while IsURIChar(FLine[FRun]) do
  begin
    if FLine[FRun] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (DotPos >= 0) and (DotPos = FRun - 1) and not IsRelativePath then
        break
      else
      begin
        DotPos := FRun;
        if SecondDotPos = -2 then SecondDotPos := DotPos;
        if SecondDotPos = -1 then SecondDotPos := -2;
      end;
    Inc(FRun);
  end;

  while (FRun > WWWEndPos) and IsNeverAtEnd(FLine[FRun - 1]) do
    dec(FRun);

  Result := (FRun > WWWEndPos) and (FLine[WWWEndPos] = '.') and
            (SecondDotPos > WWWEndPos + 1) and (SecondDotPos < FRun);
end;

class function TSynURISyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangURI;
end;

initialization
  RegisterPlaceableHighlighter(TSynURISyn);
end.
