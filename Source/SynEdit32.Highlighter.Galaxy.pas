{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGalaxy.pas, released 2000-04-07.
The Original Code is based on the mkGalaxySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martijn van der Kooij.
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

$Id: SynHighlighterGalaxy.pas,v 1.12.2.8 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Galaxy highlighter for SynEdit)
@author(Martijn van der Kooij, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(May 1999, converted to SynEdit June 19, 2000)
@lastmod(2000-06-23)
The SynHighlighterGalaxy unit provides SynEdit with a Galaxy highlighter.
Galaxy is a PBEM game for 10 to 500+ players, to see it wokring goto: http://members.tripod.com/~erisande/kooij.html .
The keywords in the string list KeyWords have to be in lowercase and sorted.
}

unit SynEdit32.Highlighter.Galaxy;

{$I SynEdit32.Inc}

interface

uses
  Windows, Graphics, SysUtils, Classes,
  SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkMessage,
    tkUnknown);

  TRangeState = (rsUnKnown, rsMessageStyle);

type
  TSynEdit32HighlighterGalaxy = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FMessageAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyWords: TUnicodeStrings;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure MessageStyleProc;
    procedure SetKeyWords(const Value: TUnicodeStrings);
  protected
    function IsFilterStored: Boolean; override;
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
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property KeyWords: TUnicodeStrings read FKeyWords write SetKeyWords;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property MessageAttri: TSynEdit32HighlighterAttributes read FMessageAttri
      write FMessageAttri;
  end;

implementation

uses
  Registry, SynEdit32.StrConst;

function TSynEdit32HighlighterGalaxy.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
   '_', '0'..'9', 'a'..'z', 'A'..'Z', '#':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynEdit32HighlighterGalaxy.IsKeyword(const AKeyword: UnicodeString): Boolean;
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
    Compare := WideCompareStr(FKeyWords[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsKeyWord }

constructor TSynEdit32HighlighterGalaxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeyWords := TUnicodeStringList.Create;
  TUnicodeStringList(FKeyWords).Sorted := True;
  TUnicodeStringList(FKeyWords).Duplicates := dupIgnore;
  TUnicodeStringList(FKeyWords).CommaText :=
    '#end,#galaxy,a,anonymous,autounload,b,battleprotocol,c,cap,cargo,col,' +
    'compress,d,drive,e,emp,f,fleet,fleettables,g,galaxytv,gplus,groupforecast,' +
    'h,i,j,l,m,machinereport,mat,n,namecase,no,o,options,p,planetforecast,' +
    'prodtable,produce,q,r,routesforecast,s,send,shields,shiptypeforecast,' +
    'sortgroups,t,twocol,u,underscores,v,w,war,weapons,x,y,z';
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FMessageAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrMessage, SYNS_FriendlyAttrMessage);
  AddAttribute(FMessageAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  FRange := rsUnknown;
  fDefaultFilter := SYNS_FilterGalaxy;
end; { Create }

destructor TSynEdit32HighlighterGalaxy.Destroy;
begin
  FKeyWords.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynEdit32HighlighterGalaxy.MessageStyleProc;
begin
  FTokenID := tkMessage;
  case FLine[FRun] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  if (FRun = 0) and (FLine[FRun] = '@') then begin
    FRange := rsUnKnown;
    Inc(FRun);
  end else
    while FLine[FRun] <> #0 do
      Inc(FRun);
end;

procedure TSynEdit32HighlighterGalaxy.PointCommaProc;
begin
  FTokenID := tkComment;
  FRange := rsUnknown;
  repeat
    Inc(FRun);
  until FLine[FRun] = #0;
end;

procedure TSynEdit32HighlighterGalaxy.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterGalaxy.IdentProc;
begin
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
  if IsKeyWord(GetToken) then
    FTokenID := tkKey
  else
    FTokenID := tkIdentifier;
end;

procedure TSynEdit32HighlighterGalaxy.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterGalaxy.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterGalaxy.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterGalaxy.StringProc;
begin
  if (FRun = 0) and (FTokenID <> tkMessage) then
  begin
    FTokenID := tkMessage;
    FRange := rsMessageStyle;
  end;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterGalaxy.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnKnown;
end;

procedure TSynEdit32HighlighterGalaxy.Next;
begin
  FTokenPos := FRun;
  if FRange = rsMessageStyle then
    MessageStyleProc
  else
    case FLine[FRun] of
      ';': PointCommaProc;                                      
      #13: CRProc;
      '#','A'..'Z', 'a'..'z', '_': IdentProc;
      #10: LFProc;
      #0: NullProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '@': StringProc;
      else UnknownProc;
    end;
  inherited;
end;

function TSynEdit32HighlighterGalaxy.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterGalaxy.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterGalaxy.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterGalaxy.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkMessage: Result := FMessageAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterGalaxy.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterGalaxy.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterGalaxy.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterGalaxy.SetKeyWords(const Value: TUnicodeStrings);
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
  FKeyWords.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynEdit32HighlighterGalaxy.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGalaxy;
end;

class function TSynEdit32HighlighterGalaxy.GetLanguageName: string;
begin
  Result := SYNS_LangGalaxy;
end;

function TSynEdit32HighlighterGalaxy.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then
    begin
      if r.ValueExists('KeyWords') then KeyWords.Text:= r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynEdit32HighlighterGalaxy.SaveToRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TRegistry;
begin
  r:= TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then
    begin
      {$IFNDEF SYN_COMPILER_25_UP}
      Result := true;
      {$ENDIF}
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else
      Result := false;
  finally
    r.Free;
  end;
end;

class function TSynEdit32HighlighterGalaxy.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGalaxy;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterGalaxy);
end.
