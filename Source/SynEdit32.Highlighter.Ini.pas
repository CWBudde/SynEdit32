{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterIni.pas, released 2000-04-21.
The Original Code is based on the izIniSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Igor P. Zenkov.
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

$Id: SynHighlighterIni.pas,v 1.13.2.5 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an Ini-files highlighter for SynEdit)
@author(Igor P. Zenkov, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999-11-02, converted to SynEdit 2000-04-21)
@lastmod(2000-04-21)
The SynHighlighterIni unit provides SynEdit with an Ini-files highlighter.
Thanks to Primoz Gabrijelcic, Martin Waldenburg and Michael Hieke.
}

unit SynEdit32.Highlighter.Ini;

{$I SynEdit.Inc}

interface

uses
  Graphics, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (tkComment, tkText, tkSection, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

type
  TSynEdit32HighlighterIni = class(TSynEdit32CustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FTextAttri: TSynEdit32HighlighterAttributes;
    FSectionAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    procedure SectionOpenProc;
    procedure KeyProc;
    procedure CRProc;
    procedure EqualProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SemiColonProc;
    procedure SpaceProc;
    procedure StringProc;  // ""
    procedure StringProc1; // ''
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
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property TextAttri: TSynEdit32HighlighterAttributes read FTextAttri
      write FTextAttri;
    property SectionAttri: TSynEdit32HighlighterAttributes read FSectionAttri
      write FSectionAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri
      write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

constructor TSynEdit32HighlighterIni.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);
  FTextAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(FTextAttri);
  FSectionAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  FSectionAttri.Style := [fsBold];
  AddAttribute(FSectionAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  fDefaultFilter := SYNS_FilterINI;
end; { Create }

procedure TSynEdit32HighlighterIni.SectionOpenProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if FRun > 0 then
  begin
    FTokenID := tkText;
    Inc(FRun);
    Exit;
  end;

  // this is column 0 ok it is a Section
  FTokenID := tkSection;
  Inc(FRun);
  while FLine[FRun] <> #0 do
    case FLine[FRun] of
      ']':
        begin
          Inc(FRun);
          Break
        end;
      #10: Break;
      #13: Break;
    else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterIni.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
    else Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterIni.EqualProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterIni.KeyProc;
begin
  FTokenID := tkKey;
  Inc(FRun);
  while FLine[FRun] <> #0 do
    case FLine[FRun] of
      '=': Break;
      #10: Break;
      #13: Break;
      else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterIni.TextProc;

  function IsTextChar: Boolean;
  begin
    case FLine[FRun] of
      'a'..'z', 'A'..'Z', '0'..'9':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if FRun = 0 then
    KeyProc
  else
  begin
    FTokenID := tkText;
    Inc(FRun);
    while FLine[FRun] <> #0 do
      if IsTextChar then
        Inc(FRun)
      else
        Break;
  end;
end;

procedure TSynEdit32HighlighterIni.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterIni.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterIni.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsAlphaChar: Boolean;
  begin
    case FLine[FRun] of
      'a'..'z', 'A'..'Z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if FRun = 0 then
    KeyProc
  else
  begin
    Inc(FRun);
    FTokenID := tkNumber;
    while IsNumberChar do Inc(FRun);
    if IsAlphaChar then TextProc;
  end;
end;

// ;
procedure TSynEdit32HighlighterIni.SemiColonProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if FRun > 0 then
  begin
    FTokenID := tkText;
    Inc(FRun);
    Exit;
  end;

  // this is column 0 ok it is a comment
  FTokenID := tkComment;
  Inc(FRun);
  while FLine[FRun] <> #0 do
    case FLine[FRun] of
      #10: Break;
      #13: Break;
      else Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterIni.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

// ""
procedure TSynEdit32HighlighterIni.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
    end;
    Inc(FRun);
  until FLine[FRun] = #34;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

// ''
procedure TSynEdit32HighlighterIni.StringProc1;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #39) and (FLine[FRun + 2] = #39) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: Break;
    end;
    Inc(FRun);
  until FLine[FRun] = #39;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterIni.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #34: StringProc;
    #39: StringProc1;
    '0'..'9': NumberProc;
    #59: SemiColonProc;
    #61: EqualProc;
    #91: SectionOpenProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    else TextProc;
  end;
  inherited;
end;

function TSynEdit32HighlighterIni.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterIni.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterIni.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkText: Result := FTextAttri;
    tkSection: Result := FSectionAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FTextAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterIni.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterIni.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterINI;
end;

class function TSynEdit32HighlighterIni.GetLanguageName: string;
begin
  Result := SYNS_LangINI;
end;

function TSynEdit32HighlighterIni.GetSampleSource: UnicodeString;
begin
  Result :=
    '; Syntax highlighting'#13#10+
    '[Section]'#13#10+
    'Key=value'#13#10+
    'String="Arial"'#13#10+
    'Number=123456';
end;

class function TSynEdit32HighlighterIni.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangINI;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterIni);
end.
