{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterRC.pas, released 2004-06-12.
The initial author of this file is Yiannis Mandravellos.
Unicode translation by Maël Hörz.
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

$Id: SynHighlighterRC.pas,v 1.6.2.8 2008/09/14 16:25:02 maelh Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERRC}
unit SynEdit32.Highlighter.RC;
{$ENDIF}

{$I SynEdit.Inc}

interface

uses
  Windows, Controls, Graphics, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
 TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
                 tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

 TRangeState = (rsUnknown, rsDirective, rsComment);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

 TSynEdit32HighlighterRC = class(TSynEdit32CustomHighlighter)
  private
   FRange: TRangeState;
   FTokenID: TtkTokenKind;
   FIdentFuncTable: array[0..240] of TIdentFuncTableFunc;
   FCommentAttri: TSynEdit32HighlighterAttributes;
   FDirecAttri: TSynEdit32HighlighterAttributes;
   FIdentifierAttri: TSynEdit32HighlighterAttributes;
   FKeyAttri: TSynEdit32HighlighterAttributes;
   FNumberAttri: TSynEdit32HighlighterAttributes;
   FSpaceAttri: TSynEdit32HighlighterAttributes;
   FStringAttri: TSynEdit32HighlighterAttributes;
   FSymbolAttri: TSynEdit32HighlighterAttributes;
   function AltFunc(Index: Integer): TtkTokenKind;
   function KeyWordFunc(Index: Integer): TtkTokenKind;
   function HashKey(Str: PWideChar): Cardinal;
   function IdentKind(MayBe: PWideChar): TtkTokenKind;
   procedure InitIdent;
   procedure CommentProc;
   procedure CRProc;
   procedure DirectiveProc;
   procedure IdentProc;
   procedure LFProc;
   procedure NullProc;
   procedure NumberProc;
   procedure QuoteProc;
   procedure SlashProc;
   procedure SpaceProc;
   procedure SymbolProc;
   procedure UnknownProc;
  protected
   function GetSampleSource: UnicodeString; override;
   function IsFilterStored: Boolean; override;
  public
   class function GetCapabilities: TSynHighlighterCapabilities; override;
   class function GetLanguageName: string; override;
   class function GetFriendlyLanguageName: UnicodeString; override;
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   function GetDefaultAttribute(index: integer): TSynEdit32HighlighterAttributes; override;
   function GetRange: pointer; override;
   function GetTokenID: TtkTokenKind;
   function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
   function GetTokenKind: integer; override;
   procedure Next; override;
   procedure SetRange(value: pointer); override;
   procedure ResetRange; override;
   function UseUserSettings(SettingIndex: integer): boolean; override;
   procedure EnumUserSettings(Settings: TStrings); override;
  published
   property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
   property DirecAttri: TSynEdit32HighlighterAttributes read FDirecAttri write FDirecAttri;
   property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
   property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
   property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri write FNumberAttri;
   property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
   property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
   property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
 end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..77] of UnicodeString = (
    'ACCELERATORS', 'ALT', 'ASCII', 'AUTO3STATE', 'AUTOCHECKBOX', 
    'AUTORADIOBUTTON', 'BITMAP', 'BLOCK', 'CAPTION', 'CHARACTERISTICS', 
    'CHECKBOX', 'CHECKED', 'CLASS', 'COMBOBOX', 'COMMENTS', 'COMPANYNAME', 
    'CONTROL', 'CTEXT', 'CURSOR', 'DEFPUSHBUTTON', 'DIALOG', 'DIALOGEX', 
    'DISCARDABLE', 'EDITTEXT', 'EXSTYLE', 'FILEDESCRIPTION', 'FILEFLAGS', 
    'FILEFLAGSMASK', 'FILEOS', 'FILESUBTYPE', 'FILETYPE', 'FILEVERSION', 
    'FIXED', 'FONT', 'GRAYED', 'GROUPBOX', 'HELP', 'ICON', 'IMPURE', 'INACTIVE', 
    'INTERNALNAME', 'LANGUAGE', 'LEGALCOPYRIGHT', 'LEGALTRADEMARKS', 'LISTBOX', 
    'LOADONCALL', 'LTEXT', 'MENU', 'MENUBARBREAK', 'MENUBREAK', 'MENUEX', 
    'MENUITEM', 'MESSAGETABLE', 'MOVEABLE', 'NOINVERT', 'ORIGINALFILENAME', 
    'POPUP', 'PRELOAD', 'PRIVATEBUILD', 'PRODUCTNAME', 'PRODUCTVERSION', 'PURE', 
    'PUSHBOX', 'PUSHBUTTON', 'RADIOBUTTON', 'RCDATA', 'RTEXT', 'SCROLLBAR', 
    'SEPARATOR', 'SHIFT', 'SPECIALBUILD', 'STATE3', 'STRINGTABLE', 'STYLE', 
    'VALUE', 'VERSION', 'VERSIONINFO', 'VIRTKEY' 
  );

  KeyIndices: array[0..240] of Integer = (
    -1, -1, -1, 35, -1, 57, 54, -1, -1, -1, 74, -1, -1, -1, 64, -1, -1, -1, -1, 
    9, 68, -1, 41, -1, -1, 10, -1, -1, 13, 24, -1, -1, -1, 42, -1, -1, -1, -1, 
    -1, 61, -1, -1, 20, 67, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, 23, -1, 
    -1, -1, -1, -1, 48, -1, 12, -1, -1, -1, -1, -1, -1, -1, 75, 73, 14, -1, 77, 
    -1, 4, 63, -1, -1, -1, -1, 65, 19, 27, -1, 31, 38, -1, -1, -1, -1, -1, 50, 
    -1, -1, -1, 28, -1, -1, -1, -1, -1, -1, -1, 8, 6, 18, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 49, 76, -1, 59, -1, -1, 52, 47, 29, -1, -1, -1, 
    -1, -1, -1, -1, 56, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 1, -1, -1, 71, 17, 32, 34, -1, 45, -1, -1, -1, 70, -1, 3, 
    -1, 62, 43, 5, -1, -1, 33, 0, 51, 16, 69, -1, -1, -1, 39, -1, -1, 7, -1, 11, 
    -1, -1, -1, 21, -1, 40, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, 
    -1, 26, -1, 66, 25, -1, -1, 72, -1, -1, 60, 15, -1, -1, -1, -1, 55, -1, -1, 
    -1, 30, -1, -1, -1, 46, -1, 58, -1, 37, 22, -1 
  );

{ TSynEdit32HighlighterRC }

{$Q-}
function TSynEdit32HighlighterRC.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 25 + Ord(Str^) * 298;
    Inc(Str);
  end;
  Result := Result mod 241;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterRC.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterRC.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynEdit32HighlighterRC.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterRC.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynEdit32HighlighterRC.Create(aOwner: TComponent);
begin
  inherited;

  FCaseSensitive := True;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(FCommentAttri);

  FDirecAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FDirecAttri);

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
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  fDefaultFilter := SYNS_FilterRC;
end;

destructor TSynEdit32HighlighterRC.Destroy;
begin
  inherited;
end;

procedure TSynEdit32HighlighterRC.QuoteProc;
begin
  FTokenID:= tkString;
  repeat
   Inc(FRun);
  until IsLineEnd(FRun) or (FLine[FRun] = #34);
  if FLine[FRun] = #34 then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterRC.SlashProc;
begin
  case FLine[FRun + 1] of
   #13: CRPRoc;
   #10: LFProc;
   '/':
    begin
      FTokenID := tkComment;
      Inc(FRun, 2);
      while not IsLineEnd(FRun) do Inc(FRun);
    end;
   '*':
    begin
      FTokenID := tkComment;
      FRange := rsComment;
      Inc(FRun, 2);
      while FLine[FRun] <> #0 do
       case FLine[FRun] of
        '*':
         if FLine[FRun + 1] = '/' then
          begin
            Inc(FRun, 2);
            FRange := rsUnknown;
            break;
          end
         else Inc(FRun);
        #10, #13: break;
       else
        Inc(FRun);
       end;
    end;
  else
   FTokenID := tkSymbol;
   Inc(FRun);
  end
end;

procedure TSynEdit32HighlighterRC.CommentProc;
begin
  FTokenID := tkComment;
  case FLine[FRun] of
   #0: NullProc;
  #13: CRProc;
  #10: LFProc;
  else
   FTokenID := tkComment;
   repeat
    if (FLine[FRun] = '*') and (FLine[FRun +1] = '/') then
     begin
       Inc(FRun, 2);
       FRange := rsUnknown;
       break;
     end
    else
     Inc(FRun);
   until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterRC.DirectiveProc;
begin
  FTokenID := tkDirective;
  repeat
   if (FLine[FRun] = '/') then
    begin
      if FLine[FRun +1] = '/' then
       begin
         FRange := rsUnknown;
         exit;
       end
      else
       if FLine[FRun +1] = '*' then
        begin
          FRange := rsComment;
          exit;
        end
    end;
   Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterRC.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterRC.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
   Inc(FRun);
end;

procedure TSynEdit32HighlighterRC.LFProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
end;

procedure TSynEdit32HighlighterRC.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterRC.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterRC.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'u', 'U', 'x', 'X',
      'A'..'F', 'a'..'f', 'L', 'l', '-', '+':
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
      '.': if FLine[FRun + 1] = '.' then break;
     end;
     Inc(FRun);
   end;
end;

procedure TSynEdit32HighlighterRC.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterRC.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterRC.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsDirective: DirectiveProc;
    rsComment: CommentProc;
    else
      case FLine[FRun] of
        #0: NullProc;
        #13: CRProc;
        #10: LFProc;
        '/': SlashProc;
        '"': QuoteProc;
        '#': DirectiveProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '|', ',', '{', '}': SymbolProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynEdit32HighlighterRC.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterRC.GetRange: pointer;
begin
  Result := pointer(FRange);
end;

function TSynEdit32HighlighterRC.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterRC.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterRC.GetTokenKind: Integer;
begin
  Result := ord(GetTokenID);
end;

procedure TSynEdit32HighlighterRC.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterRC.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterRC.EnumUserSettings(Settings: TStrings);
begin
  // ** ??
end;

function TSynEdit32HighlighterRC.UseUserSettings(SettingIndex: integer): boolean;
begin
  Result := False;
end;

class function TSynEdit32HighlighterRC.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities;
end;

function TSynEdit32HighlighterRC.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterRC;
end;

class function TSynEdit32HighlighterRC.GetLanguageName: string;
begin
  Result := SYNS_LangRC;
end;

function TSynEdit32HighlighterRC.GetSampleSource: UnicodeString;
begin
  Result := '';
end;

class function TSynEdit32HighlighterRC.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangRC;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterRC);
end.
