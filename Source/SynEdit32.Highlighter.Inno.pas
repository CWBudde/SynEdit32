{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterInno.pas, released 2000-05-01.
The Initial Author of this file is Satya.
Portions created by Satya are Copyright 2000 Satya.
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

$Id: SynHighlighterInno.pas,v 1.22.2.9 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an Inno script file highlighter for SynEdit)
@author(Satya)
@created(2000-05-01)
@lastmod(2001-01-23)
The SynHighlighterInno unit provides an Inno script file highlighter for SynEdit.
Check out http://www.jrsoftware.org for the free Inno Setup program,
and http://www.wintax.nl/isx/ for My Inno Setup Extensions.
}

unit SynEdit32.Highlighter.Inno;

{$I SynEdit.Inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Highlighter.HashEntries,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkConstant, tkIdentifier, tkKey, tkKeyOrParameter,
    tkNull, tkNumber, tkParameter, tkSection, tkSpace, tkString, tkSymbol,
    tkUnknown);

  TSynInnoSyn = class(TSynEdit32CustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FConstantAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FSectionAttri: TSynEdit32HighlighterAttributes;
    FParamAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FInvalidAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FKeywords: TSynEdit32HashEntryList;
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymbolProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SectionProc;
    procedure SpaceProc;
    procedure EqualProc;
    procedure ConstantProc;
    procedure SemiColonProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
  protected
    function IsCurrentToken(const Token: UnicodeString): Boolean; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
      override;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property ConstantAttri: TSynEdit32HighlighterAttributes read FConstantAttri
      write FConstantAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynEdit32HighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property ParameterAttri: TSynEdit32HighlighterAttributes read FParamAttri
      write FParamAttri;
    property SectionAttri: TSynEdit32HighlighterAttributes read FSectionAttri
      write FSectionAttri;
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

const
  {Note: new 'Section names' and the new 'Constants' need not be added
         as they are highlighted automatically}

  {Ref:  Keywords and Parameters are updated as they last appeared in
         Inno Setup / ISX version 1.3.26}

  Keywords: UnicodeString =
    'adminprivilegesrequired,allownoicons,allowrootdirectory,allowuncpath,' +
    'alwayscreateuninstallicon,alwaysrestart,alwaysshowcomponentslist,' +
    'alwaysshowdironreadypage,alwaysshowgrouponreadypage,' +
    'alwaysusepersonalgroup,appcopyright,appid,appmutex,appname,apppublisher,' +
    'apppublisherurl,appsupporturl,appupdatesurl,appvername,appversion,' +
    'attribs,backcolor,backcolor2,backcolordirection,backsolid,bits,' +
    'changesassociations,check,codefile,comment,components,compression,compresslevel,copymode,'+
    'createappdir,createuninstallregkey,defaultdirname,defaultgroupname,' +
    'description,destdir,destname,direxistswarning,disableappenddir,' +
    'disabledirexistswarning,disabledirpage,disablefinishedpage,' +
    'disableprogramgrouppage,disablereadymemo,disablereadypage,' +
    'disablestartupprompt,diskclustersize,disksize,diskspacemblabel,' +
    'diskspanning,dontmergeduplicatefiles,enabledirdoesntexistwarning,' +
    'extradiskspacerequired,filename,flags,flatcomponentslist,fontinstall,' +
    'groupdescription,hotkey,iconfilename,iconindex,infoafterfile,infobeforefile,' +
    'installmode,internalcompresslevel,key,licensefile,messagesfile,minversion,name,' +
    'onlybelowversion,outputbasefilename,outputdir,overwriteuninstregentries,' +
    'parameters,password,reservebytes,root,runonceid,section,' +
    'showcomponentsizes,source,sourcedir,statusmsg,subkey,tasks,type,types,' +
    'uninstalldisplayicon,uninstalldisplayname,uninstallfilesdir,' +
    'uninstalliconname,uninstalllogmode,uninstallstyle,uninstallable,' +
    'updateuninstalllogappname,usepreviousappdir,usepreviousgroup,' +
    'useprevioustasks,useprevioussetuptype,usesetupldr,valuedata,valuename,' +
    'valuetype,windowresizable,windowshowcaption,windowstartmaximized,' +
    'windowvisible,wizardimagebackcolor,wizardimagefile,wizardsmallimagefile,' +
    'wizardstyle,workingdir';

  Parameters: UnicodeString =
    'hkcc,hkcr,hkcu,hklm,hku,alwaysoverwrite,alwaysskipifsameorolder,append,' +
    'binary,classic,closeonexit,comparetimestampalso,confirmoverwrite,' +
    'createkeyifdoesntexist,createonlyiffileexists,createvalueifdoesntexist,' +
    'deleteafterinstall,deletekey,deletevalue,dirifempty,dontcloseonexit,' +
    'dontcreatekey,disablenouninstallwarning,dword,exclusive,expandsz,' +
    'external,files,filesandordirs,fixed,fontisnttruetype,iscustom,isreadme,' +
    'modern,multisz,new,noerror,none,normal,nowait,onlyifdestfileexists,' +
    'onlyifdoesntexist,overwrite,overwritereadonly,postinstall,' +
    'preservestringtype,regserver,regtypelib,restart,restartreplace,' +
    'runmaximized,runminimized,sharedfile,shellexec,showcheckbox,' +
    'skipifnotsilent,skipifsilent,silent,skipifdoesntexist,' +
    'skipifsourcedoesntexist,unchecked,uninsalwaysuninstall,' +
    'uninsclearvalue,uninsdeleteentry,uninsdeletekey,uninsdeletekeyifempty,' +
    'uninsdeletesection,uninsdeletesectionifempty,uninsdeletevalue,' +
    'uninsneveruninstall,useapppaths,verysilent,waituntilidle';

  KeyOrParameter: UnicodeString = 'string';

function TSynInnoSyn.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
     case Str^ of
       '_': Result := 1;
       'a'..'z': Result := 2 + Ord(Str^) - Ord('a');
       'A'..'Z': Result := 2 + Ord(Str^) - Ord('A');
       else Result := 0;
     end;
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
  Result := Result and $1FF; // 511
  FStringLen := Str - FToIdent;
end;

function TSynInnoSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynEdit32HashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      Break
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

function TSynInnoSyn.IsCurrentToken(const Token: UnicodeString): Boolean;
  var
  I: Integer;
  Temp: PWideChar;
begin
  Temp := FToIdent;
  if Length(Token) = FStringLen then
  begin
    Result := True;
    for i := 1 to FStringLen do
    begin
      if SynWideLowerCase(Temp^)[1] <> SynWideLowerCase(Token[i])[1] then
      begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end
  else
    Result := False;
end;

constructor TSynInnoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;

  FKeywords := TSynEdit32HashEntryList.Create;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGray;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FInvalidAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FInvalidAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clNavy;
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FConstantAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  FConstantAttri.Style := [fsBold, fsItalic];
  FConstantAttri.Foreground := clTeal;
  AddAttribute(FConstantAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  //Parameters
  FParamAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FParamAttri.Style := [fsBold];
  FParamAttri.Foreground := clOlive;
  AddAttribute(FParamAttri);

  FSectionAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  FSectionAttri.Style := [fsBold];
  FSectionAttri.Foreground := clRed;
  AddAttribute(FSectionAttri);

  SetAttributesOnChange(DefHighlightChange);
  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkParameter), Parameters, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKeyOrParameter), KeyOrParameter, IsIdentChar,
    DoAddKeyword);
  fDefaultFilter := SYNS_FilterInno;
end;

destructor TSynInnoSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynInnoSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
end;

procedure TSynInnoSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynInnoSyn.EqualProc;
begin
// If any word has equal (=) symbol,
// then the immediately followed text is treated as string
// (though it does not have quotes)
  FTokenID := tkString;
  repeat
    Inc(FRun);
    if FLine[FRun] = ';' then
    begin
      Inc(FRun);
      Break;
    end;
  until IsLineEnd(FRun);
end;

procedure TSynInnoSyn.IdentProc;
var
  LookAhead: integer;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  if FTokenID = tkKeyOrParameter then
  begin
    LookAhead := FRun;
    while CharInSet(FLine[LookAhead], [#9, ' ']) do
      Inc(LookAhead);
    if FLine[LookAhead] = ':' then
      FTokenID := tkKey
    else
      FTokenID := tkParameter;
  end;
end;

procedure TSynInnoSyn.SectionProc;
begin
  // if it is not column 0 mark as tkParameter and get out of here
  if FRun > 0 then
  begin
    FTokenID := tkUnknown;
    Inc(FRun);
    Exit;
  end;

  // this is column 0 ok it is a Section
  FTokenID := tkSection;
  repeat
    Inc(FRun);
    if FLine[FRun] = ']' then
    begin
      Inc(FRun);
      Break;
    end;
  until IsLineEnd(FRun);
end;

procedure TSynInnoSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynInnoSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynInnoSyn.NumberProc;
begin
  FTokenID := tkNumber;
  repeat
    Inc(FRun);
  until not CharInSet(FLine[FRun], ['0'..'9']);
end;

procedure TSynInnoSyn.ConstantProc;
var
  BraceLevel, LastOpenBrace: Integer;
begin
  { Much of this is based on code from the SkipPastConst function in IS's
    CmnFunc2 unit. [jr] }
  if FLine[FRun + 1] = '{' then
  begin
    { '{{' is not a constant }
    FTokenID := tkUnknown;
    Inc(FRun, 2);
    Exit;
  end;
  FTokenID := tkConstant;
  BraceLevel := 1;
  LastOpenBrace := Low(Integer);
  repeat
    Inc(FRun);
    case FLine[FRun] of
      '{': begin
             if LastOpenBrace <> FRun - 1 then
             begin
               Inc(BraceLevel);
               LastOpenBrace := FRun;
             end
             else
               { Skip over '{{' when in an embedded constant }
               Dec(BraceLevel);
           end;
      '}': begin
             Dec (BraceLevel);
             if BraceLevel = 0 then
             begin
               Inc(FRun);
               Break;
             end;
           end;
    end;
  until IsLineEnd(FRun);
end;

procedure TSynInnoSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(FRun);
  until (FLine[FRun] > #32) or IsLineEnd(FRun);
end;

procedure TSynInnoSyn.SemiColonProc;
var
  I: Integer;
begin
  for I := FRun-1 downto 0 do
    if FLine[I] > ' ' then begin
      // If the semicolon is not the first non-whitespace character on the
      // line, then it isn't the start of a comment.
      FTokenID := tkUnknown;
      Inc(FRun);
      Exit;
    end;
  FTokenID := tkComment;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynInnoSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    Inc(FRun);
    if FLine[FRun] = '"' then begin
      Inc(FRun);
      if FLine[FRun] <> '"' then // embedded "" does not end the string
        Break;
    end;
  until IsLineEnd(FRun);
end;

procedure TSynInnoSyn.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynInnoSyn.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #59 {';'}: SemiColonProc;
    #61 {'='}: EqualProc;
    #34: StringProc;
    '#', ':', ',', '(', ')': SymbolProc;
    '{': ConstantProc;
    #91 {'['} : SectionProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynInnoSyn.GetDefaultAttribute(Index: integer):
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

function TSynInnoSyn.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkParameter: Result := FParamAttri;
    tkSection: Result := FSectionAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkConstant: Result := FConstantAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynInnoSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynInnoSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterInno;
end;

class function TSynInnoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangInno;
end;

procedure TSynInnoSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

class function TSynInnoSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangInno;
end;

initialization
  RegisterPlaceableHighlighter(TSynInnoSyn);
end.
