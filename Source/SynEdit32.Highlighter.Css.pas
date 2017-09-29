{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterEnhCSS.pas, released 2001-10-28
Initial modifications to this CSS Highlighter were made by Ashley Brown,
ashley@ashleybrown.co.uk.

The Original Code is based on the SynHighlighterHTML.pas, released 2000-04-10 - 
this in turn was based on the hkHTMLSyn.pas file from the mwEdit component suite
by Martin Waldenburg and other developers, the Initial Author of this file is
Hideo Koiso.
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

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

You may retrieve the latest version of this file from
http://www.ashleybrown.co.uk/synedit/

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an improved CSS highlighter for SynEdit)
@author(Ashley Brown, based on HTML highlighter by Hideo Koiso and converted to SynEdit by Michael Hieke)
@created(2001-10-28)
@lastmod(2003-05-11)
The SynHighlighterEnhCSS unit provides SynEdit with an improved CSS highlighter.

http://www.ashleybrown.co.uk/
ashley@ashleybrown.co.uk
}

unit SynEdit32.Highlighter.CSS;

{$I SynEdit32.Inc}

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
  TtkTokenKind = (tkComment, tkAtRule, tkProperty, tkSelector, tkSelectorAttrib,
    tkNull, tkSpace, tkString, tkSymbol, tkText, tkUndefProperty, tkValue,
    tkColor, tkNumber, tkImportant);

  TRangeState = (rsComment, rsSelector, rsDeclaration, rsUnknown, rsProperty,
    rsValue, rsAttrib, rsParameter);

  TSynEdit32HighlighterCss = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FCommentRange: TRangeState;
    FParameterRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FPropertyAttri: TSynEdit32HighlighterAttributes;
    FAttributeAttri: TSynEdit32HighlighterAttributes;
    FSelectorAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FColorAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FTextAttri: TSynEdit32HighlighterAttributes;
    FValueAttri: TSynEdit32HighlighterAttributes;
    FUndefPropertyAttri: TSynEdit32HighlighterAttributes;
    FImportantPropertyAttri: TSynEdit32HighlighterAttributes;
    FAtRuleAttri: TSynEdit32HighlighterAttributes;
    FKeywords: TSynEdit32HashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AtRuleProc;
    procedure SelectorProc;
    procedure AttributeProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ParenOpenProc;
    procedure ParenCloseProc;
    procedure BracketOpenProc;
    procedure BracketCloseProc;
    procedure CRProc;
    procedure SemiProc;
    procedure StartValProc;
    procedure NumberProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure HashProc;
    procedure SlashProc;
    procedure GreaterProc;
    procedure PlusProc;
    procedure TildeProc;
    procedure PipeProc;
    procedure CircumflexProc;
    procedure EqualProc;
    procedure ExclamProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextDeclaration;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property PropertyAttri: TSynEdit32HighlighterAttributes read FPropertyAttri
      write FPropertyAttri;
    property ColorAttri: TSynEdit32HighlighterAttributes read FColorAttri
      write FColorAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property AtRuleAttri: TSynEdit32HighlighterAttributes read FAtRuleAttri
      write FAtRuleAttri;
    property SelectorAttri: TSynEdit32HighlighterAttributes read FSelectorAttri
      write FSelectorAttri;
    property AttributeAttri: TSynEdit32HighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property TextAttri: TSynEdit32HighlighterAttributes read FTextAttri
      write FTextAttri;
    property ValueAttri: TSynEdit32HighlighterAttributes read FValueAttri
      write FValueAttri;
    property UndefPropertyAttri: TSynEdit32HighlighterAttributes read FUndefPropertyAttri
      write FUndefPropertyAttri;
    property ImportantPropertyAttri: TSynEdit32HighlighterAttributes read FImportantPropertyAttri
      write FImportantPropertyAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
   Properties_CSS1 : UnicodeString =
                      'background'
                     +',background-attachment'
                     +',background-color'
                     +',background-image'
                     +',background-position'
                     +',background-repeat'
                     +',border'
                     +',border-bottom'
                     +',border-bottom-color'
                     +',border-bottom-style'
                     +',border-bottom-width'
                     +',border-color'
                     +',border-left'
                     +',border-left-color'
                     +',border-left-style'
                     +',border-left-width'
                     +',border-right'
                     +',border-right-color'
                     +',border-right-style'
                     +',border-right-width'
                     +',border-style'
                     +',border-top'
                     +',border-top-color'
                     +',border-top-style'
                     +',border-top-width'
                     +',border-width'
                     +',clear'
                     +',color'
                     +',display'
                     +',float'
                     +',font'
                     +',font-family'
                     +',font-size'
                     +',font-style'
                     +',font-variant'
                     +',font-weight'
                     +',height'
                     +',letter-spacing'
                     +',line-height'
                     +',list-style'
                     +',list-style-image'
                     +',list-style-position'
                     +',list-style-type'
                     +',margin'
                     +',margin-bottom'
                     +',margin-left'
                     +',margin-right'
                     +',margin-top'
                     +',padding'
                     +',padding-bottom'
                     +',padding-left'
                     +',padding-right'
                     +',padding-top'
                     +',text-align'
                     +',text-decoration'
                     +',text-indent'
                     +',text-transform'
                     +',vertical-align'
                     +',white-space'
                     +',width'
                     +',word-spacing';
   Properties_CSS2 : UnicodeString =
                      'border-collapse'
                     +',border-spacing'
                     +',bottom'
                     +',caption-side'
                     +',clip'
                     +',content'
                     +',counter-increment'
                     +',counter-reset'
                     +',cursor'
                     +',direction'
                     +',empty-cells'
                     +',left'
                     +',max-height'
                     +',max-width'
                     +',min-height'
                     +',min-width'
                     +',orphans'
                     +',outline'
                     +',outline-color'
                     +',outline-style'
                     +',outline-width'
                     +',overflow'
                     +',page-break-after'
                     +',page-break-before'
                     +',page-break-inside'
                     +',position'
                     +',quotes'
                     +',right'
                     +',table-layout'
                     +',top'
                     +',unicode-bidi'
                     +',visibility'
                     +',widows'
                     +',z-index';
   Properties_CSS2_Aural : UnicodeString =
                      'azimuth'
                     +',cue'
                     +',cue-after'
                     +',cue-before'
                     +',elevation'
                     +',pause'
                     +',pause-after'
                     +',pause-before'
                     +',pitch'
                     +',pitch-range'
                     +',play-during'
                     +',richness'
                     +',speak'
                     +',speak-header'
                     +',speak-numeral'
                     +',speak-punctuation'
                     +',speech-rate'
                     +',stress'
                     +',voice-family'
                     +',volume';
   Properties_CSS3 : UnicodeString =
                      '@font-face'
                     +',@font-feature-values'
                     +',@keyframes'
                     +',align-content'
                     +',align-items'
                     +',align-self'
                     +',alignment-adjust'
                     +',alignment-baseline'
                     +',animation'
                     +',animation-delay'
                     +',animation-direction'
                     +',animation-duration'
                     +',animation-fill-mode'
                     +',animation-iteration-count'
                     +',animation-name'
                     +',animation-play-state'
                     +',animation-timing-function'
                     +',appearance'
                     +',backface-visibility'
                     +',background-clip'
                     +',background-origin'
                     +',background-size'
                     +',baseline-shift'
                     +',bookmark-label'
                     +',bookmark-level'
                     +',bookmark-target'
                     +',border-bottom-left-radius'
                     +',border-bottom-right-radius'
                     +',border-image'
                     +',border-image-outset'
                     +',border-image-repeat'
                     +',border-image-slice'
                     +',border-image-source'
                     +',border-image-width'
                     +',border-radius'
                     +',border-top-left-radius'
                     +',border-top-right-radius'
                     +',box-align'
                     +',box-decoration-break'
                     +',box-direction'
                     +',box-flex'
                     +',box-flex-group'
                     +',box-lines'
                     +',box-ordinal-group'
                     +',box-orient'
                     +',box-pack'
                     +',box-shadow'
                     +',box-sizing'
                     +',break-after'
                     +',break-before'
                     +',break-inside'
                     +',color-profile'
                     +',column-count'
                     +',column-fill'
                     +',column-gap'
                     +',column-rule'
                     +',column-rule-color'
                     +',column-rule-style'
                     +',column-rule-width'
                     +',columns'
                     +',column-span'
                     +',column-width'
                     +',crop'
                     +',dominant-baseline'
                     +',drop-initial-after-adjust'
                     +',drop-initial-after-align'
                     +',drop-initial-before-adjust'
                     +',drop-initial-before-align'
                     +',drop-initial-size'
                     +',drop-initial-value'
                     +',filter'
                     +',fit'
                     +',fit-position'
                     +',float-offset'
                     +',flex'
                     +',flex-basis'
                     +',flex-direction'
                     +',flex-flow'
                     +',flex-grow'
                     +',flex-shrink'
                     +',flex-wrap'
                     +',font-size-adjust'
                     +',font-feature-setting'
                     +',font-kerning'
                     +',font-language-override'
                     +',font-synthesis'
                     +',font-variant-alternates'
                     +',font-variant-caps'
                     +',font-variant-east-asian'
                     +',font-variant-ligatures'
                     +',font-variant-numeric'
                     +',font-variant-position'
                     +',font-stretch'
                     +',grid-columns'
                     +',grid-rows'
                     +',hanging-punctuation'
                     +',hyphenate-after'
                     +',hyphenate-before'
                     +',hyphenate-character'
                     +',hyphenate-lines'
                     +',hyphenate-resource'
                     +',hyphens'
                     +',icon'
                     +',image-orientation'
                     +',image-rendering'
                     +',image-resolution'
                     +',ime-mode'
                     +',justify-content'
                     +',inline-box-align'
                     +',line-break'
                     +',line-stacking'
                     +',line-stacking-ruby'
                     +',line-stacking-shift'
                     +',line-stacking-strategy'
                     +',mark'
                     +',mark-after'
                     +',mark-before'
                     +',marks'
                     +',marquee-direction'
                     +',marquee-play-count'
                     +',marquee-speed'
                     +',marquee-style'
                     +',mask'
                     +',mask-type'
                     +',move-to'
                     +',nav-down'
                     +',nav-index'
                     +',nav-left'
                     +',nav-right'
                     +',nav-up'
                     +',object-fit'
                     +',object-position'
                     +',opacity'
                     +',order'
                     +',outline-offset'
                     +',overflow-style'
                     +',overflow-x'
                     +',overflow-y'
                     +',overflow-wrap'
                     +',page'
                     +',page-policy'
                     +',perspective'
                     +',perspective-origin'
                     +',phonemes'
                     +',punctuation-trim'
                     +',rendering-intent'
                     +',resize'
                     +',rest'
                     +',rest-after'
                     +',rest-before'
                     +',rotation'
                     +',rotation-point'
                     +',ruby-align'
                     +',ruby-overhang'
                     +',ruby-position'
                     +',ruby-span'
                     +',size'
                     +',string-set'
                     +',tab-size'
                     +',target'
                     +',target-name'
                     +',target-new'
                     +',target-position'
                     +',text-align-last'
                     +',text-combine-horizontal'
                     +',text-decoration-color'
                     +',text-decoration-line'
                     +',text-decoration-style'
                     +',text-height'
                     +',text-justify'
                     +',text-orientation'
                     +',text-outline'
                     +',text-overflow'
                     +',text-shadow'
                     +',text-underline-position'
                     +',text-wrap'
                     +',transform'
                     +',transform-origin'
                     +',transform-style'
                     +',transition'
                     +',transition-delay'
                     +',transition-duration'
                     +',transition-property'
                     +',transition-timing-function'
                     +',voice-balance'
                     +',voice-duration'
                     +',voice-pitch'
                     +',voice-pitch-range'
                     +',voice-rate'
                     +',voice-stress'
                     +',voice-volume'
                     +',word-break'
                     +',word-wrap'
                     +',writing-mode';

{ TSynEdit32HighlighterCss }

{$Q-}
function TSynEdit32HighlighterCss.HashKey(Str: PWideChar): Integer;
begin
  Result := 0;
  while CharInSet(Str^, ['a'..'z', 'A'..'Z', '_', '-']) do
  begin
    if Str^ <> '-' then
    case Str^ of
      '_': Inc(Result, 27);
      '-': Inc(Result, 28);
      else Inc(Result, Ord(SynWideUpperCase(Str^)[1]) - 64);
    end;
    Inc(Str);
  end;
  while CharInSet(Str^, ['0'..'9']) do
  begin
    Inc(Result, Ord(Str^) - Ord('0'));
    Inc(Str);
  end;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterCss.IdentKind(MayBe: PWideChar): TtkTokenKind;
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
  Result := tkUndefProperty;
end;

procedure TSynEdit32HighlighterCss.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

constructor TSynEdit32HighlighterCss.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeywords := TSynEdit32HashEntryList.Create;
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(FCommentAttri);

  FPropertyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrProperty, SYNS_FriendlyAttrProperty);
  FPropertyAttri.Style := [fsBold];
  AddAttribute(FPropertyAttri);

  FSelectorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FSelectorAttri.Style := [fsBold];
  FSelectorAttri.Foreground := $00ff0080;
  AddAttribute(FSelectorAttri);

  FAttributeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAttribute, SYNS_FriendlyAttrAttribute);
  FAttributeAttri.Style := [];
  FAttributeAttri.Foreground := $00ff0080;
  AddAttribute(FAttributeAttri);

  FAtRuleAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAtRules, SYNS_FriendlyAttrAttribute);
  FAtRuleAttri.Style := [];
  FAtRuleAttri.Foreground := $00808000;
  AddAttribute(FAtRuleAttri);

  FUndefPropertyAttri := TSynEdit32HighlighterAttributes.Create(
    SYNS_AttrUndefinedProperty, SYNS_FriendlyAttrUndefinedProperty);
  FUndefPropertyAttri.Style := [fsBold];
  FUndefPropertyAttri.Foreground := $00ff0080;
  AddAttribute(FUndefPropertyAttri);

  FImportantPropertyAttri := TSynEdit32HighlighterAttributes.Create(
    'Important', 'Important Marker');
  FImportantPropertyAttri.Style := [fsBold];
  FImportantPropertyAttri.Foreground := clRed;
  AddAttribute(FImportantPropertyAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FColorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrColor, SYNS_FriendlyAttrColor);
  AddAttribute(FColorAttri);

  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FTextAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(FTextAttri);

  FValueAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  FValueAttri.Foreground := $00ff8000;
  AddAttribute(FValueAttri);

  SetAttributesOnChange(DefHighlightChange);

  // TODO: differentiating tkProperty for CSS1, CSS2 & CSS3 highlighting
  EnumerateKeywords(Ord(tkProperty), Properties_CSS1, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS2, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS2_Aural, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS3, IsIdentChar, DoAddKeyword);

  FRange := rsSelector;
  FDefaultFilter := SYNS_FilterCSS;
end;

destructor TSynEdit32HighlighterCss.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynEdit32HighlighterCss.AttributeProc;

  function IsStopChar: Boolean;
  begin
    case FLine[FRun] of
      #0..#31, ']', '~', '^', '$', '*', '|', '=':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if IsStopChar then
  begin
    case FLine[FRun] of
      #0..#31, '{', '/': NextDeclaration;
      ']': BracketCloseProc;
      '~': TildeProc;
      '|': PipeProc;
      '=': EqualProc;
      '^': CircumflexProc;
    end;
    Exit;
  end;

  FTokenID := tkSelectorAttrib;
  while not IsStopChar do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.BraceCloseProc;
begin
  FRange := rsSelector;
  FTokenID := tkSymbol;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.BraceOpenProc;
begin
  Inc(FRun);
  FRange := rsDeclaration;
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterCss.BracketCloseProc;
begin
  FTokenID := tkSymbol;
  FRange := rsSelector;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.BracketOpenProc;
begin
  Inc(FRun);
  FRange := rsAttrib;
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterCss.CircumflexProc;
begin
  Inc(FRun);
  if FLine[FRun] = '=' then
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterCss.CommentProc;
begin
  if FLine[FRun] = #0 then
    NullProc
  else
  begin
    FTokenID := tkComment;
    repeat
      if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
      begin
        FRange := FCommentRange;
        Inc(FRun, 2);
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun)
  end;
end;

procedure TSynEdit32HighlighterCss.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.SemiProc;
begin
  FRange := rsUnknown;
  FTokenID := tkSymbol;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.StartValProc;
begin
  FRange := rsValue;
  FTokenID := tkSymbol;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.NumberProc;
begin
  if (FLine[FRun] = '-') and not CharInSet(FLine[FRun + 1], ['0'..'9']) then
    IdentProc
  else
  begin
    Inc(FRun);
    FTokenID := tkNumber;
    while CharInSet(FLine[FRun], ['0'..'9', '.']) do
    begin
      case FLine[FRun] of
        '.':
          if FLine[FRun + 1] = '.' then break;
      end;
      Inc(FRun);
    end;
  end;
end;

procedure TSynEdit32HighlighterCss.ParenCloseProc;
begin
  FRange := FParameterRange;
  FTokenID := tkSymbol;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.ParenOpenProc;
begin
  Inc(FRun);
  FParameterRange := FRange;
  FRange := rsParameter;
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterCss.PipeProc;
begin
  Inc(FRun);
  if FLine[FRun] = '=' then
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterCss.PlusProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterCss.IdentProc;
begin
  case FRange of
    rsProperty:
      begin
        FRange := rsDeclaration;
        FTokenID := tkSelector;
        Inc(FRun, FStringLen);
      end;
    rsValue, rsParameter:
      begin
        FTokenID := tkValue;

        while not IsLineEnd(FRun) and
          not CharInSet(FLine[FRun], ['(', ')', '}', ';', ',', ' ']) do
        begin
          Inc(FRun);
        end;

        if IsLineEnd(FRun) or CharInSet(FLine[FRun], ['}', ';']) then
          FRange := rsDeclaration;
      end;
    else
      FTokenID := IdentKind((FLine + FRun));
      repeat
        Inc(FRun);
      until (FLine[FRun] <= #32) or CharInSet(FLine[FRun], [':', '"', '}', ';']);
  end;
end;

procedure TSynEdit32HighlighterCss.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.AtRuleProc;

  function IsStopChar: Boolean;
  begin
    case FLine[FRun] of
      #0..#31, '{', ';':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if IsStopChar then
  begin
    case FLine[FRun] of
      #0..#31, '{', ';': SelectorProc;
    end;
    Exit;
  end;

  FTokenID := tkAtRule;
  while not IsStopChar do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.SelectorProc;

  function IsStopChar: Boolean;
  begin
    case FLine[FRun] of
      #0..#31, '{', '/', '[', ']', '>', '+', '~':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if FLine[FRun] = '}' then
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
    Exit;
  end;

  if FLine[FRun] = '@' then
  begin
    Inc(FRun);
    AtRuleProc;
    Exit;
  end;

  if IsStopChar then
  begin
    case FLine[FRun] of
      #0..#31, '{', '/': NextDeclaration;
      '[': BracketOpenProc;
      ']': BracketCloseProc;
      '>': GreaterProc;
      '+': PlusProc;
      '~': TildeProc;
    end;
    Exit;
  end;

  FTokenID := tkSelector;
  while not IsStopChar do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.TildeProc;
begin
  Inc(FRun);
  if FLine[FRun] = '=' then
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterCss.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.StringProc;
begin
  FTokenID := tkString;
  Inc(FRun);  // first '"'
  while not (IsLineEnd(FRun) or (FLine[FRun] = '"')) do Inc(FRun);
  if FLine[FRun] = '"' then Inc(FRun);  // last '"'
end;

procedure TSynEdit32HighlighterCss.HashProc;

  function IsHexChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkColor;
  Inc(FRun);  // '#'
  while IsHexChar do Inc(FRun);
end;

procedure TSynEdit32HighlighterCss.EqualProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterCss.ExclamProc;
begin
  if (FLine[FRun + 1] = 'i') and
    (FLine[FRun + 2] = 'm') and
    (FLine[FRun + 3] = 'p') and
    (FLine[FRun + 4] = 'o') and
    (FLine[FRun + 5] = 'r') and
    (FLine[FRun + 6] = 't') and
    (FLine[FRun + 7] = 'a') and
    (FLine[FRun + 8] = 'n') and
    (FLine[FRun + 9] = 't') then
  begin
    FTokenID := tkImportant;
    Inc(FRun, 10);
  end
  else
    IdentProc;
end;

procedure TSynEdit32HighlighterCss.SlashProc;
begin
  Inc(FRun);
  if FLine[FRun] = '*' then
  begin
    FTokenID := tkComment;
    FCommentRange := FRange;
    FRange := rsComment;
    Inc(FRun);
    if not IsLineEnd(FRun) then
      CommentProc;
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterCss.Next;
begin
  FTokenPos := FRun;
  case FRange of
    rsSelector:
      SelectorProc;
    rsAttrib:
      AttributeProc;
    rsComment:
      CommentProc;
    else
      NextDeclaration;
  end;

  inherited;
end;

procedure TSynEdit32HighlighterCss.NextDeclaration;
begin
  case FLine[FRun] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '"': StringProc;
    '#': HashProc;
    '{': BraceOpenProc;
    '}': BraceCloseProc;
    '(': ParenOpenProc;
    ')': ParenCloseProc;
    ':', ',': StartValProc;
    ';': SemiProc;
    '0'..'9', '-', '.': NumberProc;
    '/': SlashProc;
    '!': ExclamProc;
    else IdentProc;
  end;
end;

function TSynEdit32HighlighterCss.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FSelectorAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterCss.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterCss.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkAtRule: Result := FAtRuleAttri;
    tkProperty: Result := FPropertyAttri;
    tkSelector: Result := FSelectorAttri;
    tkSelectorAttrib: Result := FAttributeAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkText: Result := FTextAttri;
    tkUndefProperty: Result := FUndefPropertyAttri;
    tkImportant: Result := FImportantPropertyAttri;
    tkValue: Result := FValueAttri;
    tkColor: Result := FColorAttri;
    tkNumber: Result := FNumberAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterCss.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterCss.GreaterProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

function TSynEdit32HighlighterCss.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynEdit32HighlighterCss.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterCss.ResetRange;
begin
  FRange:= rsSelector;
end;

function TSynEdit32HighlighterCss.GetSampleSource: UnicodeString;
begin
  Result := '/* Syntax Highlighting */'#13#10 +
        'body { font-family: Tahoma, Verdana, Arial, Helvetica, sans-serif; font-size: 8pt }'#13#10 +
        'H1 { font-size: 18pt; color: #000099; made-up-property: 1 }';
end; { GetSampleSource }

class function TSynEdit32HighlighterCss.GetLanguageName: string;
begin
  Result := SYNS_LangCSS;
end;

function TSynEdit32HighlighterCss.IsFilterStored: boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterCSS;
end;

function TSynEdit32HighlighterCss.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '-', '0'..'9', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynEdit32HighlighterCss.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCSS;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterCss);
end.
