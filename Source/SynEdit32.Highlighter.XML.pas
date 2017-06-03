{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterXML.pas, released 2000-11-20.
The Initial Author of this file is Jeff Rafter.
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

$Id: SynHighlighterXML.pas,v 1.11.2.6 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

History:
-------------------------------------------------------------------------------
2000-11-30 Removed mHashTable and MakeIdentTable per Michael Hieke

Known Issues:
- Nothing is really constrained (properly) to valid name chars
- Entity Refs are not constrained to valid name chars
- Support for "Combining Chars and Extender Chars" in names are lacking
- The internal DTD is not parsed (and not handled correctly)
-------------------------------------------------------------------------------}
{
@abstract(Provides an XML highlighter for SynEdit)
@author(Jeff Rafter-- Phil 4:13, based on SynHighlighterHTML by Hideo Koiso)
@created(2000-11-17)
@lastmod(2001-03-12)
The SynHighlighterXML unit provides SynEdit with an XML highlighter.
}

unit SynEdit32.Highlighter.XML;

interface

{$I SynEdit.Inc}

uses
  Windows, Messages, Controls, Graphics, Registry, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (tkAposAttrValue, tkAposEntityRef, tkAttribute, tkCDATA,
    tkComment, tkElement, tkEntityRef, tkEqual, tkNull, tkProcessingInstruction,
    tkQuoteAttrValue, tkQuoteEntityRef, tkSpace, tkSymbol, tkText,
    //
    tknsAposAttrValue, tknsAposEntityRef, tknsAttribute, tknsEqual,
    tknsQuoteAttrValue, tknsQuoteEntityRef,
    //These are unused at the moment
    tkDocType
    {tkDocTypeAposAttrValue, tkDocTypeAposEntityRef, tkDocTypeAttribute,
     tkDocTypeElement, tkDocTypeEqual tkDocTypeQuoteAttrValue,
     tkDocTypeQuoteEntityRef}
  );

  TRangeState = (rsAposAttrValue, rsAPosEntityRef, rsAttribute, rsCDATA,
    rsComment, rsElement, rsEntityRef, rsEqual, rsProcessingInstruction,
    rsQuoteAttrValue, rsQuoteEntityRef, rsText,
    //
    rsnsAposAttrValue, rsnsAPosEntityRef, rsnsEqual, rsnsQuoteAttrValue,
    rsnsQuoteEntityRef,
    //These are unused at the moment
    rsDocType, rsDocTypeSquareBraces
    {rsDocTypeAposAttrValue, rsDocTypeAposEntityRef, rsDocTypeAttribute,
     rsDocTypeElement, rsDocTypeEqual, rsDocTypeQuoteAttrValue,
     rsDocTypeQuoteEntityRef}
  );

  TSynEdit32HighlighterXML = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FElementAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FTextAttri: TSynEdit32HighlighterAttributes;
    FEntityRefAttri: TSynEdit32HighlighterAttributes;
    FProcessingInstructionAttri: TSynEdit32HighlighterAttributes;
    FCDATAAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDocTypeAttri: TSynEdit32HighlighterAttributes;
    FAttributeAttri: TSynEdit32HighlighterAttributes;
    FnsAttributeAttri: TSynEdit32HighlighterAttributes;
    FAttributeValueAttri: TSynEdit32HighlighterAttributes;
    FnsAttributeValueAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FWantBracesParsed: Boolean;
    procedure NullProc;
    procedure CarriageReturnProc;
    procedure LineFeedProc;
    procedure SpaceProc;
    procedure LessThanProc;
    procedure GreaterThanProc;
    procedure CommentProc;
    procedure ProcessingInstructionProc;
    procedure DocTypeProc;
    procedure CDATAProc;
    procedure TextProc;
    procedure ElementProc;
    procedure AttributeProc;
    procedure QAttributeValueProc;
    procedure AAttributeValueProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure NextProcedure;
    function NextTokenIs(Token: UnicodeString): Boolean;
    procedure EntityRefProc;
    procedure QEntityRefProc;
    procedure AEntityRefProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    function IsNameChar: Boolean; virtual;
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
    property ElementAttri: TSynEdit32HighlighterAttributes read FElementAttri
      write FElementAttri;
    property AttributeAttri: TSynEdit32HighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
    property NamespaceAttributeAttri: TSynEdit32HighlighterAttributes
      read FnsAttributeAttri write FnsAttributeAttri;
    property AttributeValueAttri: TSynEdit32HighlighterAttributes
      read FAttributeValueAttri write FAttributeValueAttri;
    property NamespaceAttributeValueAttri: TSynEdit32HighlighterAttributes
      read FnsAttributeValueAttri write FnsAttributeValueAttri;
    property TextAttri: TSynEdit32HighlighterAttributes read FTextAttri
      write FTextAttri;
    property CDATAAttri: TSynEdit32HighlighterAttributes read FCDATAAttri
      write FCDATAAttri;
    property EntityRefAttri: TSynEdit32HighlighterAttributes read FEntityRefAttri
      write FEntityRefAttri;
    property ProcessingInstructionAttri: TSynEdit32HighlighterAttributes
      read FProcessingInstructionAttri write FProcessingInstructionAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DocTypeAttri: TSynEdit32HighlighterAttributes read FDocTypeAttri
      write FDocTypeAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property WantBracesParsed : Boolean read FWantBracesParsed
      write FWantBracesParsed default True;
  end;

implementation

uses
  SynEdit32.StrConst;

constructor TSynEdit32HighlighterXML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FElementAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrElementName, SYNS_FriendlyAttrElementName);
  FTextAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrWhitespace, SYNS_FriendlyAttrWhitespace);
  FEntityRefAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrEntityReference, SYNS_FriendlyAttrEntityReference);
  FProcessingInstructionAttri := TSynEdit32HighlighterAttributes.Create(
    SYNS_AttrProcessingInstr, SYNS_FriendlyAttrProcessingInstr);
  FCDATAAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrCDATASection, SYNS_FriendlyAttrCDATASection);
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FDocTypeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrDOCTYPESection, SYNS_FriendlyAttrDOCTYPESection);
  FAttributeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrAttributeName, SYNS_FriendlyAttrAttributeName);
  FnsAttributeAttri := TSynEdit32HighlighterAttributes.Create(
    SYNS_AttrNamespaceAttrName, SYNS_FriendlyAttrNamespaceAttrName);
  FAttributeValueAttri := TSynEdit32HighlighterAttributes.Create(
    SYNS_AttrAttributeValue, SYNS_FriendlyAttrAttributeValue);
  FnsAttributeValueAttri := TSynEdit32HighlighterAttributes.Create(
    SYNS_AttrNamespaceAttrValue, SYNS_FriendlyAttrNamespaceAttrValue);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);

  FElementAttri.Foreground := clMaroon;
  FElementAttri.Style := [fsBold];

  FDocTypeAttri.Foreground := clblue;
  FDocTypeAttri.Style := [fsItalic];

  FCDATAAttri.Foreground := clOlive;
  FCDATAAttri.Style := [fsItalic];

  FEntityRefAttri.Foreground := clblue;
  FEntityRefAttri.Style := [fsbold];

  FProcessingInstructionAttri.Foreground:= clblue;
  FProcessingInstructionAttri.Style:= [];

  FTextAttri.Foreground := clBlack;
  FTextAttri.Style := [fsBold];

  FAttributeAttri.Foreground := clMaroon;
  FAttributeAttri.Style := [];

  FnsAttributeAttri.Foreground := clRed;
  FnsAttributeAttri.Style := [];

  FAttributeValueAttri.Foreground := clNavy;
  FAttributeValueAttri.Style := [fsBold];

  FnsAttributeValueAttri.Foreground := clRed;
  FnsAttributeValueAttri.Style := [fsBold];

  FCommentAttri.Background := clSilver;
  FCommentAttri.Foreground := clGray;
  FCommentAttri.Style := [fsbold, fsItalic];

  FSymbolAttri.Foreground := clblue;
  FSymbolAttri.Style := [];

  AddAttribute(FSymbolAttri);
  AddAttribute(FProcessingInstructionAttri);
  AddAttribute(FDocTypeAttri);
  AddAttribute(FCommentAttri);
  AddAttribute(FElementAttri);
  AddAttribute(FAttributeAttri);
  AddAttribute(FnsAttributeAttri);
  AddAttribute(FAttributeValueAttri);
  AddAttribute(FnsAttributeValueAttri);
  AddAttribute(FEntityRefAttri);
  AddAttribute(FCDATAAttri);
  AddAttribute(FSpaceAttri);
  AddAttribute(FTextAttri);

  SetAttributesOnChange(DefHighlightChange);

  FRange := rsText;
  fDefaultFilter := SYNS_FilterXML;
end;

procedure TSynEdit32HighlighterXML.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterXML.CarriageReturnProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterXML.LineFeedProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterXML.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while FLine[FRun] <= #32 do
  begin
    if CharInSet(FLine[FRun], [#0, #9, #10, #13]) then break;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterXML.LessThanProc;
begin
  Inc(FRun);
  if (FLine[FRun] = '/') then
    Inc(FRun);

  if (FLine[FRun] = '!') then
  begin
    if NextTokenIs('--') then
    begin
      FTokenID := tkSymbol;
      FRange := rsComment;
      Inc(FRun, 3);
    end
    else if NextTokenIs('DOCTYPE') then
    begin
      FTokenID := tkDocType;
      FRange := rsDocType;
      Inc(FRun, 7);
    end
    else if NextTokenIs('[CDATA[') then
    begin
      FTokenID := tkCDATA;
      FRange := rsCDATA;
      Inc(FRun, 7);
    end
    else
    begin
      FTokenID := tkSymbol;
      FRange := rsElement;
      Inc(FRun);
    end;
  end
  else if FLine[FRun]= '?' then
  begin
    FTokenID := tkProcessingInstruction;
    FRange := rsProcessingInstruction;
    Inc(FRun);
  end
  else
  begin
    FTokenID := tkSymbol;
    FRange := rsElement;
  end;
end;

procedure TSynEdit32HighlighterXML.GreaterThanProc;
begin
  FTokenID := tkSymbol;
  FRange:= rsText;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterXML.CommentProc;
begin
  if (FLine[FRun] = '-') and (FLine[FRun + 1] = '-') and (FLine[FRun + 2] = '>') then
  begin
    FTokenID := tkSymbol;
    FRange := rsText;
    Inc(FRun, 3);
    Exit;
  end;

  FTokenID := tkComment;

  if IsLineEnd(FRun) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(FRun) do
  begin
    if (FLine[FRun] = '-') and (FLine[FRun + 1] = '-') and (FLine[FRun + 2] = '>') then
    begin
      FRange := rsComment;
      break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterXML.ProcessingInstructionProc;
begin
  FTokenID := tkProcessingInstruction;
  if IsLineEnd(FRun) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(FRun) do
  begin
    if (FLine[FRun] = '>') and (FLine[FRun - 1] = '?')
    then
    begin
      FRange := rsText;
      Inc(FRun);
      break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterXML.DocTypeProc;
begin
  FTokenID := tkDocType;

  if IsLineEnd(FRun) then
  begin
    NextProcedure;
    Exit;
  end;

  case FRange of
    rsDocType:
      begin
        while not IsLineEnd(FRun) do
        begin
          case FLine[FRun] of
            '[': begin
                   while True do
                   begin
                     Inc(FRun);
                     case FLine[FRun] of
                       ']':
                         begin
                           Inc(FRun);
                           Exit;
                         end;
                       #0, #10, #13:
                         begin
                           FRange := rsDocTypeSquareBraces;
                           Exit;
                         end;
                     end;
                   end;
                 end;
            '>': begin
                   FRange := rsAttribute;
                   Inc(FRun);
                   Break;
                 end;
          end;
          Inc(FRun);
        end;
    end;
    rsDocTypeSquareBraces:
      begin
        while not IsLineEnd(FRun) do
        begin
          if (FLine[FRun] = ']') then
          begin
            FRange := rsDocType;
            Inc(FRun);
            Exit;
          end;
          Inc(FRun);
        end;
      end;
  end;
end;

procedure TSynEdit32HighlighterXML.CDATAProc;
begin
  FTokenID := tkCDATA;
  if IsLineEnd(FRun) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(FRun) do
  begin
    if (FLine[FRun] = '>') and (FLine[FRun - 1] = ']')
    then
    begin
      FRange := rsText;
      Inc(FRun);
      break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterXML.ElementProc;
begin
  if FLine[FRun] = '/' then Inc(FRun);
  while IsNameChar do Inc(FRun);
  FRange := rsAttribute;
  FTokenID := tkElement;
end;

procedure TSynEdit32HighlighterXML.AttributeProc;
begin
  //Check if we are starting on a closing quote
  if CharInSet(FLine[FRun], [#34, #39]) then
  begin
    FTokenID := tkSymbol;
    FRange := rsAttribute;
    Inc(FRun);
    Exit;
  end;
  //Read the name
  while IsNameChar do Inc(FRun);
  //Check if this is an xmlns: attribute
  if (Pos('xmlns', GetToken) > 0) then
  begin
    FTokenID := tknsAttribute;
    FRange := rsnsEqual;
  end
  else
  begin
    FTokenID := tkAttribute;
    FRange := rsEqual;
  end;
end;

procedure TSynEdit32HighlighterXML.EqualProc;
begin
  if FRange = rsnsEqual then
    FTokenID := tknsEqual
  else
    FTokenID := tkEqual;

  while not IsLineEnd(FRun) do
  begin
    if (FLine[FRun] = '/') then
    begin
      FTokenID := tkSymbol;
      FRange := rsElement;
      Inc(FRun);
      Exit;
    end
    else if (FLine[FRun] = #34) then
    begin
      if FRange = rsnsEqual then
        FRange := rsnsQuoteAttrValue
      else
        FRange := rsQuoteAttrValue;
      Inc(FRun);
      Exit;
    end
    else if (FLine[FRun] = #39) then
    begin
      if FRange = rsnsEqual then
        FRange := rsnsAPosAttrValue
      else
        FRange := rsAPosAttrValue;
      Inc(FRun);
      Exit;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterXML.QAttributeValueProc;
begin
  if FRange = rsnsQuoteAttrValue then
    FTokenID := tknsQuoteAttrValue
  else
    FTokenID := tkQuoteAttrValue;

  while not (IsLineEnd(FRun) or (FLine[FRun] = '&') or (FLine[FRun] = #34)) do
    Inc(FRun);

  if FLine[FRun] = '&' then
  begin
    if FRange = rsnsQuoteAttrValue then
      FRange := rsnsQuoteEntityRef
    else
      FRange := rsQuoteEntityRef;
    Exit;
  end
  else if FLine[FRun] <> #34 then
    Exit;

  FRange := rsAttribute;
end;

procedure TSynEdit32HighlighterXML.AAttributeValueProc;
begin
  if FRange = rsnsAPosAttrValue then
    FTokenID := tknsAPosAttrValue
  else
    FTokenID := tkAPosAttrValue;

  while not (IsLineEnd(FRun) or (FLine[FRun] = '&') or (FLine[FRun] = #39)) do
    Inc(FRun);

  if FLine[FRun] = '&' then
  begin
    if FRange = rsnsAPosAttrValue then
      FRange := rsnsAPosEntityRef
    else
      FRange := rsAPosEntityRef;
    Exit;
  end
  else if FLine[FRun] <> #39 then
    Exit;

  FRange := rsAttribute;
end;

procedure TSynEdit32HighlighterXML.TextProc;
begin
  if (FLine[FRun] <= #31) or (FLine[FRun] = '<') then
  begin
    NextProcedure;
    exit;
  end;

  FTokenID := tkText;
  while not ((FLine[FRun] <= #31) or (FLine[FRun] = '<') or (FLine[FRun] = '&')) do
    Inc(FRun);

  if (FLine[FRun] = '&') then
  begin
    FRange := rsEntityRef;
    Exit;
  end;
end;

procedure TSynEdit32HighlighterXML.EntityRefProc;
begin
  FTokenID := tkEntityRef;
  FRange := rsEntityRef;
  while not ((FLine[FRun] <= #32) or (FLine[FRun] = ';')) do Inc(FRun);
  if (FLine[FRun] = ';') then Inc(FRun);
  FRange := rsText;
end;

procedure TSynEdit32HighlighterXML.QEntityRefProc;
begin
  if FRange = rsnsQuoteEntityRef then
    FTokenID := tknsQuoteEntityRef
  else
    FTokenID := tkQuoteEntityRef;

  while not ((FLine[FRun] <= #32) or (FLine[FRun] = ';')) do Inc(FRun);
  if (FLine[FRun] = ';') then Inc(FRun);

  if FRange = rsnsQuoteEntityRef then
    FRange := rsnsQuoteAttrValue
  else
    FRange := rsQuoteAttrValue;
end;

procedure TSynEdit32HighlighterXML.AEntityRefProc;
begin
  if FRange = rsnsAPosEntityRef then
    FTokenID := tknsAPosEntityRef
  else
    FTokenID := tkAPosEntityRef;

  while not ((FLine[FRun] <= #32) or (FLine[FRun] = ';')) do Inc(FRun);
  if (FLine[FRun] = ';') then Inc(FRun);

  if FRange = rsnsAPosEntityRef then
    FRange := rsnsAPosAttrValue
  else
    FRange := rsAPosAttrValue;
end;

procedure TSynEdit32HighlighterXML.IdentProc;
begin
  case FRange of
    rsElement:
      begin
        ElementProc;
      end;
    rsAttribute:
      begin
        AttributeProc;
      end;
    rsEqual, rsnsEqual:
      begin
        EqualProc;
      end;
    rsQuoteAttrValue, rsnsQuoteAttrValue:
      begin
        QAttributeValueProc;
      end;
    rsAposAttrValue, rsnsAPosAttrValue:
      begin
        AAttributeValueProc;
      end;
    rsQuoteEntityRef, rsnsQuoteEntityRef:
      begin
        QEntityRefProc;
      end;
    rsAposEntityRef, rsnsAPosEntityRef:
      begin
        AEntityRefProc;
      end;
    rsEntityRef:
      begin
        EntityRefProc;
      end;
    else ;
  end;
end;

procedure TSynEdit32HighlighterXML.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsText: TextProc;
    rsComment: CommentProc;
    rsProcessingInstruction: ProcessingInstructionProc;
    rsDocType, rsDocTypeSquareBraces: DocTypeProc;
    rsCDATA: CDATAProc;
    else NextProcedure;
  end;
  // ensure that one call of Next is enough to reach next token
  if (fOldRun = FRun) and not GetEol then Next;
  inherited;
end;

procedure TSynEdit32HighlighterXML.NextProcedure;
begin
  case FLine[FRun] of
    #0: NullProc;
    #10: LineFeedProc;
    #13: CarriageReturnProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '<': LessThanProc;
    '>': GreaterThanProc;
    else IdentProc;
  end;
end;

function TSynEdit32HighlighterXML.NextTokenIs(Token: UnicodeString): Boolean;
var
  I, Len: Integer;
begin
  Result := True;
  Len := Length(Token);
  for I := 1 to Len do
    if (FLine[FRun + I] <> Token[I]) then
    begin
      Result:= False;
      Break;
    end;
end;

function TSynEdit32HighlighterXML.GetDefaultAttribute(Index: Integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FAttributeAttri;
    SYN_ATTR_KEYWORD: Result := FElementAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterXML.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterXML.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkElement: Result:= FElementAttri;
    tkAttribute: Result:= FAttributeAttri;
    tknsAttribute: Result:= FnsAttributeAttri;
    tkEqual: Result:= FSymbolAttri;
    tknsEqual: Result:= FSymbolAttri;
    tkQuoteAttrValue: Result:= FAttributeValueAttri;
    tkAPosAttrValue: Result:= FAttributeValueAttri;
    tknsQuoteAttrValue: Result:= FnsAttributeValueAttri;
    tknsAPosAttrValue: Result:= FnsAttributeValueAttri;
    tkText: Result:= FTextAttri;
    tkCDATA: Result:= FCDATAAttri;
    tkEntityRef: Result:= FEntityRefAttri;
    tkQuoteEntityRef: Result:= FEntityRefAttri;
    tkAposEntityRef: Result:= FEntityRefAttri;
    tknsQuoteEntityRef: Result:= FEntityRefAttri;
    tknsAposEntityRef: Result:= FEntityRefAttri;
    tkProcessingInstruction: Result:= FProcessingInstructionAttri;
    tkComment: Result:= FCommentAttri;
    tkDocType: Result:= FDocTypeAttri;
    tkSymbol: Result:= FSymbolAttri;
    tkSpace: Result:= FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterXML.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterXML.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynEdit32HighlighterXML.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterXML.ResetRange;
begin
  FRange := rsText;
end;

function TSynEdit32HighlighterXML.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterXML;
end;

{ TODO: In fact every Number also non-arabics and every letter also German umlauts
  can be used. Something like IsAlphaNumericCharW should be used instead. }
function TSynEdit32HighlighterXML.IsNameChar: Boolean;
begin
  case FLine[FRun] of
    '0'..'9', 'a'..'z', 'A'..'Z', '_', '.', ':', '-':
      Result := True;
    else if FLine[FRun] > 'À' then // TODO: this here is very vague, see above
      Result := True
    else
      Result := False;
  end;
end;

class function TSynEdit32HighlighterXML.GetLanguageName: string;
begin
  Result := SYNS_LangXML;
end;

function TSynEdit32HighlighterXML.GetSampleSource: UnicodeString;
begin
  Result :=
    '<?xml version="1.0"?>'#13#10+
    '<!DOCTYPE root ['#13#10+
    '  ]>'#13#10+
    '<!-- Comment -->'#13#10+
    '<root version="&test;">'#13#10+
    '  <![CDATA[ **CDATA section** ]]>'#13#10+
    '</root>';
end;

class function TSynEdit32HighlighterXML.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangXML;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterXML);
end.
