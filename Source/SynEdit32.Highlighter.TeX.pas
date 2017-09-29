{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterTex.pas, released 2002-09-18.
Author of this file is Soeren Sproessig.
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

$Id: SynHighlighterTeX.pas,v 1.5.2.5 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file from sproessig@bs-webdesign.de

The unit SynEdit32.Highlighter.TeX provides SynEdit with a TeX highlighter.

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.Highlighter.TeX;

{$I SynEdit32.Inc}

interface

uses
  Graphics,
  Classes,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode;

type
  TtkTokenKind = (tkBrace, tkBracket, tkNull, tkSpace, tkText, tkComment,
                  tkControlSequence, tkMathMode);

type
  TSynEdit32HighlighterTeX = class(TSynEdit32CustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FTextAttri: TSynEdit32HighlighterAttributes;
    FControlSequenceAttri: TSynEdit32HighlighterAttributes;
    FMathmodeAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FBracketAttri: TSynEdit32HighlighterAttributes;
    FBraceAttri: TSynEdit32HighlighterAttributes;

    function CreateHighlighterAttributes(Name: string; FriendlyName: UnicodeString;
      Foreground, Background: TColor; FontStyles: TFontStyles): TSynEdit32HighlighterAttributes;
    procedure CRProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure ControlSequenceProc;
    procedure BraceOpenProc;
    procedure BraceCloseProc;
    procedure BracketOpenProc;
    procedure BracketCloseProc;
    procedure MathmodeProc;
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
    property CommentAttri : TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property TextAttri: TSynEdit32HighlighterAttributes read FTextAttri
      write FTextAttri;
    property ControlSequenceAttri: TSynEdit32HighlighterAttributes
      read FControlSequenceAttri write FControlSequenceAttri;
    property MathmodeAttri: TSynEdit32HighlighterAttributes read FMathmodeAttri
      write FMathmodeAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property BraceAttri: TSynEdit32HighlighterAttributes read FBraceAttri
      write FBraceAttri;
    property BracketAttri: TSynEdit32HighlighterAttributes read FBracketAttri
      write FBracketAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

constructor TSynEdit32HighlighterTeX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommentAttri := CreateHighlighterAttributes(SYNS_AttrComment, SYNS_FriendlyAttrComment, clTeal, clNone, []);
  AddAttribute(FCommentAttri);

  FTextAttri := CreateHighlighterAttributes(SYNS_AttrText, SYNS_FriendlyAttrText, clBlack, clNone, []);
  AddAttribute(FTextAttri);

  FMathmodeAttri := CreateHighlighterAttributes(SYNS_AttrMathmode, SYNS_FriendlyAttrMathmode, clOlive, clNone,
    [fsbold]);
  AddAttribute(FMathmodeAttri);

  FSpaceAttri := CreateHighlighterAttributes(SYNS_AttrSpace, SYNS_FriendlyAttrSpace, clNone, clWhite, []);
  AddAttribute(FSpaceAttri);

  FControlSequenceAttri := CreateHighlighterAttributes(SYNS_AttrTexCommand, SYNS_FriendlyAttrTexCommand, clBlue,
    clWhite, [fsBold]);
  AddAttribute(FControlSequenceAttri);

  FBracketAttri := CreateHighlighterAttributes(SYNS_AttrSquareBracket, SYNS_FriendlyAttrSquareBracket, clPurple,
    clNone, []);
  AddAttribute(FBracketAttri);

  FBraceAttri:= CreateHighlighterAttributes(SYNS_AttrRoundBracket, SYNS_FriendlyAttrRoundBracket, clRed,
    clNone, [fsBold]);
  AddAttribute(FBraceAttri);

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterTeX;
end;  { Create }

procedure TSynEdit32HighlighterTeX.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
    else Inc(FRun);
  end;
end;  { CRProc }


procedure TSynEdit32HighlighterTeX.SpaceProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;  { SpaceProc }

procedure TSynEdit32HighlighterTeX.TextProc;
begin
  FTokenID := tkText;
  Inc(FRun);
end;  { TextProc }

procedure TSynEdit32HighlighterTeX.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;  { SpaceProc }

procedure TSynEdit32HighlighterTeX.BraceOpenProc;
begin
  FTokenID := tkBrace;
  Inc(FRun);
end;  { BraceOpen }

procedure TSynEdit32HighlighterTeX.BraceCloseProc;
begin
  FTokenID := tkBrace;
  Inc(FRun);
end;  { BraceClose }

procedure TSynEdit32HighlighterTeX.BracketOpenProc;
begin
  FTokenID := tkBracket;
  Inc(FRun);
end;  { BracketOpen }

procedure TSynEdit32HighlighterTeX.BracketCloseProc;
begin
  FTokenID := tkBracket;
  Inc(FRun);
end;  { BracketClose }

procedure TSynEdit32HighlighterTeX.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;  { NullProc }

procedure TSynEdit32HighlighterTeX.CommentProc;
begin
 FTokenID := tkComment;
 repeat
    case FLine[FRun] of
      #0, #10: Break;
    end;
    Inc(FRun);
  until FLine[FRun] = #13;
  Exit;
end;  { CommentProc }

procedure TSynEdit32HighlighterTeX.MathModeProc;
begin
 FTokenID := tkMathMode;
 Inc(FRun);
end;  { MathModeProc }

procedure TSynEdit32HighlighterTeX.ControlSequenceProc;
begin
 FTokenID := tkControlSequence;
 repeat
   case FLine[FRun] of
     #0..#31: Break;  //No Control Chars !
     #48..#57: Break;  //No Numbers !
     #33..#47, #58..#64,               //Just the Characters that
     #91, #93,#94, #123,              //only can follow to '\'
     #125, #126:
       begin
         if (FLine[FRun-1]='\') then
           Inc(FRun,1);
         Break;
       end;
   end;
   Inc(FRun);
 until FLine[FRun] = #32;
 exit;
end;  { ControlSequenceProc }

procedure TSynEdit32HighlighterTeX.Next;
begin
  fTokenPos := FRun;
  case  FLine[FRun] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #37: CommentProc;
    #92: ControlSequenceProc;
    #123: BraceOpenProc;
    #125: BraceCloseProc;
    #91: BracketOpenProc;
    #93: BracketCloseProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #36: MathmodeProc;
    else TextProc;
  end;
  inherited;
end;  { Next }

function TSynEdit32HighlighterTeX.GetDefaultAttribute(Index: integer):
  TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterTeX.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;  { GetTokenID }

function TSynEdit32HighlighterTeX.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkText: Result := FTextAttri;
    tkControlSequence: Result := FControlSequenceAttri;
    tkMathMode: Result := FMathmodeAttri;
    tkSpace: Result := FSpaceAttri;
    tkBrace: Result := FBraceAttri;
    tkBracket: Result := FBracketAttri;
  else
    Result := nil;
  end;
end;  { GetTokenAttribute }

function TSynEdit32HighlighterTeX.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;  { GetTokenKind }

function TSynEdit32HighlighterTeX.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterTeX;
end;

class function TSynEdit32HighlighterTeX.GetLanguageName: string;
begin
  Result := SYNS_LangTeX;
end;  { GetLanguageName }

function TSynEdit32HighlighterTeX.CreateHighlighterAttributes(Name: string; FriendlyName: UnicodeString;
  Foreground, Background: TColor; FontStyles: TFontStyles): TSynEdit32HighlighterAttributes;
begin
  Result := TSynEdit32HighlighterAttributes.Create(Name, FriendlyName);
  if Foreground <> clNone then Result.Foreground := Foreground;
  if Background <> clNone then Result.Background := Background;
  Result.Style := FontStyles;
end;

function TSynEdit32HighlighterTeX.GetSampleSource: UnicodeString;
begin
  Result :=
    '\documentclass[a4paper]{article}'+#13#10+
    '% LaTeX sample source'+#13#10+
    '\begin{document}'+#13#10+
    'Here is a formula: $ (2x + 3)*5y $'+#13#10+
    '\end{document}';
end;

class function TSynEdit32HighlighterTeX.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangTeX;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterTeX);
end.
