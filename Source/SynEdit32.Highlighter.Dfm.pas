{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterDfm.pas, released 2000-04-14.
The Original Code is based on the dmDfmSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is David H. Muir.
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

$Id: SynHighlighterDfm.pas,v 1.16.2.7 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Delphi Form Source highlighter for SynEdit)
@author(David Muir <david@loanhead45.freeserve.co.uk>)
@created(April 13, 2000)
@lastmod(2000-06-23)
The SynHighlighterDfm unit provides SynEdit with a Delphi Form Source (.dfm) highlighter.
The highlighter formats form source code similar to when forms are viewed as text in the Delphi editor.
}

unit SynEdit32.Highlighter.Dfm;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown);

type
  TSynEdit32HighlighterDfm = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    procedure AltProc;
    procedure AsciiCharProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CommentProc;
    procedure CRProc;
    procedure EndProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure ObjectProc;
    procedure InheritedProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
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
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

function LoadDFMFile2Strings(const AFile: UnicodeString; AStrings: TUnicodeStrings;
  var WasText: Boolean): Integer; {$IFNDEF UNICODE} overload; {$ENDIF}
{$IFNDEF UNICODE}
function LoadDFMFile2Strings(const AFile: string; AStrings: TStrings;
  var WasText: Boolean): Integer; overload;
{$ENDIF}
function SaveStrings2DFMFile(AStrings: TUnicodeStrings;
  const AFile: UnicodeString): Integer; {$IFNDEF UNICODE} overload; {$ENDIF}
{$IFNDEF UNICODE}
function SaveStrings2DFMFile(AStrings: TStrings;
  const AFile: string): Integer; overload;
{$ENDIF}

implementation

uses
  SynEdit32.StrConst;

{ A couple of useful Delphi Form functions }

function LoadDFMFile2Strings(const AFile: UnicodeString; AStrings: TUnicodeStrings;
  var WasText: Boolean): Integer;
var
  Src, Dest: TStream;
  origFormat: TStreamOriginalFormat;
begin
  Result := 0;
  WasText := FALSE;
  AStrings.Clear;
  try
    Src := TWideFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
    try
      Dest := TMemoryStream.Create;
      try
        origFormat := sofUnknown;
        ObjectResourceToText(Src, Dest, origFormat);
        WasText := origFormat = sofText;
        Dest.Seek(0, soFromBeginning);
        AStrings.LoadFromStream(Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

{$IFNDEF UNICODE}
function LoadDFMFile2Strings(const AFile: string; AStrings: TStrings;
  var WasText: Boolean): Integer;
var
  Src, Dest: TStream;
{$IFDEF SYN_COMPILER_5_UP}
  origFormat: TStreamOriginalFormat;
{$ENDIF}
begin
  Result := 0;
  WasText := FALSE;
  AStrings.Clear;
  try
    Src := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
    try
      Dest := TMemoryStream.Create;
      try
{$IFDEF SYN_COMPILER_5_UP}
        origFormat := sofUnknown;
        ObjectResourceToText(Src, Dest, origFormat);
        WasText := origFormat = sofText;
{$ELSE}
        ObjectResourceToText(Src, Dest);
{$ENDIF}
        Dest.Seek(0, soFromBeginning);
        AStrings.LoadFromStream(Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;
{$ENDIF}

function SaveStrings2DFMFile(AStrings: TUnicodeStrings; const AFile: UnicodeString): Integer;
var
  Src, Dest: TStream;
{$IFNDEF UNICODE}
  OldSaveUnicode: Boolean;
{$ENDIF}
begin
  Result := 0;
  try
    Src := TMemoryStream.Create;
    try
{$IFNDEF UNICODE}
      OldSaveUnicode := AStrings.SaveUnicode;
      AStrings.SaveUnicode := False;
{$ENDIF}
      AStrings.SaveToStream(Src);
{$IFNDEF UNICODE}
      AStrings.SaveUnicode := OldSaveUnicode;
{$ENDIF}
      Src.Seek(0, soFromBeginning);
      Dest := TWideFileStream.Create(AFile, fmCreate);
      try
        ObjectTextToResource(Src, Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

{$IFNDEF UNICODE}
function SaveStrings2DFMFile(AStrings: TStrings; const AFile: string): Integer;
var
  Src, Dest: TStream;
begin
  Result := 0;
  try
    Src := TMemoryStream.Create;
    try
      AStrings.SaveToStream(Src);
      Src.Seek(0, soFromBeginning);
      Dest := TFileStream.Create(AFile, fmCreate);
      try
        ObjectTextToResource(Src, Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;
{$ENDIF}

{ TSynEdit32HighlighterDfm }

constructor TSynEdit32HighlighterDfm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
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
  fDefaultFilter := SYNS_FilterDFM;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterDfm.AltProc;
begin
  FTokenID := tkIdentifier;
  repeat
    Inc(FRun);
  until not IsIdentChar(FLine[FRun]);
end;

procedure TSynEdit32HighlighterDfm.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    Inc(FRun);
  until not CharInSet(FLine[FRun], ['0'..'9']);
end;

procedure TSynEdit32HighlighterDfm.BraceCloseProc;
begin
  Inc(FRun);
  FRange := rsUnknown;
  FTokenID := tkIdentifier;
end;

procedure TSynEdit32HighlighterDfm.BraceOpenProc;
begin
  FRange := rsComment;
  CommentProc;
end;

procedure TSynEdit32HighlighterDfm.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(FRun);
    if FLine[FRun] = '}' then begin
      Inc(FRun);
      FRange := rsUnknown;
      break;
    end;
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterDfm.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if (FLine[FRun] = #10) then Inc(FRun);
end;

procedure TSynEdit32HighlighterDfm.EndProc;
begin
  if CharInSet(FLine[FRun + 1], ['n', 'N']) and
     CharInSet(FLine[FRun + 2], ['d', 'D']) and
     not IsIdentChar(FLine[FRun + 3])
  then
  begin
    FTokenID := tkKey;
    Inc(FRun, 3);
  end
  else
    AltProc;
end;

procedure TSynEdit32HighlighterDfm.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkNumber;
  repeat
    Inc(FRun);
  until not IsIntegerChar;
end;

procedure TSynEdit32HighlighterDfm.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterDfm.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterDfm.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkNumber;
  repeat
    Inc(FRun);
    if FLine[FRun] = '.' then
    begin
      if FLine[FRun + 1] <> '.' then Inc(FRun);
      break;
    end;
  until not IsNumberChar;
end;

procedure TSynEdit32HighlighterDfm.ObjectProc;
begin
  if CharInSet(FLine[FRun + 1], ['b', 'B']) and
     CharInSet(FLine[FRun + 2], ['j', 'J']) and
     CharInSet(FLine[FRun + 3], ['e', 'E']) and
     CharInSet(FLine[FRun + 4], ['c', 'C']) and
     CharInSet(FLine[FRun + 5], ['t', 'T']) and
     not IsIdentChar(FLine[FRun + 6])
  then
  begin
    FTokenID := tkKey;
    Inc(FRun, 6);
  end
  else
    AltProc;
end;

procedure TSynEdit32HighlighterDfm.InheritedProc;
begin
  if CharInSet(FLine[FRun + 1], ['n', 'N']) and
     CharInSet(FLine[FRun + 2], ['h', 'H']) and
     CharInSet(FLine[FRun + 3], ['e', 'E']) and
     CharInSet(FLine[FRun + 4], ['r', 'R']) and
     CharInSet(FLine[FRun + 5], ['i', 'I']) and
     CharInSet(FLine[FRun + 6], ['t', 'T']) and
     CharInSet(FLine[FRun + 7], ['e', 'E']) and
     CharInSet(FLine[FRun + 8], ['d', 'D']) and
     not IsIdentChar(FLine[FRun + 9])
  then
  begin
    FTokenID := tkKey;
    Inc(FRun, 9);
  end
  else if CharInSet(FLine[FRun + 1], ['n', 'N']) and
          CharInSet(FLine[FRun + 2], ['l', 'L']) and
          CharInSet(FLine[FRun + 3], ['i', 'I']) and
          CharInSet(FLine[FRun + 4], ['n', 'N']) and
          CharInSet(FLine[FRun + 5], ['e', 'E']) and
          not IsIdentChar(FLine[FRun + 6])
  then
  begin
    FTokenID := tkKey;
    Inc(FRun, 6);
  end
  else
    AltProc;
end;

procedure TSynEdit32HighlighterDfm.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(FRun);
  until (FLine[FRun] > #32) or IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterDfm.StringProc;
begin
  FTokenID := tkString;
  repeat
    Inc(FRun);
    if FLine[FRun] = '''' then begin
      Inc(FRun);
      if FLine[FRun] <> '''' then break
    end;
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterDfm.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterDfm.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterDfm.Next;
begin
  fTokenPos := FRun;
  if FRange = rsComment then
  begin
    if FLine[FRun] = #0 then
      NullProc
    else
      CommentProc;
  end
  else
    case FLine[FRun] of
      '#': AsciiCharProc;
      '}': BraceCloseProc;
      '{': BraceOpenProc;
      #13: CRProc;
      'A'..'Z', 'a'..'z', '_':
        if CharInSet(FLine[FRun], ['e', 'E']) then
          EndProc
        else if CharInSet(FLine[FRun], ['o', 'O']) then
          ObjectProc
        else if CharInSet(FLine[FRun], ['i', 'I']) then
          InheritedProc
        else
          AltProc;
      '$': IntegerProc;
      #10: LFProc;
      #0: NullProc;
      '0'..'9': NumberProc;
      '(', ')', '/', '=', '<', '>', '.', ',', '[', ']': SymbolProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      #39: StringProc;
      else UnknownProc;
    end;
  inherited;
end;

function TSynEdit32HighlighterDfm.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterDfm.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterDfm.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterDfm.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterDfm.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterDfm.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterDfm.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterDfm.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterDFM;
end;

class function TSynEdit32HighlighterDfm.GetLanguageName: string;
begin
  Result := SYNS_LangDfm;
end;

function TSynEdit32HighlighterDfm.GetSampleSource: UnicodeString;
begin
  Result := '{ Delphi/C++ Builder Form Definitions }'#13#10 +
            'object TestForm: TTestForm'#13#10 +
            '  Left = 273'#13#10 +
            '  Top = 103'#13#10 +
            '  Caption = ''SynEdit sample source'''#13#10 +
            'end';
end; { GetSampleSource }

class function TSynEdit32HighlighterDfm.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangDfm;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterDfm);
end.
