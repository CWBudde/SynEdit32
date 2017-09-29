{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHC11.pas, released 2000-04-21.
The Original Code is based on the CIHC11Syn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Nils Springob.
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

$Id: SynHighlighterHC11.pas,v 1.13.2.5 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a 68HC11 Assembler Language syntax highlighter for SynEdit)
@author(Nils Springob <delphi.nils@crazy-idea.de>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(January 2000, converted to SynEdit April 21, 2000)
@lastmod(2000-06-23)
The SynHighlighterHC11 unit provides SynEdit with a 68HC11 Assembler (.asm) highlighter.
The highlighter supports all 68HC11 op codes.
Thanks to Martin Waldenburg, David Muir, Hideo Koiso and Nick Hoddinott.
}

unit SynEdit32.Highlighter.HC11;

{$I SynEdit32.Inc}

interface

uses
  Graphics,
  SynEdit32.Highlighter,
  SynEdit32.Types,
  SynEdit32.Highlighter.HashEntries,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TkwKeyWordType = (kwNone, kwOperand, kwOperandOver, kwNoOperand);

  PHashListEntry = ^THashListEntry;
  THashListEntry = record
    Next: PHashListEntry;
    Token: UnicodeString;
    Kind: TtkTokenKind;
    Op: Boolean;
  end;

  TSynEdit32HighlighterHC11 = class(TSynEdit32CustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FKeyWordType: TkwKeyWordType;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FDirecAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FInvalidAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FKeywords: TSynEdit32HashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymAsciiCharProc;
    procedure SymbolProc;
    procedure SymDollarProc;
    procedure SymCRProc;
    procedure SymIdentProc;
    procedure SymLFProc;
    procedure SymPercentProc;
    procedure SymNullProc;
    procedure SymNumberProc;
    procedure SymSpaceProc;
    procedure SymStarProc;
    procedure SymStringProc;
    procedure SymUnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirecAttri: TSynEdit32HighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynEdit32HighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
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

implementation

uses
  SynEdit32.StrConst;

const
  { TODO: seems as if the Ansi version ignores the underscores and therfore
    highlights more KeyWords than this(=Unicode) version.
    Also the SampleSource uses EQU_ and EQU, so it isn't clear what is
    the correct syntax: with other without the underscores.
  }
  KeyWords: UnicodeString = (
    'ABA,ABX,ABY,ADCA_,ADCB_,ADDA_,ADDB_,ADDD_,ANDA_,ANDB_,ASLA,ASLB,' +
    'ASL_,ASLD,ASRA,ASRB,ASR_,BCC_,BCLR_,BCS_,BEQ_,BGE_,BGT_,BHI_,BHS' +
    '_,BITA_,BITB_,BLE_,BLO_,BLS_,BLT_,BMI_,BNE_,BPL_,BRA_,BRCLR_,BRN' +
    '_,BRSET_,BSET_,BSR_,BVC_,BVS_,CBA,CLC,CLI,CLRA,CLRB,CLR_,CLV,CMP' +
    'A_,CMPB_,COMA,COMB,COM_,CPD_,CPX_,CPY_,DAA,DECA,DECB,DEC_,DES,DE' +
    'X,DEY,EORA_,EORB_,FDIV,IDIV,INCA,INCB,INC_,INS,INX,INY,JMP_,JSR_' +
    ',LDAA_,LDAB_,LDD_,LDS_,LDX_,LDY_,LSLA,LSLB,LSL_,LSLD,LSRA,LSRB,L' +
    'SR_,LSRD,MUL,NEGA,NEGB,NEG_,NOP,ORAA_,ORAB_,PSHA,PSHB,PSHX,PSHY,' +
    'PULA,PULB,PULX,PULY,ROLA,ROLB,ROL_,RORA,RORB,ROR_,RTI,RTS,SBA,SB' +
    'CA_,SBCB_,SEC,SEI,SEV,STAA_,STAB_,STD_,STOP,STS_,STX_,STY_,SUBA_' +
    ',SUBB_,SUBD_,SWI,TAB,TAP,TBA,TEST,' +
    'TPA,TSTA,TSTB,TST_,TSX,TSY,TXS,TYS,WAI,XGDX,XGDY,' + // end commands
    'FCC_,FCB_,BSZ_,FDB_' // codegenerating directives
  );

  Directives: UnicodeString = (
    'EQU_,OPT_,PAGE,ORG_,RMB_,END'  // directives
  );

procedure TSynEdit32HighlighterHC11.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

function TSynEdit32HighlighterHC11.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 1 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 28 + Ord(Str^) - Ord('0');
      '_': Result := 27;
      else Result := 0;
    end
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
  Result := Result and $FF; // 255
  FStringLen := Str - FToIdent;
end;

function TSynEdit32HighlighterHC11.IdentKind(MayBe: PWideChar): TtkTokenKind;
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
  Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterHC11.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FKeywords := TSynEdit32HashEntryList.Create;
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FInvalidAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FInvalidAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FDirecAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FDirecAttri);
  SetAttributesOnChange(DefHighlightChange);

  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkDirective), Directives, IsIdentChar, DoAddKeyword);
  fDefaultFilter := SYNS_FilterAsm68HC11;
end; { Create }

destructor TSynEdit32HighlighterHC11.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynEdit32HighlighterHC11.SymAsciiCharProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #39) and (FLine[FRun + 2] = #39) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13:
      begin
        FKeyWordType:=kwNone;
        break;
      end;
    end;
    Inc(FRun);
  until FLine[FRun] = #39;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymDollarProc;
begin
  FTokenID := tkNumber;
  Inc(FRun);
  while CharInSet(FLine[FRun], ['0'..'9', 'A'..'F', 'a'..'f']) do
  begin
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterHC11.SymCRProc;
begin
  FTokenID := tkSpace;
  FKeyWordType := kwNone;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymIdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymLFProc;
begin
  FKeyWordType := kwNone;
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymPercentProc;
begin
  Inc(FRun);
  FTokenID := tkNumber;
  while CharInSet(FLine[FRun], ['0'..'1']) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymNullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymNumberProc;
begin
  Inc(FRun);
  FTokenID := tkNumber;
  while CharInSet(FLine[FRun], ['0'..'9']) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymSpaceProc;
begin
  Inc(FRun);
  if FKeyWordType in [kwOperandOver, kwNoOperand] then
  begin
    FKeyWordType := kwNone;
    FTokenID := tkComment;
    while not IsLineEnd(FRun) do
      Inc(FRun);
  end
  else
  begin
    if FKeyWordType = kwOperand then
      FKeyWordType := kwOperandOver;
    FTokenID := tkSpace;
    while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do
      Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterHC11.SymStarProc;
begin
  Inc(FRun);
  if FKeyWordType = kwOperandOver then
    FTokenID := tkSymbol
  else
  begin
    FTokenID := tkComment;
    while not IsLineEnd(FRun) do
      Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterHC11.SymStringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34) then Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: break;
    end;
    Inc(FRun);
  until FLine[FRun] = #34;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterHC11.SymUnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterHC11.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
    #39: SymAsciiCharProc;
    '$': SymDollarProc;
    #13: SymCRProc;
    'A'..'Z', 'a'..'z', '_': SymIdentProc;
    #10: SymLFProc;
    '%': SymPercentProc;
    #0: SymNullProc;
    '0'..'9': SymNumberProc;
    #1..#9, #11, #12, #14..#32: SymSpaceProc;
    '*': SymStarProc;
    #34: SymStringProc;
    '#', ':', ',', ';', '(', ')': SymbolProc;
    else SymUnknownProc;
  end;
  inherited;
end;

function TSynEdit32HighlighterHC11.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterHC11.GetTokenAttribute: TSynEdit32HighlighterAttributes;
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
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterHC11.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterHC11.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterHC11.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterAsm68HC11;
end;

class function TSynEdit32HighlighterHC11.GetLanguageName: string;
begin
  Result := SYNS_Lang68HC11;
end;

function TSynEdit32HighlighterHC11.GetSampleSource: UnicodeString;
begin
  Result :=
    '* TX.ASM'#13#10 +
    'MAINORG EQU_    $F800'#13#10 +
    '        ORG     $F800'#13#10 +
    'MAIN    EQU     *        ;Start assembling here'#13#10 +
    '        STAA    SCCR2'#13#10 +
    'loop:'#13#10 +
    '        LDAA    #$05'#13#10 +
    '	BRA	loop		;Do it again'#13#10 +
    '	ORG	$FFFE		;Reset vector interrupt setup'#13#10 +
    '	END';
end;

class function TSynEdit32HighlighterHC11.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLang68HC11;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterHC11);
end.
