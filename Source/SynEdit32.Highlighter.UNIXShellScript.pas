{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterUNIXShellScript.pas, released 2001-11-13.
The Initial Author of this file is Stefan Ascher.
Portions by Jan Verhoeven (http://jansfreeware.com/jfdelphi.htm)
"Heredoc" syntax highlighting implementation by Marko Njezic.
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

$Id: SynHighlighterUNIXShellScript.pas,v 1.7.2.11 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a UNIX Shell Script highlighter for SynEdit)
@author(Stefan Ascher <stievie2002@yahoo.com>)
@created(10 November 2001)
@lastmod(2001-11-13)
The SynHighlighterUNIXShellScript unit provides SynEdit with a UNIX Shell Script highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERUNIXSHELLSCRIPT}
unit SynEdit32.Highlighter.UNIXShellScript;
{$ENDIF}

{$I SynEdit.Inc}

interface

uses
  Graphics, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey,
    tkSpace, tkString, tkSymbol, tkVariable, tkUnknown);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnknown, rsHeredoc, rsIndentedHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnknown);
{$ENDIF}

type
  TSynEdit32HighlighterUNIXShellScript = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    FHeredocLength: Byte;
    FHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FSecondKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FVarAttri: TSynEdit32HighlighterAttributes;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure DollarProc;
    procedure DotProc;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
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
    function IsKeyword(const AKeyword: UnicodeString): Boolean; override;
    function IsSecondKeyWord(AToken: UnicodeString): Boolean;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property SecondKeyAttri: TSynEdit32HighlighterAttributes read FSecondKeyAttri
      write FSecondKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property VarAttri: TSynEdit32HighlighterAttributes read FVarAttri
      write FVarAttri;
  end;

implementation

uses
  SynEdit32.MiscProcs,
  SynEdit32.StrConst;

const
  ShellScriptKeys: array[0..109] of UnicodeString = (
    'awk', 'banner', 'basename', 'bdiff', 'bg', 'break', 'case', 'cat', 'cc',
    'cd', 'chdir', 'chgrp', 'chmod', 'chown', 'clear', 'compress', 'continue',
    'cp', 'cpio', 'cut', 'date', 'dd', 'df', 'diff', 'do', 'done', 'dtpad',
    'echo', 'elif', 'else', 'esac', 'eval', 'exit', 'export', 'expr', 'fg',
    'fi', 'finger', 'fold', 'for', 'ftp', 'g++', 'gcc', 'getopts', 'grep',
    'gzip', 'hash', 'head', 'if', 'in', 'jobs', 'kill', 'ld', 'ln', 'login',
    'ls', 'make', 'mkdir', 'mt', 'mv', 'newgrp', 'nohup', 'od', 'paste', 'perl',
    'pg', 'ping', 'pr', 'ps', 'pwd', 'rcp', 'read', 'remsh', 'return', 'rm',
    'rsh', 'rwho', 'sed', 'set', 'sh', 'shift', 'stop', 'strings', 'strip',
    'sync', 'tail', 'tar', 'telnet', 'test', 'then', 'times', 'tput', 'trap',
    'true', 'tty', 'type', 'ulimit', 'umask', 'unset', 'until', 'uudecode',
    'uuencode', 'vi', 'wait', 'wc', 'while', 'who', 'xtern', 'zcat', 'zip'
  );

  ShellScriptSecondKeys: array[0..22] of UnicodeString = (
    'cdpath', 'editor', 'home', 'ifs', 'lang', 'lc_messages', 'lc_type',
    'ld_library_path', 'logname', 'mail', 'mailcheck', 'mailpath', 'manpath',
    'path', 'ps1', 'ps2', 'pwd', 'shacct', 'shell', 'shlib_path', 'term',
    'termcap', 'tz'
  );

function TSynEdit32HighlighterUNIXShellScript.IsKeyword(const AKeyword: UnicodeString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := High(ShellScriptKeys);
  Result := False;
  Token := SynWideLowerCase(AKeyword);

  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(ShellScriptKeys[I], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsKeyWord }

function TSynEdit32HighlighterUNIXShellScript.IsSecondKeyWord(AToken: UnicodeString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := High(ShellScriptSecondKeys);
  Result := False;
  Token := SynWideLowerCase(AToken);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(ShellScriptSecondKeys[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsSecondKeyWord }

constructor TSynEdit32HighlighterUNIXShellScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FSecondKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  AddAttribute(FSecondKeyAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clMaroon;
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := clRed;
  AddAttribute(FSymbolAttri);
  FVarAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVarAttri.Foreground := clPurple;
  AddAttribute(FVarAttri);
  SetAttributesOnChange(DefHighlightChange);

  FRange := rsUnknown;
  fDefaultFilter := SYNS_FilterUNIXShellScript;
end; { Create }

destructor TSynEdit32HighlighterUNIXShellScript.Destroy;
begin
  inherited Destroy;
end; { Destroy }

procedure TSynEdit32HighlighterUNIXShellScript.DollarProc;
var
  cc: WideChar;
begin
  Inc(FRun);
  FTokenID := tkVariable;
  if IsLineEnd(FRun) then Exit;
  cc := FLine[FRun];
  Inc(FRun);
  if (cc = '{') then
  begin
    // ${var}
    while IsIdentChar(FLine[FRun]) do
    begin
      if IsLineEnd(FRun) then break;
      Inc(FRun);
    end;
    if FLine[FRun] = '}' then Inc(FRun);
  end
  else
    // $var
    while IsIdentChar(FLine[FRun]) do
      Inc(FRun);
end;

procedure TSynEdit32HighlighterUNIXShellScript.DotProc;

  function TestDot: Boolean;
  var
    i: Integer;
  begin
    result := False;
    i := FRun;
    Inc(i);
    while CharInSet(FLine[i], ['a'..'z', 'A'..'Z']) do
      Inc(i);
    if i > (FRun + 1) then
      Result := True;
    if Result then
      FRun := i;
  end;
  
begin
  // Don't highlight filenames like filename.zip
  if TestDot then
    FTokenID := tkIdentifier
  else
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterUNIXShellScript.BraceOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterUNIXShellScript.PointCommaProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterUNIXShellScript.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[FRun + 1] of
    #10: Inc(FRun, 2);
    else Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterUNIXShellScript.IdentProc;
begin
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
  if IsKeyWord(GetToken) then
  begin
    FTokenID := tkKey;
    Exit;
  end
  else
    FTokenID := tkIdentifier;
    
  if IsSecondKeyWord(GetToken) then
    FTokenID := tkSecondKey
  else if FLine[FRun] = '=' then
    FTokenID := tkVariable
  else
    FTokenID := tkIdentifier;
end;

procedure TSynEdit32HighlighterUNIXShellScript.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterUNIXShellScript.LowerProc;
{$IFDEF SYN_HEREDOC}

  // In UNIX Shell, Heredoc delimiter can be pretty much anything and the list
  // of alpha-numeric characters is extended with a few common special characters
  function IsAlphaNumChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      'A'..'Z', 'a'..'z', '0'..'9', '_', '-', '+', '!', '#', '%':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  i, Len, SkipRun: Integer;
  IndentedHeredoc: Boolean;
  QuoteChar: WideChar;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  if FLine[FRun + 1] = '<' then
  begin
    FTokenID := tkSymbol;

    SkipRun := 0;
    QuoteChar := #0;
    if (FLine[FRun + 2] = '-') and (FLine[FRun + 3] in
      [WideChar('"'), WideChar(''''), WideChar('`'), WideChar('\')]) then
    begin
      SkipRun := 2;
      if FLine[FRun + 3] <> '\' then
        QuoteChar := FLine[FRun + 3];
    end
    else if (FLine[FRun + 2] in
      [WideChar('-'), WideChar('"'), WideChar(''''), WideChar('`'), WideChar('\')]) then
    begin
      SkipRun := 1;
      if not (FLine[FRun + 2] in [WideChar('-'), WideChar('\')]) then
        QuoteChar := FLine[FRun + 2];
    end;
    IndentedHeredoc := (SkipRun > 0) and (FLine[FRun + 2] = '-');

    if IsAlphaNumChar(FRun + SkipRun + 2) then
    begin
      Inc(FRun, 2);

      i := FRun;
      while IsAlphaNumChar(SkipRun + i) do Inc(i);
      Len := i - FRun;

      if Len > 255 then
      begin
        FTokenID := tkUnknown;
        Exit;
      end;

      if (QuoteChar <> #0) and (FLine[FRun + SkipRun + Len] <> QuoteChar) then
      begin
        FTokenID := tkUnknown;
        Exit;
      end;

      if IndentedHeredoc then
        FRange := rsIndentedHeredoc
      else
        FRange := rsHeredoc;
      FHeredocLength := Len;
      FHeredocChecksum := CalcFCS(FLine[FRun + SkipRun], Len);

      Inc(FRun, SkipRun + Len);
      FTokenID := tkString;
    end
    else
      Inc(FRun, 2);
  end
  else
{$ENDIF}
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterUNIXShellScript.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterUNIXShellScript.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'e', 'E':
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
      '.':
        if FLine[FRun + 1] = '.' then break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterUNIXShellScript.RoundOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterUNIXShellScript.SlashProc;
begin
  if FLine[FRun] = '#' then
  begin
    // Perl Styled Comment
    Inc(FRun);
    FTokenID := tkComment;
    while not IsLineEnd(FRun) do
    begin
      Inc(FRun);
    end;
  end
  else
  begin
    Inc(FRun);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterUNIXShellScript.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterUNIXShellScript.StringProc;
var
  QuoteChar: WideChar;
begin
// Single and Double Quotes.

  FTokenID := tkString;
  QuoteChar := FLine[FRun];      // either " or '
  if (FLine[FRun + 1] = QuoteChar) and (FLine[FRun + 2] = QuoteChar)
    then Inc(FRun, 2);
  repeat
    if IsLineEnd(FRun) then break;
    Inc(FRun);
  until FLine[FRun] = QuoteChar;
  if not IsLineEnd(FRun) then Inc(FRun);
end;

procedure TSynEdit32HighlighterUNIXShellScript.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynEdit32HighlighterUNIXShellScript.HeredocProc;

  procedure SkipToEOL;
  begin
    case FLine[FRun] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      else
        repeat
          Inc(FRun);
        until IsLineEnd(FRun);
    end;
  end;

var
  i: Integer;
begin
  if IsLineEnd(FRun) and (fTokenPos = FRun) then
  begin
    NextProcedure;
    Exit;
  end;
  FTokenID := tkString;

  if FRange = rsIndentedHeredoc then
    while FLine[FRun] in [WideChar(#9), WideChar(#32)] do Inc(FRun);

  if ((FRun = 0) and (FRange = rsHeredoc)) or (FRange = rsIndentedHeredoc) then
  begin
    i := 0;

    while not IsLineEnd(FLine[FRun + i]) do
    begin
      if i > FHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(i);
    end;

    if i <> FHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[FRun], i) = FHeredocChecksum) then
    begin
      FRange := rsUnknown;
      FRun := FRun + i;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynEdit32HighlighterUNIXShellScript.Next;
begin
  fTokenPos := FRun;
{$IFDEF SYN_HEREDOC}
  if FRange in [rsHeredoc, rsIndentedHeredoc] then
    HeredocProc
  else
{$ENDIF}
    NextProcedure;
  inherited;
end;

procedure TSynEdit32HighlighterUNIXShellScript.NextProcedure;
begin
  case FLine[FRun] of
    '<': LowerProc;
    '#': SlashProc;
    '{': BraceOpenProc;
    ';': PointCommaProc;
    '.': DotProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '(': RoundOpenProc;
    '/': SlashProc;
    '$': DollarProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34, #39: StringProc;
    '}', ')', '!', '%', '&',':','@','[',']','^','`','~': SymbolProc;
    else UnknownProc;
  end;
end;

function TSynEdit32HighlighterUNIXShellScript.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterUNIXShellScript.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(FRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if FRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    RangePointer.Length := FHeredocLength;
    RangePointer.Checksum := FHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(FRange);
{$ENDIF}
end;

function TSynEdit32HighlighterUNIXShellScript.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterUNIXShellScript.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkSecondKey: Result := FSecondKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVarAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterUNIXShellScript.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterUNIXShellScript.ResetRange;
begin
  FRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  FHeredocLength := 0;
  FHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynEdit32HighlighterUNIXShellScript.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer : TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  FRange := TRangeState(RangePointer.Range);
  FHeredocLength := 0;
  FHeredocChecksum := 0;
  if FRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    FHeredocLength := RangePointer.Length;
    FHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  FRange := TRangeState(Value);
{$ENDIF}
end;

function TSynEdit32HighlighterUNIXShellScript.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterUNIXShellScript;
end;

class function TSynEdit32HighlighterUNIXShellScript.GetLanguageName: string;
begin
  Result := SYNS_LangNameUNIXShellScript;
end;

function TSynEdit32HighlighterUNIXShellScript.GetSampleSource: UnicodeString;
begin
  Result := '######################################'#13#10 +
            '# Here is a comment about some stuff #'#13#10 +
            '######################################'#13#10 +
            ''#13#10 +
            'case $BUILD_MODE in'#13#10 +
            '  full )'#13#10 +
            '      MyFirstFunction'#13#10 +
            '      ;;'#13#10 +
            '  rekit)'#13#10 +
            '      MySecondFunction'#13#10 +
            '    ;;'#13#10 +
            '  installer)'#13#10 +
            '      MyThirdFunction'#13#10 +
            '    ;;'#13#10 +
            'esac';
end;

class function TSynEdit32HighlighterUNIXShellScript.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangNameUNIXShellScript;
end;

procedure TSynEdit32HighlighterUNIXShellScript.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterUNIXShellScript);
end.
