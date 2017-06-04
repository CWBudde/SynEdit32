{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynEdit32.Unicode.pas by Ma�l H�rz, released 2004-05-30.
All Rights Reserved.
TUnicodeStrings/TUnicodeStringList-code (originally written by Mike Lischke) is based
on JclUnicode.pas which is part of the JCL (www.delphi-jedi.org).

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

$Id: SynEdit32.Unicode.pas,v 1.1.3.19 2012/11/07 08:54:20 CodehunterWorks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Provides:
- Unicode(PWideChar) versions of the most important PAnsiChar-functions in
  SysUtils and some functions unavailable in Delphi 5.
- An adapted and lighter version of TUnicodeStrings/TUnicodeStringList taken
  from JCL, but made portable.
- function for loading and saving of Unicode files, and detecting the encoding
- Unicode clipboard support
- Unicode-version of TCanvas-methods
- Some character constants like CR&LF.

Last Changes:
- 1.1.3.19: Added TUnicodeStringList.CustomSort
-------------------------------------------------------------------------------}

unit SynEdit32.Unicode;

{$I SynEdit.Inc}

interface

uses
  Windows, Messages, Controls, Forms, Graphics, Clipbrd, Types, Classes,
  SysUtils, TypInfo;

const
  SLineBreak = #13#10;
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  UTF16BOMLE: array[0..1] of Byte = ($FF, $FE);
  UTF16BOMBE: array[0..1] of Byte = ($FE, $FF);
  UTF32BOMLE: array[0..3] of Byte = ($FF, $FE, $00, $00);
  UTF32BOMBE: array[0..3] of Byte = ($00, $00, $FE, $FF);

const
  // constants describing range of the Unicode Private Use Area (Unicode 3.2)
  PrivateUseLow = WideChar($E000);
  PrivateUseHigh = WideChar($F8FF);
  // filler char: helper for painting wide glyphs 
  FillerChar = PrivateUseLow;

const
  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar(#10);
  WideLineFeed = WideChar(#10);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideCR = WideChar(#13);
  WideCarriageReturn = WideChar(#13);
  WideCRLF = UnicodeString(#13#10);
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

  // byte order marks for Unicode files
  // Unicode text files (in UTF-16 format) should contain $FFFE as first character to
  // identify such a file clearly. Depending on the system where the file was created
  // on this appears either in big endian or little endian style.
  BOM_LSB_FIRST = WideChar($FEFF);
  BOM_MSB_FIRST = WideChar($FFFE);

type
  TSaveFormat = (sfUTF16LSB, sfUTF16MSB, sfUTF8, sfAnsi);

const
  sfUnicodeLSB = sfUTF16LSB;
  sfUnicodeMSB = sfUTF16MSB;

type
  TFontCharSet = 0..255;

  TUnicodeStrings = TStrings;
  TUnicodeStringList = TStringList;

// Kylix has them, but Delphi 5 doesn't and Delphi 6&7 versions are buggy
// in Win9X (fix taken from Troy Wolbrinks TntUnicode-package)
function WCharUpper(lpsz: PWideChar): PWideChar;
function WCharUpperBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
function WCharLower(lpsz: PWideChar): PWideChar;
function WCharLowerBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
function SynWideUpperCase(const S: UnicodeString): UnicodeString;
function SynWideLowerCase(const S: UnicodeString): UnicodeString;
function SynIsCharAlpha(const C: WideChar): Boolean;
function SynIsCharAlphaNumeric(const C: WideChar): Boolean;

function WideLastDelimiter(const Delimiters, S: UnicodeString): Integer;
function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;
  Flags: TReplaceFlags): UnicodeString;

{ functions taken from JCLUnicode.pas }
function WStrComp(Str1, Str2: PWideChar): Integer;
function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
procedure StrSwapByteOrder(Str: PWideChar);
function WideQuotedStr(const S: UnicodeString; Quote: WideChar): UnicodeString;
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): UnicodeString;
function UnicodeStringOfChar(C: WideChar; Count: Cardinal): UnicodeString;
function WideTrim(const S: UnicodeString): UnicodeString;
function WideTrimLeft(const S: UnicodeString): UnicodeString;
function WideTrimRight(const S: UnicodeString): UnicodeString;
function CharSetFromLocale(Language: LCID): TFontCharSet;
function CodePageFromLocale(Language: LCID): Integer;
function KeyboardCodePage: Word;
function KeyUnicode(C: AnsiChar): WideChar;
function StringToUnicodeStringEx(const S: AnsiString; CodePage: Word): UnicodeString;
function UnicodeStringToStringEx(const WS: UnicodeString; CodePage: Word): AnsiString;

{ functions providing same behavior on Win9x and WinNT based systems}
function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;

{ Unicode versions of TCanvas-methods }
function TextExtent(ACanvas: TCanvas; const Text: UnicodeString): TSize;
function TextWidth(ACanvas: TCanvas; const Text: UnicodeString): Integer;
function TextHeight(ACanvas: TCanvas; const Text: UnicodeString): Integer;
procedure TextOut(ACanvas: TCanvas; X, Y: Integer; const Text: UnicodeString);
procedure TextRect(ACanvas: TCanvas; Rect: TRect; X, Y: Integer;
  const Text: UnicodeString);

{ Unicode streaming-support }
type
  TSynEncoding = (seUTF8, seUTF16LE, seUTF16BE, seAnsi);
  TSynEncodings = set of TSynEncoding;

  TWideFileStream = TFileStream;

function IsAnsiOnly(const WS: UnicodeString): Boolean;
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean; overload;
function IsUTF8(const FileName: UnicodeString; out WithBOM: Boolean): Boolean; overload;
function GetEncoding(const FileName: UnicodeString; out WithBOM: Boolean): TSynEncoding; overload;
function GetEncoding(Stream: TStream; out WithBOM: Boolean): TSynEncoding; overload;
procedure SaveToFile(const WS: UnicodeString; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True); overload;
procedure SaveToFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True); overload;
function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding; overload;
procedure SaveToStream(const WS: UnicodeString; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean  = True); overload;
procedure SaveToStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean  = True); overload;
function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding): TSynEncoding; overload;

function ClipboardProvidesText: Boolean;
function GetClipboardText: UnicodeString;
procedure SetClipboardText(const Text: UnicodeString);

{ misc functions }
function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
function IsUnicodeStringMappableToAnsi(const WS: UnicodeString): Boolean;

var
  Win32PlatformIsUnicode: Boolean;

implementation

uses
  Math, SysConst, RTLConsts,
  SynEdit32.TextBuffer;

function WStrLen(const Str: PWideChar): Cardinal;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function WStrEnd(const Str: PWideChar): PWideChar;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        LEA     EAX,[EDI-2]
        MOV     EDI,EDX
end;

function WStrMove(Dest: PWideChar; const Source: PWideChar; Count: Integer): PWideChar;
begin
  Result := Dest;
  System.Move(Source^, Dest^, Count * SizeOf(WideChar));
end;

function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
{$IFDEF SYN_COMPILER_16_UP}
begin
  Result := SysUtils.StrCopy(Dest, Source)
{$ELSE}
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        POP     ESI
        POP     EDI
{$ENDIF}
end;

function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
{$IFDEF SYN_COMPILER_16_UP}
begin
  Result := SysUtils.StrLCopy(Dest, Source, MaxLen)
{$ELSE}
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        XOR     AX,AX
        TEST    ECX,ECX
        JZ      @@1
        REPNE   SCASW
        JNE     @@1
        Inc     ECX
@@1:    SUB     EBX,ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,EDI
        MOV     ECX,EBX
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EBX
        AND     ECX,1
        REP     MOVSW
        STOSW
        MOV     EAX,EDX
        POP     EBX
        POP     ESI
        POP     EDI
{$ENDIF}
end;

function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  WStrCopy(WStrEnd(Dest), Source);
  Result := Dest;
end;

function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function WStrAlloc(Size: Cardinal): PWideChar;
begin
  Size := SizeOf(WideChar) * Size + SizeOf(Cardinal);
  GetMem(Result, Size);
  Cardinal(Pointer(Result)^) := Size;
  Inc(PByte(Result), SizeOf(Cardinal));
end;

function WStrNew(const Str: PWideChar): PWideChar;
var
  Size: Cardinal;
begin
  if Str = nil then
    Result := nil
  else
  begin
    Size := WStrLen(Str) + 1;
    Result := WStrMove(WStrAlloc(Size), Str, Size);
  end;
end;

procedure WStrDispose(Str: PWideChar);
begin
  if Str <> nil then
  begin
    Dec(PByte(Str), SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

function WCharUpper(lpsz: PWideChar): PWideChar;
var
  AStr: AnsiString;
  WStr: UnicodeString;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharUpperW(lpsz)
  else
  begin
    if HiWord(Cardinal(lpsz)) = 0 then
    begin
      // literal char mode
      Result := lpsz;
      if IsWideCharMappableToAnsi(WideChar(lpsz)) then
      begin
        AStr := AnsiString(WideChar(lpsz)); // single character may be more than one byte
        Windows.CharUpperA(PAnsiChar(AStr));
        WStr := UnicodeString(AStr); // should always be single wide char
        if Length(WStr) = 1 then
          Result := PWideChar(WStr[1]);
      end
    end
    else
    begin
      // null-terminated string mode
      Result := lpsz;
      while lpsz^ <> #0 do
      begin
        lpsz^ := WideChar(SynEdit32.Unicode.WCharUpper(PWideChar(lpsz^)));
        Inc(lpsz);
      end;
    end;
  end;
end;

function WCharUpperBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
var
  i: integer;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharUpperBuffW(lpsz, cchLength)
  else
  begin
    Result := cchLength;
    for i := 1 to cchLength do
    begin
      lpsz^ := WideChar(SynEdit32.Unicode.WCharUpper(PWideChar(lpsz^)));
      Inc(lpsz);
    end;
  end;
end;

function WCharLower(lpsz: PWideChar): PWideChar;
var
  AStr: AnsiString;
  WStr: UnicodeString;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharLowerW(lpsz)
  else
  begin
    if HiWord(Cardinal(lpsz)) = 0 then
    begin
      // literal char mode
      Result := lpsz;
      if IsWideCharMappableToAnsi(WideChar(lpsz)) then
      begin
        AStr := AnsiString(WideChar(lpsz)); // single character may be more than one byte
        Windows.CharLowerA(PAnsiChar(AStr));
        WStr := UnicodeString(AStr); // should always be single wide char
        if Length(WStr) = 1 then
          Result := PWideChar(WStr[1]);
      end
    end
    else
    begin
      // null-terminated string mode
      Result := lpsz;
      while lpsz^ <> #0 do
      begin
        lpsz^ := WideChar(SynEdit32.Unicode.WCharLower(PWideChar(lpsz^)));
        Inc(lpsz);
      end;
    end;
  end;
end;

function WCharLowerBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
var
  i: integer;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharLowerBuffW(lpsz, cchLength)
  else
  begin
    Result := cchLength;
    for i := 1 to cchLength do
    begin
      lpsz^ := WideChar(SynEdit32.Unicode.WCharLower(PWideChar(lpsz^)));
      Inc(lpsz);
    end;
  end;
end;

function SynWideUpperCase(const S: UnicodeString): UnicodeString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then
    SynEdit32.Unicode.WCharUpperBuff(Pointer(Result), Len);
end;

function SynWideLowerCase(const S: UnicodeString): UnicodeString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then
    SynEdit32.Unicode.WCharLowerBuff(Pointer(Result), Len);
end;

function SynIsCharAlpha(const C: WideChar): Boolean;
begin
  if Win32PlatformIsUnicode then
    Result := IsCharAlphaW(C)
  else
    // returns false if C is not mappable to ANSI
    Result := IsCharAlphaA(AnsiChar(C));
end;

function SynIsCharAlphaNumeric(const C: WideChar): Boolean;
begin
  if Win32PlatformIsUnicode then
    Result := IsCharAlphaNumericW(C)
  else
    // returns false if C is not mappable to ANSI
    Result := IsCharAlphaNumericA(AnsiChar(C));
end;

function WideLastDelimiter(const Delimiters, S: UnicodeString): Integer;
var
  P: PWideChar;
begin
  Result := Length(S);
  P := PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (WStrScan(P, S[Result]) <> nil) then
      Exit;
    Dec(Result);
  end;
end;

function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;
  Flags: TReplaceFlags): UnicodeString;
var
  SearchStr, Patt, NewStr: UnicodeString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := SynWideUpperCase(S);
    Patt := SynWideUpperCase(OldPattern);
  end
  else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

const
  // data used to bring UTF-16 coded strings into correct UTF-32 order for correct comparation
  UTF16Fixup: array[0..31] of Word = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    $2000, $F800, $F800, $F800, $F800
  );

// Binary comparation of Str1 and Str2 with surrogate fix-up.
// Returns < 0 if Str1 is smaller in binary order than Str2, = 0 if both strings are
// equal and > 0 if Str1 is larger than Str2.
//
// This code is based on an idea of Markus W. Scherer (IBM).
// Note: The surrogate fix-up is necessary because some single value code points have
//       larger values than surrogates which are in UTF-32 actually larger.
function WStrComp(Str1, Str2: PWideChar): Integer;
var
  C1, C2: Word;
  Run1, Run2: PWideChar;
begin
  Run1 := Str1;
  Run2 := Str2;
  repeat
    C1 := Word(Run1^);
    C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
    C2 := Word(Run2^);
    C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

    // now C1 and C2 are in UTF-32-compatible order
    Result := Integer(C1) - Integer(C2);
    if(Result <> 0) or (C1 = 0) or (C2 = 0) then
      Break;
    Inc(Run1);
    Inc(Run2);
  until False;

  // If the strings have different lengths but the comparation returned equity so far
  // then adjust the result so that the longer string is marked as the larger one.
  if Result = 0 then
    Result := (Run1 - Str1) - (Run2 - Str2);
end;

// compares strings up to MaxLen code points
// see also StrCompW
function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
var
  C1, C2: Word;
begin
  if MaxLen > 0 then
  begin
    repeat
      C1 := Word(Str1^);
      C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
      C2 := Word(Str2^);
      C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

      // now C1 and C2 are in UTF-32-compatible order
      { TODO : surrogates take up 2 words and are counted twice here, count them only once }
      Result := Integer(C1) - Integer(C2);
      Dec(MaxLen);
      if(Result <> 0) or (C1 = 0) or (C2 = 0) or (MaxLen = 0) then
        Break;
      Inc(Str1);
      Inc(Str2);
    until False;
  end
  else
    Result := 0;
end;

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string
procedure StrSwapByteOrder(Str: PWideChar);
{$IFDEF SYN_COMPILER_16_UP}
var
  P: PWord;
begin
  P := PWord(Str);
  while P^ <> 0 do 
  begin
    P^ := MakeWord(HiByte(P^), LoByte(P^));
    Inc(P);
  end;
{$ELSE}
asm
       PUSH    ESI
       PUSH    EDI
       MOV     ESI, EAX
       MOV     EDI, ESI
       XOR     EAX, EAX // clear high order byte to be able to use 32bit operand below
@@1:
       LODSW
       OR      EAX, EAX
       JZ      @@2
       XCHG    AL, AH
       STOSW
       JMP     @@1


@@2:
       POP     EDI
       POP     ESI
{$ENDIF}
end;

// works like QuotedStr from SysUtils.pas but can insert any quotation character
function WideQuotedStr(const S: UnicodeString; Quote: WideChar): UnicodeString;
var
  P, Src,
  Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := WStrScan(PWideChar(S), Quote);
  while (P <> nil) do
  begin
    Inc(P);
    Inc(AddCount);
    P := WStrScan(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := WStrScan(Src, Quote);
    repeat
      Inc(P);
      Move(Src^, Dest^, 2 * (P - Src));
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := WStrScan(Src, Quote);
    until P = nil;
    P := WStrEnd(Src);
    Move(Src^, Dest^, 2 * (P - Src));
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;

// extracts a string enclosed in quote characters given by Quote
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): UnicodeString;
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;

  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := WStrScan(Src, Quote);

  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := WStrScan(Src, Quote);
  end;

  if Src = nil then
    Src := WStrEnd(P);
  if (Src - P) <= 1 then
    Exit;

  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := WStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
        Break;
      Move(P^, Dest^, 2 * (Src - P));
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := WStrScan(Src, Quote);
    end;
    if Src = nil then
      Src := WStrEnd(P);
    Move(P^, Dest^, 2 * (Src - P - 1));
  end;
end;

// returns a string of Count characters filled with C
function UnicodeStringOfChar(C: WideChar; Count: Cardinal): UnicodeString;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := C;
end;

function WideTrim(const S: UnicodeString): UnicodeString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function WideTrimLeft(const S: UnicodeString): UnicodeString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function WideTrimRight(const S: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function TranslateCharsetInfoEx(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall;
  external 'gdi32.dll' name 'TranslateCharsetInfo';

function CharSetFromLocale(Language: LCID): TFontCharSet;
var
  CP: Cardinal;
  CSI: TCharsetInfo;
begin
  CP:= CodePageFromLocale(Language);
  TranslateCharsetInfoEx(Pointer(CP), CSI, TCI_SRCCODEPAGE);
  Result:= CSI.ciCharset;
end;

// determines the code page for a given locale
function CodePageFromLocale(Language: LCID): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

// converts the given character (as it comes with a WM_CHAR message) into its
// corresponding Unicode character depending on the active keyboard layout
function KeyUnicode(C: AnsiChar): WideChar;
begin
  MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @C, 1, @Result, 1);
end;

function StringToUnicodeStringEx(const S: AnsiString; CodePage: Word): UnicodeString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength,
    nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, PWideChar(Result),
    OutputLength);
end;

function UnicodeStringToStringEx(const WS: UnicodeString; CodePage: Word): AnsiString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(WS);
  OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength,
    nil, 0, nil, nil);
  SetLength(Result, OutputLength);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PAnsiChar(Result),
    OutputLength, nil, nil);
end;

function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
var
  tm: TTextMetricA;
begin
  Result.cx := 0;
  Result.cy := 0;

  GetTextExtentPoint32W(DC, Str, Count, Result);
  if not Win32PlatformIsUnicode then
  begin
    GetTextMetricsA(DC, tm);
    if tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0 then
      Result.cx := Result.cx - tm.tmOverhang
    else
      Result.cx := tm.tmAveCharWidth * Count;
  end;
end;

type
  TAccessCanvas = class(TCanvas)
  end;

function TextExtent(ACanvas: TCanvas; const Text: UnicodeString): TSize;
begin
  with TAccessCanvas(ACanvas) do
  begin
    RequiredState([csHandleValid, csFontValid]);
    Result := GetTextSize(Handle, PWideChar(Text), Length(Text));
  end;
end;

function TextWidth(ACanvas: TCanvas; const Text: UnicodeString): Integer;
begin
  Result := TextExtent(ACanvas, Text).cX;
end;

function TextHeight(ACanvas: TCanvas; const Text: UnicodeString): Integer;
begin
  Result := TextExtent(ACanvas, Text).cY;
end;

procedure TextOut(ACanvas: TCanvas; X, Y: Integer; const Text: UnicodeString);
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    if CanvasOrientation = coRightToLeft then
      Inc(X, SynEdit32.Unicode.TextWidth(ACanvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, TextFlags, nil, PWideChar(Text),
     Length(Text), nil);
    MoveTo(X + SynEdit32.Unicode.TextWidth(ACanvas, Text), Y);
    Changed;
  end;
end;

procedure TextRect(ACanvas: TCanvas; Rect: TRect; X, Y: Integer;
  const Text: UnicodeString);
var
  Options: Longint;
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := ETO_CLIPPED or TextFlags;
    if Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    if ((TextFlags and ETO_RTLREADING) <> 0) and
       (CanvasOrientation = coRightToLeft)
    then
      Inc(X, SynEdit32.Unicode.TextWidth(ACanvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),
      Length(Text), nil);
    Changed;
  end;
end;

function IsAnsiOnly(const WS: UnicodeString): Boolean;
begin
  Result := IsUnicodeStringMappableToAnsi(WS);
end;

function IsUTF8(const FileName: UnicodeString; out WithBOM: Boolean): Boolean;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsUTF8(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

// checks for a BOM in UTF-8 format or searches the first 4096 bytes for
// typical UTF-8 octet sequences
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean;
const
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = $4000;
var
  Buffer: array of Byte;
  BufferSize, i, FoundUTF8Strings: Integer;

  // 3 trailing bytes are the maximum in valid UTF-8 streams,
  // so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    Inc(i);
    while (i < BufferSize) and (Result < 4) do
    begin
      if Buffer[i] in [$80..$BF] then
        Inc(Result)
      else
        Break;
      Inc(i);
    end;
  end;

begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result

  // start analysis at actual Stream.Position
  BufferSize := Min(MaxBufferSize, Stream.Size - Stream.Position);

  // if no special characteristics are found it is not UTF-8
  Result := False;
  WithBOM := False;

  if BufferSize > 0 then
  begin
    SetLength(Buffer, BufferSize);
    Stream.ReadBuffer(Buffer[0], BufferSize);
    Stream.Seek(-BufferSize, soFromCurrent);

    { first search for BOM }

    if (BufferSize >= Length(UTF8BOM)) and CompareMem(@Buffer[0], @UTF8BOM[0], Length(UTF8BOM)) then
    begin
      WithBOM := True;
      Result := True;
      Exit;
    end;

    { If no BOM was found, check for leading/trailing byte sequences,
      which are uncommon in usual non UTF-8 encoded text.

      NOTE: There is no 100% save way to detect UTF-8 streams. The bigger
            MinimumCountOfUTF8Strings, the lower is the probability of
            a false positive. On the other hand, a big MinimumCountOfUTF8Strings
            makes it unlikely to detect files with only little usage of non
            US-ASCII chars, like usual in European languages. }

    FoundUTF8Strings := 0;
    i := 0;
    while i < BufferSize do
    begin
      case Buffer[i] of
        $00..$7F: // skip US-ASCII characters as they could belong to various charsets
          ;
        $C2..$DF:
          if CountOfTrailingBytes = 1 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $E0:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$A0..$BF]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $E1..$EC, $EE..$EF:
          if CountOfTrailingBytes = 2 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $ED:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80..$9F]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F0:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$90..$BF]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F1..$F3:
          if CountOfTrailingBytes = 3 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $F4:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80..$8F]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $C0, $C1, $F5..$FF: // invalid UTF-8 bytes
          Break;
        $80..$BF: // trailing bytes are consumed when handling leading bytes,
                   // any occurence of "orphaned" trailing bytes is invalid UTF-8
          Break;
      end;

      if FoundUTF8Strings = MinimumCountOfUTF8Strings then
      begin
        Result := True;
        Break;
      end;

      Inc(i);
    end;
  end;
end;

function GetEncoding(const FileName: UnicodeString; out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetEncoding(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

function GetEncoding(Stream: TStream; out WithBOM: Boolean): TSynEncoding;
var
  BOM: WideChar;
  Size: Integer;
begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result
  
  // start analysis at actual Stream.Position
  Size := Stream.Size - Stream.Position;

  // if no special characteristics are found it is probably ANSI
  Result := seAnsi;

  if IsUTF8(Stream, WithBOM) then
  begin
    Result := seUTF8;
    Exit;
  end;

  { try to detect UTF-16 by finding a BOM in UTF-16 format }

  if Size >= 2 then
  begin
    Stream.ReadBuffer(BOM, sizeof(BOM));
    Stream.Seek(-sizeof(BOM), soFromCurrent);
    if BOM = WideChar(UTF16BOMLE) then
    begin
      Result := seUTF16LE;
      WithBOM := True;
      Exit;
    end
    else if BOM = WideChar(UTF16BOMBE) then
    begin
      Result := seUTF16BE;
      WithBOM := True;
      Exit;
    end
  end;
end;

procedure SaveToFile(const WS: UnicodeString; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(WS, Stream, Encoding, WithBom);
  finally
    Stream.Free;
  end;
end;

procedure SaveToFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(UnicodeStrings, Stream, Encoding, WithBom);
  finally
    Stream.Free;
  end;
end;

function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(UnicodeStrings, Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(UnicodeStrings, Stream, Encoding, WithBOM);
  finally
    Stream.Free;
  end;
end;

procedure SaveToStream(const WS: UnicodeString; Stream: TStream; Encoding: TSynEncoding;
  WithBom: Boolean  = True);
var
  UTF16BOM: UnicodeString;
  UTF8Str: UTF8String;
  AnsiStr: AnsiString;
begin
  if WithBom then
    case Encoding of
      seUTF8:
        Stream.WriteBuffer(UTF8BOM, 3);
      seUTF16LE:
        begin
          UTF16BOM := BOM_LSB_FIRST;
          Stream.WriteBuffer(PWideChar(UTF16BOM)^, 2);
        end;
      seUTF16BE:
        begin
          UTF16BOM := BOM_MSB_FIRST;
          Stream.WriteBuffer(PWideChar(UTF16BOM)^, 2);
        end;
    end;

  case Encoding of
    seUTF8:
      begin
        UTF8Str := UTF8Encode(WS);
        Stream.WriteBuffer(UTF8Str[1], Length(UTF8Str));
      end;
    seUTF16LE:
      Stream.WriteBuffer(WS[1], Length(WS) * sizeof(WideChar));
    seUTF16BE:
      begin
        StrSwapByteOrder(PWideChar(WS));
        Stream.WriteBuffer(WS[1], Length(WS) * sizeof(WideChar));
      end;
    seAnsi:
      begin
        AnsiStr := AnsiString(PWideChar(WS));
        Stream.WriteBuffer(AnsiStr[1], Length(AnsiStr));
      end;
  end;
end;

type
  TSynEdit32StringListAccess = class(TSynEdit32StringList);

procedure SaveToStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  SText: UnicodeString;
  SaveFStreaming: Boolean;
begin
  // if UnicodeStrings or Stream is nil, let Delphi raise the exception to flag the error

  if UnicodeStrings is TSynEdit32StringList then
  begin
    SaveFStreaming := TSynEdit32StringListAccess(UnicodeStrings).FStreaming;
    TSynEdit32StringListAccess(UnicodeStrings).FStreaming := True;
    SText := UnicodeStrings.Text;
    TSynEdit32StringListAccess(UnicodeStrings).FStreaming := SaveFStreaming;
  end
  else
    SText := UnicodeStrings.Text;
  SaveToStream(SText, Stream, Encoding, WithBom);
end;

function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  out WithBOM: Boolean): TSynEncoding;
var
  Dummy: Boolean;
begin
  Result := LoadFromStream(UnicodeStrings, Stream, GetEncoding(Stream, WithBOM),
    Dummy);
end;

function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding): TSynEncoding; overload;
var
  Dummy: Boolean;
begin
  Result := LoadFromStream(UnicodeStrings, Stream, Encoding, Dummy);
end;

function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding;
var
  WideStr: UnicodeString;
  UTF8Str: UTF8String;
  AnsiStr: AnsiString;
  Size: Integer;

  function SkipBOM: Boolean;
  var
    BOM: array of Byte;
  begin
    Result := False;
    case Encoding of
      seUTF8:
        begin
          SetLength(BOM, Min(Length(UTF8BOM), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF8BOM)) or
            not CompareMem(@BOM[0], @UTF8BOM[0], Length(UTF8BOM))
          then
            Stream.Seek(-Length(BOM), {$IFDEF SYN_DELPHI_XE4_UP}soCurrent{$ELSE}soFromCurrent{$ENDIF})
          else
            Result := True;
        end;
      seUTF16LE:
        begin
          SetLength(BOM, Min(Length(UTF16BOMLE), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF16BOMLE)) or
            not CompareMem(@BOM[0], @UTF16BOMLE[0], Length(UTF16BOMLE))
          then
            Stream.Seek(-Length(BOM), {$IFDEF SYN_DELPHI_XE4_UP}soCurrent{$ELSE}soFromCurrent{$ENDIF})
          else
            Result := True;
        end;
      seUTF16BE:
        begin
          SetLength(BOM, Min(Length(UTF16BOMBE), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF16BOMBE)) or
            not CompareMem(@BOM[0], @UTF16BOMBE[0], Length(UTF16BOMBE))
          then
            Stream.Seek(-Length(BOM), {$IFDEF SYN_DELPHI_XE4_UP}soCurrent{$ELSE}soFromCurrent{$ENDIF})
          else
            Result := True;
        end;
    end;
    Size := Stream.Size - Stream.Position;
  end;

begin
  // if UnicodeStrings or Stream is nil, let Delphi raise the exception to
  // signal an invalid result
  UnicodeStrings.BeginUpdate;
  try
    Result := Encoding;
    // start decoding at actual Stream.Position
    Size := Stream.Size - Stream.Position;

    // skip BOM, if it exists
    WithBOM := SkipBOM;

    case Result of
      seUTF8:
        begin
          SetLength(UTF8Str, Size);
          Stream.ReadBuffer(UTF8Str[1], Size);
          UnicodeStrings.Text := UTF8ToUnicodeString(UTF8Str);
        end;
      seUTF16LE:
        begin
          SetLength(WideStr, Size div 2);
          Stream.ReadBuffer(WideStr[1], Size);
          UnicodeStrings.Text := WideStr;
        end;
      seUTF16BE:
        begin
          SetLength(WideStr, Size div 2);
          Stream.ReadBuffer(WideStr[1], Size);
          StrSwapByteOrder(PWideChar(WideStr));
          UnicodeStrings.Text := WideStr;
        end;
      seAnsi:
        begin
          SetLength(AnsiStr, Size);
          Stream.ReadBuffer(AnsiStr[1], Size);
          UnicodeStrings.Text := UnicodeString(AnsiStr);
        end;
    end;
  finally
    UnicodeStrings.EndUpdate
  end
end;

function ClipboardProvidesText: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_TEXT) or IsClipboardFormatAvailable(CF_UNICODETEXT);
end;

function GetClipboardText: UnicodeString;
var
  Mem: HGLOBAL;
  LocaleID: LCID;
  P: PByte;
begin
  Result := '';
  Clipboard.Open;
  try
    if Clipboard.HasFormat(CF_UNICODETEXT) then
    begin
      Mem := Clipboard.GetAsHandle(CF_UNICODETEXT);
        try
          if Mem <> 0 then
            Result := PWideChar(GlobalLock(Mem));
        finally
          if Mem <> 0 then GlobalUnlock(Mem);
        end;
    end
    else
    begin
      LocaleID := 0;
      Mem := Clipboard.GetAsHandle(CF_LOCALE);
      try
        if Mem <> 0 then LocaleID := PInteger(GlobalLock(Mem))^;
      finally
        if Mem <> 0 then GlobalUnlock(Mem);
      end;

      Mem := Clipboard.GetAsHandle(CF_TEXT);
      try
        if Mem <> 0 then
        begin
          P := GlobalLock(Mem);
          Result := StringToUnicodeStringEx(PAnsiChar(P), CodePageFromLocale(LocaleID));
        end
      finally
        if Mem <> 0 then GlobalUnlock(Mem);
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure SetClipboardText(const Text: UnicodeString);
var
  Mem: HGLOBAL;
  P: PByte;
  SLen: Integer;
begin
  if Text = '' then Exit;
  SLen := Length(Text);
  Clipboard.Open;
  try
    Clipboard.Clear;

    // set ANSI text only on Win9X, WinNT automatically creates ANSI from Unicode
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
    begin
      Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen + 1);
      if Mem <> 0 then
      begin
        P := GlobalLock(Mem);
        try
          if P <> nil then
          begin
            Move(PAnsiChar(AnsiString(Text))^, P^, SLen + 1);
            Clipboard.SetAsHandle(CF_TEXT, Mem);
          end;
        finally
          GlobalUnlock(Mem);
        end;
      end;
    end;

    // set unicode text, this also works on Win9X, even if the clipboard-viewer
    // can't show it, Word 2000+ can paste it including the unicode only characters
    Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,
      (SLen + 1) * sizeof(WideChar));
    if Mem <> 0 then
    begin
      P := GlobalLock(Mem);
      try
        if P <> nil then
        begin
          Move(PWideChar(Text)^, P^, (SLen + 1) * sizeof(WideChar));
          Clipboard.SetAsHandle(CF_UNICODETEXT, Mem);
        end;
      finally
        GlobalUnlock(Mem);
      end;
    end;
    // Don't free Mem!  It belongs to the clipboard now, and it will free it
    // when it is done with it.
  finally
    Clipboard.Close;
  end;
end;

function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(@WC), 1, nil, 0, nil,
    @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function IsUnicodeStringMappableToAnsi(const WS: UnicodeString): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(WS), Length(WS), nil, 0,
    nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

initialization
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);

end.
