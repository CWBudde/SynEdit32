{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscProcs.pas, released 2000-04-07.
The Original Code is based on the mwSupportProcs.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
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

$Id: SynEditMiscProcs.pas,v 1.35.2.8 2009/09/28 17:54:20 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.MiscProcs;

{$I SynEdit.inc}

interface

uses
  Windows, Graphics, Math, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode,
  GR32, GR32_ColorGradients;

const
  MaxIntArraySize = MaxInt div 16;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxIntArraySize - 1] of Integer;

{$IFNDEF SYN_COMPILER_4_UP}
function Max(x, y: Integer): Integer;
function Min(x, y: Integer): Integer;
{$ENDIF}

function MinMax(x, mi, ma: Integer): Integer;
procedure SwapInt(var l, r: Integer);
function MaxPoint(const P1, P2: TPoint): TPoint;
function MinPoint(const P1, P2: TPoint): TPoint;

function GetIntArray(Count: Cardinal; InitialValue: integer): PIntArray;

{$IFNDEF SYN_CLX}
procedure InternalFillRect(dc: HDC; const rcPaint: TRect);
{$ENDIF}

// Converting tabs to spaces: To use the function several times it's better
// to use a function pointer that is set to the fastest conversion function.
type
  TConvertTabsProc = function(const Line: UnicodeString;
    TabWidth: Integer): UnicodeString;

function GetBestConvertTabsProc(TabWidth: Integer): TConvertTabsProc;
// This is the slowest conversion function which can handle TabWidth <> 2^n.
function ConvertTabs(const Line: UnicodeString; TabWidth: Integer): UnicodeString;

type
  TConvertTabsProcEx = function(const Line: UnicodeString; TabWidth: Integer;
    var HasTabs: Boolean): UnicodeString;

function GetBestConvertTabsProcEx(TabWidth: Integer): TConvertTabsProcEx;
// This is the slowest conversion function which can handle TabWidth <> 2^n.
function ConvertTabsEx(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;

function GetExpandedLength(const aStr: UnicodeString; aTabWidth: Integer): Integer;

function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: UnicodeString): Integer;
function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: UnicodeString;
  var InsideTabChar: Boolean): Integer;

// search for the first char of set AChars in Line, starting at index Start
function StrScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TSynEdit32CategoryMethod): Integer;
// the same, but searching backwards
function StrRScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TSynEdit32CategoryMethod): Integer;

function GetEOL(Line: PWideChar): PWideChar;

// Remove all '/' characters from string by changing them into '\.'.
// Change all '\' characters into '\\' to allow for unique decoding.
function EncodeString(s: UnicodeString): UnicodeString;

// Decodes string, encoded with EncodeString.
function DecodeString(s: UnicodeString): UnicodeString;

{$IFNDEF SYN_COMPILER_5_UP}
procedure FreeAndNil(var Obj);
{$ENDIF}

{$IFNDEF SYN_COMPILER_3_UP}
procedure Assert(Expr: Boolean);  { stub for Delphi 2 }
{$ENDIF}

{$IFNDEF SYN_COMPILER_3_UP}
function LastDelimiter(const Delimiters, S: UnicodeString): Integer;
{$ENDIF}

{$IFNDEF SYN_COMPILER_4_UP}
type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function StringReplace(const S, OldPattern, NewPattern: UnicodeString;
  Flags: TReplaceFlags): UnicodeString;
{$ENDIF}

type
  THighlighterAttriProc = function (Highlighter: TSynEdit32CustomHighlighter;
    Attri: TSynEdit32HighlighterAttributes; UniqueAttriName: string;
    Params: array of Pointer): Boolean of object;

// Enums all child highlighters and their attributes of a TSynEdit32MultiSyn through a
// callback function.
// This function also handles nested TSynMultiSyns including their MarkerAttri.
function EnumHighlighterAttris(Highlighter: TSynEdit32CustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer): Boolean;

procedure SynEdit32DrawGradient(const ABitmap: TBitmap32; const AStartColor,
  AEndColor: TColor; const ARect: TRect; const AHorizontal: Boolean);

function DeleteTypePrefixAndSynSuffix(S: string): string;

implementation

uses
  SysUtils,
  SynEdit32.Highlighter.Multi;

{$IFNDEF SYN_COMPILER_4_UP}
function Max(x, y: Integer): Integer;
begin
  if x > y then Result := x else Result := y;
end;

function Min(x, y: Integer): Integer;
begin
  if x < y then Result := x else Result := y;
end;
{$ENDIF}

function MinMax(x, mi, ma: Integer): Integer;
begin
  x := Min(x, ma);
  Result := Max(x, mi);
end;

procedure SwapInt(var l, r: Integer);
var
  tmp: Integer;
begin
  tmp := r;
  r := l;
  l := tmp;
end;

function MaxPoint(const P1, P2: TPoint): TPoint;
begin
  if (P2.y > P1.y) or ((P2.y = P1.y) and (P2.x > P1.x)) then
    Result := P2
  else
    Result := P1;
end;

function MinPoint(const P1, P2: TPoint): TPoint;
begin
  if (P2.y < P1.y) or ((P2.y = P1.y) and (P2.x < P1.x)) then
    Result := P2
  else
    Result := P1;
end;

function GetIntArray(Count: Cardinal; InitialValue: Integer): PIntArray;
var
  p: PInteger;
begin
  Result := AllocMem(Count * SizeOf(Integer));
  if Assigned(Result) and (InitialValue <> 0) then
  begin
    p := PInteger(Result);
    while (Count > 0) do
    begin
      p^ := InitialValue;
      Inc(p);
      Dec(Count);
    end;
  end;
end;

{$IFNDEF SYN_CLX}
procedure InternalFillRect(dc: HDC; const rcPaint: TRect);
begin
  ExtTextOut(dc, 0, 0, ETO_OPAQUE, @rcPaint, nil, 0, nil);
end;
{$ENDIF}

// Please don't change this function; no stack frame and efficient register use.
function GetHasTabs(pLine: PWideChar; var CharsBefore: Integer): Boolean;
begin
  CharsBefore := 0;
  if Assigned(pLine) then
  begin
    while pLine^ <> #0 do 
    begin
      if pLine^ = #9 then break;
      Inc(CharsBefore);
      Inc(pLine);
    end;
    Result := pLine^ = #9;
  end
  else
    Result := False;
end;


function ConvertTabs1Ex(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;
var
  pDest: PWideChar;
  nBeforeTab: Integer;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), nBeforeTab) then
  begin
    HasTabs := True;
    pDest := @Result[nBeforeTab + 1]; // this will make a copy of Line
    // We have at least one tab in the string, and the tab width is 1.
    // pDest points to the first tab char. We overwrite all tabs with spaces.
    repeat
      if (pDest^ = #9) then pDest^ := ' ';
      Inc(pDest);
    until (pDest^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs1(const Line: UnicodeString; TabWidth: Integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabs1Ex(Line, TabWidth, HasTabs);
end;

function ConvertTabs2nEx(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;
var
  i, DestLen, TabCount, TabMask: Integer;
  pSrc, pDest: PWideChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width equals 2^n.
    // pSrc points to the first tab char in Line. We get the number of tabs
    // and the length of the expanded string now.
    TabCount := 0;
    TabMask := (TabWidth - 1) xor $7FFFFFFF;
    repeat
      if pSrc^ = #9 then
      begin
        DestLen := (DestLen + TabWidth) and TabMask;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PWideChar(Line);
    pDest := PWideChar(Result);
    // We use another TabMask here to get the difference to 2^n.
    TabMask := TabWidth - 1;
    repeat
      if pSrc^ = #9 then
      begin
        i := TabWidth - (DestLen and TabMask);
        Inc(DestLen, i);
        //This is used for both drawing and other stuff and is meant to be #9 and not #32
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs2n(const Line: UnicodeString; TabWidth: Integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabs2nEx(Line, TabWidth, HasTabs);
end;

function ConvertTabsEx(const Line: UnicodeString; TabWidth: Integer;
  var HasTabs: Boolean): UnicodeString;
var
  i, DestLen, TabCount: Integer;
  pSrc, pDest: PWideChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if pSrc^ = #9 then
      begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PWideChar(Line);
    pDest := PWideChar(Result);
    repeat
      if pSrc^ = #9 then
      begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end
  else
    HasTabs := False;
end;

function ConvertTabs(const Line: UnicodeString; TabWidth: Integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  Result := ConvertTabsEx(Line, TabWidth, HasTabs);
end;

function IsPowerOfTwo(TabWidth: Integer): Boolean;
var
  nW: Integer;
begin
  nW := 2;
  repeat
    if (nW >= TabWidth) then break;
    Inc(nW, nW);
  until (nW >= $10000);  // we don't want 64 kByte spaces...
  Result := (nW = TabWidth);
end;

function GetBestConvertTabsProc(TabWidth: Integer): TConvertTabsProc;
begin
  if (TabWidth < 2) then Result := TConvertTabsProc(@ConvertTabs1)
    else if IsPowerOfTwo(TabWidth) then
      Result := TConvertTabsProc(@ConvertTabs2n)
    else
      Result := TConvertTabsProc(@ConvertTabs);
end;

function GetBestConvertTabsProcEx(TabWidth: Integer): TConvertTabsProcEx;
begin
  if (TabWidth < 2) then Result := ConvertTabs1Ex
    else if IsPowerOfTwo(TabWidth) then
      Result := ConvertTabs2nEx
    else
      Result := ConvertTabsEx;
end;

function GetExpandedLength(const aStr: UnicodeString; aTabWidth: Integer): Integer;
var
  iRun: PWideChar;
begin
  Result := 0;
  iRun := PWideChar(aStr);
  while iRun^ <> #0 do
  begin
    if iRun^ = #9 then
      Inc(Result, aTabWidth - (Result mod aTabWidth))
    else
      Inc(Result);
    Inc(iRun);
  end;
end;

function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: UnicodeString): Integer;
var
  iChar: Integer;
  pNext: PWideChar;
begin
// possible sanity check here: Index := Max(Index, Length(Line));
  if Index > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(pointer(Line), iChar) then
      Result := Index
    else
    begin
      if iChar + 1 >= Index then
        Result := Index
      else
      begin
        // iChar is number of chars before first #9
        Result := iChar;
        // Index is *not* zero-based
        Inc(iChar);
        Dec(Index, iChar);
        pNext := @Line[iChar];
        while Index > 0 do
        begin
          case pNext^ of
            #0:
              begin
                Inc(Result, Index);
                break;
              end;
            #9:
              begin
                // Result is still zero-based
                Inc(Result, TabWidth);
                Dec(Result, Result mod TabWidth);
              end;
            else
              Inc(Result);
          end;
          Dec(Index);
          Inc(pNext);
        end;
        // done with zero-based computation
        Inc(Result);
      end;
    end;
  end
  else
    Result := 1;
end;

function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: UnicodeString;
  var InsideTabChar: Boolean): Integer;
var
  iPos: Integer;
  pNext: PWideChar;
begin
  InsideTabChar := False;
  if Position > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(pointer(Line), iPos) then
      Result := Position
    else
    begin
      if iPos + 1 >= Position then
        Result := Position
      else
      begin
        // iPos is number of chars before first #9
        Result := iPos + 1;
        pNext := @Line[Result];
        // for easier computation go zero-based (mod-operation)
        Dec(Position);
        while iPos < Position do
        begin
          case pNext^ of
            #0: break;
            #9: begin
                  Inc(iPos, TabWidth);
                  Dec(iPos, iPos mod TabWidth);
                  if iPos > Position then
                  begin
                    InsideTabChar := True;
                    break;
                  end;
                end;
            else
              Inc(iPos);
          end;
          Inc(Result);
          Inc(pNext);
        end;
      end;
    end;
  end
  else
    Result := Position;
end;

function StrScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TSynEdit32CategoryMethod): Integer;
var
  p: PWideChar;
begin
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    p := PWideChar(@Line[Start]);
    repeat
      if IsOfCategory(p^) then
      begin
        Result := Start;
        exit;
      end;
      Inc(p);
      Inc(Start);
    until p^ = #0;
  end;
  Result := 0;
end;

function StrRScanForCharInCategory(const Line: UnicodeString; Start: Integer;
  IsOfCategory: TSynEdit32CategoryMethod): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    for I := Start downto 1 do
      if IsOfCategory(Line[I]) then
      begin
        Result := I;
        Exit;
      end;
  end;
end;

function GetEOL(Line: PWideChar): PWideChar;
begin
  Result := Line;
  if Assigned(Result) then
    while (Result^ <> #0) and (Result^ <> #10) and (Result^ <> #13) do
      Inc(Result);
end;

{$IFOPT R+}{$DEFINE RestoreRangeChecking}{$ELSE}{$UNDEF RestoreRangeChecking}{$ENDIF}
{$R-}
function EncodeString(s: UnicodeString): UnicodeString;
var
  i, j: Integer;
begin
  SetLength(Result, 2 * Length(s)); // worst case
  j := 0;
  for i := 1 to Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Result[j] := '\';
      Result[j + 1] := '\';
      Inc(j);
    end
    else if s[i] = '/' then
    begin
      Result[j] := '\';
      Result[j + 1] := '.';
      Inc(j);
    end
    else
      Result[j] := s[i];
  end; //for
  SetLength(Result, j);
end; { EncodeString }

function DecodeString(s: UnicodeString): UnicodeString;
var
  i, j: Integer;
begin
  SetLength(Result, Length(s)); // worst case
  j := 0;
  i := 1;
  while i <= Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Inc(i);
      if s[i] = '\' then
        Result[j] := '\'
      else
        Result[j] := '/';
    end
    else
      Result[j] := s[i];
    Inc(i);
  end; //for
  SetLength(Result,j);
end; { DecodeString }
{$IFDEF RestoreRangeChecking}{$R+}{$ENDIF}

{$IFNDEF SYN_COMPILER_5_UP}
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;
  P.Free;
end;
{$ENDIF}

{$IFNDEF SYN_COMPILER_3_UP}
procedure Assert(Expr: Boolean);  { stub for Delphi 2 }
begin
end;
{$ENDIF}

{$IFNDEF SYN_COMPILER_3_UP}
function LastDelimiter(const Delimiters, S: UnicodeString): Integer;
var
  P: PWideChar;
begin
  Result := Length(S);
  P := PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
      exit;
    Dec(Result);
  end;
end;
{$ENDIF}

{$IFNDEF SYN_COMPILER_4_UP}
function StringReplace(const S, OldPattern, NewPattern: UnicodeString;
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
{$ENDIF}

function DeleteTypePrefixAndSynSuffix(S: string): string;
begin
  Result := S;
  if CharInSet(Result[1], ['T', 't']) then //ClassName is never empty so no AV possible
    if Pos('tsyn', LowerCase(Result)) = 1 then
      Delete(Result, 1, 4)
    else
      Delete(Result, 1, 1);

  if Copy(LowerCase(Result), Length(Result) - 2, 3) = 'syn' then
    SetLength(Result, Length(Result) - 3);
end;

function GetHighlighterIndex(Highlighter: TSynEdit32CustomHighlighter;
  HighlighterList: TList): Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to HighlighterList.Count - 1 do
    if HighlighterList[i] = Highlighter then
      Exit
    else if Assigned(HighlighterList[i]) and (TObject(HighlighterList[i]).ClassType = Highlighter.ClassType) then
      Inc(Result);
end;

function InternalEnumHighlighterAttris(Highlighter: TSynEdit32CustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer; HighlighterList: TList): Boolean;
var
  i: Integer;
  UniqueAttriName: string;
begin
  Result := True;

  if (HighlighterList.IndexOf(Highlighter) >= 0) then
  begin
    if SkipDuplicates then Exit;
  end
  else
    HighlighterList.Add(Highlighter);

  if Highlighter is TSynEdit32HighlighterMulti then
    with TSynEdit32HighlighterMulti(Highlighter) do
    begin
      Result := InternalEnumHighlighterAttris(DefaultHighlighter, SkipDuplicates,
        HighlighterAttriProc, Params, HighlighterList);
      if not Result then Exit;

      for i := 0 to Schemes.Count - 1 do
      begin
        UniqueAttriName := Highlighter.ExportName +
          IntToStr(GetHighlighterIndex(Highlighter, HighlighterList)) + '.' +
          Schemes[i].MarkerAttri.Name + IntToStr(i + 1);

        Result := HighlighterAttriProc(Highlighter, Schemes[i].MarkerAttri,
          UniqueAttriName, Params);
        if not Result then Exit;

        Result := InternalEnumHighlighterAttris(Schemes[i].Highlighter,
          SkipDuplicates, HighlighterAttriProc, Params, HighlighterList);
        if not Result then Exit
      end
    end
  else if Assigned(Highlighter) then
    for i := 0 to Highlighter.AttrCount - 1 do
    begin
      UniqueAttriName := Highlighter.ExportName +
        IntToStr(GetHighlighterIndex(Highlighter, HighlighterList)) + '.' +
        Highlighter.Attribute[i].Name;

      Result := HighlighterAttriProc(Highlighter, Highlighter.Attribute[i],
        UniqueAttriName, Params);
      if not Result then Exit
    end
end;

function EnumHighlighterAttris(Highlighter: TSynEdit32CustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer): Boolean;
var
  HighlighterList: TList;
begin
  if not Assigned(Highlighter) or not Assigned(HighlighterAttriProc) then
  begin
    Result := False;
    Exit;
  end;

  HighlighterList := TList.Create;
  try
    Result := InternalEnumHighlighterAttris(Highlighter, SkipDuplicates,
      HighlighterAttriProc, Params, HighlighterList)
  finally
    HighlighterList.Free
  end
end;

procedure SynEdit32DrawGradient(const ABitmap: TBitmap32; const AStartColor,
  AEndColor: TColor; const ARect: TRect; const AHorizontal: Boolean);
var
  LinearGradient: TColor32Gradient;
  BaseScanLine: PColor32Array;
  ScanLine: PColor32Array;
  X, Y: Integer;
  ScaleFactor: Single;
begin
  if (ARect.Top > ARect.Bottom) or (ARect.Left > ARect.Right) then
    Exit;

  LinearGradient := TColor32Gradient.Create(Color32(AStartColor), Color32(AEndColor));
  try
    BaseScanLine := ABitmap.ScanLine[ARect.Top];
    ScaleFactor := 1 / ARect.Width;
    for X := 0 to ARect.Width - 1 do
      BaseScanLine^[ARect.Left + X] := LinearGradient.GetColorAt(X * ScaleFactor);

    for Y := ARect.Top + 1 to ARect.Bottom do
    begin
      ScanLine := ABitmap.ScanLine[Y];
      Move(BaseScanLine^[ARect.Left], ScanLine^[ARect.Left], ARect.Width * SizeOf(TColor32));
    end;
  finally
    LinearGradient.Free;
  end;
end;

end.
