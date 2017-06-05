{==============================================================================
  Content:  TSynEdit32TextDrawer, a helper class for drawing of
            fixed-pitched font characters
 ==============================================================================
  The contents of this file are subject to the Mozilla Public License Ver. 1.0
  (the "License"); you may not use this file except in compliance with the
  License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.
 ==============================================================================
  The Original Code is HANAI Tohru's private delphi library.
 ==============================================================================
  The Initial Developer of the Original Code is HANAI Tohru (Japan)
  Portions created by HANAI Tohru are Copyright (C) 1999.
  All Rights Reserved.
 ==============================================================================
  Contributor(s):   HANAI Tohru
  Unicode translation by Maël Hörz.
 ==============================================================================
  History:  01/19/1999  HANAI Tohru
                        Initial Version
            02/13/1999  HANAI Tohru
                        Changed default intercharacter spacing
            09/09/1999  HANAI Tohru
                        Redesigned all. Simplified interfaces.
                        When drawing text now it uses TextOut + SetTextCharacter-
                        Extra insted ExtTextOut since ExtTextOut has a little
                        heavy behavior.
            09/10/1999  HANAI Tohru
                        Added code to call ExtTextOut because there is a problem
                        when TextOut called with italicized raster type font.
                        After this changing, ExtTextOut is called without the
                        last parameter `lpDx' and be with SetTextCharacterExtra.
                        This pair performs faster than with `lpDx'.
            09/14/1999  HANAI Tohru
                        Changed code for saving/restoring DC
            09/15/1999  HANAI Tohru
                        Added X/Y parameters to ExtTextOut.
            09/16/1999  HANAI Tohru
                        Redesigned for multi-bytes character drawing.
            09/19/1999  HANAI Tohru
                        Since TSynEdit32TextDrawer grew fat it was split into three
                        classes - TSynEdit32FontStock, TSynEdit32TextDrawer and TheTextDrawerEx.
                        Currently it should avoid TSynEdit32TextDrawer because it is
                        slower than TSynEdit32TextDrawer.
            09/25/1999  HANAI Tohru
                        Added internally definition of LeadBytes for Delphi 2
            10/01/1999  HANAI Tohru
                        To save font resources, now all fonts data are shared
                        among all of TSynEdit32FontStock instances. With this changing,
                        there added a new class `TSynEdit32FontsInfoManager' to manage
                        those shared data.
            10/09/1999  HANAI Tohru
                        Added BaseStyle property to TheFontFont class.
 ==============================================================================}

// $Id: SynTextDrawer.pas,v 1.6.2.17 2008/09/17 13:59:12 maelh Exp $

// SynEdit note: The name had to be changed to get SynEdit to install 
//   together with mwEdit into the same Delphi installation

unit SynEdit32.TextDrawer;

{$I SynEdit.inc}

interface

uses
  {$IFDEF SYN_COMPILER_17_UP} Types, UITypes, {$ENDIF}
  SynEdit32.Unicode, SysUtils, Classes, Windows, Graphics, Math;

const
  FontStyleCount = Ord(High(TFontStyle)) +1;
  FontStyleCombineCount = (1 shl FontStyleCount);
  
type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;

  TSynEdit32StockFontPatterns = 0..FontStyleCombineCount - 1;

  PSynEdit32FontData = ^TSynEdit32FontData;
  TSynEdit32FontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharAdv: Integer;
    CharHeight: Integer;
  end;

  PSynEdit32FontsData = ^TSynEdit32FontsData;
  TSynEdit32FontsData = array[TSynEdit32StockFontPatterns] of TSynEdit32FontData;

  PSynEdit32SharedFontsInfo = ^TSynEdit32SharedFontsInfo;
  TSynEdit32SharedFontsInfo = record
    // reference counters
    RefCount: Integer;
    LockCount: Integer;
    // font information
    BaseFont: TFont;
    BaseLF: TLogFont;
    IsTrueType: Boolean;
    FontsData: TSynEdit32FontsData;
  end;

  { TSynEdit32StockFontManager }

  TSynEdit32FontsInfoManager = class
  private
    FFontsInfo: TList;
    function FindFontsInfo(const LF: TLogFont): PSynEdit32SharedFontsInfo;
    function CreateFontsInfo(ABaseFont: TFont;
      const LF: TLogFont): PSynEdit32SharedFontsInfo;
    procedure DestroyFontHandles(pFontsInfo: PSynEdit32SharedFontsInfo);
    procedure RetrieveLogFontForComparison(ABaseFont: TFont; var LF: TLogFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockFontsInfo(pFontsInfo: PSynEdit32SharedFontsInfo);
    procedure UnLockFontsInfo(pFontsInfo: PSynEdit32SharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PSynEdit32SharedFontsInfo;
    procedure ReleaseFontsInfo(pFontsInfo: PSynEdit32SharedFontsInfo);
  end;

  { TSynEdit32FontStock }

  TTextOutOptions = set of (tooOpaque, tooClipped);

  TSynEdit32ExtTextOutProc = procedure (X, Y: Integer; fuOptions: TTextOutOptions;
    const ARect: TRect; const Text: UnicodeString; Length: Integer) of object;

  ESynEdit32FontStockException = class(Exception);

  TSynEdit32FontStock = class
  private
    // private DC
    FDC: HDC;
    FDCRefCount: Integer;

    // Shared fonts
    FpInfo: PSynEdit32SharedFontsInfo;
    FUsingFontHandles: Boolean;

    // Current font
    FCrntFont: HFONT;
    FCrntStyle: TFontStyles;
    FpCrntFontData: PSynEdit32FontData;
    // local font info
    FBaseLF: TLogFont;
    function GetBaseFont: TFont;
    function GetIsTrueType: Boolean;
  protected
    function InternalGetDC: HDC; virtual;
    procedure InternalReleaseDC(Value: HDC); virtual;
    function InternalCreateFont(Style: TFontStyles): HFONT; virtual;
    function CalcFontAdvance(DC: HDC; pCharHeight: PInteger): Integer; virtual;
    function GetCharAdvance: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    function GetFontData(idx: Integer): PSynEdit32FontData; virtual;
    procedure UseFontHandles;
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    property FontData[idx: Integer]: PSynEdit32FontData read GetFontData;
    property FontsInfo: PSynEdit32SharedFontsInfo read FpInfo;
  public
    constructor Create(InitialFont: TFont); virtual;
    destructor Destroy; override;
    procedure ReleaseFontHandles; virtual;
    property BaseFont: TFont read GetBaseFont;
    property Style: TFontStyles read FCrntStyle write SetStyle;
    property FontHandle: HFONT read FCrntFont;
    property CharAdvance: Integer read GetCharAdvance;
    property CharHeight: Integer read GetCharHeight;
    property IsTrueType: Boolean read GetIsTrueType;
  end;

  { TSynEdit32TextDrawer }
  ESynEdit32TextDrawerException = class(Exception);

  TSynEdit32TextDrawer = class(TObject)
  private
    FDC: HDC;
    FSaveDC: Integer;

    // Font information
    FFontStock: TSynEdit32FontStock;
    FStockBitmap: TBitmap;
    FCalcExtentBaseStyle: TFontStyles;
    FBaseCharWidth: Integer;
    FBaseCharHeight: Integer;

    // current font and properties
    FCrntFont: HFONT;
    FETODist: PIntegerArray;

    // current font attributes
    FColor: TColor;
    FBkColor: TColor;
    FCharExtra: Integer;

    // Begin/EndDrawing calling count
    FDrawingCount: Integer;

    // GetCharABCWidthsW cache
    FCharABCWidthCache : array [0..127] of TABC;
    FCharWidthCache : array [0..127] of Integer;

  protected
    procedure ReleaseETODist; virtual;
    procedure AfterStyleSet; virtual;
    procedure DoSetCharExtra(Value: Integer); virtual;
    procedure FlushCharABCWidthCache;
    function GetCachedABCWidth(c : Cardinal; var abc : TABC) : Boolean;
    property StockDC: HDC read FDC;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TSynEdit32FontStock read FFontStock;
    property BaseCharWidth: Integer read FBaseCharWidth;
    property BaseCharHeight: Integer read FBaseCharHeight;

  public
    constructor Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont); virtual;
    destructor Destroy; override;

    function GetCharWidth: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    procedure BeginDrawing(DC: HDC); virtual;
    procedure EndDrawing; virtual;
    procedure TextOut(X, Y: Integer; Text: PWideChar; Length: Integer); virtual;
    procedure ExtTextOut(X, Y: Integer; Options: TTextOutOptions; ARect: TRect;
      Text: PWideChar; Length: Integer); virtual;
    function TextExtent(const Text: UnicodeString): TSize; overload;
    function TextExtent(Text: PWideChar; Count: Integer): TSize; overload;
    function TextWidth(const Text: UnicodeString): Integer; overload;
    function TextWidth(Text: PWideChar; Count: Integer): Integer; overload;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetBaseStyle(const Value: TFontStyles); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    procedure SetForeColor(Value: TColor); virtual;
    procedure SetBackColor(Value: TColor); virtual;
    procedure SetCharExtra(Value: Integer); virtual;
    procedure ReleaseTemporaryResources; virtual;

    property CharWidth: Integer read GetCharWidth;
    property CharHeight: Integer read GetCharHeight;
    property BaseFont: TFont write SetBaseFont;
    property BaseStyle: TFontStyles write SetBaseStyle;
    property ForeColor: TColor write SetForeColor;
    property BackColor: TColor write SetBackColor;
    property Style: TFontStyles write SetStyle;
    property CharExtra: Integer read FCharExtra write SetCharExtra;
  end;

function GetFontsInfoManager: TSynEdit32FontsInfoManager;

function UniversalExtTextOut(DC: HDC; X, Y: Integer; Options: TTextOutOptions;
  Rect: TRect; Str: PWideChar; Count: Integer; ETODist: PIntegerArray): Boolean;

implementation

var
  GFontsInfoManager: TSynEdit32FontsInfoManager;

{ utility routines }

function GetFontsInfoManager: TSynEdit32FontsInfoManager;
begin
  if not Assigned(GFontsInfoManager) then
    GFontsInfoManager := TSynEdit32FontsInfoManager.Create;
  Result := GFontsInfoManager;
end;

function Min(x, y: integer): integer;
begin
  if x < y then Result := x else Result := y;
end;

// UniversalExtTextOut uses UniScribe where available for the best possible
// output quality. This also avoids a bug in (Ext)TextOut that surfaces when
// displaying a combination of Chinese and Korean text.
//
// See here for details: http://groups.google.com/group/microsoft.public.win32.programmer.international/browse_thread/thread/77cd596f2b96dc76/146300208098285c?lnk=st&q=font+substitution+problem#146300208098285c
function UniversalExtTextOut(DC: HDC; X, Y: Integer; Options: TTextOutOptions;
  Rect: TRect; Str: PWideChar; Count: Integer; ETODist: PIntegerArray): Boolean;
var
  TextOutFlags: DWORD;
begin
  TextOutFlags := 0;
  if tooOpaque in Options then
    TextOutFlags := TextOutFlags or ETO_OPAQUE;
  if tooClipped in Options then
    TextOutFlags := TextOutFlags or ETO_CLIPPED;

  Result := ExtTextOutW(DC, X, Y, TextOutFlags, @Rect, Str, Count,
    Pointer(ETODist));
end;

{ TSynEdit32FontsInfoManager }

procedure TSynEdit32FontsInfoManager.LockFontsInfo(
  pFontsInfo: PSynEdit32SharedFontsInfo);
begin
  Inc(pFontsInfo^.LockCount);
end;

constructor TSynEdit32FontsInfoManager.Create;
begin
  inherited;

  FFontsInfo := TList.Create;
end;

function TSynEdit32FontsInfoManager.CreateFontsInfo(ABaseFont: TFont;
  const LF: TLogFont): PSynEdit32SharedFontsInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(TSynEdit32SharedFontsInfo), 0);
  with Result^ do
    try
      BaseFont := TFont.Create;
      BaseFont.Assign(ABaseFont);
      BaseLF := LF;
      IsTrueType := (0 <> (TRUETYPE_FONTTYPE and LF.lfPitchAndFamily));
  except
    Result^.BaseFont.Free;
    Dispose(Result);
    raise;
  end;
end;

procedure TSynEdit32FontsInfoManager.UnlockFontsInfo(
  pFontsInfo: PSynEdit32SharedFontsInfo);
begin
  with pFontsInfo^ do
  begin
    Dec(LockCount);
    if 0 = LockCount then
      DestroyFontHandles(pFontsInfo);
  end;
end;

destructor TSynEdit32FontsInfoManager.Destroy;
begin
  GFontsInfoManager := nil;
  
  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      ASSERT(1 = PSynEdit32SharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      ReleaseFontsInfo(PSynEdit32SharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]));
    end;
    FFontsInfo.Free;
  end;

  inherited;
end;

procedure TSynEdit32FontsInfoManager.DestroyFontHandles(
  pFontsInfo: PSynEdit32SharedFontsInfo);
var
  i: Integer;
begin
  with pFontsInfo^ do
    for i := Low(TSynEdit32StockFontPatterns) to High(TSynEdit32StockFontPatterns) do
      with FontsData[i] do
        if Handle <> 0 then
        begin
          DeleteObject(Handle);
          Handle := 0;
        end;
end;

function TSynEdit32FontsInfoManager.FindFontsInfo(
  const LF: TLogFont): PSynEdit32SharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PSynEdit32SharedFontsInfo(FFontsInfo[i]);
    if CompareMem(@(Result^.BaseLF), @LF, SizeOf(TLogFont)) then
      Exit;
  end;
  Result := nil;
end;

function TSynEdit32FontsInfoManager.GetFontsInfo(ABaseFont: TFont): PSynEdit32SharedFontsInfo;
var
  LF: TLogFont;
begin
  ASSERT(Assigned(ABaseFont));

  RetrieveLogFontForComparison(ABaseFont, LF);
  Result := FindFontsInfo(LF);
  if not Assigned(Result) then
  begin
    Result := CreateFontsInfo(ABaseFont, LF);
    FFontsInfo.Add(Result);
  end;

  if Assigned(Result) then
    Inc(Result^.RefCount);
end;

procedure TSynEdit32FontsInfoManager.ReleaseFontsInfo(pFontsInfo: PSynEdit32SharedFontsInfo);
begin
  ASSERT(Assigned(pFontsInfo));

  with pFontsInfo^ do
  begin
    Assert(LockCount < RefCount);
    if RefCount > 1 then
      Dec(RefCount)
    else
    begin
      FFontsInfo.Remove(pFontsInfo);
      // free all objects
      BaseFont.Free;
      Dispose(pFontsInfo);
    end;
  end;
end;

procedure TSynEdit32FontsInfoManager.RetrieveLogFontForComparison(ABaseFont: TFont;
  var LF: TLogFont);
var
  pEnd: PChar;
begin
  GetObject(ABaseFont.Handle, SizeOf(TLogFont), @LF);
  with LF do
  begin
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    pEnd := StrEnd(lfFaceName);
    FillChar(pEnd[1], @lfFaceName[High(lfFaceName)] - pEnd, 0);
  end;
end;

{ TSynEdit32FontStock }

// CalcFontAdvance : Calculation a advance of a character of a font.
//  [*]hCalcFont will be selected as FDC's font if FDC wouldn't be zero.
function TSynEdit32FontStock.CalcFontAdvance(DC: HDC; pCharHeight: PInteger): Integer;
var
  TM: TTextMetric;
  ABC: TABC;
  HasABC: Boolean;
begin
  // Calculate advance of a character.
  // The following code uses ABC widths instead TextMetric.tmAveCharWidth
  // because ABC widths always tells truth but tmAveCharWidth does not.
  // A true-type font will have ABC widths but others like raster type will not
  // so if the function fails then use TextMetric.tmAveCharWidth.
  GetTextMetrics(DC, TM);
  HasABC := GetCharABCWidths(DC, Ord('M'), Ord('M'), ABC);
  if not HasABC then
  begin
    with ABC do
    begin
      abcA := 0;
      abcB := TM.tmAveCharWidth;
      abcC := 0;
    end;
    TM.tmOverhang := 0;
  end;

  // Result(CharWidth)
  with ABC do
    Result := abcA + Integer(abcB) + abcC + TM.tmOverhang;
  // pCharHeight
  if Assigned(pCharHeight) then
    pCharHeight^ := Abs(TM.tmHeight) {+ TM.tmInternalLeading};
end;

constructor TSynEdit32FontStock.Create(InitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(InitialFont);
end;

destructor TSynEdit32FontStock.Destroy;
begin
  ReleaseFontsInfo;
  ASSERT(FDCRefCount = 0);

  inherited;
end;

function TSynEdit32FontStock.GetBaseFont: TFont;
begin
  Result := FpInfo^.BaseFont;
end;

function TSynEdit32FontStock.GetCharAdvance: Integer;
begin
  Result := FpCrntFontData^.CharAdv;
end;

function TSynEdit32FontStock.GetCharHeight: Integer;
begin
  Result := FpCrntFontData^.CharHeight;
end;

function TSynEdit32FontStock.GetFontData(idx: Integer): PSynEdit32FontData;
begin
  Result := @FpInfo^.FontsData[idx];
end;

function TSynEdit32FontStock.GetIsTrueType: Boolean;
begin
  Result := FpInfo^.IsTrueType
end;

function TSynEdit32FontStock.InternalCreateFont(Style: TFontStyles): HFONT;
const
  Bolds: array[Boolean] of Integer = (400, 700);
begin
  with FBaseLF do
  begin
    lfWeight := Bolds[fsBold in Style];
    lfItalic := Ord(BOOL(fsItalic in Style));
    lfUnderline := Ord(BOOL(fsUnderline in Style));
    lfStrikeOut := Ord(BOOL(fsStrikeOut in Style));
  end;
  Result := CreateFontIndirect(FBaseLF);
end;

function TSynEdit32FontStock.InternalGetDC: HDC;
begin
  if FDCRefCount = 0 then
  begin
    ASSERT(FDC = 0);
    FDC := GetDC(0);
  end;
  Inc(FDCRefCount);
  Result := FDC;
end;

procedure TSynEdit32FontStock.InternalReleaseDC(Value: HDC);
begin
  Dec(FDCRefCount);
  if FDCRefCount <= 0 then
  begin
    ASSERT((FDC <> 0) and (FDC = Value));
    ReleaseDC(0, FDC);
    FDC := 0;
    ASSERT(FDCRefCount = 0);
  end;
end;

procedure TSynEdit32FontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnlockFontsInfo(FpInfo);
      FUsingFontHandles := False;
    end;
end;

procedure TSynEdit32FontStock.ReleaseFontsInfo;
begin
  if Assigned(FpInfo) then
    with GetFontsInfoManager do
    begin
      if FUsingFontHandles then
      begin
        UnlockFontsInfo(FpInfo);
        FUsingFontHandles := False;
      end;
      ReleaseFontsInfo(FpInfo);
      FpInfo := nil;
    end;
end;

procedure TSynEdit32FontStock.SetBaseFont(Value: TFont);
var
  pInfo: PSynEdit32SharedFontsInfo;
begin
  if Assigned(Value) then
  begin
    pInfo := GetFontsInfoManager.GetFontsInfo(Value);
    if pInfo = FpInfo then
      GetFontsInfoManager.ReleaseFontsInfo(pInfo)
    else
    begin
      ReleaseFontsInfo;
      FpInfo := pInfo;
      FBaseLF := FpInfo^.BaseLF;
      SetStyle(Value.Style);
    end;
  end
  else
    raise ESynEdit32FontStockException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TSynEdit32FontStock.SetStyle(Value: TFontStyles);
var
  idx: Integer;
  DC: HDC;
  hOldFont: HFONT;
  p: PSynEdit32FontData;
begin
  Assert(SizeOf(TFontStyles) = 1);

  idx := Byte(Value);
  Assert(idx <= High(TSynEdit32StockFontPatterns));

  UseFontHandles;
  p := FontData[idx];
  if FpCrntFontData = p then
    Exit;

  FpCrntFontData := p;
  with p^ do
    if Handle <> 0 then
    begin
      FCrntFont := Handle;
      FCrntStyle := Style;
      Exit;
    end;

  // create font
  FCrntFont := InternalCreateFont(Value);
  DC := InternalGetDC;
  hOldFont := SelectObject(DC, FCrntFont);

  // retrieve height and advances of new font
  with FpCrntFontData^ do
  begin
    Handle := FCrntFont;
    CharAdv := CalcFontAdvance(DC, @CharHeight);
  end;

  SelectObject(DC, hOldFont);
  InternalReleaseDC(DC);
end;

procedure TSynEdit32FontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FpInfo);
      FUsingFontHandles := True;
    end;
end;

{ TSynEdit32TextDrawer }

constructor TSynEdit32TextDrawer.Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont);
begin
  inherited Create;

  FFontStock := TSynEdit32FontStock.Create(BaseFont);
  FStockBitmap := TBitmap.Create;
  FCalcExtentBaseStyle := CalcExtentBaseStyle;
  SetBaseFont(BaseFont);
  FColor := clWindowText;
  FBkColor := clWindow;
end;

destructor TSynEdit32TextDrawer.Destroy;
begin
  FStockBitmap.Free;
  FFontStock.Free;
  ReleaseETODist;
  
  inherited;
end;

procedure TSynEdit32TextDrawer.ReleaseETODist;
begin
  if Assigned(FETODist) then
  begin
    FreeMem(FETODist);
    FETODist := nil;
  end;
end;

procedure TSynEdit32TextDrawer.BeginDrawing(DC: HDC);
begin
  if (FDC = DC) then
    ASSERT(FDC <> 0)
  else
  begin
    ASSERT((FDC = 0) and (DC <> 0) and (FDrawingCount = 0));
    FDC := DC;
    FSaveDC := SaveDC(DC);
    SelectObject(DC, FCrntFont);
    Windows.SetTextColor(DC, ColorToRGB(FColor));
    Windows.SetBkColor(DC, ColorToRGB(FBkColor));
    DoSetCharExtra(FCharExtra);
  end;
  Inc(FDrawingCount);
end;

procedure TSynEdit32TextDrawer.EndDrawing;
begin
  ASSERT(FDrawingCount >= 1);
  Dec(FDrawingCount);
  if FDrawingCount <= 0 then
  begin
    if FDC <> 0 then
      RestoreDC(FDC, FSaveDC);
    FSaveDC := 0;
    FDC := 0;
    FDrawingCount := 0;
  end;
end;

function TSynEdit32TextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth + FCharExtra;
end;

function TSynEdit32TextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;

procedure TSynEdit32TextDrawer.SetBaseFont(Value: TFont);
begin
  if Assigned(Value) then
  begin
    FlushCharABCWidthCache;
    ReleaseETODist;
    FStockBitmap.Canvas.Font.Assign(Value);
    FStockBitmap.Canvas.Font.Style := [];
    with FFontStock do
    begin
      SetBaseFont(Value);
      Style := FCalcExtentBaseStyle;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
    SetStyle(Value.Style);
  end
  else
    raise ESynEdit32TextDrawerException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TSynEdit32TextDrawer.SetBaseStyle(const Value: TFontStyles);
begin
  if FCalcExtentBaseStyle <> Value then
  begin
    FCalcExtentBaseStyle := Value;
    FlushCharABCWidthCache;
    ReleaseETODist;
    with FFontStock do
    begin
      Style := Value;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
  end;
end;

procedure TSynEdit32TextDrawer.SetStyle(Value: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(Value);
    Self.FCrntFont := FontHandle;
  end;
  AfterStyleSet;
end;

procedure TSynEdit32TextDrawer.AfterStyleSet;
begin
  if FDC <> 0 then
    SelectObject(FDC, FCrntFont);
end;

procedure TSynEdit32TextDrawer.SetForeColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FDC <> 0 then
      SetTextColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TSynEdit32TextDrawer.SetBackColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    if FDC <> 0 then
      Windows.SetBkColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TSynEdit32TextDrawer.SetCharExtra(Value: Integer);
begin
  if FCharExtra <> Value then
  begin
    FCharExtra := Value;
    DoSetCharExtra(FCharExtra);
  end;
end;

procedure TSynEdit32TextDrawer.DoSetCharExtra(Value: Integer);
begin
  if FDC <> 0 then
    SetTextCharacterExtra(FDC, Value);
end;

procedure TSynEdit32TextDrawer.FlushCharABCWidthCache;
begin
   FillChar(FCharABCWidthCache, SizeOf(TABC)*Length(FCharABCWidthCache), 0);
   FillChar(FCharWidthCache, SizeOf(Integer)*Length(FCharWidthCache), 0);
end;

function TSynEdit32TextDrawer.GetCachedABCWidth(c : Cardinal; var abc : TABC) : Boolean;
begin
   if c>High(FCharABCWidthCache) then
   begin
      Result:=GetCharABCWidthsW(FDC, c, c, abc);
      Exit;
   end;
   abc:=FCharABCWidthCache[c];
   if (abc.abcA or Integer(abc.abcB) or abc.abcC)=0 then
   begin
      Result:=GetCharABCWidthsW(FDC, c, c, abc);
      if Result then
         FCharABCWidthCache[c]:=abc;
   end else Result:=True;
end;

procedure TSynEdit32TextDrawer.TextOut(X, Y: Integer; Text: PWideChar;
  Length: Integer);
var
  r: TRect;
begin
  r := Rect(X, Y, X, Y);
  UniversalExtTextOut(FDC, X, Y, [], r, Text, Length, nil);
end;

procedure TSynEdit32TextDrawer.ExtTextOut(X, Y: Integer; Options: TTextOutOptions;
  ARect: TRect; Text: PWideChar; Length: Integer);

  procedure InitETODist(CharWidth: Integer);
  var
    Size: TSize;
    i: Integer;
  begin
    ReallocMem(FETODist, Length * SizeOf(Integer));
    for i := 0 to Length - 1 do
    begin
      Size := TextExtent(PWideChar(@Text[i]), 1);
      if Size.cx <> CharWidth then
         FETODist[i] := Ceil(Size.cx / CharWidth) * CharWidth
      else FETODist[i] := CharWidth;
    end;
  end;

  procedure AdjustLastCharWidthAndRect;
  var
    LastChar: Cardinal;
    RealCharWidth, CharWidth: Integer;
    CharInfo: TABC;
    tm: TTextMetricA;
  begin
    if Length <= 0 then Exit;
    
    LastChar := Ord(Text[Length - 1]);
    CharWidth := FETODist[Length - 1];
    RealCharWidth := CharWidth;
    if Win32PlatformIsUnicode then
    begin
      if GetCachedABCWidth(LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(FDC, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end
    else if WideChar(LastChar) <= High(AnsiChar) then
    begin
      if GetCharABCWidthsA(FDC, LastChar, LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(FDC, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end;
    if RealCharWidth > CharWidth then
      Inc(ARect.Right, RealCharWidth - CharWidth);
    FETODist[Length - 1] := Max(RealCharWidth, CharWidth);
  end;

begin
  InitETODist(GetCharWidth);
  AdjustLastCharWidthAndRect;
  UniversalExtTextOut(FDC, X, Y, Options, ARect, Text, Length, FETODist);
end;

procedure TSynEdit32TextDrawer.ReleaseTemporaryResources;
begin
  FFontStock.ReleaseFontHandles;
end;

function TSynEdit32TextDrawer.TextExtent(const Text: UnicodeString): TSize;
begin
  Result := SynEdit32.Unicode.TextExtent(FStockBitmap.Canvas, Text);
end;

function TSynEdit32TextDrawer.TextExtent(Text: PWideChar; Count: Integer): TSize;
begin
  Result := SynEdit32.Unicode.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count);
end;

function TSynEdit32TextDrawer.TextWidth(const Text: UnicodeString): Integer;
var
  c: Cardinal;
begin
  if Length(Text) = 1 then
  begin
    c := Ord(Text[1]);
    if c <= High(FCharWidthCache) then
    begin
      Result := FCharWidthCache[c];
      if Result = 0 then
      begin
        Result := SynEdit32.Unicode.TextExtent(FStockBitmap.Canvas, Text).cX;
        FCharWidthCache[c]:=Result;
      end;
      Exit;
    end;
  end;

  Result := SynEdit32.Unicode.TextExtent(FStockBitmap.Canvas, Text).cX;
end;

function TSynEdit32TextDrawer.TextWidth(Text: PWideChar; Count: Integer): Integer;
begin
  Result := SynEdit32.Unicode.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count).cX;
end;

initialization

finalization
  GFontsInfoManager.Free;

end.
