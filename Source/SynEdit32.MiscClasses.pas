{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscClasses.pas, released 2000-04-07.
The Original Code is based on the mwSupportClasses.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Unicode translation by Ma�l H�rz.
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

$Id: SynEditMiscClasses.pas,v 1.35.2.9 2008/09/17 13:59:12 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.MiscClasses;

{$I SynEdit32.inc}

interface

uses
  Consts, Windows, Messages, Graphics, Controls, Forms, StdCtrls, Menus,
  Registry, Math, Classes, SysUtils, GR32,
  SynEdit32.Types, SynEdit32.KeyConst, SynEdit32.Unicode;

type
  TSynEdit32SelectedColor = class(TPersistent)
  private
    FBG: TColor;
    FFG: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read FBG write SetBG default clHighLight;
    property Foreground: TColor read FFG write SetFG default clHighLightText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSynEdit32GutterBorderStyle = (gbsNone, gbsMiddle, gbsRight);

  TSynEdit32Gutter = class(TPersistent)
  private
    FFont: TFont;
    FColor: TColor;
    FBorderColor: TColor;
    FWidth: Integer;
    FShowLineNumbers: Boolean;
    FShowModification: Boolean;
    FDigitCount: Integer;
    FLeadingZeros: Boolean;
    FZeroStart: Boolean;
    FLeftOffset: Integer;
    FRightOffset: Integer;
    FOnChange: TNotifyEvent;
    FCursor: TCursor;
    FVisible: Boolean;
    FUseFontStyle: Boolean;
    FAutoSize: Boolean;
    FAutoSizeDigitCount: Integer;
    FBorderStyle: TSynEdit32GutterBorderStyle;
    FLineNumberStart: Integer;
    FGradient: Boolean;
    FGradientStartColor: TColor;
    FGradientEndColor: TColor;
    FGradientSteps: Integer;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetDigitCount(Value: Integer);
    procedure SetLeadingZeros(const Value: Boolean);
    procedure SetLeftOffset(Value: Integer);
    procedure SetRightOffset(Value: Integer);
    procedure SetShowLineNumbers(const Value: Boolean);
    procedure SetShowModification(const Value: Boolean);
    procedure SetUseFontStyle(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure SetZeroStart(const Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure OnFontChange(Sender: TObject);
    procedure SetBorderStyle(const Value: TSynEdit32GutterBorderStyle);
    procedure SetLineNumberStart(const Value: Integer);
    procedure SetGradient(const Value: Boolean);
    procedure SetGradientStartColor(const Value: TColor);
    procedure SetGradientEndColor(const Value: TColor);
    procedure SetGradientSteps(const Value: Integer);
    function GetWidth: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AutoSizeDigitCount(LinesCount: Integer);
    function FormatLineNumber(Line: Integer): string;
    function RealGutterWidth(CharWidth: Integer): Integer;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default FALSE;
    property BorderStyle: TSynEdit32GutterBorderStyle read FBorderStyle
      write SetBorderStyle default gbsMiddle;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clWindow;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property DigitCount: Integer read FDigitCount write SetDigitCount
      default 4;
    property Font: TFont read FFont write SetFont;
    property LeadingZeros: Boolean read FLeadingZeros write SetLeadingZeros
      default FALSE;
    property LeftOffset: Integer read FLeftOffset write SetLeftOffset
      default 16;
    property RightOffset: Integer read FRightOffset write SetRightOffset
      default 2;
    property ShowLineNumbers: Boolean read FShowLineNumbers
      write SetShowLineNumbers default FALSE;
    property ShowModification: Boolean read FShowModification
      write SetShowModification default FALSE;
    property UseFontStyle: Boolean read FUseFontStyle write SetUseFontStyle
      default True;
    property Visible: Boolean read FVisible write SetVisible default TRUE;
    property Width: Integer read GetWidth write SetWidth default 30;
    property ZeroStart: Boolean read FZeroStart write SetZeroStart
      default False;
    property LineNumberStart: Integer read FLineNumberStart write SetLineNumberStart default 1;
    property Gradient: Boolean read FGradient write SetGradient default False;
    property GradientStartColor: TColor read FGradientStartColor write SetGradientStartColor default clWindow;
    property GradientEndColor: TColor read FGradientEndColor write SetGradientEndColor default clBtnFace;
    property GradientSteps: Integer read FGradientSteps write SetGradientSteps default 48;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSynEdit32BookMarkOpt = class(TPersistent)
  private
    FBookmarkImages: TImageList;
    FDrawBookmarksFirst: Boolean;
    FEnableKeys: Boolean;
    FGlyphsVisible: Boolean;
    FLeftMargin: Integer;
    FOwner: TComponent;
    FXoffset: Integer;
    FOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TImageList);
    procedure SetDrawBookmarksFirst(Value: Boolean);
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: Integer);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property BookmarkImages: TImageList
      read FBookmarkImages write SetBookmarkImages;
    property DrawBookmarksFirst: Boolean read FDrawBookmarksFirst
      write SetDrawBookmarksFirst default True;
    property EnableKeys: Boolean
      read FEnableKeys write FEnableKeys default True;
    property GlyphsVisible: Boolean
      read FGlyphsVisible write SetGlyphsVisible default True;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 2;
    property Xoffset: Integer read FXoffset write SetXOffset default 12;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSynGlyph = class(TPersistent)
  private
    FVisible: Boolean;
    FInternalGlyph, FGlyph: TBitmap;
    FInternalMaskColor, FMaskColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetGlyph(Value: TBitmap);
    procedure GlyphChange(Sender: TObject);
    procedure SetMaskColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
    function GetWidth : Integer;
    function GetHeight : Integer;
  public
    constructor Create(aModule: THandle; const aName: string; aMaskColor: TColor);
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
    procedure Draw(aBitmap: TBitmap32; aX, aY, aLineHeight: Integer);
    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property MaskColor: TColor read FMaskColor write SetMaskColor default clNone;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TSynEdit32MethodChain }

  ESynEdit32MethodChain = class(Exception);
  TSynEdit32ExceptionEvent = procedure (Sender: TObject; E: Exception;
    var DoContinue: Boolean) of object;

  TSynEdit32MethodChain = class(TObject)
  private
    FNotifyProcs: TList;
    FExceptionHandler: TSynEdit32ExceptionEvent;
  protected
    procedure DoFire(const AEvent: TMethod); virtual; abstract;
    function DoHandleException(E: Exception): Boolean; virtual;
    property ExceptionHandler: TSynEdit32ExceptionEvent read FExceptionHandler
      write FExceptionHandler;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AEvent: TMethod);
    procedure Remove(AEvent: TMethod);
    procedure Fire;
  end;

  { TSynEdit32NotifyEventChain }

  TSynEdit32NotifyEventChain = class(TSynEdit32MethodChain)
  private
    FSender: TObject;
  protected
    procedure DoFire(const AEvent: TMethod); override;
  public
    constructor CreateEx(ASender: TObject);
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    property ExceptionHandler;
    property Sender: TObject read FSender write FSender;
  end;

  { TSynEdit32InternalImage }
  
  TSynEdit32InternalImage = class(TObject)
  private
    FImages : TBitmap;
    FWidth  : Integer;
    FHeight : Integer;
    FCount  : Integer;

    function CreateBitmapFromInternalList(aModule: THandle; const Name: string): TBitmap;
    procedure FreeBitmapFromInternalList;
  public
    constructor Create(aModule: THandle; const Name: string; Count: Integer);
    destructor Destroy; override;
    procedure Draw(Bitmap32: TBitmap32; Number, X, Y, LineHeight: Integer);
    procedure DrawTransparent(ACanvas: TCanvas; Number, X, Y,
      LineHeight: Integer; TransparentColor: TColor);
  end;

{ TSynEdit32HotKey }

const
  BorderWidth = 0;

type
  TSynBorderStyle = TBorderStyle;

  THKModifier = (hkShift, hkCtrl, hkAlt);
  THKModifiers = set of THKModifier;
  THKInvalidKey = (hcNone, hcShift, hcCtrl, hcAlt, hcShiftCtrl,
    hcShiftAlt, hcCtrlAlt, hcShiftCtrlAlt);
  THKInvalidKeys = set of THKInvalidKey;

  TSynEdit32HotKey = class(TCustomControl)
  private
    FBorderStyle: TSynBorderStyle;
    FHotKey: TShortCut;
    FInvalidKeys: THKInvalidKeys;
    FModifiers: THKModifiers;
    FPressedOnlyModifiers: Boolean;
    procedure SetBorderStyle(const Value: TSynBorderStyle);
    procedure SetHotKey(const Value: TShortCut);
    procedure SetInvalidKeys(const Value: THKInvalidKeys);
    procedure SetModifiers(const Value: THKModifiers);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property HotKey: TShortCut read FHotKey write SetHotKey default $0041; { Alt+A }
    property InvalidKeys: THKInvalidKeys read FInvalidKeys write SetInvalidKeys default [hcNone, hcShift];
    property Modifiers: THKModifiers read FModifiers write SetModifiers default [hkAlt];
  end;

  TSynEdit32SearchCustom = class(TComponent)
  protected
    function GetPattern: UnicodeString; virtual; abstract;
    procedure SetPattern(const Value: UnicodeString); virtual; abstract;
    function GetLength(Index: Integer): Integer; virtual; abstract;
    function GetResult(Index: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure SetOptions(const Value: TSynEdit32SearchOptions); virtual; abstract;
  public
    function FindAll(const NewText: UnicodeString): Integer; virtual; abstract;
    function Replace(const aOccurrence, aReplacement: UnicodeString): UnicodeString; virtual; abstract;
    property Pattern: UnicodeString read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[Index: Integer]: Integer read GetResult;
    property Lengths[Index: Integer]: Integer read GetLength;
    property Options: TSynEdit32SearchOptions write SetOptions;
  end;

implementation

uses
  SynEdit32.MiscProcs;

{ TSynEdit32SelectedColor }

constructor TSynEdit32SelectedColor.Create;
begin
  inherited Create;
  FBG := clHighLight;
  FFG := clHighLightText;
end;

procedure TSynEdit32SelectedColor.Assign(Source: TPersistent);
var
  Src: TSynEdit32SelectedColor;
begin
  if (Source <> nil) and (Source is TSynEdit32SelectedColor) then
  begin
    Src := TSynEdit32SelectedColor(Source);
    FBG := Src.FBG;
    FFG := Src.FFG;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited Assign(Source);
end;

procedure TSynEdit32SelectedColor.SetBG(Value: TColor);
begin
  if (FBG <> Value) then
  begin
    FBG := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32SelectedColor.SetFG(Value: TColor);
begin
  if (FFG <> Value) then
  begin
    FFG := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{ TSynEdit32Gutter }

constructor TSynEdit32Gutter.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FFont.Style := [];
  FUseFontStyle := True;
  FFont.OnChange := OnFontChange;

  FColor := clBtnFace;
  FVisible := TRUE;
  FWidth := 30;
  FLeftOffset := 16;
  FDigitCount := 4;
  FAutoSizeDigitCount := FDigitCount;
  FRightOffset := 2;
  FBorderColor := clWindow;
  FBorderStyle := gbsMiddle;
  FLineNumberStart := 1;
  FZeroStart := False;
  FGradient := False;
  FGradientStartColor := clWindow;
  FGradientEndColor := clBtnFace;
  FGradientSteps := 48;
end;

destructor TSynEdit32Gutter.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TSynEdit32Gutter.Assign(Source: TPersistent);
var
  Src: TSynEdit32Gutter;
begin
  if Assigned(Source) and (Source is TSynEdit32Gutter) then
  begin
    Src := TSynEdit32Gutter(Source);
    FFont.Assign(src.Font);
    FUseFontStyle := src.FUseFontStyle;
    FColor := Src.FColor;
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FShowLineNumbers := Src.FShowLineNumbers;
    FLeadingZeros := Src.FLeadingZeros;
    FZeroStart := Src.FZeroStart;
    FLeftOffset := Src.FLeftOffset;
    FDigitCount := Src.FDigitCount;
    FRightOffset := Src.FRightOffset;
    FAutoSize := Src.FAutoSize;
    FAutoSizeDigitCount := Src.FAutoSizeDigitCount;
    FLineNumberStart := Src.FLineNumberStart;
    FBorderColor := Src.FBorderColor;
    FBorderStyle := Src.FBorderStyle;
    FGradient := Src.FGradient;
    FGradientStartColor := Src.FGradientStartColor;
    FGradientEndColor := Src.FGradientEndColor;
    FGradientSteps := Src.FGradientSteps;
    if Assigned(FOnChange) then FOnChange(Self);
  end 
  else
    inherited;
end;

procedure TSynEdit32Gutter.AutoSizeDigitCount(LinesCount: Integer);
var
  nDigits: Integer;
begin
  if FVisible and FAutoSize and FShowLineNumbers then
  begin
    if FZeroStart then
      Dec(LinesCount)
    else if FLineNumberStart > 1 then
      Inc(LinesCount, FLineNumberStart - 1);

    nDigits := Max(Length(IntToStr(LinesCount)), FDigitCount);
    if FAutoSizeDigitCount <> nDigits then
    begin
      FAutoSizeDigitCount := nDigits;
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end else
    FAutoSizeDigitCount := FDigitCount;
end;

function TSynEdit32Gutter.FormatLineNumber(Line: Integer): string;
var
  i: Integer;
begin
  if FZeroStart then
    Dec(Line)
  else if FLineNumberStart > 1 then
    Inc(Line, FLineNumberStart - 1);
  Result := Format('%*d', [FAutoSizeDigitCount, Line]);
  if FLeadingZeros then
    for i := 1 to FAutoSizeDigitCount - 1 do
    begin
      if (Result[i] <> ' ') then break;
      Result[i] := '0';
    end;
end;

function TSynEdit32Gutter.RealGutterWidth(CharWidth: Integer): Integer;
begin
  if not FVisible then
    Result := 0
  else
  begin
    if FShowLineNumbers then
      Result := FLeftOffset + FRightOffset + FAutoSizeDigitCount * CharWidth + 2
    else
      Result := FWidth;

    // take modification indicator into account
    if FShowModification then
      Result := Result + 4;
  end;
end;

procedure TSynEdit32Gutter.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSynEdit32Gutter.OnFontChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSynEdit32Gutter.SetDigitCount(Value: Integer);
begin
  Value := MinMax(Value, 2, 12);
  if FDigitCount <> Value then
  begin
    FDigitCount := Value;
    FAutoSizeDigitCount := FDigitCount;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetLeadingZeros(const Value: Boolean);
begin
  if FLeadingZeros <> Value then
  begin
    FLeadingZeros := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetLeftOffset(Value: Integer);
begin
  Value := Max(0, Value);
  if FLeftOffset <> Value then
  begin
    FLeftOffset := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetRightOffset(Value: Integer);
begin
  Value := Max(0, Value);
  if FRightOffset <> Value then
  begin
    FRightOffset := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetShowLineNumbers(const Value: Boolean);
begin
  if FShowLineNumbers <> Value then
  begin
    FShowLineNumbers := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetShowModification(const Value: Boolean);
begin
  if FShowModification <> Value then
  begin
    FShowModification := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetUseFontStyle(Value: Boolean);
begin
  if FUseFontStyle <> Value then
  begin
    FUseFontStyle := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetWidth(Value: Integer);
begin
  Value := Max(0, Value);
  if FWidth <> Value then
  begin
    FWidth := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetZeroStart(const Value: Boolean);
begin
  if FZeroStart <> Value then
  begin
    FZeroStart := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetBorderStyle(const Value: TSynEdit32GutterBorderStyle);
begin
  FBorderStyle := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSynEdit32Gutter.SetLineNumberStart(const Value: Integer);
begin
  if Value <> FLineNumberStart then
  begin
    FLineNumberStart := Value;
    if FLineNumberStart < 0 then
      FLineNumberStart := 0;
    if FLineNumberStart = 0 then
      FZeroStart := True
    else
      FZeroStart := False;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetGradient(const Value: Boolean);
begin
  if Value <> FGradient then
  begin
    FGradient := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetGradientEndColor(const Value: TColor);
begin
  if Value <> FGradientEndColor then
  begin
    FGradientEndColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetGradientStartColor(const Value: TColor);
begin
  if Value <> FGradientStartColor then
  begin
    FGradientStartColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32Gutter.SetGradientSteps(const Value: Integer);
begin
  if Value <> FGradientSteps then
  begin
    FGradientSteps := Value;
    if FGradientSteps < 2 then
      FGradientSteps := 2;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

function TSynEdit32Gutter.GetWidth: Integer;
begin
  if not Visible then
    Result := 0
  else
    Result := FWidth;
end;

{ TSynEdit32BookMarkOpt }

constructor TSynEdit32BookMarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  FDrawBookmarksFirst := TRUE;
  FEnableKeys := True;
  FGlyphsVisible := True;
  FLeftMargin := 2;
  FOwner := AOwner;
  FXoffset := 12;
end;

procedure TSynEdit32BookMarkOpt.Assign(Source: TPersistent);
var
  Src: TSynEdit32BookMarkOpt;
begin
  if (Source <> nil) and (Source is TSynEdit32BookMarkOpt) then
  begin
    Src := TSynEdit32BookMarkOpt(Source);
    FBookmarkImages := Src.FBookmarkImages;
    FDrawBookmarksFirst := Src.FDrawBookmarksFirst;
    FEnableKeys := Src.FEnableKeys;
    FGlyphsVisible := Src.FGlyphsVisible;
    FLeftMargin := Src.FLeftMargin;
    FXoffset := Src.FXoffset;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited Assign(Source);
end;

procedure TSynEdit32BookMarkOpt.SetBookmarkImages(const Value: TImageList);
begin
  if FBookmarkImages <> Value then
  begin
    FBookmarkImages := Value;
    if Assigned(FBookmarkImages) then FBookmarkImages.FreeNotification(FOwner);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32BookMarkOpt.SetDrawBookmarksFirst(Value: Boolean);
begin
  if Value <> FDrawBookmarksFirst then
  begin
    FDrawBookmarksFirst := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32BookMarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if FGlyphsVisible <> Value then
  begin
    FGlyphsVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32BookMarkOpt.SetLeftMargin(Value: Integer);
begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynEdit32BookMarkOpt.SetXOffset(Value: Integer);
begin
  if FXoffset <> Value then
  begin
    FXoffset := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{ TSynGlyph }

constructor TSynGlyph.Create(aModule: THandle; const aName: string; aMaskColor: TColor);
begin
  inherited Create;

  if aName <> '' then
  begin
    FInternalGlyph := TBitmap.Create;
    FInternalGlyph.LoadFromResourceName(aModule, aName);
    FInternalMaskColor := aMaskColor;
  end
  else
    FInternalMaskColor := clNone;

  FVisible := True;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChange;
  FMaskColor := clNone;
end;

destructor TSynGlyph.Destroy;
begin
  if Assigned(FInternalGlyph) then
    FreeAndNil(FInternalGlyph);

  FGlyph.Free;

  inherited Destroy;
end;

procedure TSynGlyph.Assign(aSource: TPersistent);
var
  vSrc : TSynGlyph;
begin
  if Assigned(aSource) and (aSource is TSynGlyph) then
  begin
    vSrc := TSynGlyph(aSource);
    FInternalGlyph := vSrc.FInternalGlyph;
    FInternalMaskColor := vSrc.FInternalMaskColor;
    FVisible := vSrc.FVisible;
    FGlyph := vSrc.FGlyph;
    FMaskColor := vSrc.FMaskColor;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

procedure TSynGlyph.Draw(aBitmap: TBitmap32; aX, aY, aLineHeight: Integer);
var
  rcSrc, rcDest : TRect;
  vGlyph : TBitmap;
  vMaskColor : TColor;
begin
  if not FGlyph.Empty then
  begin
    vGlyph := FGlyph;
    vMaskColor := FMaskColor;
  end
  else if Assigned(FInternalGlyph) then
  begin
    vGlyph := FInternalGlyph;
    vMaskColor := FInternalMaskColor;
  end
  else
    Exit;

  if aLineHeight >= vGlyph.Height then
  begin
    rcSrc := Rect(0, 0, vGlyph.Width, vGlyph.Height);
    Inc(aY, (aLineHeight - vGlyph.Height) div 2);
    rcDest := Rect(aX, aY, aX + vGlyph.Width, aY + vGlyph.Height);
  end
  else
  begin
    rcDest := Rect(aX, aY, aX + vGlyph.Width, aY + aLineHeight);
    aY := (vGlyph.Height - aLineHeight) div 2;
    rcSrc := Rect(0, aY, vGlyph.Width, aY + aLineHeight);
  end;

  aBitmap.Canvas.BrushCopy(rcDest, vGlyph, rcSrc, vMaskColor);
end;

procedure TSynGlyph.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TSynGlyph.GlyphChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSynGlyph.SetMaskColor(Value: TColor);
begin
  if FMaskColor <> Value then
  begin
    FMaskColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSynGlyph.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

function TSynGlyph.GetWidth : Integer;
begin
  if not FGlyph.Empty then
    Result := FGlyph.Width
  else
  if Assigned(FInternalGlyph) then
    Result := FInternalGlyph.Width
  else
    Result := 0;
end;

function TSynGlyph.GetHeight : Integer;
begin
  if not FGlyph.Empty then
    Result := FGlyph.Height
  else
  if Assigned(FInternalGlyph) then
    Result := FInternalGlyph.Height
  else
    Result := 0;
end;

{ TSynEdit32MethodChain }

procedure TSynEdit32MethodChain.Add(AEvent: TMethod);
begin
  if not Assigned(@AEvent) then
    raise ESynEdit32MethodChain.CreateFmt(
      '%s.Entry : the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    Add(Code);
    Add(Data);
  end
end;

constructor TSynEdit32MethodChain.Create;
begin
  inherited;
  FNotifyProcs := TList.Create;
end;

destructor TSynEdit32MethodChain.Destroy;
begin
  FNotifyProcs.Free;
  inherited;
end;

function TSynEdit32MethodChain.DoHandleException(E: Exception): Boolean;
begin
  if not Assigned(FExceptionHandler) then
    raise E
  else
    try
      Result := True;
      FExceptionHandler(Self, E, Result);
    except
      raise ESynEdit32MethodChain.CreateFmt(
        '%s.DoHandleException : MUST NOT occur any kind of exception in '+
        'ExceptionHandler', [ClassName]);
    end;
end;

procedure TSynEdit32MethodChain.Fire;
var
  AMethod: TMethod;
  i: Integer;
begin
  i := 0;
  with FNotifyProcs, AMethod do
    while i < Count do
      try
        repeat
          Code := Items[i];
          Inc(i);
          Data := Items[i];
          Inc(i);

          DoFire(AMethod)
        until i >= Count;
      except
        on E: Exception do
          if not DoHandleException(E) then
            i := MaxInt;
      end;
end;

procedure TSynEdit32MethodChain.Remove(AEvent: TMethod);
var
  i: Integer;
begin
  if not Assigned(@AEvent) then
    raise ESynEdit32MethodChain.CreateFmt(
      '%s.Remove: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    i := Count - 1;
    while i > 0 do
      if Items[i] <> Data then
        Dec(i, 2)
      else
      begin
        Dec(i);
        if Items[i] = Code then
        begin
          Delete(i);
          Delete(i);
        end;
        Dec(i);
      end;
  end;
end;

{ TSynEdit32NotifyEventChain }

procedure TSynEdit32NotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  inherited Add(TMethod(AEvent));
end;

constructor TSynEdit32NotifyEventChain.CreateEx(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
end;

procedure TSynEdit32NotifyEventChain.DoFire(const AEvent: TMethod);
begin
  TNotifyEvent(AEvent)(FSender);
end;

procedure TSynEdit32NotifyEventChain.Remove(AEvent: TNotifyEvent);
begin
  inherited Remove(TMethod(AEvent));
end;


{ TSynEdit32InternalImage }

type
  TInternalResource = class (TObject)
  public
    UsageCount: Integer;
    Name: string;
    Bitmap: TBitmap;
  end;

var
  InternalResources: TList;

constructor TSynEdit32InternalImage.Create(aModule: THandle; const Name: string; Count: Integer);
begin
  inherited Create;
  FImages := CreateBitmapFromInternalList( aModule, Name );
  FWidth := (FImages.Width + Count shr 1) div Count;
  FHeight := FImages.Height;
  FCount := Count;
  end;

destructor TSynEdit32InternalImage.Destroy;
begin
  FreeBitmapFromInternalList;
  inherited Destroy;
end;

function TSynEdit32InternalImage.CreateBitmapFromInternalList(aModule: THandle;
  const Name: string): TBitmap;
var
  idx: Integer;
  newIntRes: TInternalResource;
begin
  { There is no list until now }
  if (InternalResources = nil) then
    InternalResources := TList.Create;

  { Search the list for the needed resource }
  for idx := 0 to InternalResources.Count - 1 do
    if (TInternalResource(InternalResources[idx]).Name = UpperCase(Name)) then
      with TInternalResource(InternalResources[idx]) do
      begin
        UsageCount := UsageCount + 1;
        Result := Bitmap;
        exit;
      end;

  { There is no loaded resource in the list so let's create a new one }
  Result := TBitmap.Create;
  Result.LoadFromResourceName(aModule, Name);

  { Add the new resource to our list }
  newIntRes:= TInternalResource.Create;
  newIntRes.UsageCount := 1;
  newIntRes.Name := UpperCase(Name);
  newIntRes.Bitmap := Result;
  InternalResources.Add(newIntRes);
end;

procedure TSynEdit32InternalImage.FreeBitmapFromInternalList;
var
  idx: Integer;
  intRes: TInternalResource;
  function FindImageInList: Integer;
  begin
    for Result := 0 to InternalResources.Count - 1 do
      if (TInternalResource (InternalResources[Result]).Bitmap = FImages) then
        exit;
    Result := -1;
  end;
begin
  { Search the index of our resource in the list }
  idx := FindImageInList;

  { Ey, what's this ???? }
  if (idx = -1) then
    exit;

  { Decrement the usagecount in the object. If there are no more users
    remove the object from the list and free it }
  intRes := TInternalResource (InternalResources[idx]);
  with intRes do
  begin
    UsageCount := UsageCount - 1;
    if (UsageCount = 0) then
    begin
      Bitmap.Free;
      InternalResources.Delete (idx);
      intRes.Free;
    end;
  end;

  { If there are no more entries in the list free it }
  if (InternalResources.Count = 0) then
  begin
    InternalResources.Free;
    InternalResources := nil;
  end;
end;

procedure TSynEdit32InternalImage.Draw(Bitmap32: TBitmap32;
  Number, X, Y, LineHeight: Integer);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < FCount) then
  begin
    if LineHeight >= FHeight then
    begin
      rcSrc := Rect(Number * FWidth, 0, (Number + 1) * FWidth, FHeight);
      Inc(Y, (LineHeight - FHeight) div 2);
      rcDest := Rect(X, Y, X + FWidth, Y + FHeight);
    end else
    begin
      rcDest := Rect(X, Y, X + FWidth, Y + LineHeight);
      Y := (FHeight - LineHeight) div 2;
      rcSrc := Rect(Number * FWidth, Y, (Number + 1) * FWidth,
        Y + LineHeight);
    end;
    Bitmap32.Canvas.CopyRect(rcDest, FImages.Canvas, rcSrc);
  end;
end;

procedure TSynEdit32InternalImage.DrawTransparent(ACanvas: TCanvas; Number, X, Y,
  LineHeight: Integer; TransparentColor: TColor);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < FCount) then
  begin
    if LineHeight >= FHeight then
    begin
      rcSrc := Rect(Number * FWidth, 0, (Number + 1) * FWidth, FHeight);
      Inc(Y, (LineHeight - FHeight) div 2);
      rcDest := Rect(X, Y, X + FWidth, Y + FHeight);
    end else
    begin
      rcDest := Rect(X, Y, X + FWidth, Y + LineHeight);
      Y := (FHeight - LineHeight) div 2;
      rcSrc := Rect(Number * FWidth, Y, (Number + 1) * FWidth,
        Y + LineHeight);
    end;
    ACanvas.BrushCopy(rcDest, FImages, rcSrc, TransparentColor);
  end;
end;

{ TSynEdit32HotKey }

function KeySameAsShiftState(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (Key = SYNEDIT_SHIFT) and (ssShift in Shift) or
            (Key = SYNEDIT_CONTROL) and (ssCtrl in Shift) or
            (Key = SYNEDIT_MENU) and (ssAlt in Shift);
end;

function ModifiersToShiftState(Modifiers: THKModifiers): TShiftState;
begin
  Result := [];
  if hkShift in Modifiers then Include(Result, ssShift);
  if hkCtrl in Modifiers then Include(Result, ssCtrl);
  if hkAlt in Modifiers then Include(Result, ssAlt);
end;

function ShiftStateToTHKInvalidKey(Shift: TShiftState): THKInvalidKey;
begin
  Shift := Shift * [ssShift, ssAlt, ssCtrl];
  if Shift = [ssShift] then
    Result := hcShift
  else if Shift = [ssCtrl] then
    Result := hcCtrl
  else if Shift = [ssAlt] then
    Result := hcAlt
  else if Shift = [ssShift, ssCtrl] then
    Result := hcShiftCtrl
  else if Shift = [ssShift, ssAlt] then
    Result := hcShiftAlt
  else if Shift = [ssCtrl, ssAlt] then
    Result := hcCtrlAlt
  else if Shift = [ssShift, ssCtrl, ssAlt] then
    Result := hcShiftCtrlAlt
  else
    Result := hcNone;
end;

function ShortCutToTextEx(Key: Word; Shift: TShiftState): UnicodeString;
begin
  if ssCtrl in Shift then Result := SmkcCtrl;
  if ssShift in Shift then Result := Result + SmkcShift;
  if ssAlt in Shift then Result := Result + SmkcAlt;

  Result := Result + ShortCutToText(TShortCut(Key));
  if Result = '' then
    Result := srNone;
end;

constructor TSynEdit32HotKey.Create(AOwner: TComponent);
begin
  inherited;

  BorderStyle := bsSingle;
  ControlStyle := ControlStyle + [csNeedsBorderPaint];

  FInvalidKeys := [hcNone, hcShift];
  FModifiers := [hkAlt];
  SetHotKey($0041); { Alt+A }

  ParentColor := False;
  Color := clWindow;
  TabStop := True;
end;

procedure TSynEdit32HotKey.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TSynBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TSynEdit32HotKey.DoExit;
begin
  inherited;
  if FPressedOnlyModifiers then
  begin
    Text := srNone;
    Invalidate;
  end;
end;

procedure TSynEdit32HotKey.KeyDown(var Key: Word; Shift: TShiftState);
var
  MaybeInvalidKey: THKInvalidKey;
  SavedKey: Word;
begin
  SavedKey := Key;
  FPressedOnlyModifiers := KeySameAsShiftState(Key, Shift);

  MaybeInvalidKey := ShiftStateToTHKInvalidKey(Shift);
  if MaybeInvalidKey in FInvalidKeys then
    Shift := ModifiersToShiftState(FModifiers);

  if not FPressedOnlyModifiers then
    FHotKey := ShortCut(Key, Shift)
  else
  begin
    FHotKey := 0;
    Key := 0;
  end;

  if Text <> ShortCutToTextEx(Key, Shift) then
  begin
    Text := ShortCutToTextEx(Key, Shift);
    Invalidate;
    SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
  end;

  Key := SavedKey;
end;

procedure TSynEdit32HotKey.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FPressedOnlyModifiers then
  begin
    Text := srNone;
    Invalidate;
    SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
  end;
end;

procedure TSynEdit32HotKey.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  SetFocus;
end;

procedure TSynEdit32HotKey.Paint;
var
  r: TRect;
begin
  r := ClientRect;
  
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  InflateRect(r, -BorderWidth, -BorderWidth);
  Canvas.FillRect(r);
  TextRect(Canvas, r, BorderWidth + 1, BorderWidth + 1, Text);
end;

procedure TSynEdit32HotKey.SetBorderStyle(const Value: TSynBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TSynEdit32HotKey.SetHotKey(const Value: TShortCut);
var
  Key: Word;
  Shift: TShiftState;
  MaybeInvalidKey: THKInvalidKey;
begin
  ShortCutToKey(Value, Key, Shift);

  MaybeInvalidKey := ShiftStateToTHKInvalidKey(Shift);
  if MaybeInvalidKey in FInvalidKeys then
    Shift := ModifiersToShiftState(FModifiers);

  FHotKey := ShortCut(Key, Shift);
  Text := ShortCutToTextEx(Key, Shift);
  Invalidate;
  if not Visible then
    SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
end;

procedure TSynEdit32HotKey.SetInvalidKeys(const Value: THKInvalidKeys);
begin
  FInvalidKeys := Value;
  SetHotKey(FHotKey);
end;

procedure TSynEdit32HotKey.SetModifiers(const Value: THKModifiers);
begin
  FModifiers := Value;
  SetHotKey(FHotKey);
end;

procedure TSynEdit32HotKey.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

procedure TSynEdit32HotKey.WMKillFocus(var Msg: TWMKillFocus);
begin
  DestroyCaret;
end;

procedure TSynEdit32HotKey.WMSetFocus(var Msg: TWMSetFocus);
begin
  Canvas.Font := Font;
  CreateCaret(Handle, 0, 1, -Canvas.Font.Height + 2);
  SetCaretPos(BorderWidth + 1 + TextWidth(Canvas, Text), BorderWidth + 1);
  ShowCaret(Handle);
end;

begin
  InternalResources := nil;
end.
