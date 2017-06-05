unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdActns, ActnList, StdCtrls, ExtCtrls,
  SynEdit32, SynEdit32.Highlighter, SynEdit32.Highlighter.DWS,
  SynEditHighlighter, SynHighlighterDWS;

type
  TFormMain = class(TForm)
    CheckBoxAutoSize: TCheckBox;
    CheckBoxCustomLineNumbers: TCheckBox;
    CheckBoxCustomPaint: TCheckBox;
    CheckBoxGradient: TCheckBox;
    CheckBoxLeadingZeroes: TCheckBox;
    CheckBoxShowLineNumbers: TCheckBox;
    CheckBoxShowModifications: TCheckBox;
    CheckBoxUseFontStyle: TCheckBox;
    CheckBoxVisible: TCheckBox;
    CheckBoxZeroStart: TCheckBox;
    ColorDialog: TColorDialog;
    LabelBorderColor: TLabel;
    LabelColor: TLabel;
    LabelGradientStart: TLabel;
    LabelGradientStop: TLabel;
    Panel: TPanel;
    ShapeColorBackground: TShape;
    ShapeColorBorder: TShape;
    ShapeGradientStartColor: TShape;
    ShapeGradientStopColor: TShape;
    SynEdit: TSynEdit32;
    SynDWSSyn: TSynEdit32HighlighterDWS;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxAutoSizeClick(Sender: TObject);
    procedure CheckBoxCustomLineNumbersClick(Sender: TObject);
    procedure CheckBoxCustomPaintClick(Sender: TObject);
    procedure CheckBoxGradientClick(Sender: TObject);
    procedure CheckBoxLeadingZeroesClick(Sender: TObject);
    procedure CheckBoxShowLineNumbersClick(Sender: TObject);
    procedure CheckBoxShowModificationsClick(Sender: TObject);
    procedure CheckBoxUseFontStyleClick(Sender: TObject);
    procedure CheckBoxVisibleClick(Sender: TObject);
    procedure CheckBoxZeroStartClick(Sender: TObject);
    procedure ShapeColorBackgroundMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeColorBorderMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeGradientStartColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeGradientStopColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SynEdit32GutterGetText(Sender: TObject; aLine: Integer;
      var aText: string);
    procedure SynEdit32GutterPaint(Sender: TObject; aLine, X, Y: Integer);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ShapeColorBackground.Brush.Color := SynEdit.Gutter.Color;
  ShapeColorBorder.Brush.Color := SynEdit.Gutter.BorderColor;
  ShapeGradientStartColor.Brush.Color := SynEdit.Gutter.GradientStartColor;
  ShapeGradientStopColor.Brush.Color := SynEdit.Gutter.GradientEndColor;
end;

procedure TFormMain.CheckBoxAutoSizeClick(Sender: TObject);
begin
  SynEdit.Gutter.AutoSize := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxCustomLineNumbersClick(Sender: TObject);
begin
  if CheckBoxCustomLineNumbers.Checked then
    SynEdit.OnGutterGetText := SynEdit32GutterGetText
  else
    SynEdit.OnGutterGetText := nil;
  SynEdit.InvalidateGutter;
end;

procedure TFormMain.CheckBoxCustomPaintClick(Sender: TObject);
begin
  if CheckBoxCustomPaint.Checked then
    SynEdit.OnGutterPaint := SynEdit32GutterPaint
  else
    SynEdit.OnGutterPaint := nil;
  SynEdit.InvalidateGutter;
end;

procedure TFormMain.CheckBoxGradientClick(Sender: TObject);
begin
  SynEdit.Gutter.Gradient := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxLeadingZeroesClick(Sender: TObject);
begin
  SynEdit.Gutter.LeadingZeros := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxShowLineNumbersClick(Sender: TObject);
begin
  SynEdit.Gutter.ShowLineNumbers := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxShowModificationsClick(Sender: TObject);
begin
  SynEdit.Gutter.ShowModification := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxUseFontStyleClick(Sender: TObject);
begin
  SynEdit.Gutter.UseFontStyle := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxVisibleClick(Sender: TObject);
begin
  SynEdit.Gutter.Visible := CheckBoxVisible.Checked;
end;

procedure TFormMain.CheckBoxZeroStartClick(Sender: TObject);
begin
  SynEdit.Gutter.ZeroStart := TCheckBox(Sender).Checked;
end;

procedure TFormMain.ShapeColorBackgroundMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeColorBackground.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeColorBackground.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.Color := ColorDialog.Color;
  end;
end;

procedure TFormMain.ShapeColorBorderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeColorBorder.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeColorBorder.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.BorderColor := ColorDialog.Color;
  end;
end;

procedure TFormMain.ShapeGradientStartColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeGradientStartColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeGradientStartColor.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.GradientStartColor := ColorDialog.Color;
  end;
end;

procedure TFormMain.ShapeGradientStopColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeGradientStopColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeGradientStopColor.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.GradientEndColor := ColorDialog.Color;
  end;
end;

procedure TFormMain.SynEdit32GutterGetText(Sender: TObject; aLine: Integer;
  var aText: string);
begin
  if aLine = TSynEdit32(Sender).CaretY then
    Exit;

  if aLine mod 10 <> 0 then
    if aLine mod 5 <> 0 then
      aText := '�'
    else
      aText := '-';
end;

procedure TFormMain.SynEdit32GutterPaint(Sender: TObject; aLine, X, Y: Integer);
var
  StrLineNumber: string;
  LineNumberRect: TRect;
  GutterWidth, Offset: Integer;
  OldFont: TFont;
begin
  with TSynEdit32(Sender), Canvas do
  begin
    Brush.Style := bsClear;
    GutterWidth := Gutter.Width - 5;
    if (ALine = 1) or (ALine = CaretY) or ((ALine mod 10) = 0) then
    begin
      StrLineNumber := IntToStr(ALine);
      LineNumberRect := Rect(x, y, GutterWidth, y + LineHeight);
      OldFont := TFont.Create;
      try
        OldFont.Assign(Canvas.Font);
        Canvas.Font := Gutter.Font;
        Canvas.TextRect(LineNumberRect, StrLineNumber, [tfVerticalCenter,
          tfSingleLine, tfRight]);
        Canvas.Font := OldFont;
      finally
        OldFont.Free;
      end;
    end
    else
    begin
      Canvas.Pen.Color := Gutter.Font.Color;
      if (ALine mod 5) = 0 then
        Offset := 5
      else
        Offset := 2;
      Inc(y, LineHeight div 2);
      Canvas.MoveTo(GutterWidth - Offset, y);
      Canvas.LineTo(GutterWidth, y);
    end;
  end;
end;

end.
