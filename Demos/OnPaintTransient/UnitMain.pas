unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SynEdit32, SynEdit32.Types, SynEdit32.Highlighter,
  SynEdit32.Highlighter.Java;

type
  TFormMain = class(TForm)
    Panel: TPanel;
    ButtonOpen: TButton;
    OpenDialog: TOpenDialog;
    Button1: TButton;
    Editor: TSynEdit32;
    SynJavaSyn: TSynEdit32HighlighterJava;
    procedure ButtonOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TSynEdit32TransientType);
  private
    FBracketFG: TColor;
    FBracketBG: TColor;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.ButtonOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    Editor.Lines.LoadFromFile(OpenDialog.Filename);
end;

procedure TFormMain.EditorPaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TSynEdit32TransientType);

var
  Editor: TSynEdit32;
  OpenChars: array of WideChar;//[0..2] of WideChar=();
  CloseChars: array of WideChar;//[0..2] of WideChar=();

  function IsCharBracket(AChar: WideChar): Boolean;
  begin
    case AChar of
      '{','[','(','<','}',']',')','>':
        Result := True;
      else
        Result := False;
    end;
  end;

  function CharToPixels(P: TSynEdit32BufferCoord): TPoint;
  begin
    Result := Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;

var
  P: TSynEdit32BufferCoord;
  Pix: TPoint;
  D: TSynEdit32DisplayCoord;
  S: UnicodeString;
  I: Integer;
  Attri: TSynEdit32HighlighterAttributes;
  ArrayLength: Integer;
  Start: Integer;
  TmpCharA, TmpCharB: WideChar;
begin
  if TSynEdit32(Sender).SelAvail then
    Exit;
  Editor := TSynEdit32(Sender);
  ArrayLength := 3;
//if you had a highlighter that used a markup language, like html or xml,
//then you would want to highlight the greater and less than signs
//as illustrated below

//  if (Editor.Highlighter = shHTML) or (Editor.Highlighter = shXML) then
//    inc(ArrayLength);

  SetLength(OpenChars, ArrayLength);
  SetLength(CloseChars, ArrayLength);
  for i := 0 to ArrayLength - 1 do
    case i of
      0: begin OpenChars[i] := '('; CloseChars[i] := ')'; end;
      1: begin OpenChars[i] := '{'; CloseChars[i] := '}'; end;
      2: begin OpenChars[i] := '['; CloseChars[i] := ']'; end;
      3: begin OpenChars[i] := '<'; CloseChars[i] := '>'; end;
    end;

  P := Editor.CaretXY;
  D := Editor.DisplayXY;

  Start := Editor.SelStart;

  if (Start > 0) and (Start <= Length(Editor.Text)) then
    TmpCharA := Editor.Text[Start]
  else
    TmpCharA := #0;

  if (Start < Length(Editor.Text)) then
    TmpCharB := Editor.Text[Start + 1]
  else
    TmpCharB := #0;

  if not IsCharBracket(TmpCharA) and not IsCharBracket(TmpCharB) then exit;
  S := TmpCharB;
  if not IsCharBracket(TmpCharB) then
  begin
    P.Char := P.Char - 1;
    S := TmpCharA;
  end;
  Editor.GetHighlighterAttriAtRowCol(P, S, Attri);

  if (Editor.Highlighter.SymbolAttribute = Attri) then
  begin
    for i := Low(OpenChars) to High(OpenChars) do
    begin
      if (S = OpenChars[i]) or (S = CloseChars[i]) then
      begin
        Pix := CharToPixels(P);

        Editor.Canvas.Brush.Style := bsSolid;//Clear;
        Editor.Canvas.Font.Assign(Editor.Font);
        Editor.Canvas.Font.Style := Attri.Style;

        if (TransientType = ttAfter) then
        begin
          Editor.Canvas.Font.Color := FBracketFG;
          Editor.Canvas.Brush.Color := FBracketBG;
        end else begin
          Editor.Canvas.Font.Color := Attri.Foreground;
          Editor.Canvas.Brush.Color := Attri.Background;
        end;
        if Editor.Canvas.Font.Color = clNone then
          Editor.Canvas.Font.Color := Editor.Font.Color;
        if Editor.Canvas.Brush.Color = clNone then
          Editor.Canvas.Brush.Color := Editor.Color;

        Editor.Canvas.TextOut(Pix.X, Pix.Y, S);
        P := Editor.GetMatchingBracketEx(P);

        if (P.Char > 0) and (P.Line > 0) then
        begin
          Pix := CharToPixels(P);
          if Pix.X > Editor.Gutter.Width then
          begin
            if S = OpenChars[i] then
              Editor.Canvas.TextOut(Pix.X, Pix.Y, CloseChars[i])
            else
              Editor.Canvas.TextOut(Pix.X, Pix.Y, OpenChars[i]);
          end;
        end;
      end; //if
    end;//for i :=
    Editor.Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Editor.Text := SynJavaSyn.SampleSource;
  FBracketFG := clRed;
  FBracketBG := clNone;
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  SynJavaSyn.Enabled := not(SynJavaSyn.Enabled);
  Editor.Repaint;
end;

end.
