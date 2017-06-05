unit SynMiniMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Math, ExtCtrls, SynEdit32.Highlighter, SynEdit32.Highlighter.DWS,
  SynEdit32;

type
  TFormSynEdit32Minimap = class(TForm)
    Splitter: TSplitter;
    SynDWSSyn: TSynEdit32HighlighterDWS;
    SynEdit32: TSynEdit32;
    SynEdit32MiniMap: TSynEdit32;
    TimerReplicate: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SynEdit32Change(Sender: TObject);
    procedure SynEdit32MiniMapSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure SynEdit32MiniMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEdit32MiniMapEnter(Sender: TObject);
    procedure TimerReplicateTimer(Sender: TObject);
    procedure SynEdit32StatusChange(Sender: TObject;
      Changes: TSynEdit32StatusChanges);
  end;

var
  FormSynEdit32Minimap: TFormSynEdit32Minimap;

implementation

{$R *.dfm}

{ TFormSynEdit32Minimap }

procedure TFormSynEdit32Minimap.FormCreate(Sender: TObject);
begin
  // double buffer both SynEdit32 instances
  SynEdit32MiniMap.DoubleBuffered := True;
  SynEdit32.DoubleBuffered := True;

  TimerReplicateTimer(Self);
end;

procedure TFormSynEdit32Minimap.FormResize(Sender: TObject);
begin
  SynEdit32StatusChange(Self, []);
end;

procedure TFormSynEdit32Minimap.SynEdit32MiniMapEnter(Sender: TObject);
begin
  SynEdit32.SetFocus;
end;

procedure TFormSynEdit32Minimap.SynEdit32MiniMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Coord: TSynEdit32DisplayCoord;
begin
  Coord := SynEdit32MiniMap.PixelsToNearestRowColumn(X, Y);
  SynEdit32.CaretXY := SynEdit32.DisplayToBufferPos(Coord);
  SynEdit32.Invalidate;
  SynEdit32.TopLine := Max(1, Coord.Row - (SynEdit32.LinesInWindow div 2));
end;

procedure TFormSynEdit32Minimap.SynEdit32MiniMapSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  Special := (Cardinal(Line - SynEdit32.TopLine) <= Cardinal(SynEdit32.LinesInWindow));
  BG := clBtnFace;
end;

procedure TFormSynEdit32Minimap.SynEdit32StatusChange(Sender: TObject;
  Changes: TSynEdit32StatusChanges);
begin
  if SynEdit32MiniMap.Tag = SynEdit32.TopLine then
    Exit;
  SynEdit32MiniMap.Tag := SynEdit32.TopLine;
  SynEdit32MiniMap.TopLine :=
    Max(1, SynEdit32.TopLine - (SynEdit32MiniMap.LinesInWindow -
    SynEdit32.LinesInWindow) div 2);
  SynEdit32MiniMap.Invalidate;
end;

procedure TFormSynEdit32Minimap.SynEdit32Change(Sender: TObject);
begin
  TimerReplicate.Enabled := True;
end;

procedure TFormSynEdit32Minimap.TimerReplicateTimer(Sender: TObject);
var
  I: Integer;
begin
  TimerReplicate.Enabled := False;

  SynEdit32MiniMap.BeginUpdate;
  try
    while SynEdit32MiniMap.Lines.Count > SynEdit32.Lines.Count do
      SynEdit32MiniMap.Lines.Delete(SynEdit32MiniMap.Lines.Count - 1);
    for I := 0 to Min(SynEdit32.Lines.Count, SynEdit32MiniMap.Lines.Count) - 1 do
      if SynEdit32MiniMap.Lines[I] <> SynEdit32.Lines[I] then
        SynEdit32MiniMap.Lines[I] := SynEdit32.Lines[I];
    for I := SynEdit32MiniMap.Lines.Count to SynEdit32.Lines.Count - 1 do
      SynEdit32MiniMap.Lines.Add(SynEdit32.Lines[I]);
  finally
    SynEdit32MiniMap.EndUpdate;
  end;

  SynEdit32MiniMap.Tag := 0;
  SynEdit32MiniMap.Invalidate;
  SynEdit32StatusChange(Sender, []);
end;

end.
