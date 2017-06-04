unit SynEdit32.DocumentManager;

interface

uses
  Classes, Messages, ExtCtrls,
  SynEdit32.Types,
  SynEdit32,
  SynEdit32.Memo,
  SynEdit32.TextBuffer,
  SynEdit32.Highlighter;

type
  ISynEdit32Document = interface
  ['{DC80C7CF-FC56-4FDE-9E3E-6A1C53D6EFCD}']
    procedure SetCaretXY(const Value : TSynEdit32BufferCoord);
    function GetCaretXY : TSynEdit32BufferCoord;
    procedure SetLines(const Value : TStrings);
    function GetLines : TStrings;
    function GetUndoList: TSynEdit32UndoList;
    function GetRedoList: TSynEdit32UndoList;
    function GetTopLine: Integer;
    procedure SetTopLine(const Value : Integer);
    procedure SetModified(const Value : Boolean);
    function GetModified : Boolean;
    function GetName : string;
    function GetHighLighter : TSynEdit32CustomHighlighter;
    procedure SetHighlighter(const Value : TSynEdit32CustomHighlighter);
    function GetDataIntf : IInterface;
    procedure SetDataIntf(const Value : IInterface);
    function GetMarks: TSynEdit32MarkList;

    property CaretXY: TSynEdit32BufferCoord read GetCaretXY write SetCaretXY;
    property Lines: TStrings read GetLines write SetLines;
    property UndoList: TSynEdit32UndoList read GetUndoList;
    property RedoList: TSynEdit32UndoList read GetRedoList;
    property TopLine: Integer read GetTopLine write SetTopLine;
    property Modified: Boolean read GetModified write SetModified;
    property Name: string read GetName;
    property Highlighter: TSynEdit32CustomHighlighter read GetHighlighter write SetHighLighter;
    property DataIntf: IInterface read GetDataIntf write SetDataIntf;
    property Marks: TSynEdit32MarkList read GetMarks;
    //Line info allows us to store stuff like gutter icons, breakpoints etc.
  end;

  TSynEdit32DocumentManager = class(TComponent)
  private
    FDocuments: IInterfaceList;
    FCurrentDocumentIndex: Integer;
    FMemo: TSynEdit32Memo;
    FMemoWndProc: TWndMethod;
    FUpdateTimer: TTimer;
    function GetCount: Integer;
    function GetCurrentDocument: ISynEdit32Document;
  protected
    procedure MemoWndProc(var Msg: TMessage);
    procedure SetMemo(const Value: TSynEdit32Memo);
    function GetDocument(index: Integer): ISynEdit32Document;
    function GetDocumentByName(index: string): ISynEdit32Document;
    procedure SetCurrentDocumentIndex(const Value: Integer);
    procedure UpdateTimerEvent(Sender : TObject);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure UpdateCurrentDocument; //saves editor to document
    procedure ApplyCurrentDocument; //applies document to editor
    function AddDocument(const AName : string; const ALines : TStrings; const AHighlighter : TSynEdit32CustomHighlighter) : ISynEdit32Document;
    procedure RemoveDocument(const index : Integer);overload;
    procedure RemoveDocument(const AName : string);overload;
    procedure RemoveDocument(const ADocument : ISynEdit32Document);overload;
    procedure RemoveAll;
    property Documents[index : Integer] : ISynEdit32Document read GetDocument;
    property CurrentDocument : ISynEdit32Document read GetCurrentDocument;
    property DocumentsByName[index : string] : ISynEdit32Document read GetDocumentByName;
    property CurrentDocumentIndex : Integer read FCurrentDocumentIndex write SetCurrentDocumentIndex;
    property Count : Integer read GetCount;
  published
    property Memo : TSynEdit32Memo read FMemo write SetMemo;
  end;

implementation

uses
  Windows, SysUtils;
{ TSynEdit32DocumentManager }

type
  TSynEdit32Document = class(TInterfacedObject, ISynEdit32Document)
  private
    FName         : string;
    FLines        : TStringList;
    FCaretXY      : TSynEdit32BufferCoord;
    FModified     : Boolean;
    FRedoList     : TSynEdit32UndoList;
    FUndoList     : TSynEdit32UndoList;
    FTopLine      : Integer;
    FHighLighter  : TSynEdit32CustomHighlighter;
    FDataIntf     : IInterface;
    FMarks        : TSynEdit32MarkList;
  protected
    function GetCaretXY : TSynEdit32BufferCoord;
    function GetLines : TStrings;
    function GetModified  : Boolean;
    function GetName  : String;
    function GetRedoList  : TSynEdit32UndoList;
    function GetTopLine : Integer;
    function GetUndoList  : TSynEdit32UndoList;
    procedure SetCaretXY(const Value: TSynEdit32BufferCoord);
    procedure SetLines(const Value: TStrings);
    procedure SetModified(const Value: Boolean);
    procedure SetTopLine(const Value: Integer);
    function GetHighLighter : TSynEdit32CustomHighlighter;
    procedure SetHighlighter(const Value : TSynEdit32CustomHighlighter);
    function GetDataIntf : IInterface;
    procedure SetDataIntf(const Value : IInterface);
    function GetMarks   : TSynEdit32MarkList;
  public
    constructor Create(const AName : string; ALines : TStrings);
    destructor Destroy;override;
  end;


function TSynEdit32DocumentManager.AddDocument(const AName: string; const ALines: TStrings; const AHighlighter : TSynEdit32CustomHighlighter): ISynEdit32Document;
begin
  Result := GetDocumentByName(AName);
  if Result <> nil then
  begin
    Result.Lines.Assign(ALines);
    Result.Highlighter := AHighlighter;
  end
  else
  begin
    Result := TSynEdit32Document.Create(AName,ALines);
    Result.Highlighter := AHighlighter;
    FDocuments.Add(Result);
{    if CurrentDocumentIndex = -1 then
      CurrentDocumentIndex := 0;}
  end;

end;

constructor TSynEdit32DocumentManager.Create(AOwner: TComponent);
begin
  inherited;
  FDocuments            := TInterfaceList.Create;
  FCurrentDocumentIndex := -1;
  FUpdateTimer          := TTimer.Create(Self);
  FUpdateTimer.enabled  := False;
  FUpdateTimer.Interval := 200;
  FUpdateTimer.OnTimer  := UpdateTimerEvent;
end;

function TSynEdit32DocumentManager.GetDocument(index: Integer): ISynEdit32Document;
begin
  if (index >= 0) and (index < FDocuments.Count) then
    Result := FDocuments.Items[index] as ISynEdit32Document
  else
    Result := nil;
end;

function TSynEdit32DocumentManager.GetDocumentByName(index: string): ISynEdit32Document;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FDocuments.Count -1 do
  begin
    Result := GetDocument(i);
    if CompareText(Result.Name,index) = 0 then
      break
    else
      Result := nil;
  end;
end;

procedure TSynEdit32DocumentManager.RemoveDocument(const index: Integer);
begin
  FDocuments.Delete(index);
  if FDocuments.Count = 0 then
    FCurrentDocumentIndex := -1;
end;

procedure TSynEdit32DocumentManager.RemoveDocument(const AName: string);
var
  doc : ISynEdit32Document;
begin
  doc := GetDocumentByName(AName);
  if doc <> nil then
    FDocuments.Remove(doc);
  if FDocuments.Count = 0 then
    FCurrentDocumentIndex := -1;

end;

procedure TSynEdit32DocumentManager.MemoWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_CHAR) then
  begin
    FUpdateTimer.Enabled := False;
    FUpdateTimer.Enabled := True;
  end;
  if Assigned(FMemoWndProc) then
    FMemoWndProc(Msg);
end;

procedure TSynEdit32DocumentManager.RemoveDocument(const ADocument: ISynEdit32Document);
begin
  FDocuments.Remove(ADocument);
  if FDocuments.Count = 0 then
    FCurrentDocumentIndex := -1;
end;

procedure TSynEdit32DocumentManager.SetCurrentDocumentIndex(const Value: Integer);
begin
  if FCurrentDocumentIndex <> Value then
  begin
    UpdateCurrentDocument;
    if (Value >= 0) and (Value < FDocuments.Count) then
    begin
      FCurrentDocumentIndex := Value;
      ApplyCurrentDocument;
    end;
  end;
end;

procedure TSynEdit32DocumentManager.SetMemo(const Value: TSynEdit32Memo);
begin
  if FMemo <> Value then
  begin
    if FMemo <> nil then
    begin
      FMemo.RemoveFreeNotification(Self);
      if not (csDesigning in ComponentState) then
      begin
        if Assigned(FMemoWndProc) then
          FMemo.WindowProc := FMemoWndProc;
      end;
    end;
    FMemo := Value;
    if FMemo <> nil then
    begin
      FMemo.FreeNotification(Self);
      if not (csDesigning in ComponentState) then
      begin
        FMemoWndProc := FMemo.WindowProc;
        FMemo.WindowProc := Self.MemoWndProc;
      end;
    end;
  end;
end;

procedure TSynEdit32DocumentManager.RemoveAll;
begin
  FDocuments.Clear;
  FCurrentDocumentIndex := -1;
end;

function TSynEdit32DocumentManager.GetCount: Integer;
begin
  Result := FDocuments.Count;
end;

function TSynEdit32DocumentManager.GetCurrentDocument: ISynEdit32Document;
begin
  if FCurrentDocumentIndex <> -1 then
    Result := GetDocument(FCurrentDocumentIndex)
  else
    Result := nil;
end;

function CloneMark(const AOwner: TCustomSynEdit32; const source: TSynEdit32Mark): TSynEdit32Mark;
begin
  Result := TSynEdit32Mark.Create(AOwner);
  Result.Line := source.Line;
  Result.Char := source.Char;
  Result.ImageIndex := source.ImageIndex;
  Result.BookmarkNumber := source.BookmarkNumber;
  Result.InternalImage := source.InternalImage;
  Result.Visible := source.Visible;
end;

procedure TSynEdit32DocumentManager.ApplyCurrentDocument;
var
  doc : ISynEdit32Document;
  I: Integer;
begin
  if FCurrentDocumentIndex <> -1 then
  begin
    if FMemo <> nil then
    begin
      doc := GetDocument(FCurrentDocumentIndex);
      if doc <> nil then
      begin
        FMemo.Lines.Assign(doc.Lines);
        FMemo.TopLine := doc.TopLine;
        FMemo.CaretXY := doc.CaretXY;
        FMemo.UndoList.Assign(doc.UndoList);
        FMemo.RedoList.Assign(doc.RedoList);
        FMemo.Highlighter := doc.Highlighter;
        //can't do this because it av's now???
//        FMemo.Marks.Assign(doc.Marks);
        FMemo.Marks.Clear;
        for i := 0 to doc.Marks.Count - 1 do
        begin
          FMemo.Marks.Place(CloneMark(FMemo,doc.Marks.Items[i]));
        end;
        FMemo.Modified := doc.Modified;
        FMemo.Refresh;
      end;
    end;
  end;
end;

{ TSynEdit32Document }

constructor TSynEdit32Document.Create(const AName: string; ALines: TStrings);
begin
  inherited Create;
  FLines    := TStringList.Create;
  FRedoList := TSynEdit32UndoList.Create;
  FUndoList := TSynEdit32UndoList.Create;
  FName := AName;
  FLines.Assign(ALines);
  FModified := False;
  FTopLine := 0;
  FMarks   := TSynEdit32MarkList.Create(nil);
end;

destructor TSynEdit32Document.Destroy;
begin
  FLines.Free;
  FRedoList.Free;
  FUndoList.Free;
  FMarks.Free;
  inherited;
end;

function TSynEdit32Document.GetCaretXY: TSynEdit32BufferCoord;
begin
  Result := FCaretXY;
end;

function TSynEdit32Document.GetDataIntf: IInterface;
begin
  Result := FDataIntf;
end;

function TSynEdit32Document.GetHighLighter: TSynEdit32CustomHighlighter;
begin
  Result := FHighLighter;
end;


function TSynEdit32Document.GetLines: TStrings;
begin
  Result := FLines;
end;

function TSynEdit32Document.GetMarks: TSynEdit32MarkList;
begin
  Result := FMarks;
end;

function TSynEdit32Document.GetModified: Boolean;
begin
  Result := FModified;
end;

function TSynEdit32Document.GetName: String;
begin
  Result := FName;
end;

function TSynEdit32Document.GetRedoList: TSynEdit32UndoList;
begin
  Result := FRedoList;
end;

function TSynEdit32Document.GetTopLine: Integer;
begin
  Result := FTopLine;
end;

function TSynEdit32Document.GetUndoList: TSynEdit32UndoList;
begin
  Result := FUndoList;
end;

procedure TSynEdit32Document.SetCaretXY(const Value: TSynEdit32BufferCoord);
begin
  FCaretXY := Value;
end;

procedure TSynEdit32Document.SetDataIntf(const Value: IInterface);
begin
  FDataIntf := Value;
end;

procedure TSynEdit32Document.SetHighlighter(const Value: TSynEdit32CustomHighlighter);
begin
  FHighLighter := Value;
end;


procedure TSynEdit32Document.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TSynEdit32Document.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;

procedure TSynEdit32Document.SetTopLine(const Value: Integer);
begin
  FTopLine := Value;
end;

procedure TSynEdit32DocumentManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FMemo) and (Operation = opRemove) then
  begin
    SetMemo(nil);
  end;
end;

destructor TSynEdit32DocumentManager.Destroy;
begin
  FUpdateTimer.Free;
  inherited;
end;

procedure TSynEdit32DocumentManager.UpdateTimerEvent(Sender: TObject);
var
  doc : ISynEdit32Document;
begin
  FUpdateTimer.Enabled := False;
  if FCurrentDocumentIndex <> -1 then
  begin
    if FMemo <> nil then
    begin
      doc := GetDocument(FCurrentDocumentIndex);
      if doc <> nil then
      begin
        doc.Modified := FMemo.Modified;
        doc.Lines.Assign(FMemo.Lines);
        doc.TopLine := FMemo.TopLine;
        doc.CaretXY := FMemo.CaretXY;
        doc.UndoList.Assign(FMemo.UndoList);
        doc.RedoList.Assign(FMemo.RedoList);
      end;
    end;
  end;
end;


procedure TSynEdit32DocumentManager.UpdateCurrentDocument;
var
  doc : ISynEdit32Document;
  i: Integer;
begin
  if FCurrentDocumentIndex <> -1 then
  begin
    //save the state of the current document
    if FMemo <> nil then
    begin
      doc := GetDocument(FCurrentDocumentIndex);
      if doc <> nil then
      begin
        doc.Modified := FMemo.Modified;
        doc.Lines.Assign(FMemo.Lines);
        doc.TopLine := FMemo.TopLine;
        doc.CaretXY := FMemo.CaretXY;
        doc.UndoList.Assign(FMemo.UndoList);
        doc.RedoList.Assign(FMemo.RedoList);
        doc.Marks.Clear;
        for i := 0 to FMemo.Marks.Count - 1 do
          doc.Marks.Place(CloneMark(nil,FMemo.Marks.Items[i]));
        FMemo.Highlighter := doc.Highlighter;
      end;
    end;
  end;
end;
end.
