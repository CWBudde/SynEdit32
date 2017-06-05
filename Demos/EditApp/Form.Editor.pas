{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmEditor.pas, released 2000-09-08.

The Original Code is part of the EditAppDemos project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: frmEditor.pas,v 1.5.2.2 2008/09/14 16:24:57 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit Form.Editor;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  UnitEditAppIntfs, SynEdit32.MiscClasses, SynEdit32.Types, SynEdit32.Search,
  SynEdit32;

type
  TEditorKind = (ekBorderless, ekInTabsheet, ekMDIChild);

  TEditor = class;

  TEditorForm = class(TForm)
    MenuItemEditCopy: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditDelete: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditRedo: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PopupMenuEditor: TPopupMenu;
    SynEditor: TSynEdit32;
    SynEditSearch: TSynEdit32Search;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure SynEditorChange(Sender: TObject);
    procedure SynEditorEnter(Sender: TObject);
    procedure SynEditorExit(Sender: TObject);
    procedure SynEditorStatusChange(Sender: TObject;
      Changes: TSynEdit32StatusChanges);
    procedure SynEditorReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: Integer;
      var Action: TSynEdit32ReplaceAction);
  private
    FEditor: TEditor;
    FKind: TEditorKind;
  private
    FSearchFromCaret: Boolean;
    function DoAskSaveChanges: Boolean;
    procedure DoAssignInterfacePointer(AActive: Boolean);
    function DoSave: Boolean;
    function DoSaveFile: Boolean;
    function DoSaveAs: Boolean;
    procedure DoSearchReplaceText(AReplace: Boolean; ABackwards: Boolean);
    procedure DoUpdateCaption;
    procedure DoUpdateHighlighter;
    procedure ShowSearchReplaceDialog(AReplace: Boolean);
  public
    procedure DoActivate;
  end;

  TEditor = class(TInterfacedObject, IEditor, IEditCommands, IFileCommands,
    ISearchCommands)
  private
    // IEditor implementation
    procedure Activate;
    function AskSaveChanges: Boolean;
    procedure Close;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetModified: Boolean;
    procedure OpenFile(AFileName: string);
    // IEditCommands implementation
    function CanCopy: Boolean;
    function CanCut: Boolean;
    function IEditCommands.CanDelete = CanCut;
    function CanPaste: Boolean;
    function CanRedo: Boolean;
    function CanSelectAll: Boolean;
    function CanUndo: Boolean;
    procedure ExecCopy;
    procedure ExecCut;
    procedure ExecDelete;
    procedure ExecPaste;
    procedure ExecRedo;
    procedure ExecSelectAll;
    procedure ExecUndo;
    // IFileCommands implementation
    function CanClose: Boolean;
    function CanPrint: Boolean;
    function CanSave: Boolean;
    function CanSaveAs: Boolean;
    procedure IFileCommands.ExecClose = Close;
    procedure ExecPrint;
    procedure ExecSave;
    procedure ExecSaveAs;
    // ISearchCommands implementation
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function ISearchCommands.CanFindPrev = CanFindNext;
    function CanReplace: Boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
  private
    FFileName: string;
    FForm: TEditorForm;
    FHasSelection: Boolean;
    FIsEmpty: Boolean;
    FIsReadOnly: Boolean;
    FModified: Boolean;
    FUntitledNumber: Integer;
    constructor Create(AForm: TEditorForm);
    procedure DoSetFileName(AFileName: string);
  end;

implementation

{$R *.DFM}

uses
  ComCtrls, DataModuleCommands, Dialog.SearchText, Dialog.ReplaceText,
  Dialog.ConfirmReplace;

const
  WM_DELETETHIS  =  WM_USER + 42;

var
  GSearchBackwards: Boolean;
  GSearchCaseSensitive: Boolean;
  GSearchFromCaret: Boolean;
  GSearchSelectionOnly: Boolean;
  GSearchTextAtCaret: Boolean;
  GSearchWholeWords: Boolean;

  GSearchText: string;
  GSearchTextHistory: string;
  GReplaceText: string;
  GReplaceTextHistory: string;

resourcestring
  RStrInsert = 'Insert';
  RStrOverwrite = 'Overwrite';
  RStrReadOnly = 'Read Only';
  RStrNonameFileTitle = 'Untitled';
  RStrEditorCaption = 'Editor';

  RStrAskSaveChanges = 'The text in the "%s" file has changed.'#13#10#13#10 +
                    'Do you want to save the modifications?';

{ TEditor }

constructor TEditor.Create(AForm: TEditorForm);
begin
  Assert(AForm <> nil);
  inherited Create;
  FForm := AForm;
  FUntitledNumber := -1;
end;

procedure TEditor.Activate;
begin
  if FForm <> nil then
    FForm.DoActivate;
end;

function TEditor.AskSaveChanges: Boolean;
begin
  if FForm <> nil then
    Result := FForm.DoAskSaveChanges
  else
    Result := TRUE;
end;

function TEditor.CanClose: Boolean;
begin
  Result := FForm <> nil;
end;

procedure TEditor.Close;
begin
  if (FFileName <> '') and (CommandsDataModule <> nil) then
    CommandsDataModule.AddMRUEntry(FFileName);
  if FUntitledNumber <> -1 then
    CommandsDataModule.ReleaseUntitledNumber(FUntitledNumber);
  if FForm <> nil then
    FForm.Close;
end;

procedure TEditor.DoSetFileName(AFileName: string);
begin
  if AFileName <> FFileName then
  begin
    FFileName := AFileName;
    if FUntitledNumber <> -1 then
    begin
      CommandsDataModule.ReleaseUntitledNumber(FUntitledNumber);
      FUntitledNumber := -1;
    end;
  end;
end;

function TEditor.GetCaretPos: TPoint;
begin
  if FForm <> nil then
    Result := TPoint(FForm.SynEditor.CaretXY)
  else
    Result := Point(-1, -1);
end;

function TEditor.GetEditorState: string;
begin
  if FForm <> nil then
  begin
    if FForm.SynEditor.ReadOnly then
      Result := RStrReadOnly
    else if FForm.SynEditor.InsertMode then
      Result := RStrInsert
    else
      Result := RStrOverwrite;
  end else
    Result := '';
end;

function TEditor.GetFileName: string;
begin
  Result := FFileName;
end;

function TEditor.GetFileTitle: string;
begin
  if FFileName <> '' then
    Result := ExtractFileName(FFileName)
  else
  begin
    if FUntitledNumber = -1 then
      FUntitledNumber := CommandsDataModule.GetUntitledNumber;
    Result := RStrNonameFileTitle + IntToStr(FUntitledNumber);
  end;
end;

function TEditor.GetModified: Boolean;
begin
  if FForm <> nil then
    Result := FForm.SynEditor.Modified
  else
    Result := FALSE;
end;

procedure TEditor.OpenFile(AFileName: string);
begin
  FFileName := AFileName;
  if FForm <> nil then
  begin
    if (AFileName <> '') and FileExists(AFileName) then
      FForm.SynEditor.Lines.LoadFromFile(AFileName)
    else
      FForm.SynEditor.Lines.Clear;
    FForm.DoUpdateCaption;
    FForm.DoUpdateHighlighter;
  end;
end;

// IEditCommands implementation

function TEditor.CanCopy: Boolean;
begin
  Result := (FForm <> nil) and FHasSelection;
end;

function TEditor.CanCut: Boolean;
begin
  Result := (FForm <> nil) and FHasSelection and not FIsReadOnly;
end;

function TEditor.CanPaste: Boolean;
begin
  Result := (FForm <> nil) and FForm.SynEditor.CanPaste;
end;

function TEditor.CanRedo: Boolean;
begin
  Result := (FForm <> nil) and FForm.SynEditor.CanRedo;
end;

function TEditor.CanSelectAll: Boolean;
begin
  Result := FForm <> nil;
end;

function TEditor.CanUndo: Boolean;
begin
  Result := (FForm <> nil) and FForm.SynEditor.CanUndo;
end;

procedure TEditor.ExecCopy;
begin
  if FForm <> nil then
    FForm.SynEditor.CopyToClipboard;
end;

procedure TEditor.ExecCut;
begin
  if FForm <> nil then
    FForm.SynEditor.CutToClipboard;
end;

procedure TEditor.ExecDelete;
begin
  if FForm <> nil then
    FForm.SynEditor.SelText := '';
end;

procedure TEditor.ExecPaste;
begin
  if FForm <> nil then
    FForm.SynEditor.PasteFromClipboard;
end;

procedure TEditor.ExecRedo;
begin
  if FForm <> nil then
    FForm.SynEditor.Redo;
end;

procedure TEditor.ExecSelectAll;
begin
  if FForm <> nil then
    FForm.SynEditor.SelectAll;
end;

procedure TEditor.ExecUndo;
begin
  if FForm <> nil then
    FForm.SynEditor.Undo;
end;

// IFileCommands implementation

function TEditor.CanPrint: Boolean;
begin
  Result := FALSE;
end;

function TEditor.CanSave: Boolean;
begin
  Result := (FForm <> nil) and (FModified or (FFileName = ''));
end;

function TEditor.CanSaveAs: Boolean;
begin
  Result := FForm <> nil;
end;

procedure TEditor.ExecPrint;
begin
  if FForm <> nil then
// TODO
end;

procedure TEditor.ExecSave;
begin
  if FForm <> nil then
  begin
    if FFileName <> '' then
      FForm.DoSave
    else
      FForm.DoSaveAs
  end;
end;

procedure TEditor.ExecSaveAs;
begin
  if FForm <> nil then
    FForm.DoSaveAs;
end;

// ISearchCommands implementation

function TEditor.CanFind: Boolean;
begin
  Result := (FForm <> nil) and not FIsEmpty;
end;

function TEditor.CanFindNext: Boolean;
begin
  Result := (FForm <> nil) and not FIsEmpty and (GSearchText <> '');
end;

function TEditor.CanReplace: Boolean;
begin
  Result := (FForm <> nil) and not FIsReadOnly and not FIsEmpty;
end;

procedure TEditor.ExecFind;
begin
  if FForm <> nil then
    FForm.ShowSearchReplaceDialog(FALSE);
end;

procedure TEditor.ExecFindNext;
begin
  if FForm <> nil then
    FForm.DoSearchReplaceText(FALSE, FALSE);
end;

procedure TEditor.ExecFindPrev;
begin
  if FForm <> nil then
    FForm.DoSearchReplaceText(FALSE, TRUE);
end;

procedure TEditor.ExecReplace;
begin
  if FForm <> nil then
    FForm.ShowSearchReplaceDialog(TRUE);
end;

{ TEditorTabSheet }

type
  TEditorTabSheet = class(TTabSheet)
  private
    procedure WMDeleteThis(var Msg: TMessage);
      message WM_DELETETHIS;
  end;

procedure TEditorTabSheet.WMDeleteThis(var Msg: TMessage);
begin
  Free;
end;

{ TEditorFactory }

type
  TEditorFactory = class(TInterfacedObject, IEditorFactory)
  private
    // IEditorFactory implementation
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function CreateBorderless(AOwner: TForm): IEditor;
    function CreateMDIChild(AOwner: TForm): IEditor;
    function CreateTabSheet(AOwner: TPageControl): IEditor;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
  private
    FEditors: TInterfaceList;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TEditorFactory.Create;
begin
  inherited Create;
  FEditors := TInterfaceList.Create;
end;

destructor TEditorFactory.Destroy;
begin
  FEditors.Free;
  inherited Destroy;
end;

function TEditorFactory.CanCloseAll: Boolean;
var
  i: Integer;
  LEditor: IEditor;
begin
  i := FEditors.Count - 1;
  while i >= 0 do
  begin
    LEditor := IEditor(FEditors[i]);
    if not LEditor.AskSaveChanges then
    begin
      Result := FALSE;
      exit;
    end;
    Dec(i);
  end;
  Result := TRUE;
end;

procedure TEditorFactory.CloseAll;
var
  i: Integer;
begin
  i := FEditors.Count - 1;
  while i >= 0 do
  begin
    IEditor(FEditors[i]).Close;
    Dec(i);
  end;
end;

function TEditorFactory.CreateBorderless(AOwner: TForm): IEditor;
var
  LForm: TEditorForm;
begin
  LForm := TEditorForm.Create(AOwner);
  with LForm do
  begin
    FEditor := TEditor.Create(LForm);
    Result := FEditor;
    FKind := ekBorderless;
    BorderStyle := bsNone;
    Parent := AOwner;
    Align := alClient;
    Visible := TRUE;
  end;
  if Result <> nil then
    FEditors.Add(Result);
end;

function TEditorFactory.CreateMDIChild(AOwner: TForm): IEditor;
var
  LForm: TEditorForm;
begin
  LForm := TEditorForm.Create(AOwner);
  with LForm do
  begin
    FEditor := TEditor.Create(LForm);
    Result := FEditor;
    FKind := ekMDIChild;
    FormStyle := fsMDIChild;
  end;
  if Result <> nil then
    FEditors.Add(Result);
end;

function TEditorFactory.CreateTabSheet(AOwner: TPageControl): IEditor;
var
  Sheet: TTabSheet;
  LForm: TEditorForm;
begin
  Sheet := TEditorTabSheet.Create(AOwner);
  try
    Sheet.PageControl := AOwner;
    LForm := TEditorForm.Create(Sheet);
    with LForm do
    begin
      FEditor := TEditor.Create(LForm);
      Result := FEditor;
      FKind := ekInTabsheet;
      BorderStyle := bsNone;
      Parent := Sheet;
      Align := alClient;
      Visible := TRUE;
      AOwner.ActivePage := Sheet;
      LForm.SetFocus;
    end;
    // fix for Delphi 4 (???)
    LForm.Realign;
    if Result <> nil then
      FEditors.Add(Result);
  except
    Sheet.Free;
  end;
end;

function TEditorFactory.GetEditorCount: Integer;
begin
  Result := FEditors.Count;
end;

function TEditorFactory.GetEditor(Index: Integer): IEditor;
begin
  Result := IEditor(FEditors[Index]);
end;

procedure TEditorFactory.RemoveEditor(AEditor: IEditor);
var
  i: Integer;
begin
  i := FEditors.IndexOf(AEditor);
  if i > -1 then
    FEditors.Delete(i);
end;

{ TEditorForm }

procedure TEditorForm.FormActivate(Sender: TObject);
begin
  DoAssignInterfacePointer(TRUE);
end;

procedure TEditorForm.FormDeactivate(Sender: TObject);
begin
  DoAssignInterfacePointer(FALSE);
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
  DoUpdateCaption;
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FKind = ekInTabSheet then
  begin
    PostMessage(Parent.Handle, WM_DELETETHIS, 0, 0);
    Action := caNone;
  end else
    Action := caFree;
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // need to prevent this from happening more than once (e.g. with MDI childs)
  if not (csDestroying in ComponentState) then
    CanClose := DoAskSaveChanges;
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
var
  LEditor: IEditor;
begin
  LEditor := FEditor;
  Assert(FEditor <> nil);
  FEditor.FForm := nil;
  Assert(GEditorFactory <> nil);
  GEditorFactory.RemoveEditor(LEditor);
end;

procedure TEditorForm.SynEditorChange(Sender: TObject);
var
  Empty: Boolean;
  i: Integer;
begin
  Assert(FEditor <> nil);
  Empty := TRUE;
  for i := SynEditor.Lines.Count - 1 downto 0 do
    if SynEditor.Lines[i] <> '' then
    begin
      Empty := FALSE;
      break;
    end;
  FEditor.FIsEmpty := Empty;
end;

procedure TEditorForm.SynEditorEnter(Sender: TObject);
begin
  DoAssignInterfacePointer(TRUE);
end;

procedure TEditorForm.SynEditorExit(Sender: TObject);
begin
  DoAssignInterfacePointer(FALSE);
end;

procedure TEditorForm.SynEditorReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: Integer; var Action: TSynEdit32ReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else
  begin
    APos := SynEditor.ClientToScreen(
      SynEditor.RowColumnToPixels(
      SynEditor.BufferToDisplayPos(
      SynEdit32BufferCoord(Column, Line) ) ) );
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if ConfirmReplaceDialog = nil then
      ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
    ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
      APos.Y + SynEditor.LineHeight, ASearch);
    case ConfirmReplaceDialog.ShowModal of
      mrYes: Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo: Action := raSkip;
      else Action := raCancel;
    end;
  end;
end;

procedure TEditorForm.SynEditorStatusChange(Sender: TObject;
  Changes: TSynEdit32StatusChanges);
begin
  Assert(FEditor <> nil);
  if Changes * [scAll, scSelection] <> [] then
    FEditor.FHasSelection := SynEditor.SelAvail;
  if Changes * [scAll, scSelection] <> [] then
    FEditor.FIsReadOnly := SynEditor.ReadOnly;
  if Changes * [scAll, scModified] <> [] then
    FEditor.FModified := SynEditor.Modified;
end;

procedure TEditorForm.DoActivate;
var
  Sheet: TTabSheet;
  PCtrl: TPageControl;
begin
  if FormStyle = fsMDIChild then
    BringToFront
  else if Parent is TTabSheet then
  begin
    Sheet := TTabSheet(Parent);
    PCtrl := Sheet.PageControl;
    if PCtrl <> nil then
      PCtrl.ActivePage := Sheet;
  end;
end;

function TEditorForm.DoAskSaveChanges: Boolean;
const
  MBType = MB_YESNOCANCEL or MB_ICONQUESTION;
var
  s: string;
begin
  // this is necessary to prevent second confirmation when closing MDI childs
  if SynEditor.Modified then
  begin
    DoActivate;
    MessageBeep(MB_ICONQUESTION);
    Assert(FEditor <> nil);
    s := Format(RStrAskSaveChanges, [ExtractFileName(FEditor.GetFileTitle)]);
    case Application.MessageBox(PChar(s), PChar(Application.Title), MBType) of
      IDYes: Result := DoSave;
      IDNo: Result := TRUE;
    else
      Result := FALSE;
    end;
  end else
    Result := TRUE;
end;

procedure TEditorForm.DoAssignInterfacePointer(AActive: Boolean);
begin
  if AActive then
  begin
    GActiveEditor := FEditor;
    GEditCmds := FEditor;
    GFileCmds := FEditor;
    GSearchCmds := FEditor;
  end
  else
  begin
    if GActiveEditor = IEditor(FEditor) then
      GActiveEditor := nil;
    if GEditCmds = IEditCommands(FEditor) then
      GEditCmds := nil;
    if GFileCmds = IFileCommands(FEditor) then
      GFileCmds := nil;
    if GSearchCmds = ISearchCommands(FEditor) then
      GSearchCmds := nil;
  end;
end;

function TEditorForm.DoSave: Boolean;
begin
  Assert(FEditor <> nil);
  if FEditor.FFileName <> '' then
    Result := DoSaveFile
  else
    Result := DoSaveAs;
end;

function TEditorForm.DoSaveFile: Boolean;
begin
  Assert(FEditor <> nil);
  try
    SynEditor.Lines.SaveToFile(FEditor.FFileName);
    SynEditor.Modified := FALSE;
    Result := TRUE;
  except
    Application.HandleException(Self);
    Result := FALSE;
  end;
end;

function TEditorForm.DoSaveAs: Boolean;
var
  NewName: string;
begin
  Assert(FEditor <> nil);
  NewName := FEditor.FFileName;
  if CommandsDataModule.GetSaveFileName(NewName, SynEditor.Highlighter) then
  begin
    FEditor.DoSetFileName(NewName);
    DoUpdateCaption;
    DoUpdateHighlighter;
    Result := DoSaveFile;
  end else
    Result := FALSE;
end;

procedure TEditorForm.DoSearchReplaceText(AReplace: Boolean;
  ABackwards: Boolean);
var
  Options: TSynEdit32SearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if GSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not FSearchFromCaret then
    Include(Options, ssoEntireScope);
  if GSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if GSearchWholeWords then
    Include(Options, ssoWholeWord);
  if SynEditor.SearchReplace(GSearchText, GReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      SynEditor.BlockEnd := SynEditor.BlockBegin
    else
      SynEditor.BlockBegin := SynEditor.BlockEnd;
    SynEditor.CaretXY := SynEditor.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TEditorForm.DoUpdateCaption;
begin
  Assert(FEditor <> nil);
  case FKind of
    ekInTabsheet:
      (Parent as TTabSheet).Caption := FEditor.GetFileTitle;
    ekMDIChild:
      Caption := FEditor.GetFileTitle + ' - ' + RStrEditorCaption;
  end;
end;

procedure TEditorForm.DoUpdateHighlighter;
begin
  Assert(FEditor <> nil);
  if FEditor.FFileName <> '' then
  begin
    SynEditor.Highlighter := CommandsDataModule.GetHighlighterForFile(
      FEditor.FFileName);
  end else
    SynEditor.Highlighter := nil;
end;

procedure TEditorForm.ShowSearchReplaceDialog(AReplace: Boolean);
var
  dlg: TTextSearchDialog;
begin
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := GSearchBackwards;
    SearchCaseSensitive := GSearchCaseSensitive;
    SearchFromCursor := GSearchFromCaret;
    SearchInSelectionOnly := GSearchSelectionOnly;
    // start with last search text
    SearchText := GSearchText;
    if GSearchTextAtCaret then
    begin
      // if something is selected search for that text
      if SynEditor.SelAvail and (SynEditor.BlockBegin.Line = SynEditor.BlockEnd.Line)
      then
        SearchText := SynEditor.SelText
      else
        SearchText := SynEditor.GetWordAtRowCol(SynEditor.CaretXY);
    end;
    SearchTextHistory := GSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do
    begin
      ReplaceText := GReplaceText;
      ReplaceTextHistory := GReplaceTextHistory;
    end;
    SearchWholeWords := GSearchWholeWords;
    if ShowModal = mrOK then
    begin
      GSearchBackwards := SearchBackwards;
      GSearchCaseSensitive := SearchCaseSensitive;
      GSearchFromCaret := SearchFromCursor;
      GSearchSelectionOnly := SearchInSelectionOnly;
      GSearchWholeWords := SearchWholeWords;
      GSearchText := SearchText;
      GSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do
      begin
        GReplaceText := ReplaceText;
        GReplaceTextHistory := ReplaceTextHistory;
      end;
      FSearchFromCaret := GSearchFromCaret;
      if GSearchText <> '' then
      begin
        DoSearchReplaceText(AReplace, GSearchBackwards);
        FSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

initialization
  GEditorFactory := TEditorFactory.Create;
finalization
  GEditorFactory := nil;
end.
