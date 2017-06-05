{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-09-08.

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

$Id: frmMain.pas,v 1.2 2000/11/22 08:34:14 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit Form.Main;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, Actions, UnitEditAppIntfs, ComCtrls;

type
  TMainForm = class(TForm)
    ActionFileCloseAll: TAction;
    ActionFileExit: TAction;
    ActionFileNew: TAction;
    ActionFileOpen: TAction;
    ActionListStandard: TActionList;
    ActionUpdateStatusBarPanels: TAction;
    ActionViewStatusbar: TAction;
    MainMenu: TMainMenu;
    MenuItemEdit: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditDelete: TMenuItem;
    MenuItemEditFind: TMenuItem;
    MenuItemEditFindNext: TMenuItem;
    MenuItemEditFindPrev: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditRedo: TMenuItem;
    MenuItemEditReplace: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileClose: TMenuItem;
    MenuItemFileCloseAll: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFileMRU1: TMenuItem;
    MenuItemFileMRU2: TMenuItem;
    MenuItemFileMRU3: TMenuItem;
    MenuItemFileMRU4: TMenuItem;
    MenuItemFileMRU5: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFilePrint: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemRecentFiles: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemViewStatusbar: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionFileCloseAllExecute(Sender: TObject);
    procedure ActionFileCloseAllUpdate(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileNewOrOpenUpdate(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionUpdateStatusBarPanelsUpdate(Sender: TObject);
    procedure ActionViewStatusbarExecute(Sender: TObject);
    procedure ActionViewStatusbarUpdate(Sender: TObject);
    procedure MenuItemFileClick(Sender: TObject);
    procedure MenuItemRecentFilesClick(Sender: TObject);
    procedure OnOpenMRUFile(Sender: TObject);
  protected
    FMostRecentlyUsedItems: array[1..5] of TMenuItem;
    function CanCloseAll: boolean;
    function CmdLineOpenFiles(AMultipleFiles: boolean): boolean;
    function DoCreateEditor(AFileName: TFileName): IEditor; virtual;
    procedure DoOpenFile(AFileName: TFileName);
    procedure ReadIniSettings;
    procedure WriteIniSettings;
  end;

implementation

{$R *.DFM}

uses
  IniFiles, DataModuleCommands;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMostRecentlyUsedItems[1] := MenuItemFileMRU1;
  FMostRecentlyUsedItems[2] := MenuItemFileMRU2;
  FMostRecentlyUsedItems[3] := MenuItemFileMRU3;
  FMostRecentlyUsedItems[4] := MenuItemFileMRU4;
  FMostRecentlyUsedItems[5] := MenuItemFileMRU5;
  CommandsDataModule := TCommandsDataModule.Create(Self);
  ReadIniSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if GEditorFactory <> nil then
    GEditorFactory.CloseAll;
  WriteIniSettings;
  CommandsDataModule.Free;
end;

// implementation

function TMainForm.CanCloseAll: boolean;
begin
  Result := TRUE;
end;

function TMainForm.CmdLineOpenFiles(AMultipleFiles: boolean): boolean;
var
  i, Cnt: integer;
begin
  Cnt := ParamCount;
  if Cnt > 0 then
  begin
    if not AMultipleFiles and (Cnt > 1) then
      Cnt := 1;
    for i := 1 to Cnt do
      DoOpenFile(ParamStr(i));
    Result := TRUE;
  end else
    Result := FALSE;
end;

function TMainForm.DoCreateEditor(AFileName: TFileName): IEditor;
begin
  Result := nil;
end;

procedure TMainForm.DoOpenFile(AFileName: TFileName);
var
  i: integer;
  LEditor: IEditor;
begin
  AFileName := ExpandFileName(AFileName);
  if AFileName <> '' then
  begin
    CommandsDataModule.RemoveMRUEntry(AFileName);
    // activate the editor if already open
    Assert(GEditorFactory <> nil);
    for i := GEditorFactory.GetEditorCount - 1 downto 0 do
    begin
      LEditor := GEditorFactory.Editor[i];
      if CompareText(LEditor.GetFileName, AFileName) = 0 then
      begin
        LEditor.Activate;
        exit;
      end;
    end;
  end;
  // create a new editor, add it to the editor list, open the file
  LEditor := DoCreateEditor(AFileName);
  if LEditor <> nil then
    LEditor.OpenFile(AFileName);
end;

procedure TMainForm.ReadIniSettings;
var
  iniFile: TIniFile;
  x, y, w, h: integer;
  i: integer;
  s: string;
begin
  iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    x := iniFile.ReadInteger('Main', 'Left', 0);
    y := iniFile.ReadInteger('Main', 'Top', 0);
    w := iniFile.ReadInteger('Main', 'Width', 0);
    h := iniFile.ReadInteger('Main', 'Height', 0);
    if (w > 0) and (h > 0) then
      SetBounds(x, y, w, h);
    if iniFile.ReadInteger('Main', 'Maximized', 0) <> 0 then
      WindowState := wsMaximized;
    StatusBar.Visible := iniFile.ReadInteger('Main', 'ShowStatusbar', 1) <> 0;
    // MRU files
    for i := 5 downto 1 do
    begin
      s := iniFile.ReadString('MRUFiles', Format('MRUFile%d', [i]), '');
      if s <> '' then
        CommandsDataModule.AddMRUEntry(s);
    end;
  finally
    iniFile.Free;
  end;
end;

procedure TMainForm.WriteIniSettings;
var
  iniFile: TIniFile;
  wp: TWindowPlacement;
  i: integer;
  s: string;
begin
  iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    wp.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @wp);
    // form properties
    with wp.rcNormalPosition do
    begin
      iniFile.WriteInteger('Main', 'Left', Left);
      iniFile.WriteInteger('Main', 'Top', Top);
      iniFile.WriteInteger('Main', 'Width', Right - Left);
      iniFile.WriteInteger('Main', 'Height', Bottom - Top);
    end;
    iniFile.WriteInteger('Main', 'Maximized', Ord(WindowState = wsMaximized));
    iniFile.WriteInteger('Main', 'ShowStatusbar', Ord(Statusbar.Visible));
    // MRU files
    for i := 1 to 5 do
    begin
      s := CommandsDataModule.GetMRUEntry(i - 1);
      if s <> '' then
        iniFile.WriteString('MRUFiles', Format('MRUFile%d', [i]), s)
      else
        iniFile.DeleteKey('MRUFiles', Format('MRUFile%d', [i]));
    end;
  finally
    iniFile.Free;
  end;
end;

// action handler methods

procedure TMainForm.ActionFileNewOrOpenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GEditorFactory <> nil;
end;

procedure TMainForm.ActionFileNewExecute(Sender: TObject);
begin
  DoOpenFile('');
end;

procedure TMainForm.ActionFileOpenExecute(Sender: TObject);
begin
  with CommandsDataModule.DialogFileOpen do
  begin
    if Execute then
      DoOpenFile(FileName);
  end;
end;

procedure TMainForm.ActionFileCloseAllExecute(Sender: TObject);
var
  i: integer;
begin
  if GEditorFactory <> nil then
  begin
    if not CanCloseAll then
      exit;
    i := GEditorFactory.GetEditorCount - 1;
    // close all editor childs
    while i >= 0 do
    begin
      GEditorFactory.GetEditor(i).Close;
      Dec(i);
    end;
  end;
end;

procedure TMainForm.ActionFileCloseAllUpdate(Sender: TObject);
begin
  ActionFileCloseAll.Enabled := (GEditorFactory <> nil)
    and (GEditorFactory.GetEditorCount > 0);
end;

procedure TMainForm.ActionFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItemRecentFilesClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  for i := Low(FMostRecentlyUsedItems) to High(FMostRecentlyUsedItems) do
    if FMostRecentlyUsedItems[i] <> nil then
    begin
      s := CommandsDataModule.GetMRUEntry(i - Low(FMostRecentlyUsedItems));
      FMostRecentlyUsedItems[i].Visible := s <> '';
      FMostRecentlyUsedItems[i].Caption := s;
    end;
end;

procedure TMainForm.MenuItemFileClick(Sender: TObject);
begin
  MenuItemRecentFiles.Enabled := CommandsDataModule.GetMRUEntries > 0;
end;

procedure TMainForm.ActionViewStatusbarUpdate(Sender: TObject);
begin
  ActionViewStatusbar.Checked := StatusBar.Visible;
end;

procedure TMainForm.ActionViewStatusbarExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TMainForm.OnOpenMRUFile(Sender: TObject);
var
  i: integer;
  s: string;
begin
  for i := Low(FMostRecentlyUsedItems) to High(FMostRecentlyUsedItems) do
    if Sender = FMostRecentlyUsedItems[i] then
    begin
      s := CommandsDataModule.GetMRUEntry(i - 1);
      if s <> '' then
        DoOpenFile(s);
    end;
end;

procedure TMainForm.ActionUpdateStatusBarPanelsUpdate(Sender: TObject);
resourcestring
  SModified = 'Modified';
var
  ptCaret: TPoint;
begin
  ActionUpdateStatusBarPanels.Enabled := TRUE;
  if GActiveEditor <> nil then
  begin
    ptCaret := GActiveEditor.GetCaretPos;
    if (ptCaret.X > 0) and (ptCaret.Y > 0) then
      StatusBar.Panels[0].Text := Format(' %6d:%3d ', [ptCaret.Y, ptCaret.X])
    else
      StatusBar.Panels[0].Text := '';
    if GActiveEditor.GetModified then
      StatusBar.Panels[1].Text := SModified
    else
      StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := GActiveEditor.GetEditorState;
  end else
  begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
  end;
end;

end.

