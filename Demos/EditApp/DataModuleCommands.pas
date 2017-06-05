{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dmCommands.pas, released 2000-09-08.

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

$Id: dmCommands.pas,v 1.2 2000/11/22 08:34:13 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit DataModuleCommands;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Actions, SynEdit32.Highlighter.SQL, SynEdit32.Highlighter.Pas,
  SynEdit32.Highlighter, SynEdit32.Highlighter.Cpp;

type
  TCommandsDataModule = class(TDataModule)
    DialogFileOpen: TOpenDialog;
    ActionListMain: TActionList;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    ActionFileClose: TAction;
    ActionFilePrint: TAction;
    ActionEditCut: TAction;
    ActionEditCopy: TAction;
    ActionEditPaste: TAction;
    ActionEditDelete: TAction;
    ActionEditUndo: TAction;
    ActionEditRedo: TAction;
    ActionEditSelectAll: TAction;
    ActionSearchFind: TAction;
    ActionSearchFindNext: TAction;
    ActionSearchFindPrev: TAction;
    ActionSearchReplace: TAction;
    DialogFileSave: TSaveDialog;
    SynCppSyn: TSynEdit32HighlighterCpp;
    SynPasSyn: TSynEdit32HighlighterPas;
    SynSQLSyn: TSynEdit32HighlighterSQL;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionFileSaveUpdate(Sender: TObject);
    procedure ActionFileSaveAsExecute(Sender: TObject);
    procedure ActionFileSaveAsUpdate(Sender: TObject);
    procedure ActionFilePrintExecute(Sender: TObject);
    procedure ActionFilePrintUpdate(Sender: TObject);
    procedure ActionFileCloseExecute(Sender: TObject);
    procedure ActionFileCloseUpdate(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionEditCutUpdate(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditCopyUpdate(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure ActionEditPasteUpdate(Sender: TObject);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ActionEditDeleteUpdate(Sender: TObject);
    procedure ActionEditSelectAllExecute(Sender: TObject);
    procedure ActionEditSelectAllUpdate(Sender: TObject);
    procedure ActionEditRedoExecute(Sender: TObject);
    procedure ActionEditRedoUpdate(Sender: TObject);
    procedure ActionEditUndoExecute(Sender: TObject);
    procedure ActionEditUndoUpdate(Sender: TObject);
    procedure ActionSearchFindExecute(Sender: TObject);
    procedure ActionSearchFindUpdate(Sender: TObject);
    procedure ActionSearchFindNextExecute(Sender: TObject);
    procedure ActionSearchFindNextUpdate(Sender: TObject);
    procedure ActionSearchFindPrevExecute(Sender: TObject);
    procedure ActionSearchFindPrevUpdate(Sender: TObject);
    procedure ActionSearchReplaceExecute(Sender: TObject);
    procedure ActionSearchReplaceUpdate(Sender: TObject);
  private
    FHighlighters: TStringList;
    FMRUFiles: TStringList;
    FUntitledNumbers: TBits;
  public
    procedure AddMRUEntry(AFileName: string);
    function GetHighlighterForFile(AFileName: string): TSynEdit32CustomHighlighter;
    function GetMRUEntries: integer;
    function GetMRUEntry(Index: integer): string;
    function GetSaveFileName(var ANewName: string;
      AHighlighter: TSynEdit32CustomHighlighter): boolean;
    function GetUntitledNumber: integer;
    procedure ReleaseUntitledNumber(ANumber: integer);
    procedure RemoveMRUEntry(AFileName: string);
  end;

var
  CommandsDataModule: TCommandsDataModule;

implementation

{$R *.DFM}

uses
  UnitHighlighterProcs, UnitEditAppIntfs;

const
  MAX_MRU = 5;

resourcestring
  SFilterAllFiles = 'All files|*.*|';

{ TCommandsDataModule }

procedure TCommandsDataModule.DataModuleCreate(Sender: TObject);
begin
  FHighlighters := TStringList.Create;
  GetHighlighters(Self, FHighlighters, FALSE);
  DialogFileOpen.Filter := GetHighlightersFilter(FHighlighters) + SFilterAllFiles;
  FMRUFiles := TStringList.Create;
end;

procedure TCommandsDataModule.DataModuleDestroy(Sender: TObject);
begin
  FMRUFiles.Free;
  FHighlighters.Free;
  FUntitledNumbers.Free;
  CommandsDataModule := nil;
end;

// implementation

procedure TCommandsDataModule.AddMRUEntry(AFileName: string);
begin
  if AFileName <> '' then
  begin
    RemoveMRUEntry(AFileName);
    FMRUFiles.Insert(0, AFileName);
    while FMRUFiles.Count > MAX_MRU do
      FMRUFiles.Delete(FMRUFiles.Count - 1);
  end;
end;

function TCommandsDataModule.GetHighlighterForFile(
  AFileName: string): TSynEdit32CustomHighlighter;
begin
  if AFileName <> '' then
    Result := GetHighlighterFromFileExt(FHighlighters, ExtractFileExt(AFileName))
  else
    Result := nil;
end;

function TCommandsDataModule.GetMRUEntries: integer;
begin
  Result := FMRUFiles.Count;
end;

function TCommandsDataModule.GetMRUEntry(Index: integer): string;
begin
  if (Index >= 0) and (Index < FMRUFiles.Count) then
    Result := FMRUFiles[Index]
  else
    Result := '';
end;

function TCommandsDataModule.GetSaveFileName(var ANewName: string;
  AHighlighter: TSynEdit32CustomHighlighter): boolean;
begin
  with DialogFileSave do
  begin
    if ANewName <> '' then
    begin
      InitialDir := ExtractFileDir(ANewName);
      FileName := ExtractFileName(ANewName);
    end else
    begin
      InitialDir := '';
      FileName := '';
    end;
    if AHighlighter <> nil then
      Filter := AHighlighter.DefaultFilter
    else
      Filter := SFilterAllFiles;
    if Execute then
    begin
      ANewName := FileName;
      Result := TRUE;
    end else
      Result := FALSE;
  end;
end;

function TCommandsDataModule.GetUntitledNumber: integer;
begin
  if FUntitledNumbers = nil then
    FUntitledNumbers := TBits.Create;
  Result := FUntitledNumbers.OpenBit;
  if Result = FUntitledNumbers.Size then
    FUntitledNumbers.Size := FUntitledNumbers.Size + 32;
  FUntitledNumbers[Result] := TRUE;
  Inc(Result);
end;

procedure TCommandsDataModule.ReleaseUntitledNumber(ANumber: integer);
begin
  Dec(ANumber);
  if (FUntitledNumbers <> nil) and (ANumber >= 0)
    and (ANumber < FUntitledNumbers.Size)
  then
    FUntitledNumbers[ANumber] := FALSE;
end;

procedure TCommandsDataModule.RemoveMRUEntry(AFileName: string);
var
  i: integer;
begin
  for i := FMRUFiles.Count - 1 downto 0 do
  begin
    if CompareText(AFileName, FMRUFiles[i]) = 0 then
      FMRUFiles.Delete(i);
  end;
end;

procedure TCommandsDataModule.ActionFileSaveExecute(Sender: TObject);
begin
  if GFileCmds <> nil then
    GFileCmds.ExecSave;
end;

procedure TCommandsDataModule.ActionFileSaveUpdate(Sender: TObject);
begin
  ActionFileSave.Enabled := (GFileCmds <> nil) and GFileCmds.CanSave;
end;

procedure TCommandsDataModule.ActionFileSaveAsExecute(Sender: TObject);
begin
  if GFileCmds <> nil then
    GFileCmds.ExecSaveAs;
end;

procedure TCommandsDataModule.ActionFileSaveAsUpdate(Sender: TObject);
begin
  ActionFileSaveAs.Enabled := (GFileCmds <> nil) and GFileCmds.CanSaveAs;
end;

procedure TCommandsDataModule.ActionFilePrintExecute(Sender: TObject);
begin
  if GFileCmds <> nil then
    GFileCmds.ExecPrint;
end;

procedure TCommandsDataModule.ActionFilePrintUpdate(Sender: TObject);
begin
  ActionFilePrint.Enabled := (GFileCmds <> nil) and GFileCmds.CanPrint;
end;

procedure TCommandsDataModule.ActionFileCloseExecute(Sender: TObject);
begin
  if GFileCmds <> nil then
    GFileCmds.ExecClose;
end;

procedure TCommandsDataModule.ActionFileCloseUpdate(Sender: TObject);
begin
  ActionFileClose.Enabled := (GFileCmds <> nil) and GFileCmds.CanClose;
end;

procedure TCommandsDataModule.ActionEditCutExecute(Sender: TObject);
begin
  if GEditCmds <> nil then
    GEditCmds.ExecCut;
end;

procedure TCommandsDataModule.ActionEditCutUpdate(Sender: TObject);
begin
  ActionEditCut.Enabled := (GEditCmds <> nil) and GEditCmds.CanCut;
end;

procedure TCommandsDataModule.ActionEditCopyExecute(Sender: TObject);
begin
  if GEditCmds <> nil then
    GEditCmds.ExecCopy;
end;

procedure TCommandsDataModule.ActionEditCopyUpdate(Sender: TObject);
begin
  ActionEditCopy.Enabled := (GEditCmds <> nil) and GEditCmds.CanCopy;
end;

procedure TCommandsDataModule.ActionEditPasteExecute(Sender: TObject);
begin
  if GEditCmds <> nil then
    GEditCmds.ExecPaste;
end;

procedure TCommandsDataModule.ActionEditPasteUpdate(Sender: TObject);
begin
  ActionEditPaste.Enabled := (GEditCmds <> nil) and GEditCmds.CanPaste;
end;

procedure TCommandsDataModule.ActionEditDeleteExecute(Sender: TObject);
begin
  if GEditCmds <> nil then
    GEditCmds.ExecDelete;
end;

procedure TCommandsDataModule.ActionEditDeleteUpdate(Sender: TObject);
begin
  ActionEditDelete.Enabled := (GEditCmds <> nil) and GEditCmds.CanDelete;
end;

procedure TCommandsDataModule.ActionEditSelectAllExecute(Sender: TObject);
begin
  if GEditCmds <> nil then
    GEditCmds.ExecSelectAll;
end;

procedure TCommandsDataModule.ActionEditSelectAllUpdate(Sender: TObject);
begin
  ActionEditSelectAll.Enabled := (GEditCmds <> nil) and GEditCmds.CanSelectAll;
end;

procedure TCommandsDataModule.ActionEditRedoExecute(Sender: TObject);
begin
  if GEditCmds <> nil then
    GEditCmds.ExecRedo;
end;

procedure TCommandsDataModule.ActionEditRedoUpdate(Sender: TObject);
begin
  ActionEditRedo.Enabled := (GEditCmds <> nil) and GEditCmds.CanRedo;
end;

procedure TCommandsDataModule.ActionEditUndoExecute(Sender: TObject);
begin
  if GEditCmds <> nil then
    GEditCmds.ExecUndo;
end;

procedure TCommandsDataModule.ActionEditUndoUpdate(Sender: TObject);
begin
  ActionEditUndo.Enabled := (GEditCmds <> nil) and GEditCmds.CanUndo;
end;

procedure TCommandsDataModule.ActionSearchFindExecute(Sender: TObject);
begin
  if GSearchCmds <> nil then
    GSearchCmds.ExecFind;
end;

procedure TCommandsDataModule.ActionSearchFindUpdate(Sender: TObject);
begin
  ActionSearchFind.Enabled := (GSearchCmds <> nil) and GSearchCmds.CanFind;
end;

procedure TCommandsDataModule.ActionSearchFindNextExecute(Sender: TObject);
begin
  if GSearchCmds <> nil then
    GSearchCmds.ExecFindNext;
end;

procedure TCommandsDataModule.ActionSearchFindNextUpdate(Sender: TObject);
begin
  ActionSearchFindNext.Enabled := (GSearchCmds <> nil)
    and GSearchCmds.CanFindNext;
end;

procedure TCommandsDataModule.ActionSearchFindPrevExecute(Sender: TObject);
begin
  if GSearchCmds <> nil then
    GSearchCmds.ExecFindPrev;
end;

procedure TCommandsDataModule.ActionSearchFindPrevUpdate(Sender: TObject);
begin
  ActionSearchFindPrev.Enabled := (GSearchCmds <> nil) and GSearchCmds.CanFindPrev;
end;

procedure TCommandsDataModule.ActionSearchReplaceExecute(Sender: TObject);
begin
  if GSearchCmds <> nil then
    GSearchCmds.ExecReplace;
end;

procedure TCommandsDataModule.ActionSearchReplaceUpdate(Sender: TObject);
begin
  ActionSearchReplace.Enabled := (GSearchCmds <> nil)
    and GSearchCmds.CanReplace;
end;

end.

 
