{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EditAppSDI.dpr, released 2000-09-08.

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

$Id: EditAppSDI.dpr,v 1.1 2000/09/16 07:34:43 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

program EditApp;

uses
  Forms,
  SynEdit32 in '..\..\Source\SynEdit32.pas',
  SynEdit32.Types in '..\..\Source\SynEdit32.Types.pas',
  SynEdit32.MiscClasses in '..\..\Source\SynEdit32.MiscClasses.pas',
  SynEdit32.KeyConst in '..\..\Source\SynEdit32.KeyConst.pas',
  SynEdit32.Unicode in '..\..\Source\SynEdit32.Unicode.pas',
  SynEdit32.TextBuffer in '..\..\Source\SynEdit32.TextBuffer.pas',
  SynEdit32.MiscProcs in '..\..\Source\SynEdit32.MiscProcs.pas',
  SynEdit32.RegExpr in '..\..\Source\SynEdit32.RegExpr.pas',
  SynEdit32.StrConst in '..\..\Source\SynEdit32.StrConst.pas',
  SynEdit32.TextDrawer in '..\..\Source\SynEdit32.TextDrawer.pas',
  SynEdit32.KeyCmds in '..\..\Source\SynEdit32.KeyCmds.pas',
  SynEdit32.KbdHandler in '..\..\Source\SynEdit32.KbdHandler.pas',
  SynEdit32.WordWrap in '..\..\Source\SynEdit32.WordWrap.pas',
  SynEdit32.AutoComplete in '..\..\Source\SynEdit32.AutoComplete.pas',
  SynEdit32.Search in '..\..\Source\SynEdit32.Search.pas',
  SynEdit32.HighlighterOptions in '..\..\Source\SynEdit32.HighlighterOptions.pas',
  SynEdit32.Highlighter in '..\..\Source\SynEdit32.Highlighter.pas',
  SynEdit32.Highlighter.HashEntries in '..\..\Source\SynEdit32.Highlighter.HashEntries.pas',
  SynEdit32.Highlighter.CPP in '..\..\Source\SynEdit32.Highlighter.CPP.pas',
  SynEdit32.Highlighter.Pas in '..\..\Source\SynEdit32.Highlighter.Pas.pas',
  SynEdit32.Highlighter.SQL in '..\..\Source\SynEdit32.Highlighter.SQL.pas',
  SynEdit32.Highlighter.Bat in '..\..\Source\SynEdit32.Highlighter.Bat.pas',
  SynEdit32.Highlighter.CS in '..\..\Source\SynEdit32.Highlighter.CS.pas',
  SynEdit32.Highlighter.Css in '..\..\Source\SynEdit32.Highlighter.Css.pas',
  SynEdit32.Highlighter.Html in '..\..\Source\SynEdit32.Highlighter.Html.pas',
  SynEdit32.Highlighter.JSON in '..\..\Source\SynEdit32.Highlighter.JSON.pas',
  SynEdit32.Highlighter.JScript in '..\..\Source\SynEdit32.Highlighter.JScript.pas',
  SynEdit32.Highlighter.Multi in '..\..\Source\SynEdit32.Highlighter.Multi.pas',
  DataModuleCommands in 'DataModuleCommands.pas' {CommandsDataModule: TDataModule},
  Form.Editor in 'Form.Editor.pas' {EditorForm},
  Form.Main in 'Form.Main.pas' {MainForm},
  Form.MainSDI in 'Form.MainSDI.pas' {SDIMainForm},
  UnitEditAppIntfs in 'UnitEditAppIntfs.pas',
  UnitHighlighterProcs in '..\UnitHighlighterProcs.pas',
  Dialog.SearchText in '..\SearchReplace\Dialog.SearchText.pas' {TextSearchDialog},
  Dialog.ReplaceText in '..\SearchReplace\Dialog.ReplaceText.pas' {TextReplaceDialog},
  Dialog.ConfirmReplace in '..\SearchReplace\Dialog.ConfirmReplace.pas' {ConfirmReplaceDialog};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SynEdit Demo';
  Application.CreateForm(TSDIMainForm, SDIMainForm);
  Application.Run;
end.
