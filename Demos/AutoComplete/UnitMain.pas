{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain1.pas, released 2000-06-25.

The Original Code is part of the AutoCompleteDemo1 project, written by
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

$Id: frmMain1.pas,v 1.2 2000/11/22 08:34:13 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit UnitMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, SynEdit32.Highlighter, SynEdit32.Highlighter.Pas, SynEdit32,
  SynEdit32.AutoComplete, SynEdit32.KeyCmds;

type
  TFormAutoComplete = class(TForm)
    MainMenu: TMainMenu;
    MenuItemNewForm: TMenuItem;
    SynEdit32: TSynEdit32;
    SynPasSyn: TSynEdit32HighlighterPas;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemNewFormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FAutoComplete: TSynEdit32AutoComplete;
  end;

var
  FormAutoComplete: TFormAutoComplete;

implementation

{$R *.DFM}

procedure TFormAutoComplete.FormCreate(Sender: TObject);
begin
  if Application.MainForm = nil then
  begin
    FAutoComplete := TSynEdit32AutoComplete.Create(Self);
    FAutoComplete.Editor := SynEdit32;
    FAutoComplete.AutoCompleteList.LoadFromFile('Delphi32.dci');
  end else
    FormAutoComplete.FAutoComplete.AddEditor(SynEdit32);
  SynEdit32.AddKey(ecAutoCompletion, word('J'), [ssCtrl], 0, []);
end;

procedure TFormAutoComplete.MenuItemNewFormClick(Sender: TObject);
begin
  with TFormAutoComplete.Create(Application) do
    Show;
end;

procedure TFormAutoComplete.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
