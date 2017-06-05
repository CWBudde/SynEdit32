{-------------------------------------------------------------------------------
The purpose of this demo is to show how to implement the TSynMultiSyn control
allowing you to syntax highlight documents with many highlighters based on
schemes that you define.

I thought it would be a great help by setting up the AutoComplete and
Completion proposal. When running the demo use them to correctly insert the tags
that I setup the SynMultiSyn1.Schemes to look for as Start / End Expression.
--------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uMain.pas, released 2001-03-31.

The Original Code is part of the MultiSynDemo project, written by Leon Brown
for the SynEdit32 component suite.
All Rights Reserved.

Contributors to the SynEdit32 project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: uMain.pas,v 1.3 2004/01/22 03:37:47 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit32 home page,
located at http://SynEdit32.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit UnitMain;

{-$I SynEdit32.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ComCtrls, ExtCtrls, SynEdit32, SynEdit32.CompletionProposal,
  SynEdit32.Highlighter, SynEdit32.Highlighter.JScript,
  SynEdit32.Highlighter.HTML, SynEdit32.Highlighter.Css,
  SynEdit32.Highlighter.Multi;

type
  TfrmMain = class(TForm)
    About1: TMenuItem;
    Exit1: TMenuItem;
    File1: TMenuItem;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog: TOpenDialog;
    Save1: TMenuItem;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit32;
    Panel: TPanel;
    ListBox1: TListBox;
    Label1: TLabel;
    SynCompletionProposal: TSynEdit32CompletionProposal;
    SynAutoComplete: TSynEdit32AutoComplete;
    SynMultiSyn: TSynEdit32HighlighterMulti;
    SynHTMLSyn: TSynEdit32HighlighterHTML;
    SynCssSyn: TSynEdit32HighlighterCss;
    SynJScriptSyn: TSynEdit32HighlighterJScript;
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject;
      Changes: TSynEdit32StatusChanges);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  OpenDialog.Filter := SynMultiSyn.DefaultFilter;
  SaveDialog.Filter := SynMultiSyn.DefaultFilter;
  // Set SynMultiSyn1 Filter to HTML because that is the example I'm using.
  SynMultiSyn.DefaultFilter := SynHTMLSyn.DefaultFilter;

  // Load Completion and Autocomplete from user maintained text files
  SynCompletionProposal.ItemList.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + 'SynCP.txt');
  SynAutoComplete.AutoCompleteList.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + 'SynAC.txt');
end;

procedure TfrmMain.New1Click(Sender: TObject);
begin
  SynEdit.Clear;
end;

procedure TfrmMain.Open1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    SynEdit.Lines.LoadFromFile(OpenDialog.FileName);
    SynEdit.SetFocus;
  end;
end;

procedure TfrmMain.Save1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    SynEdit.Lines.SaveToFile(SaveDialog.FileName);
    SynEdit.SetFocus;
  end;
end;

procedure TfrmMain.SynEditStatusChange(Sender: TObject;
  Changes: TSynEdit32StatusChanges);
begin
  // caret position has changed
  if Changes * [scAll, scCaretX, scCaretY] <> [] then
  begin
    Statusbar.SimpleText := Format('Ln:%6d, Col:%3d',
      [SynEdit.CaretY, SynEdit.CaretX]);
  end;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  MessageDlg('MultiHighlight HTML Demo'#13#10'by'#13#10 +
    'Leon Brown - LeonBrown77@hotmail.com', mtInformation, [mbOk], 0);
  SynEdit.SetFocus;
end;


procedure TfrmMain.ListBox1DblClick(Sender: TObject);
begin
  { Want to add text to SynEdit32 to be used with AutoComplete }
  SynEdit.SelText := ListBox1.Items.Strings[ListBox1.ItemIndex];
  SynEdit.SetFocus;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  SynEdit.SetFocus;
end;

end.
