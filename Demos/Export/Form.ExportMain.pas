{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmExportMain.pas, released 2000-06-23.

The Original Code is part of the ExportDemo project, written by Michael Hieke
for the SynEdit component suite.
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

$Id: frmExportMain.pas,v 1.2.2.1 2005/01/10 14:08:10 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit Form.ExportMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEdit, Menus, ComCtrls, SynEdit32.Export, SynEdit32.ExportRTF,
  SynEdit32.ExportHTML, SynEdit32.Highlighter, SynEdit32.Highlighter.Pas,
  SynEdit32.Highlighter.Dfm, SynEdit32.Highlighter.Cpp, SynEdit32;

type
  TForm1 = class(TForm)
    menuMain: TMainMenu;
    mFile: TMenuItem;
    miFileOpen: TMenuItem;
    N1: TMenuItem;
    miFileExit: TMenuItem;
    SynEdit32: TSynEdit32;
    dlgFileOpen: TOpenDialog;
    dlgFileSaveAs: TSaveDialog;
    mExport: TMenuItem;
    miExportToFile: TMenuItem;
    Statusbar: TStatusBar;
    SynEdit32ExporterHTML: TSynEdit32ExporterHTML;
    SynEdit32ExporterRTF: TSynEdit32ExporterRTF;
    miExportAsHTML: TMenuItem;
    miExportAsRTF: TMenuItem;
    miExportAllFormats: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    miExportClipboardNative: TMenuItem;
    miExportClipboardText: TMenuItem;
    SynEdit32HighlighterCpp: TSynEdit32HighlighterCpp;
    SynEdit32HighlighterDfm: TSynEdit32HighlighterDfm;
    SynEdit32HighlighterPas: TSynEdit32HighlighterPas;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miExportToFileClick(Sender: TObject);
    procedure mExportClick(Sender: TObject);
    procedure miExportAsClicked(Sender: TObject);
    procedure miExportClipboardNativeClick(Sender: TObject);
    procedure miExportClipboardTextClick(Sender: TObject);
  private
    FExportAs: integer;
    FHighlighters: TStringList;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  UnitHighlighterProcs, Clipbrd;

{ TForm1 }
  
procedure TForm1.FormCreate(Sender: TObject);
begin
  FHighlighters := TStringList.Create;
  FHighlighters.Sorted := TRUE;
  GetHighlighters(Self, FHighlighters, FALSE);
  dlgFileOpen.Filter := GetHighlightersFilter(FHighlighters) + 'All files|*.*|';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FHighlighters.Free;
end;

procedure TForm1.miFileOpenClick(Sender: TObject);
begin
  if dlgFileOpen.Execute then begin
    SynEdit32.Lines.LoadFromFile(dlgFileOpen.FileName);
    SynEdit32.Highlighter := GetHighlighterFromFileExt(FHighlighters,
      ExtractFileExt(dlgFileOpen.FileName));
    if Assigned(SynEdit32.Highlighter) then
      Statusbar.SimpleText := 'Using highlighter for ' +
        SynEdit32.Highlighter.GetFriendlyLanguageName
    else
      Statusbar.SimpleText := 'No highlighter assigned';
  end;
end;

procedure TForm1.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.mExportClick(Sender: TObject);
var
  HasText, IsEnabled: boolean;
  i: integer;
begin
  miExportAsHTML.Checked := FExportAs = 1;
  miExportAsRTF.Checked := FExportAs = 2;
  miExportAllFormats.Checked := FExportAs = 0;

  HasText := FALSE;
  for i := 0 to SynEdit32.Lines.Count - 1 do
    if SynEdit32.Lines[i] <> '' then begin
      HasText := TRUE;
      break;
    end;
  IsEnabled := HasText and Assigned(SynEdit32.Highlighter);
  miExportClipboardNative.Enabled := IsEnabled;
  IsEnabled := IsEnabled and (FExportAs > 0);
  miExportToFile.Enabled := IsEnabled;
  miExportClipboardText.Enabled := IsEnabled;
end;

procedure TForm1.miExportToFileClick(Sender: TObject);
var
  FileName: string;
  Exporter: TSynEdit32CustomExporter;
begin
  case FExportAs of
    1: dlgFileSaveAs.Filter := SynEdit32ExporterHTML.DefaultFilter;
    2: dlgFileSaveAs.Filter := SynEdit32ExporterRTF.DefaultFilter;
  end;
  if dlgFileSaveAs.Execute then begin
    Exporter := nil;
    FileName := dlgFileSaveAs.FileName;
    case FExportAs of
      1: begin
           if ExtractFileExt(FileName) = '' then
             FileName := FileName + '.html';
           Exporter := SynEdit32ExporterHTML;
         end;
      2: begin
           if ExtractFileExt(FileName) = '' then
             FileName := FileName + '.rtf';
           Exporter := SynEdit32ExporterRTF;
         end;
    end;
    if Assigned(Exporter) then with Exporter do begin
      Title := 'Source file exported to file';
      Highlighter := SynEdit32.Highlighter;
      ExportAsText := TRUE;
      ExportAll(SynEdit32.Lines);
      SaveToFile(FileName);
    end;
  end;
end;

procedure TForm1.miExportAsClicked(Sender: TObject);
begin
  if Sender = miExportAsHTML then
    FExportAs := 1
  else if Sender = miExportAsRTF then
    FExportAs := 2
  else
    FExportAs := 0;
end;

procedure TForm1.miExportClipboardNativeClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    Clipboard.AsText := SynEdit32.Lines.Text;
    // HTML?
    if FExportAs in [0, 1] then with SynEdit32ExporterHTML do begin
      Title := 'Source file exported to clipboard (native format)';
      ExportAsText := FALSE;
      Highlighter := SynEdit32.Highlighter;
      ExportAll(SynEdit32.Lines);
      CopyToClipboard;
    end;
    // RTF?
    if FExportAs in [0, 2] then with SynEdit32ExporterRTF do begin
      Title := 'Source file exported to clipboard (native format)';
      ExportAsText := FALSE;
      Highlighter := SynEdit32.Highlighter;
      ExportAll(SynEdit32.Lines);
      CopyToClipboard;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TForm1.miExportClipboardTextClick(Sender: TObject);
var
  Exporter: TSynEdit32CustomExporter;
begin
  Exporter := nil;
  case FExportAs of
    1: Exporter := SynEdit32ExporterHTML;
    2: Exporter := SynEdit32ExporterRTF;
  end;
  if Assigned(Exporter) then with Exporter do begin
    Title := 'Source file exported to clipboard (as text)';
    ExportAsText := TRUE;
    Highlighter := SynEdit32.Highlighter;
    ExportAll(SynEdit32.Lines);
    CopyToClipboard;
  end;
end;

end.
