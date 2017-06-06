{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: pas2html.dpr, released 2000-06-23.

The Original Code is the pas2html project of the mwEdit component suite
by Martin Waldenburg and other developers.
The Original Author of the pas2html project is Primoz Gabrijelcic.
Portions written by Primoz Gabrijelcic are copyright 1999 Primoz Gabrijelcic.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: pas2html.dpr,v 1.2.2.2 2008/09/14 16:24:57 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$APPTYPE CONSOLE}

program Pascal2Html;

{$I SynEdit.inc}

uses                                                             
  Windows, Classes, Clipbrd,
  SynEdit32 in '..\..\Source\SynEdit32.pas',
  SynEdit32.Types in '..\..\Source\SynEdit32.Types.pas',
  SynEdit32.MiscClasses in '..\..\Source\SynEdit32.MiscClasses.pas',
  SynEdit32.KeyConst in '..\..\Source\SynEdit32.KeyConst.pas',
  SynEdit32.Unicode in '..\..\Source\SynEdit32.Unicode.pas',
  SynEdit32.TextBuffer in '..\..\Source\SynEdit32.TextBuffer.pas',
  SynEdit32.MiscProcs in '..\..\Source\SynEdit32.MiscProcs.pas',
  SynEdit32.RegExpr in '..\..\Source\SynEdit32.RegExpr.pas',
  SynEdit32.StrConst in '..\..\Source\SynEdit32.StrConst.pas',
  SynEdit32.KeyCmds in '..\..\Source\SynEdit32.KeyCmds.pas',
  SynEdit32.KbdHandler in '..\..\Source\SynEdit32.KbdHandler.pas',
  SynEdit32.WordWrap in '..\..\Source\SynEdit32.WordWrap.pas',
  SynEdit32.AutoComplete in '..\..\Source\SynEdit32.AutoComplete.pas',
  SynEdit32.Export in '..\..\Source\SynEdit32.Export.pas',
  SynEdit32.ExportHTML in '..\..\Source\SynEdit32.ExportHTML.pas',
  SynEdit32.HighlighterOptions in '..\..\Source\SynEdit32.HighlighterOptions.pas',
  SynEdit32.Highlighter in '..\..\Source\SynEdit32.Highlighter.pas',
  SynEdit32.Highlighter.Pas in '..\..\Source\SynEdit32.Highlighter.Pas.pas',
  SynEdit32.Highlighter.Multi in '..\..\Source\SynEdit32.Highlighter.Multi.pas';

var
  SettingsLines: TStrings;
  TextLines: TUnicodeStringList;
  Syn: TSynEdit32HighlighterPas;
  Exp: TSynEdit32ExporterHTML;
begin
  if ClipboardProvidesText then
  begin
    Syn := TSynEdit32HighlighterPas.Create(nil);
    try
      // get syntax highlighter settings
      SettingsLines := TStringList.Create;
      try
        Syn.EnumUserSettings(SettingsLines);
        if SettingsLines.Count > 0 then
          Syn.UseUserSettings(SettingsLines.Count - 1);
      finally
        SettingsLines.Free;
      end;

      TextLines := TUnicodeStringList.Create;
      try
        // load text from clipboard
        TextLines.Text := GetClipboardText;
        // export ALines to HTML, as HTML fragment in text format
        Exp := TSynEdit32ExporterHTML.Create(nil);
        try
          Exp.Highlighter := Syn;
          Exp.ExportAsText := True;
          Exp.CreateHTMLFragment := True;
          Exp.ExportAll(TextLines);
          Exp.CopyToClipboard;
        finally
          Exp.Free;
        end;
      finally
        TextLines.Free;
      end;
    finally
      Syn.Free;
    end;
  end;
end.

