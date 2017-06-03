{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditReg.pas, released 2000-04-07.
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

$Id: SynEditReg.pas,v 1.33.2.2 2004/10/18 15:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.Reg;

{$I SynEdit.inc}

interface

uses
  // SynEdit components
  SynEdit32,
  SynEdit32.Memo,
  SynEdit32.DocumentManager,
  {$IFNDEF SYN_DELPHI_PE}
  SynEdit32.DBEdit,
  {$ENDIF}
  SynEdit32.StrConst,
  SynEdit32.Highlighter,
  SynEdit32.MiscClasses,
  SynEdit32.Plugins,
  SynEdit32.Export,
  SynEdit32.ExportHTML,
  SynEdit32.ExportRTF,
  SynEdit32.ExportTeX,
  SynEdit32.CompletionProposal,
  SynEdit32.PythonBehaviour,
  SynEdit32.Print,
  SynEdit32.PrintPreview,
  SynEdit32.MacroRecorder,
  SynEdit32.AutoCorrect,
  SynEdit32.Search,
  SynEdit32.RegexSearch,
  SynEdit32.HighlighterManager,
  SynEdit32.OptionsDialog,
  SynEdit32.Highlighter.Multi,
  SynEdit32.Highlighter.ADSP21xx,
  SynEdit32.Highlighter.Assembly,
  SynEdit32.Highlighter.AsmMASM,
  SynEdit32.Highlighter.AWK,
  SynEdit32.Highlighter.Baan,
  SynEdit32.Highlighter.Bat,
  SynEdit32.Highlighter.CAC,
  SynEdit32.Highlighter.Cache,
  SynEdit32.Highlighter.Cobol,
  SynEdit32.Highlighter.Cpp,
  SynEdit32.Highlighter.CS,
  SynEdit32.Highlighter.Css,
  SynEdit32.Highlighter.Dfm,
  SynEdit32.Highlighter.Dml,
  SynEdit32.Highlighter.DOT,
  {$ifdef SYN_DELPHI_2009_UP}
  SynEdit32.Highlighter.DWS,
  {$endif}
  SynEdit32.Highlighter.Eiffel,
  SynEdit32.Highlighter.Fortran,
  SynEdit32.Highlighter.Foxpro,
  SynEdit32.Highlighter.Galaxy,
  SynEdit32.Highlighter.General,
  SynEdit32.Highlighter.Go,
  SynEdit32.Highlighter.Haskell,
  SynEdit32.Highlighter.HC11,
  SynEdit32.Highlighter.HP48,
  SynEdit32.Highlighter.Html,
  SynEdit32.Highlighter.Ini,
  SynEdit32.Highlighter.Inno,
  SynEdit32.Highlighter.Java,
  SynEdit32.Highlighter.JScript,
  SynEdit32.Highlighter.JSON,
  SynEdit32.Highlighter.Kix,
  SynEdit32.Highlighter.Modelica,
  SynEdit32.Highlighter.M3,
  SynEdit32.Highlighter.Pas,
  SynEdit32.Highlighter.Perl,
  SynEdit32.Highlighter.PHP,
  SynEdit32.Highlighter.Progress,
  SynEdit32.Highlighter.Python,
  SynEdit32.Highlighter.RC,
  SynEdit32.Highlighter.Ruby,
  SynEdit32.Highlighter.Sml,
  SynEdit32.Highlighter.SQL,
  SynEdit32.Highlighter.TclTk,
  SynEdit32.Highlighter.TeX,
  SynEdit32.Highlighter.UNIXShellScript,
  SynEdit32.Highlighter.URI,
  SynEdit32.Highlighter.VB,
  SynEdit32.Highlighter.VBScript,
  SynEdit32.Highlighter.Vrml97,
  SynEdit32.Highlighter.GWS,
  SynEdit32.Highlighter.CPM,
  SynEdit32.Highlighter.SDD,
  SynEdit32.Highlighter.XML,
  SynEdit32.Highlighter.Msg,
  SynEdit32.Highlighter.IDL,
  SynEdit32.Highlighter.Unreal,
  SynEdit32.Highlighter.ST,
  SynEdit32.Highlighter.LDraw,
  SynEdit32.URIOpener,
  Classes;

procedure Register;

implementation

procedure Register;
begin
// SynEdit main components
  RegisterComponents(SYNS_ComponentsPage, [TSynEdit32, TSynEdit32Memo]);

{$IFNDEF SYN_DELPHI_PE}
  RegisterComponents(SYNS_ComponentsPage, [TDBSynEdit32]);
{$ENDIF}

{$IFDEF SYN_COMPILER_6_UP}
  GroupDescendentsWith(TSynEdit32CustomHighlighter, TSynEdit32);
  GroupDescendentsWith(TSynEdit32SearchCustom, TSynEdit32);
  GroupDescendentsWith(TSynEdit32CustomExporter, TSynEdit32);
  GroupDescendentsWith(TSynEdit32HighlighterMulti, TSynEdit32);
  GroupDescendentsWith(TSynEdit32BaseCompletionProposal, TSynEdit32);
  GroupDescendentsWith(TSynEdit32AutoComplete, TSynEdit32);
  GroupDescendentsWith(TAbstractSynEdit32Plugin, TSynEdit32);
  GroupDescendentsWith(TCustomSynEdit32AutoCorrect, TSynEdit32);
  GroupDescendentsWith(TSynEdit32Print, TSynEdit32);
  GroupDescendentsWith(TSynEdit32PrintPreview, TSynEdit32);
  GroupDescendentsWith(TSynEdit32PythonBehaviour, TSynEdit32);
  GroupDescendentsWith(TSynEdit32HighlighterManager, TSynEdit32);
  GroupDescendentsWith(TSynEdit32OptionsDialog, TSynEdit32);
  GroupDescendentsWith(TSynEdit32URIOpener, TSynEdit32);
{$ENDIF}

// SynEdit extra components
  RegisterComponents(SYNS_ComponentsPage, [TSynEdit32ExporterHTML,
    TSynEdit32ExporterRTF, TSynEdit32ExporterTeX, TSynEdit32PythonBehaviour,
    TSynEdit32HighlighterMulti, TSynEdit32CompletionProposal,
    TSynEdit32AutoComplete, TSynEdit32MacroRecorder, TSynEdit32Print,
    TSynEdit32PrintPreview, TSynEdit32AutoCorrect, TSynEdit32Search,
    TSynEdit32RegexSearch, TSynEdit32OptionsDialog, TSynEdit32URIOpener,
    TSynEdit32DocumentManager]);
{$IFDEF SYN_COMPILER_4_UP}
  RegisterComponents(SYNS_ComponentsPage, [TSynEdit32HighlighterManager]);
{$ENDIF}

// SynEdit highlighters
  RegisterComponents(SYNS_HighlightersPage, [
    //classic
    TSynEdit32HighlighterCpp, TSynEdit32HighlighterEiffel,
    TSynEdit32HighlighterFortran, TSynEdit32HighlighterGeneral,
    TSynEdit32HighlighterJava,
    TSynEdit32HighlighterM3, TSynEdit32HighlighterPas, TSynEdit32HighlighterVB,
    TSynEdit32HighlighterCobol, TSynEdit32HighlighterCS,
    TSynEdit32HighlighterGo,
    // internet
    TSynEdit32HighlighterCss, TSynEdit32HighlighterHTML,
    TSynEdit32HighlighterJScript, TSynEdit32HighlighterPHP,
    TSynEdit32HighlighterVBScript, TSynEdit32HighlighterXML,
    TSynEdit32HighlighterJSON, TSynEdit32HighlighterVrml97,
    //interpreted
    TSynEdit32HighlighterAWK, TSynEdit32HighlighterBAT,
    {$ifdef SYN_DELPHI_2009_UP}
    TSynEdit32HighlighterDWS,
    {$endif}
    TSynEdit32HighlighterKix, TSynEdit32HighlighterPerl,
    TSynEdit32HighlighterPython,
    TSynEdit32HighlighterTclTk, TSynEdit32HighlighterGWScript,
    TSynEdit32HighlighterRuby, TSynEdit32HighlighterUNIXShellScript,
    //database
    TSynEdit32HighlighterCAC, TSynEdit32HighlighterCache,
    TSynEdit32HighlighterFoxpro, TSynEdit32HighlighterSQL,
    TSynEdit32HighlighterSDD,
    //assembler
    TSynEdit32HighlighterADSP21xx, TSynEdit32HighlighterAsm,
    TSynEdit32HighlighterHC11, TSynEdit32HighlighterHP48,
    TSynEdit32HighlighterST, TSynEdit32HighlighterAsmMASM,
    //data modeling
    TSynEdit32HighlighterDml, TSynEdit32HighlighterModelica,
    TSynEdit32HighlighterSML,
    //data
    TSynEdit32HighlighterDfm, TSynEdit32HighlighterIni,
    TSynEdit32HighlighterInno,
    // other
    TSynEdit32HighlighterBaan, TSynEdit32HighlighterGalaxy,
    TSynEdit32HighlighterProgress, TSynEdit32HighlighterMsg,
    TSynEdit32HighlighterIdl, TSynEdit32HighlighterUnreal,
    TSynEdit32HighlighterCPM, TSynEdit32HighlighterTeX,
    TSynEdit32HighlighterHaskell, TSynEdit32HighlighterLDR,
    TSynEdit32HighlighterURI, TSynEdit32HighlighterDOT,
    TSynEdit32HighlighterRC
  ]);
end;

end.
