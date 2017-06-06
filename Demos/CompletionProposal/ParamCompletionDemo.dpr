program ParamCompletionDemo;

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
  SynEdit32.KeyCmds in '..\..\Source\SynEdit32.KeyCmds.pas',
  SynEdit32.KbdHandler in '..\..\Source\SynEdit32.KbdHandler.pas',
  SynEdit32.WordWrap in '..\..\Source\SynEdit32.WordWrap.pas',
  SynEdit32.HighlighterOptions in '..\..\Source\SynEdit32.HighlighterOptions.pas',
  SynEdit32.Highlighter in '..\..\Source\SynEdit32.Highlighter.pas',
  SynEdit32.Highlighter.Pas in '..\..\Source\SynEdit32.Highlighter.Pas.pas',
  SynEdit32.Highlighter.Multi in '..\..\Source\SynEdit32.Highlighter.Multi.pas',
  SynEdit32.CompletionProposal in '..\..\Source\SynEdit32.CompletionProposal.pas',
  UnitParameterProposal in 'UnitParameterProposal.pas' {FormParameterSuggestions};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormParameterSuggestions, FormParameterSuggestions);
  Application.Run;
end.
