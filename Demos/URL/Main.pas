unit Main;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit32, SynEdit32.Highlighter, SynEdit32.Highlighter.URI, 
  SynEdit32.URIOpener;

type
  TFormMain = class(TForm)
    SynURIOpener: TSynEdit32URIOpener;
    SynURISyn: TSynEdit32HighlighterURI;
    SynEdit: TSynEdit32;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

end.
