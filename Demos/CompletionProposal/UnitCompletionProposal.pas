unit UnitCompletionProposal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, SynEdit32, SynEdit32.CompletionProposal;

const
  cCaseSensitive = 1;
  cAnsiStrings = 2;
  cPrettyText = 3;
  cInsertList = 4;
  cMatchedText = 5;

type
  TFormCompletionProposal = class(TForm)
    ButtonFont: TButton;
    ButtonTitleFont: TButton;
    ButtonUpdateInsertList: TButton;
    ButtonUpdateItemList: TButton;
    CheckBoxCaseSensitive: TCheckBox;
    CheckBoxLimitToMatchedText: TCheckBox;
    CheckBoxPrettyText: TCheckBox;
    CheckBoxUseInsertList: TCheckBox;
    CodeCompletion: TTabSheet;
    EditBiggestWord: TEdit;
    EditTitle: TEdit;
    FontDialog: TFontDialog;
    LabelBiggestWord: TLabel;
    LabelInsertList: TLabel;
    LabelItemList: TLabel;
    LabelTitle: TLabel;
    MemoInsert: TMemo;
    MemoItem: TMemo;
    PageControl: TPageControl;
    SynEdit32: TSynEdit32;
    SynEditCompletionProposal: TSynEdit32CompletionProposal;
    SynTest: TSynEdit32;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure EditBiggestWordChange(Sender: TObject);
    procedure ButtonUpdateInsertListClick(Sender: TObject);
    procedure ButtonUpdateItemListClick(Sender: TObject);
    procedure EditTitleChange(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure ButtonTitleFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  FormCompletionProposal: TFormCompletionProposal;

implementation

{$R *.DFM}

procedure TFormCompletionProposal.FormCreate(Sender: TObject);
begin
  with MemoInsert.Lines do
  begin
    Clear;
    Add('Create');
    Add('Destroy');
    Add('Add');
    Add('ClearLine');
    Add('Delete');
    Add('First');
    Add('GetMarksForLine');
    Add('Insert');
    Add('Last');
    Add('Place');
    Add('Remove');
    Add('WMCaptureChanged');
    Add('WMCopy');
    Add('WMCut');
    Add('WMDropFiles');
    Add('WMEraseBkgnd');
    Add('WMGetDlgCode');
    Add('WMHScroll');
    Add('WMPaste');
  end;

  with MemoItem.Lines do
  begin
    Clear;              
    Add('constructor \column{}\style{+B}Create\style{-B}(AOwner: TCustomSynEdit)');
    Add('destructor \column{}\style{+B}Destroy\style{-B}');
    Add('function \column{}\style{+B}Add\style{-B}(Item: TSynEditMark): Integer');
    Add('procedure \column{}\style{+B}ClearLine\style{-B}(line: integer)');
    Add('procedure \column{}\style{+B}Delete\style{-B}(Index: Integer)');
    Add('function \column{}\style{+B}First\style{-B}: TSynEditMark');
    Add('procedure \column{}\style{+B}GetMarksForLine\style{-B}(line: integer; var Marks: TSynEditMarks)');
    Add('procedure \column{}\style{+B}Insert\style{-B}(Index: Integer; Item: TSynEditMark)');
    Add('function \column{}\style{+B}Last\style{-B}: TSynEditMark');
    Add('procedure \column{}\style{+B}Place\style{-B}(mark: TSynEditMark)');
    Add('function \column{}\style{+B}Remove\style{-B}(Item: TSynEditMark): Integer');
    Add('procedure \column{}\style{+B}WMCaptureChanged\style{-B}(var Msg: TMessage); message WM_CAPTURECHANGED');
    Add('procedure \column{}\style{+B}WMCopy\style{-B}(var Message: TMessage); message WM_COPY');
    Add('procedure \column{}\style{+B}WMCut\style{-B}(var Message: TMessage); message WM_CUT');
    Add('procedure \column{}\style{+B}WMDropFiles\style{-B}(var Msg: TMessage); message WM_DROPFILES');
    Add('procedure \column{}\style{+B}WMEraseBkgnd\style{-B}(var Msg: TMessage); message WM_ERASEBKGND');
    Add('procedure \column{}\style{+B}WMGetDlgCode\style{-B}(var Msg: TWMGetDlgCode); message WM_GETDLGCODE');
    Add('procedure \column{}\style{+B}WMHScroll\style{-B}(var Msg: TWMScroll); message WM_HSCROLL');
    Add('procedure \column{}\style{+B}WMPaste\style{-B}(var Message: TMessage); message WM_PASTE');
  end;
  SynEditCompletionProposal.InsertList.AddStrings(MemoInsert.Lines);
  SynEditCompletionProposal.ItemList.AddStrings(MemoItem.Lines);
end;

procedure TFormCompletionProposal.CheckBoxClick(Sender: TObject);
var
  Options: TSynCompletionOptions;
begin
  if Sender is TCheckBox then
  begin
    Options := SynEditCompletionProposal.Options;
    if TCheckBox(Sender).Checked then
    begin
      case TCheckBox(Sender).Tag of
        cCaseSensitive : Include(Options, scoCaseSensitive);
        cPrettyText    : Include(Options, scoUsePrettyText);
        cInsertList    : Include(Options, scoUseInsertList);
        cMatchedText   : Include(Options, scoLimitToMatchedText);
      end;
    end else begin
      case TCheckBox(Sender).Tag of
        cCaseSensitive : Exclude(Options, scoCaseSensitive);
        cPrettyText    : Exclude(Options, scoUsePrettyText);
        cInsertList    : Exclude(Options, scoUseInsertList);
        cMatchedText   : Exclude(Options, scoLimitToMatchedText);
      end;
    end;
    SynEditCompletionProposal.Options := Options;
  end;
end;

procedure TFormCompletionProposal.EditBiggestWordChange(Sender: TObject);
begin
//TODO: set column width based on word length
//  SynEditCompletionProposal.Columns[0].BiggestWord := edBiggestWord.Text;
end;

procedure TFormCompletionProposal.ButtonUpdateInsertListClick(Sender: TObject);
begin
  SynEditCompletionProposal.InsertList.Clear;
  SynEditCompletionProposal.InsertList.AddStrings(MemoInsert.Lines);
end;

procedure TFormCompletionProposal.ButtonUpdateItemListClick(Sender: TObject);
begin
  SynEditCompletionProposal.ItemList.Clear;
  SynEditCompletionProposal.ItemList.AddStrings(MemoItem.Lines);
  SynEditCompletionProposal.ResetAssignedList;
end;

procedure TFormCompletionProposal.EditTitleChange(Sender: TObject);
begin
  SynEditCompletionProposal.Title := EditTitle.Text;
end;

procedure TFormCompletionProposal.ButtonFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(SynEditCompletionProposal.Font);
  if FontDialog.Execute then
    SynEditCompletionProposal.Font.Assign(FontDialog.Font);
end;

procedure TFormCompletionProposal.ButtonTitleFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(SynEditCompletionProposal.TitleFont);
  if FontDialog.Execute then
    SynEditCompletionProposal.TitleFont.Assign(FontDialog.Font);
end;

procedure TFormCompletionProposal.FormShow(Sender: TObject);
begin
  SynEdit32.SetFocus;
end;

end.
