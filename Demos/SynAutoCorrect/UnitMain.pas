unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ShellApi, ComCtrls, ImgList,
  SynEdit32, SynEdit32.AutoCorrect;

type
  TFormAutoCorrect = class(TForm)
    AutoCorrect: TSynEdit32AutoCorrect;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    ButtonDone: TButton;
    ButtonEdit: TButton;
    CheckBoxCorrectOnMouseDown: TCheckBox;
    CheckBoxEnabled: TCheckBox;
    CheckBoxIgnoreCase: TCheckBox;
    CheckBoxMaintainCase: TCheckBox;
    CheckBoxPlaySound: TCheckBox;
    DialogOpen: TOpenDialog;
    SynEdit: TSynEdit32;
    ImageListPages: TImageList;
    LabelCorrection: TLabel;
    LabelNote: TLabel;
    LabelOriginal: TLabel;
    ListBoxItems: TListBox;
    PageControl: TPageControl;
    PanelMessage: TPanel;
    TabEditor: TTabSheet;
    TabOptions: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AutoCorrectCorrected(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonDoneClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure CheckBoxCorrectOnMouseDownClick(Sender: TObject);
    procedure CheckBoxEnabledClick(Sender: TObject);
    procedure CheckBoxIgnoreCaseClick(Sender: TObject);
    procedure CheckBoxMaintainCaseClick(Sender: TObject);
    procedure ListBoxItemsClick(Sender: TObject);
    procedure ListBoxItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

var
  FormAutoCorrect: TFormAutoCorrect;

implementation

{$R *.DFM}

{ TFormAutoCorrect }

procedure TFormAutoCorrect.FormCreate(Sender: TObject);
begin
  { Load from registry. }
  AutoCorrect.LoadFromRegistry(HKEY_CURRENT_USER,
    'Software\Aerodynamica\Components\SynAutoCorrect\Demo');
  ListBoxItems.Items.Assign(AutoCorrect.Items);
end;

procedure TFormAutoCorrect.ButtonAddClick(Sender: TObject);
var
  Original, Correction: String;

begin
  if InputQuery('Add...', 'Original:', Original) then
    InputQuery('Add...', 'Correction:', Correction)
  else
    Exit;

  with AutoCorrect do
  begin
    if (Original <> '') and (Correction <> '') then
    begin
      Add(Original, Correction);
      ListBoxItems.Items.Assign(AutoCorrect.Items);
    end;
  end;

  { Update buttons. }
  ButtonDelete.Enabled := ListBoxItems.ItemIndex > -1;
  ButtonEdit.Enabled := ListBoxItems.ItemIndex > -1;
end;

procedure TFormAutoCorrect.ButtonDeleteClick(Sender: TObject);
begin
  { Error if nothing is selected. }
  if ListBoxItems.ItemIndex < 0 then
  begin
    MessageBox(0, 'Please select an item before executing this command!',
      'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  AutoCorrect.Delete(ListBoxItems.ItemIndex);
  ListBoxItems.Items.Assign(AutoCorrect.Items);

  { Update buttons. }
  ButtonDelete.Enabled := ListBoxItems.ItemIndex > -1;
  ButtonEdit.Enabled := ListBoxItems.ItemIndex > -1;
end;

procedure TFormAutoCorrect.ButtonEditClick(Sender: TObject);
var
  Original, Correction, CurrText: string;

begin
  { Error if nothing is selected. }
  if ListBoxItems.ItemIndex < 0 then
  begin
    MessageBox(0, 'Please select an item before executing this command!',
      'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  { Get current item. }
  CurrText := AutoCorrect.Items[ListBoxItems.ItemIndex];
  Original := AutoCorrect.HalfString(CurrText, True);
  Correction := AutoCorrect.HalfString(CurrText, False);

  if InputQuery('Edit...', 'Original:', Original) then
    InputQuery('Edit...', 'Correction:', Correction)
  else
    Exit;

  with AutoCorrect do
  begin
    Edit(ListBoxItems.ItemIndex, Original, Correction);
    ListBoxItems.Items.Assign(AutoCorrect.Items);
  end;

  { Update buttons. }
  ButtonDelete.Enabled := ListBoxItems.ItemIndex > -1;
  ButtonEdit.Enabled := ListBoxItems.ItemIndex > -1;
end;

procedure TFormAutoCorrect.CheckBoxEnabledClick(Sender: TObject);
begin
  AutoCorrect.Enabled := CheckBoxEnabled.Checked;
end;

procedure TFormAutoCorrect.CheckBoxIgnoreCaseClick(Sender: TObject);
begin
  if CheckBoxIgnoreCase.Checked then
    AutoCorrect.Options := AutoCorrect.Options + [ascoIgnoreCase]
  else
    AutoCorrect.Options := AutoCorrect.Options - [ascoIgnoreCase];
end;

procedure TFormAutoCorrect.CheckBoxMaintainCaseClick(Sender: TObject);
begin
  if CheckBoxMaintainCase.Checked then
    AutoCorrect.Options := AutoCorrect.Options + [ascoMaintainCase]
  else
    AutoCorrect.Options := AutoCorrect.Options - [ascoMaintainCase];
end;

procedure TFormAutoCorrect.FormResize(Sender: TObject);
begin
  if Height < 435 then
    Height := 435;
  if Width < 568 then
    Width := 568;
end;

procedure TFormAutoCorrect.ListBoxItemsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  s: string;

begin
  { Owner-drawn stuff. }
  s := ListBoxItems.Items[Index];
  with ListBoxItems do
  begin
    Canvas.FillRect(Rect);
    Canvas.TextOut(Rect.Left + 2, Rect.Top, AutoCorrect.HalfString(s, True));
    Canvas.TextOut(Rect.Left + (ListBoxItems.ClientWidth div 2) + 2, Rect.Top,
      AutoCorrect.HalfString(s, False));

    { Repaint separator. }
    FormPaint(nil);
  end;
end;

procedure TFormAutoCorrect.ListBoxItemsClick(Sender: TObject);
begin
  { Disable buttons. }
  ButtonDelete.Enabled := ListBoxItems.ItemIndex > -1;
  ButtonEdit.Enabled := ListBoxItems.ItemIndex > -1;
end;

procedure TFormAutoCorrect.FormDestroy(Sender: TObject);
begin
  { Save to registry. }
  AutoCorrect.SaveToRegistry(HKEY_CURRENT_USER,
    'Software\Aerodynamica\Components\SynAutoCorrect\Demo');
end;

procedure TFormAutoCorrect.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Capture shortcut for AutoCorrectAll. }
  if (ssCtrl in Shift) and (Key = Word('Q')) then AutoCorrect.AutoCorrectAll;
end;

procedure TFormAutoCorrect.CheckBoxCorrectOnMouseDownClick(Sender: TObject);
begin
  if CheckBoxCorrectOnMouseDown.Checked then
    AutoCorrect.Options := AutoCorrect.Options + [ascoCorrectOnMouseDown]
  else
    AutoCorrect.Options := AutoCorrect.Options - [ascoCorrectOnMouseDown];
end;

{ Demonstration of user events (it was broken in previous releases). }
procedure TFormAutoCorrect.SynEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PanelMessage.Caption := 'User event handler: MouseDown';
end;

procedure TFormAutoCorrect.SynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  PanelMessage.Caption := 'User event handler: KeyDown';
end;

procedure TFormAutoCorrect.FormPaint(Sender: TObject);
begin
  { Paints the line in the middle of the listbox. }
  with ListBoxItems.Canvas do
  begin
    Pen.Color := clBlack;
    PenPos := Point(ListBoxItems.Width div 2 - 8, 0);
    LineTo(ListBoxItems.Width div 2 - 8, ListBoxItems.Height);
  end;
end;

procedure TFormAutoCorrect.FormShow(Sender: TObject);
begin
  Invalidate;
end;

procedure TFormAutoCorrect.ButtonDoneClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAutoCorrect.AutoCorrectCorrected(Sender: TObject);
begin
  if CheckBoxPlaySound.Checked then
    MessageBeep(0);
end;

end.
