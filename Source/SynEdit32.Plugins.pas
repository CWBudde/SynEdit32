{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPlugins.pas, released 2001-10-17.

Author of this file is Flávio Etrusco.
Portions created by Flávio Etrusco are Copyright 2001 Flávio Etrusco.
Unicode translation by Maël Hörz.
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

$Id: SynEditPlugins.pas,v 1.8.2.2 2008/09/14 16:24:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.Plugins;

{$I SynEdit.inc}

interface

uses
  {$IFDEF SYN_COMPILER_17_UP}
  Types,
  {$ENDIF}
  Windows,
  Menus,
  SynEdit32,
  SynEdit32.KeyCmds,
  SynEdit32.Unicode,
  Classes;

type
  TAbstractSynPlugin = class(TComponent)
  private
    procedure SetEditor(const Value: TCustomSynEdit32);
    function GetEditors(aIndex: integer): TCustomSynEdit32;
    function GetEditor: TCustomSynEdit32;
    function GetEditorCount: integer;
  protected
    FEditors: TList;
    procedure Notification(aComponent: TComponent;
      aOperation: TOperation); override;
    procedure DoAddEditor(aEditor: TCustomSynEdit32); virtual;
    procedure DoRemoveEditor(aEditor: TCustomSynEdit32); virtual;
    function AddEditor(aEditor: TCustomSynEdit32): integer;
    function RemoveEditor(aEditor: TCustomSynEdit32): integer;
  public
    destructor Destroy; override;
    property Editors[aIndex: integer]: TCustomSynEdit32 read GetEditors;
    property EditorCount: integer read GetEditorCount;
  published
    property Editor: TCustomSynEdit32 read GetEditor write SetEditor;
  end;

  TAbstractSynHookerPlugin = class(TAbstractSynPlugin)
  protected
    procedure HookEditor(aEditor: TCustomSynEdit32; aCommandID: TSynEditorCommand;
      aOldShortCut, aNewShortCut: TShortCut);
    procedure UnHookEditor(aEditor: TCustomSynEdit32;
      aCommandID: TSynEditorCommand; aShortCut: TShortCut);
    procedure OnCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: pointer; HandlerData: pointer); virtual; abstract;
  end;

  TPluginState = (psNone, psExecuting, psAccepting, psCancelling);

  TAbstractSynSingleHookPlugin = class(TAbstractSynHookerPlugin)
  private
    FCommandID: TSynEditorCommand;
    function IsShortCutStored: Boolean;
    procedure SetShortCut(const Value: TShortCut);
  protected
    FState: TPluginState;
    FCurrentEditor: TCustomSynEdit32;
    FShortCut: TShortCut;
    class function DefaultShortCut: TShortCut; virtual;
    procedure DoAddEditor(aEditor: TCustomSynEdit32); override;
    procedure DoRemoveEditor(aEditor: TCustomSynEdit32); override;
    {}
    procedure DoExecute; virtual; abstract;
    procedure DoAccept; virtual; abstract;
    procedure DoCancel; virtual; abstract;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property CommandID: TSynEditorCommand read FCommandID;
    property CurrentEditor: TCustomSynEdit32 read FCurrentEditor;
    function Executing: boolean;
    procedure Execute(aEditor: TCustomSynEdit32);
    procedure Accept;
    procedure Cancel;
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut
      stored IsShortCutStored;
  end;

  { use TAbstractSynCompletion for non-visual completion }

  TAbstractSynCompletion = class(TAbstractSynSingleHookPlugin)
  protected
    FCurrentString: UnicodeString;
  protected
    procedure SetCurrentString(const Value: UnicodeString); virtual;
    procedure OnCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: pointer; HandlerData: pointer); override;
    procedure DoExecute; override;
    procedure DoAccept; override;
    procedure DoCancel; override;
    function GetCurrentEditorString: UnicodeString; virtual;
  public
    procedure AddEditor(aEditor: TCustomSynEdit32);
    property CurrentString: UnicodeString read FCurrentString write SetCurrentString;
  end;

function NewPluginCommand: TSynEditorCommand;
procedure ReleasePluginCommand(aCmd: TSynEditorCommand);

implementation

uses
  Forms,
  SynEdit32.Types,
  SynEdit32.MiscProcs,
  SynEdit32.StrConst,
  SysUtils;

const
  ecPluginBase = 64000;

var
  gCurrentCommand: integer = ecPluginBase;

function NewPluginCommand: TSynEditorCommand;
begin
  Result := gCurrentCommand;
  Inc(gCurrentCommand);
end;

procedure ReleasePluginCommand(aCmd: TSynEditorCommand);
begin
  if aCmd = Pred(gCurrentCommand) then
    gCurrentCommand := aCmd;
end;

{ TAbstractSynPlugin }

function TAbstractSynPlugin.AddEditor(aEditor: TCustomSynEdit32): integer;
begin
  if FEditors = nil then
  begin
    FEditors := TList.Create;
  end
  else
    if FEditors.IndexOf(aEditor) >= 0 then
    begin
      Result := -1;
      Exit;
    end;
  aEditor.FreeNotification(Self);
  Result := FEditors.Add(aEditor);
  DoAddEditor(aEditor);
end;

destructor TAbstractSynPlugin.Destroy;
begin
  { RemoveEditor will free FEditors when it reaches count = 0}
  while Assigned(FEditors) do
    RemoveEditor(Editors[0]);
  inherited;
end;

procedure TAbstractSynPlugin.Notification(aComponent: TComponent;
  aOperation: TOperation);
begin
  inherited;
  if aOperation = opRemove then
  begin
    if (aComponent = Editor) or (aComponent is TCustomSynEdit32) then
      RemoveEditor(TCustomSynEdit32(aComponent));
  end;
end;

procedure TAbstractSynPlugin.DoAddEditor(aEditor: TCustomSynEdit32);
begin

end;

procedure TAbstractSynPlugin.DoRemoveEditor(aEditor: TCustomSynEdit32);
begin

end;

function TAbstractSynPlugin.RemoveEditor(aEditor: TCustomSynEdit32): integer;
begin
  if FEditors = nil then
  begin
    Result := -1;
    Exit;
  end;
  Result := FEditors.Remove(aEditor);
  //aEditor.RemoveFreeNotification(Self);
  if FEditors.Count = 0 then
  begin
    FEditors.Free;
    FEditors := nil;
  end;
  if Result >= 0 then
    DoRemoveEditor(aEditor);
end;

procedure TAbstractSynPlugin.SetEditor(const Value: TCustomSynEdit32);
var
  iEditor: TCustomSynEdit32;
begin
  iEditor := Editor;
  if iEditor <> Value then
  try
    if (iEditor <> nil) and (FEditors.Count = 1) then
      RemoveEditor(iEditor);
    if Value <> nil then
      AddEditor(Value);
  except
    if [csDesigning] * ComponentState = [csDesigning] then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

function TAbstractSynPlugin.GetEditors(aIndex: integer): TCustomSynEdit32;
begin
  Result := TCustomSynEdit32(FEditors[aIndex]);
end;

function TAbstractSynPlugin.GetEditor: TCustomSynEdit32;
begin
  if FEditors <> nil then
    Result := FEditors[0]
  else
    Result := nil;
end;

function TAbstractSynPlugin.GetEditorCount: integer;
begin
  if FEditors <> nil then
    Result := FEditors.Count
  else
    Result := 0;
end;

{ TAbstractSynHookerPlugin }

procedure TAbstractSynHookerPlugin.HookEditor(aEditor: TCustomSynEdit32;
  aCommandID: TSynEditorCommand; aOldShortCut, aNewShortCut: TShortCut);
var
  iIndex: integer;
  iKeystroke: TSynEdit32KeyStroke;
begin
  Assert(aNewShortCut <> 0);
  { shortcurts aren't created while in design-time }
  if [csDesigning] * ComponentState = [csDesigning] then
  begin
    if TSynEdit(aEditor).Keystrokes.FindShortcut(aNewShortCut) >= 0 then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut)
    else
      Exit;
  end;
  { tries to update old Keystroke }
  if aOldShortCut <> 0 then
  begin
    iIndex := TSynEdit(aEditor).Keystrokes.FindShortcut(aOldShortCut);
    if (iIndex >= 0) then
    begin
      iKeystroke := TSynEdit(aEditor).Keystrokes[iIndex];
      if iKeystroke.Command = aCommandID then
      begin
        iKeystroke.ShortCut := aNewShortCut;
        Exit;
      end;
    end;
  end;
  { new Keystroke }
  iKeystroke := TSynEdit(aEditor).Keystrokes.Add;
  try
    iKeystroke.ShortCut := aNewShortCut;
  except
    iKeystroke.Free;
    raise;
  end;
  iKeystroke.Command := aCommandID;
  aEditor.RegisterCommandHandler(OnCommand, Self);
end;

procedure TAbstractSynHookerPlugin.UnHookEditor(aEditor: TCustomSynEdit32;
  aCommandID: TSynEditorCommand; aShortCut: TShortCut);
var
  iIndex: integer;
begin
  aEditor.UnregisterCommandHandler(OnCommand);
  iIndex := TSynEdit(aEditor).Keystrokes.FindShortcut(aShortCut);
  if (iIndex >= 0) and
    (TSynEdit(aEditor).Keystrokes[iIndex].Command = aCommandID) then
    TSynEdit(aEditor).Keystrokes[iIndex].Free;
end;

{ TAbstractSynHookerPlugin }

procedure TAbstractSynSingleHookPlugin.Accept;
begin
  FState := psAccepting;
  try
    DoAccept;
  finally
    FCurrentEditor := nil;
    FState := psNone;
  end;
end;

procedure TAbstractSynSingleHookPlugin.Cancel;
begin
  FState := psCancelling;
  try
    DoCancel;
  finally
    FCurrentEditor := nil;
    FState := psNone;
  end;
end;

constructor TAbstractSynSingleHookPlugin.Create(aOwner: TComponent);
begin
  inherited;
  FCommandID := NewPluginCommand;
  FShortCut := DefaultShortCut;
end;

class function TAbstractSynSingleHookPlugin.DefaultShortCut: TShortCut;
begin
  Result := 0;
end;

destructor TAbstractSynSingleHookPlugin.Destroy;
begin
  if Executing then
    Cancel;
  ReleasePluginCommand(CommandID);
  inherited;
end;

procedure TAbstractSynSingleHookPlugin.DoAddEditor(
  aEditor: TCustomSynEdit32);
begin
  if ShortCut <> 0 then
    HookEditor(aEditor, CommandID, 0, ShortCut);
end;

procedure TAbstractSynSingleHookPlugin.Execute(aEditor: TCustomSynEdit32);
begin
  if Executing then
    Cancel;
  Assert(FCurrentEditor = nil);
  FCurrentEditor := aEditor;
  Assert(FState = psNone);
  FState := psExecuting;
  try
    DoExecute;
  except
    Cancel;
    raise;
  end;
end;

function TAbstractSynSingleHookPlugin.Executing: boolean;
begin
  Result := FState = psExecuting;
end;

function TAbstractSynSingleHookPlugin.IsShortCutStored: Boolean;
begin
  Result := FShortCut <> DefaultShortCut;
end;

procedure TAbstractSynSingleHookPlugin.DoRemoveEditor(aEditor: TCustomSynEdit32);
begin
  if ShortCut <> 0 then
    UnHookEditor(aEditor, CommandID, ShortCut);
  if Executing and (CurrentEditor = aEditor) then
    Cancel;
end;

procedure TAbstractSynSingleHookPlugin.SetShortCut(const Value: TShortCut);
var
  cEditor: integer;
begin
  if FShortCut <> Value then
  begin
    if Assigned(FEditors) then
      if Value <> 0 then
      begin
        for cEditor := 0 to FEditors.Count -1 do
          HookEditor(Editors[cEditor], CommandID, FShortCut, Value);
      end
      else
      begin
        for cEditor := 0 to FEditors.Count -1 do
          UnHookEditor(Editors[cEditor], CommandID, FShortCut);
      end;
    FShortCut := Value;
  end;
end;

{ TAbstractSynCompletion }

function TAbstractSynCompletion.GetCurrentEditorString: UnicodeString;
var
  S: UnicodeString;
  Col: integer;
begin
  S := CurrentEditor.LineText;
  if (CurrentEditor.CaretX > 1) and
    (CurrentEditor.CaretX - 1 <= Length(S)) then
  begin
    for Col := CurrentEditor.CaretX - 1 downto 1 do
      if not CurrentEditor.IsIdentChar(S[Col])then
        break;
    Result := Copy(S, Col + 1, CurrentEditor.CaretX - Col - 1);
  end;
end;

procedure TAbstractSynCompletion.DoAccept;
begin
  FCurrentString := '';
end;

procedure TAbstractSynCompletion.DoCancel;
begin
  FCurrentString := '';
end;

procedure TAbstractSynCompletion.DoExecute;
begin
  CurrentString := GetCurrentEditorString;
end;

procedure TAbstractSynCompletion.OnCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data,
  HandlerData: Pointer);
var
  S: UnicodeString;
begin  
  if not Executing then
  begin
    if (Command = CommandID) then
    begin
      Execute(Sender as TCustomSynEdit32);
      Handled := True;
    end;
  end
  else { Executing }
    if Sender = CurrentEditor then
    begin
      if not AfterProcessing then
      begin
          case Command of
            ecChar:
              if aChar = #27 then
              begin
                Cancel;
                Handled := True;
              end
              else
              begin
                if not(CurrentEditor.IsIdentChar(aChar)) then 
                  Accept;
                {don't handle the char}
              end;
            ecLineBreak:
            begin
              Accept;
              Handled := True;
            end;
            ecLeft, ecSelLeft:
              if CurrentString = '' then
                Handled := True;
            ecDeleteLastChar:
              if CurrentString = '' then
                Handled := True;
            ecTab:
              Accept;
            ecDeleteChar,
            ecRight, ecSelRight,
            ecLostFocus, ecGotFocus:
              ; {processed on AfterProcessing}
            else
              Cancel;
          end;
      end
      else { AfterProcessing }
        case Command of
          ecLostFocus, ecGotFocus,
          ecDeleteChar:
            ;
          ecDeleteLastChar,
          ecLeft, ecSelLeft,
          ecChar:
            CurrentString := GetCurrentEditorString;
          ecRight, ecSelRight: begin
            S := GetCurrentEditorString;
            if S = '' then
              Cancel
            else
              CurrentString := S;
          end;
          else
            if CurrentString <> GetCurrentEditorString then
              Cancel;
        end;
    end; {endif Sender = CurrentEditor}
end;

procedure TAbstractSynCompletion.SetCurrentString(const Value: UnicodeString);
begin
  FCurrentString := Value;
end;

procedure TAbstractSynCompletion.AddEditor(aEditor: TCustomSynEdit32);
begin
  inherited AddEditor(aEditor);
end;

end.
