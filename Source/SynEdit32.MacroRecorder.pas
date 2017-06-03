{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMacroRecorder.pas, released 2001-10-17.

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

$Id: SynMacroRecorder.pas,v 1.31.2.3 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.MacroRecorder;

{$I SynEdit.inc}

interface

uses
  StdCtrls,
  Controls,
  Windows,
  Messages,
  Graphics,
  Menus,
  WideStrUtils,
  Classes,
  SynEdit32,
  SynEdit32.KeyCmds,
  SynEdit32.Plugins,
  SynEdit32.Types,
  SynEdit32.Unicode;

resourcestring
  sCannotRecord = 'Cannot record macro; already recording or playing';
  sCannotPlay = 'Cannot playback macro; already playing or recording';
  sCannotPause = 'Can only pause when recording';
  sCannotResume = 'Can only resume when paused';

type
  TSynEdit32MacroState = (msStopped, msRecording, msPlaying, msPaused);
  TSynEdit32MacroCommand = (mcRecord, mcPlayback);

  TSynEdit32MacroEvent = class(TObject)
  protected
    FRepeatCount: Byte;
    function GetAsString: UnicodeString; virtual; abstract;
    procedure InitEventParameters(aStr: UnicodeString); virtual; abstract;
  public
    constructor Create; virtual;
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar; aData: Pointer);
      virtual; abstract;
    { the CommandID must not be read inside LoadFromStream/SaveToStream. It's read by the
    MacroRecorder component to decide which MacroEvent class to instanciate }
    procedure LoadFromStream(aStream: TStream); virtual; abstract;
    procedure SaveToStream(aStream: TStream); virtual; abstract;
    procedure Playback(aEditor: TCustomSynEdit32); virtual; abstract;
    property AsString: UnicodeString read GetAsString;
    property RepeatCount: Byte read FRepeatCount write FRepeatCount;
  end;

  TSynEdit32BasicEvent = class(TSynEdit32MacroEvent)
  protected
    FCommand: TSynEditorCommand;
    function GetAsString: UnicodeString; override;
    procedure InitEventParameters(aStr: UnicodeString); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit32); override;
  public
    property Command: TSynEditorCommand read FCommand write FCommand;
  end;

  TSynEdit32CharEvent = class(TSynEdit32MacroEvent)
  protected
    fKey: WideChar;
    function GetAsString: UnicodeString; override;
    procedure InitEventParameters(aStr: UnicodeString); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit32); override;
  public
    property Key: WideChar read fKey write fKey;
  end;

  TSynEdit32StringEvent = class(TSynEdit32MacroEvent)
  protected
    FString: UnicodeString;
    function GetAsString: UnicodeString; override;
    procedure InitEventParameters(aStr: UnicodeString); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit32); override;
  public
    property Value: UnicodeString read FString write FString;
  end;

  TSynEdit32PositionEvent = class(TSynEdit32BasicEvent)
  protected
    FPosition: TBufferCoord;
    function GetAsString: UnicodeString; override;
    procedure InitEventParameters(aStr: UnicodeString); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit32); override;
  public
    property Position: TBufferCoord read FPosition write FPosition;
  end;

  TSynEdit32DataEvent = class(TSynEdit32BasicEvent)
  protected
    FData: Pointer;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit32); override;
  end;

  TCustomSynEdit32MacroRecorder = class;

  TSynEdit32UserCommandEvent = procedure (aSender: TCustomSynEdit32MacroRecorder;
    aCmd: TSynEditorCommand; var aEvent: TSynEdit32MacroEvent) of object;

  { TCustomSynEdit32MacroRecorder
  OnStateChange:
    occurs right after start playing, recording, pausing or stopping
  SaveMarkerPos:
    if true, Bookmark position is recorded in the macro. Otherwise, the Bookmark
    is created in the position the Caret is at the time of playback.
  }

  TCustomSynEdit32MacroRecorder = class(TAbstractSynHookerPlugin)
  private
    FShortCuts: array [TSynEdit32MacroCommand] of TShortCut;
    FOnStateChange: TNotifyEvent;
    FOnUserCommand: TSynEdit32UserCommandEvent;
    FMacroName: string;
    FSaveMarkerPos: boolean;
    function GetEvent(aIndex: integer): TSynEdit32MacroEvent;
    function GetEventCount: integer;
    function GetAsString: UnicodeString;
    procedure SetAsString(const Value: UnicodeString);
  protected
    FCurrentEditor: TCustomSynEdit32;
    FState: TSynEdit32MacroState;
    FEvents: TList;
    FCommandIDs: array [TSynEdit32MacroCommand] of TSynEditorCommand;
    procedure SetShortCut(const Index: Integer; const Value: TShortCut);
    function GetIsEmpty: boolean;
    procedure StateChanged;
    procedure DoAddEditor(aEditor: TCustomSynEdit32); override;
    procedure DoRemoveEditor(aEditor: TCustomSynEdit32); override;
    procedure OnCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var aChar: WideChar;
      Data: pointer; HandlerData: pointer); override;
    function CreateMacroEvent(aCmd: TSynEditorCommand): TSynEdit32MacroEvent;
  protected
    property RecordCommandID: TSynEditorCommand read FCommandIDs[mcRecord];
    property PlaybackCommandID: TSynEditorCommand read FCommandIDs[mcPlayback];
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Error(const aMsg: String);
    procedure AddEditor(aEditor: TCustomSynEdit32);
    procedure RemoveEditor(aEditor: TCustomSynEdit32);
    procedure RecordMacro(aEditor: TCustomSynEdit32);
    procedure PlaybackMacro(aEditor: TCustomSynEdit32);
    procedure Stop;
    procedure Pause;
    procedure Resume;
    property IsEmpty: boolean read GetIsEmpty;
    property State: TSynEdit32MacroState read FState;
    procedure Clear;
    procedure AddEvent(aCmd: TSynEditorCommand; aChar: WideChar; aData: pointer);
    procedure InsertEvent(aIndex: integer; aCmd: TSynEditorCommand; aChar: WideChar;
      aData: pointer);
    procedure AddCustomEvent(aEvent: TSynEdit32MacroEvent);
    procedure InsertCustomEvent(aIndex: integer; aEvent: TSynEdit32MacroEvent);
    procedure DeleteEvent(aIndex: integer);
    procedure LoadFromStream(aSrc: TStream);
    procedure LoadFromStreamEx(aSrc: TStream; aClear: boolean);
    procedure SaveToStream(aDest: TStream);
    procedure LoadFromFile(aFilename : string);
    procedure SaveToFile(aFilename : string);
    property EventCount: integer read GetEventCount;
    property Events[aIndex: integer]: TSynEdit32MacroEvent read GetEvent;
    property RecordShortCut: TShortCut index Ord(mcRecord)
      read FShortCuts[mcRecord] write SetShortCut;
    property PlaybackShortCut: TShortCut index Ord(mcPlayback)
      read FShortCuts[mcPlayback] write SetShortCut;
    property SaveMarkerPos: boolean read FSaveMarkerPos
      write FSaveMarkerPos default False;
    property AsString: UnicodeString read GetAsString write SetAsString;
    property MacroName: string read FMacroName write FMacroName;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnUserCommand: TSynEdit32UserCommandEvent read FOnUserCommand
      write FOnUserCommand;
  end;

  TSynEdit32MacroRecorder = class(TCustomSynEdit32MacroRecorder)
  published
    property SaveMarkerPos;
    property RecordShortCut;
    property PlaybackShortCut;
    property OnStateChange;
    property OnUserCommand;
  end;

implementation

uses
  Forms,
  SynEdit32.MiscProcs,
  RTLConsts,
  SysUtils;

{ TSynEdit32DataEvent }

procedure TSynEdit32DataEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  FCommand := aCmd;
  Assert(aChar = #0);
  FData := aData;
end;

procedure TSynEdit32DataEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(FData, SizeOf(FData));
end;

procedure TSynEdit32DataEvent.Playback(aEditor: TCustomSynEdit32);
begin
  aEditor.CommandProcessor(Command, #0, FData);
end;

procedure TSynEdit32DataEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write(FData, SizeOf(FData));
end;

{ TCustomSynEdit32MacroRecorder }

procedure TCustomSynEdit32MacroRecorder.AddCustomEvent(aEvent: TSynEdit32MacroEvent);
begin
  InsertCustomEvent(EventCount, aEvent);
end;

procedure TCustomSynEdit32MacroRecorder.AddEditor(aEditor: TCustomSynEdit32);
begin
  inherited AddEditor(aEditor);
end;

procedure TCustomSynEdit32MacroRecorder.AddEvent(aCmd: TSynEditorCommand;
  aChar: WideChar; aData: pointer);
begin
  InsertEvent(EventCount, aCmd, aChar, aData);
end;

procedure TCustomSynEdit32MacroRecorder.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  if Assigned(FEvents) then
  begin
    for I := FEvents.Count-1 downto 0 do
    begin
      Obj := FEvents[I];
      FEvents.Delete(I);
      Obj.Free;
    end;
    FreeAndNil(FEvents);
  end;
end;

constructor TCustomSynEdit32MacroRecorder.Create(aOwner: TComponent);
begin
  inherited;
  FMacroName := 'unnamed';
  FCommandIDs[mcRecord] := NewPluginCommand;
  FCommandIDs[mcPlayback] := NewPluginCommand;
  FShortCuts[mcRecord] := Menus.ShortCut(Ord('R'), [ssCtrl, ssShift]);
  FShortCuts[mcPlayback] := Menus.ShortCut(Ord('P'), [ssCtrl, ssShift]);
end;

function TCustomSynEdit32MacroRecorder.CreateMacroEvent(aCmd: TSynEditorCommand): TSynEdit32MacroEvent;

  function WantDefaultEvent(var aEvent: TSynEdit32MacroEvent): boolean;
  begin
    if Assigned(OnUserCommand) then
      OnUserCommand(Self, aCmd, aEvent);
    Result := aEvent = nil;
  end;

begin
  case aCmd of
    ecGotoXY, ecSelGotoXY, ecSetMarker0..ecSetMarker9:
      begin
        Result := TSynEdit32PositionEvent.Create;
        TSynEdit32PositionEvent(Result).Command := aCmd;
      end;
    ecChar:
      Result := TSynEdit32CharEvent.Create;
    ecString:
      Result := TSynEdit32StringEvent.Create;
    else begin
      Result := nil;
      if (aCmd < ecUserFirst) or WantDefaultEvent(Result) then
      begin
        Result := TSynEdit32BasicEvent.Create;
        TSynEdit32BasicEvent(Result).Command := aCmd;
      end;
    end;
  end;
end;

procedure TCustomSynEdit32MacroRecorder.DeleteEvent(aIndex: integer);
var
  iObj: Pointer;
begin
  iObj := FEvents[aIndex];
  FEvents.Delete(aIndex);
  TObject(iObj).Free;
end;

destructor TCustomSynEdit32MacroRecorder.Destroy;
begin
  Clear;
  inherited;
  ReleasePluginCommand(PlaybackCommandID);
  ReleasePluginCommand(RecordCommandID);
end;

procedure TCustomSynEdit32MacroRecorder.DoAddEditor(aEditor: TCustomSynEdit32);
begin
  HookEditor(aEditor, RecordCommandID, 0, RecordShortCut);
  HookEditor(aEditor, PlaybackCommandID, 0, PlaybackShortCut);
end;

procedure TCustomSynEdit32MacroRecorder.DoRemoveEditor(aEditor: TCustomSynEdit32);
begin
  UnHookEditor(aEditor, RecordCommandID, RecordShortCut);
  UnHookEditor(aEditor, PlaybackCommandID, PlaybackShortCut);
end;

procedure TCustomSynEdit32MacroRecorder.Error(const aMsg: String);
begin
  raise Exception.Create(aMsg);
end;

function TCustomSynEdit32MacroRecorder.GetEvent(aIndex: integer): TSynEdit32MacroEvent;
begin
  Result := TSynEdit32MacroEvent(FEvents[aIndex]);
end;

function TCustomSynEdit32MacroRecorder.GetEventCount: integer;
begin
  if FEvents = nil then
    Result := 0
  else
    Result := FEvents.Count;
end;

function TCustomSynEdit32MacroRecorder.GetIsEmpty: boolean;
begin
  Result := (FEvents = nil) or (FEvents.Count = 0);
end;

procedure TCustomSynEdit32MacroRecorder.InsertCustomEvent(aIndex: integer;
  aEvent: TSynEdit32MacroEvent);
begin
  if FEvents = nil then
    FEvents := TList.Create;
  FEvents.Insert(aIndex, aEvent);
end;

procedure TCustomSynEdit32MacroRecorder.InsertEvent(aIndex: integer;
  aCmd: TSynEditorCommand; aChar: WideChar; aData: pointer);
var
  iEvent: TSynEdit32MacroEvent;
begin
  iEvent := CreateMacroEvent(aCmd);
  try
    iEvent.Initialize(aCmd, aChar, aData);
    InsertCustomEvent(aIndex, iEvent);
  except
    iEvent.Free;
    raise;
  end;
end;

procedure TCustomSynEdit32MacroRecorder.LoadFromStream(aSrc: TStream);
begin
  LoadFromStreamEx(aSrc, True);
end;

procedure TCustomSynEdit32MacroRecorder.LoadFromStreamEx(aSrc: TStream;
  aClear: boolean);
var
  iCommand: TSynEditorCommand;
  iEvent: TSynEdit32MacroEvent;
  cnt, i: Integer;
begin
  Stop;
  if aClear then
    Clear;
  FEvents := TList.Create;
  aSrc.Read(cnt, sizeof(cnt));
  i := 0;
  FEvents.Capacity := aSrc.Size div SizeOf(TSynEditorCommand);
  while (aSrc.Position < aSrc.Size) and (i < cnt) do
  begin
    aSrc.Read(iCommand, SizeOf(TSynEditorCommand));
    iEvent := CreateMacroEvent(iCommand);
    iEvent.Initialize(iCommand, #0, nil);
    iEvent.LoadFromStream(aSrc);
    FEvents.Add(iEvent);
    Inc(i);
  end;
end;

// TODO: Sender could be also something else then a TCustomSynEdit32(namely a TObject) but the code below assumes it is a TCustomSynEdit32 even if Sender is of type TObject.
procedure TCustomSynEdit32MacroRecorder.OnCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var aChar: WideChar; Data,
  HandlerData: pointer);
var
  iEvent: TSynEdit32MacroEvent;
begin
  if AfterProcessing then
  begin
    if (Sender = FCurrentEditor) and (State = msRecording) and (not Handled) then
    begin
      iEvent := CreateMacroEvent(Command);
      iEvent.Initialize(Command, aChar, Data);
      FEvents.Add(iEvent);
      if SaveMarkerPos and (Command >= ecSetMarker0) and
        (Command <= ecSetMarker9) and (Data = nil) then
      begin
        TSynEdit32PositionEvent(iEvent).Position := FCurrentEditor.CaretXY;
      end;
    end;
  end
  else
  begin
    {not AfterProcessing}
    case State of
      msStopped:
        if Command = RecordCommandID then
        begin
          RecordMacro(TCustomSynEdit32(Sender));
          Handled := True;
        end
        else if Command = PlaybackCommandID then
        begin
          PlaybackMacro(TCustomSynEdit32(Sender));
          Handled := True;
        end;
      msPlaying:
        ;
      msPaused:
        if Command = PlaybackCommandID then
        begin
          Resume;
          Handled := True;
        end;
      msRecording:
        if Command = PlaybackCommandID then
        begin
          Pause;
          Handled := True;
        end
        else if Command = RecordCommandID then
        begin
          Stop;
          Handled := True;
        end;
    end;
  end;
end;

procedure TCustomSynEdit32MacroRecorder.Pause;
begin
  if State <> msRecording then
    Error(sCannotPause);
  FState := msPaused;
  StateChanged;
end;

procedure TCustomSynEdit32MacroRecorder.PlaybackMacro(aEditor: TCustomSynEdit32);
var
  cEvent: integer;
begin
  if State <> msStopped then
    Error(sCannotPlay);
  FState := msPlaying;
  try
    StateChanged;
    for cEvent := 0 to EventCount -1 do
    begin
      Events[cEvent].Playback(aEditor);
      if State <> msPlaying then
        break;
    end;
  finally
    if State = msPlaying then
    begin
      FState := msStopped;
      StateChanged;
    end;
  end;
end;

procedure TCustomSynEdit32MacroRecorder.RecordMacro(aEditor: TCustomSynEdit32);
begin
  if FState <> msStopped then
    Error(sCannotRecord);
  Clear;
  FEvents := TList.Create;
  FEvents.Capacity := 512;
  FState := msRecording;
  FCurrentEditor := aEditor;
  StateChanged;
end;

procedure TCustomSynEdit32MacroRecorder.RemoveEditor(aEditor: TCustomSynEdit32);
begin
  inherited RemoveEditor(aEditor);
end;

procedure TCustomSynEdit32MacroRecorder.Resume;
begin
  if FState <> msPaused then
    Error(sCannotResume);
  FState := msRecording;
  StateChanged;
end;

procedure TCustomSynEdit32MacroRecorder.SaveToStream(aDest: TStream);
var
  cEvent, eCnt: integer;
begin
  eCnt := EventCount;
  aDest.Write(eCnt, sizeof(eCnt));
  for cEvent := 0 to eCnt -1 do
    Events[cEvent].SaveToStream(aDest);
end;

procedure TCustomSynEdit32MacroRecorder.SetShortCut(const Index: Integer;
  const Value: TShortCut);
var
  cEditor: integer;
begin
  if FShortCuts[TSynEdit32MacroCommand(Index)] <> Value then
  begin
    if Assigned(fEditors) then
      if Value <> 0 then
      begin
        for cEditor := 0 to fEditors.Count -1 do
          HookEditor(Editors[cEditor], FCommandIDs[TSynEdit32MacroCommand(Index)],
            FShortCuts[TSynEdit32MacroCommand(Index)], Value);
      end else
      begin
        for cEditor := 0 to fEditors.Count -1 do
          UnHookEditor(Editors[cEditor], FCommandIDs[TSynEdit32MacroCommand(Index)],
            FShortCuts[TSynEdit32MacroCommand(Index)]);
      end;
    FShortCuts[TSynEdit32MacroCommand(Index)] := Value;
  end;
end;

procedure TCustomSynEdit32MacroRecorder.StateChanged;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

procedure TCustomSynEdit32MacroRecorder.Stop;
begin
  if FState = msStopped then
    Exit;
  FState := msStopped;
  FCurrentEditor := nil;
  if FEvents.Count = 0 then
    FreeAndNil(FEvents);
  StateChanged;
end;

function TCustomSynEdit32MacroRecorder.GetAsString: UnicodeString;
var
  i: integer;
  eStr: UnicodeString;
begin
  Result := 'macro ' + MacroName + #13#10 + 'begin' + #13#10;
  if Assigned(FEvents) then
  begin
    for i := 0 to FEvents.Count -1 do
    begin
      eStr := Events[i].AsString;
      if eStr <> '' then
        Result := Result + '  '  + eStr + #13#10;
    end;
  end;
  Result := Result + 'end';
end;

procedure TCustomSynEdit32MacroRecorder.SetAsString(const Value: UnicodeString);
var
  i, p, Cmd: Integer;
  S: TUnicodeStrings;
  cmdStr: UnicodeString;
  iEvent: TSynEdit32MacroEvent;
begin
  Stop;
  Clear;
  FEvents := TList.Create;
  // process file line by line and create events
  S := TUnicodeStringList.Create;
  try
    S.Text := Value;
    for i := 0 to S.Count - 1 do
    begin
      cmdStr := WideTrim(S[i]);
      p := Pos(' ', cmdStr);
      if p = 0 then p := Length(cmdStr) + 1;
      Cmd := ecNone;
      if IdentToEditorCommand(Copy(cmdStr, 1, p - 1), Longint(Cmd)) then  // D2 needs type-cast
      begin
        Delete(cmdStr, 1, p);
        iEvent := CreateMacroEvent(Cmd);
        try
          FEvents.Add(iEvent);
          iEvent.InitEventParameters(cmdStr);
        except
          iEvent.Free;
        end;
      end;
    end;
  finally
    S.Free;
  end;
end;

procedure TCustomSynEdit32MacroRecorder.LoadFromFile(aFilename: string);
var
  F : TFileStream;
begin
  F := TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadFromStream(F);
    MacroName := ChangeFileExt(ExtractFileName(aFilename), '');
  finally
    F.Free;
  end;
end;

procedure TCustomSynEdit32MacroRecorder.SaveToFile(aFilename: string);
var
  F : TFileStream;
begin
  F := TFileStream.Create(aFilename, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

{ TSynEdit32BasicEvent }

function TSynEdit32BasicEvent.GetAsString: UnicodeString;
var
  Ident: string;
begin
  EditorCommandToIdent(Command, Ident);
  Result := Ident;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynEdit32BasicEvent.InitEventParameters(aStr: UnicodeString);
begin
  // basic events have no parameters but can contain an optional repeat count
  RepeatCount := StrToIntDef(WideTrim(aStr), 1);
end;

procedure TSynEdit32BasicEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  Command := aCmd;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  if (aChar <> #0) or (aData <> nil) then
    raise Exception.Create('TSynBasicEvent cannot handle Char <> #0 or Data <> nil');
{$ENDIF}
end;

procedure TSynEdit32BasicEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TSynEdit32BasicEvent.Playback(aEditor: TCustomSynEdit32);
var
  i : Integer;
begin
  for i := 1 to RepeatCount do
    aEditor.CommandProcessor(Command, #0, nil);
end;

procedure TSynEdit32BasicEvent.SaveToStream(aStream: TStream);
begin
  aStream.Write(Command, SizeOf(TSynEditorCommand));
  aStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TSynEdit32CharEvent }

function TSynEdit32CharEvent.GetAsString: UnicodeString;
var
  Ident: string;
begin
  EditorCommandToIdent(ecChar, Ident);
  Result := Ident + ' ' + Key;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynEdit32CharEvent.InitEventParameters(aStr: UnicodeString);
begin
  // aStr should be a Key value one character in length
  // with an optional repeat count whitespace separated
  if Length(aStr) >= 1 then
    Key := aStr[1]
  else
    Key := ' ';
  Delete(aStr, 1, 1); // if possible delete the first character
  RepeatCount := StrToIntDef(WideTrim(aStr), 1);
end;

procedure TSynEdit32CharEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  Key := aChar;
  Assert(aData = nil);
end;

procedure TSynEdit32CharEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(fKey, SizeOf(Key));
  aStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TSynEdit32CharEvent.Playback(aEditor: TCustomSynEdit32);
var
  i: Integer;
begin
  for i := 1 to RepeatCount do
    aEditor.CommandProcessor(ecChar, Key, nil);
end;

procedure TSynEdit32CharEvent.SaveToStream(aStream: TStream);
const
  iCharCommand: TSynEditorCommand = ecChar;
begin
  aStream.Write(iCharCommand, SizeOf(TSynEditorCommand));
  aStream.Write(Key, SizeOf(Key));
  aStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TSynEdit32PositionEvent }

function TSynEdit32PositionEvent.GetAsString: UnicodeString;
begin
  Result := inherited GetAsString;
  // add position data here
  Result := Result + Format(' (%d, %d)', [Position.Char, Position.Line]);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynEdit32PositionEvent.InitEventParameters(aStr: UnicodeString);
var
  i, o, c, x, y: Integer;
  valStr: UnicodeString;
begin
  inherited;
  // aStr should be (x, y) with optional repeat count whitespace separated
  aStr := WideTrim(aStr);
  i := Pos(',', aStr);
  o := Pos('(', aStr);
  c := Pos(')', aStr);
  if (not ((i = 0) or (o = 0) or (c = 0))) and
     ((i > o) and (i < c)) then
  begin
    valStr := Copy(aStr, o + 1, i - o - 1);
    x := StrToIntDef(valStr, 1);
    Delete(aStr, 1, i);
    aStr := WideTrim(aStr);
    c := Pos(')', aStr);
    valStr := Copy(aStr, 1, c - 1);
    y := StrToIntDef(valStr, 1);
    Position := BufferCoord(x, y);
    Delete(aStr, 1, c);
    aStr := WideTrim(aStr);
    RepeatCount := StrToIntDef(aStr, 1);
  end;
end;

procedure TSynEdit32PositionEvent.Initialize(aCmd: TSynEditorCommand;
  aChar: WideChar; aData: Pointer);
begin
  inherited;
  if aData <> nil then
    Position := TBufferCoord(aData^)
  else
    Position := BufferCoord(0, 0);
end;

procedure TSynEdit32PositionEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(FPosition, SizeOf(Position));
end;

procedure TSynEdit32PositionEvent.Playback(aEditor: TCustomSynEdit32);
begin
  if (Position.Char <> 0) or (Position.Line <> 0) then
    aEditor.CommandProcessor(Command, #0, @Position)
  else
    aEditor.CommandProcessor(Command, #0, nil);
end;

procedure TSynEdit32PositionEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write(Position, SizeOf(Position));
end;

{ TSynEdit32StringEvent }

function TSynEdit32StringEvent.GetAsString: UnicodeString;
var
  Ident: string;
begin
  EditorCommandToIdent(ecString, Ident);
  Result := Ident + ' ' + WideQuotedStr(Value, #39);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynEdit32StringEvent.InitEventParameters(aStr: UnicodeString);
var
  o, c: Integer;
  valStr: UnicodeString;
begin                      
  // aStr = 'test' with optional whitespace separated repeat count
  o := Pos('''', aStr);
  c := WideLastDelimiter('''', aStr);
  valStr := Copy(aStr, o + 1, c - o - 1);
  Value := UnicodeStringReplace(valStr, '''''', '''', [rfReplaceAll]);
  Delete(aStr, 1, c);
  RepeatCount := StrToIntDef(WideTrim(aStr), 1);
end;

procedure TSynEdit32StringEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  Value := UnicodeString(aData);
end;

procedure TSynEdit32StringEvent.LoadFromStream(aStream: TStream);
var
  l: Integer;
  Buff: PWideChar;
begin
  aStream.Read(l, sizeof(l));
  GetMem(Buff, l * sizeof(WideChar));
  try
    FillMemory(Buff, l, 0);
    aStream.Read(Buff^, l * sizeof(WideChar));
    FString := Buff;
  finally
    FreeMem(Buff);
  end;
  aStream.Read(FRepeatCount, sizeof(FRepeatCount));
end;

procedure TSynEdit32StringEvent.Playback(aEditor: TCustomSynEdit32);
var
  i, j: Integer;
begin
  for j := 1 to RepeatCount do
  begin
//    aEditor.CommandProcessor( ecString, #0, Pointer(Value) );
    // SynEdit doesn't actually support the ecString command so we convert
    // it into ecChar commands
    for i := 1 to Length(Value) do
      aEditor.CommandProcessor(ecChar, Value[i], nil);
  end;
end;

procedure TSynEdit32StringEvent.SaveToStream(aStream: TStream);
const
  StrCommand: TSynEditorCommand = ecString;
var
  l: Integer;
  Buff: PWideChar;
begin
  aStream.Write(StrCommand, SizeOf(StrCommand));
  l := Length(Value) + 1;
  aStream.Write(l, sizeof(l));
  GetMem(Buff, l * sizeof(WideChar));
  try
    FillMemory(Buff, l, 0);
    WStrCopy(Buff, PWideChar(Value));
    aStream.Write(Buff^, l * sizeof(WideChar));
  finally
    FreeMem(Buff);
  end;
  aStream.Write(RepeatCount, sizeof(RepeatCount));
end;


{ TSynEdit32MacroEvent }

constructor TSynEdit32MacroEvent.Create;
begin
  inherited Create;
  FRepeatCount := 1;
end;

end.
