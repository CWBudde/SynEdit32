{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-07-06.

The Original Code is part of the ScanTokensDemo project, written by
Michael Hieke for the SynEdit component suite.
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

$Id: frmMain.pas,v 1.2.2.1 2004/11/10 21:43:57 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, 
  SynEdit32, SynEdit32.Highlighter, SynEdit32.Highlighter.Pas;

type
  TScanThreadForm = class(TForm)
    SynEdit: TSynEdit32;
    Splitter: TSplitter;
    SynEdit32HighlighterPas: TSynEdit32HighlighterPas;
    StatusBar: TStatusBar;
    SynEditTokens: TSynEdit32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
  private
    FWorkerThread: TThread;
  end;

var
  ScanThreadForm: TScanThreadForm;

implementation

{$R *.DFM}

{ TScanKeywordThread }

type
  TScanKeywordThread = class(TThread)
  private
    FHighlighter: TSynEdit32CustomHighlighter;
    FKeywords: TStringList;
    FLastPercent: Integer;
    FScanEventHandle: THandle;
    FSource: string;
    FSourceChanged: Boolean;
    procedure GetSource;
    procedure SetResults;
    procedure ShowProgress;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetModified;
    procedure Shutdown;
  end;

constructor TScanKeywordThread.Create;
begin
  inherited Create(True);
  FHighlighter := TSynEdit32HighlighterPas.Create(nil);
  FKeywords := TStringList.Create;
  FScanEventHandle := CreateEvent(nil, False, False, nil);
  if (FScanEventHandle = 0) or (FScanEventHandle = INVALID_HANDLE_VALUE) then
    raise EOutOfResources.Create('Couldn''t create WIN32 event object');
  Resume;
end;

destructor TScanKeywordThread.Destroy;
begin
  FHighlighter.Free;
  FKeywords.Free;
  if (FScanEventHandle <> 0) and (FScanEventHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(FScanEventHandle);
  inherited Destroy;
end;

procedure TScanKeywordThread.Execute;
var
  i: Integer;
  s: string;
  Percent: Integer;
begin
  while not Terminated do begin
    WaitForSingleObject(FScanEventHandle, INFINITE);
    repeat
      if Terminated then
        break;
      // make sure the event is reset when we are still in the repeat loop
      ResetEvent(FScanEventHandle);
      // get the modified source and set FSourceChanged to 0
      Synchronize(GetSource);
      if Terminated then
        break;
      // clear keyword list
      FKeywords.Clear;
      FLastPercent := 0;
      // scan the source text for the keywords, cancel if the source in the
      // editor has been changed again
      FHighlighter.ResetRange;
      FHighlighter.SetLine(FSource, 1);
      while not FSourceChanged and not FHighlighter.GetEol do begin
        if FHighlighter.GetTokenKind = Ord(SynEdit32.Highlighter.Pas.tkKey) then begin
          s := FHighlighter.GetToken;
          with FKeywords do begin
            i := IndexOf(s);
            if i = -1 then
              AddObject(s, pointer(1))
            else
              Objects[i] := pointer(Integer(Objects[i]) + 1);
          end;
        end;
        // show progress (and burn some cycles ;-)
        Percent := MulDiv(100, FHighlighter.GetTokenPos, Length(FSource));
        if FLastPercent <> Percent then begin
          FLastPercent := Percent;
          Sleep(10);
          Synchronize(ShowProgress);
        end;
        FHighlighter.Next;
      end;
    until not FSourceChanged;

    if Terminated then
      break;
    // source was changed while scanning
    if FSourceChanged then begin
      Sleep(100);
      continue;
    end;

    FLastPercent := 100;
    Synchronize(ShowProgress);

    FKeywords.Sort;
    for i := 0 to FKeywords.Count - 1 do begin
      FKeywords[i] := FKeywords[i] + ': ' +
        IntToStr(Integer(FKeywords.Objects[i]));
    end;
    Synchronize(SetResults);
    // and go to sleep again
  end;
end;

procedure TScanKeywordThread.GetSource;
begin
  if ScanThreadForm <> nil then
    FSource := ScanThreadForm.SynEdit.Text
  else
    FSource := '';
  FSourceChanged := False;
end;

procedure TScanKeywordThread.SetModified;
begin
  FSourceChanged := True;
  if (FScanEventHandle <> 0) and (FScanEventHandle <> INVALID_HANDLE_VALUE) then
    SetEvent(FScanEventHandle);
end;

procedure TScanKeywordThread.SetResults;
begin
  if ScanThreadForm <> nil then
    ScanThreadForm.SynEditTokens.Lines.Assign(FKeywords);
end;

procedure TScanKeywordThread.ShowProgress;
begin
  if ScanThreadForm <> nil then
    ScanThreadForm.StatusBar.SimpleText := Format('%d %% done', [FLastPercent]);
end;

procedure TScanKeywordThread.Shutdown;
begin
  Terminate;
  if (FScanEventHandle <> 0) and (FScanEventHandle <> INVALID_HANDLE_VALUE) then
    SetEvent(FScanEventHandle);
end;

{ TScanThreadForm }

procedure TScanThreadForm.FormCreate(Sender: TObject);
begin
  FWorkerThread := TScanKeywordThread.Create;
  if FileExists('Windows.pas') then
    SynEdit.Lines.LoadFromFile('Windows.pas');
  TScanKeywordThread(FWorkerThread).SetModified;
end;

procedure TScanThreadForm.FormDestroy(Sender: TObject);
begin
  ScanThreadForm := nil;
  if FWorkerThread <> nil then
    TScanKeywordThread(FWorkerThread).Shutdown;
end;

procedure TScanThreadForm.SynEditChange(Sender: TObject);
begin
  SynEditTokens.ClearAll;
  if FWorkerThread <> nil then
    TScanKeywordThread(FWorkerThread).SetModified;
end;

end.

  
