{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
Unicode translation by Maël Hörz.
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

$Id: SynEditKbdHandler.pas,v 1.10.2.1 2004/08/31 12:55:17 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.KbdHandler;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, Graphics, Controls, Forms, SysUtils, Classes,
  SynEdit32.Types;

type
  { This class provides a TWinControl-Object which supports only the
    needed Methods }
  TKeyboardControl = class(TWinControl)
  public
    property OnKeyDown;
    property OnKeyPress;
    property OnMouseDown;
  end;

  TMouseCursorEvent =  procedure(Sender: TObject; const aLineCharPos: TSynEdit32BufferCoord;
    var aCursor: TCursor) of object;

  TSynEdit32MethodList = class
  private
    FData: TList;
    function GetItem(Index: integer): TMethod;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aHandler: TMethod);
    procedure Remove(aHandler: TMethod);
    property Items[Index: Integer]: TMethod read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TSynEdit32KbdHandler = class (TObject)
  private
    FKeyPressChain: TSynEdit32MethodList;
    FKeyDownChain: TSynEdit32MethodList;
    FKeyUpChain: TSynEdit32MethodList;
    FMouseDownChain: TSynEdit32MethodList;
    FMouseUpChain: TSynEdit32MethodList;
    FMouseCursorChain: TSynEdit32MethodList;
    { avoid infinite recursiveness }
    FInKeyPress: Boolean;
    FInKeyDown: Boolean;
    FInKeyUp: Boolean;
    FInMouseDown: Boolean;
    FInMouseUp: Boolean;
    FInMouseCursor: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExecuteKeyPress(Sender: TObject; var Key: WideChar);
    procedure ExecuteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExecuteKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExecuteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExecuteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExecuteMouseCursor(Sender: TObject; const aLineCharPos: TSynEdit32BufferCoord;
      var aCursor: TCursor);

    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
  end;


implementation

{ TSynEdit32KbdHandler }

procedure TSynEdit32KbdHandler.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  FKeyDownChain.Add(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  FKeyUpChain.Add(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.AddKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  FKeyPressChain.Add(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  FMouseDownChain.Add(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  FMouseUpChain.Add(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FMouseCursorChain.Add(TMethod(aHandler));
end;

constructor TSynEdit32KbdHandler.Create;
begin
  { Elements to handle KeyDown-Events }
  FKeyDownChain := TSynEdit32MethodList.Create;

  { Elements to handle KeyUp-Events }
  FKeyUpChain := TSynEdit32MethodList.Create;

  { Elements to handle KeyPress-Events }
  FKeyPressChain := TSynEdit32MethodList.Create;

  { Elements to handle MouseDown Events }
  FMouseDownChain := TSynEdit32MethodList.Create;

  { Elements to handle MouseUp Events }
  FMouseUpChain := TSynEdit32MethodList.Create;

  { Elements to handle MouseCursor Events }
  FMouseCursorChain := TSynEdit32MethodList.Create;
end;

destructor TSynEdit32KbdHandler.Destroy;
begin
  FKeyPressChain.Free;
  FKeyDownChain.Free;
  FKeyUpChain.Free;
  FMouseDownChain.Free;
  FMouseUpChain.Free;
  FMouseCursorChain.Free;

  inherited Destroy;
end;

procedure TSynEdit32KbdHandler.ExecuteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  idx: Integer;
begin
  if FInKeyDown then
    Exit;
  FInKeyDown := True;
  try
    with FKeyDownChain do
    begin
      for idx := Count - 1 downto 0 do
      begin
        TKeyEvent(Items[idx])(Sender, Key, Shift);
        if (Key = 0) then
        begin
          FInKeyDown := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyDown := False;
  end;
end;

procedure TSynEdit32KbdHandler.ExecuteKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  idx: Integer;
begin
  if FInKeyUp then
    Exit;
  FInKeyUp := True;
  try
    with FKeyUpChain do
    begin
      for idx := Count - 1 downto 0 do
      begin
        TKeyEvent(Items[idx])(Sender,Key,Shift);
        if (Key = 0) then
        begin
          FInKeyUp := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyUp := False;
  end;
end;

procedure TSynEdit32KbdHandler.ExecuteKeyPress(Sender: TObject; var Key: WideChar);
var
  idx: Integer;
begin
  if FInKeyPress then
    Exit;
  FInKeyPress := True;
  try
    with FKeyPressChain do
    begin
      for idx := Count - 1 downto 0 do
      begin
        TKeyPressWEvent(Items[idx])(Sender, Key);
        if (Key = #0) then
        begin
          FInKeyPress := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyPress := False;
  end;
end;

procedure TSynEdit32KbdHandler.ExecuteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cHandler: Integer;
begin
  if FInMouseDown then
    Exit;
  FInMouseDown := True;
  try
    for cHandler := FMouseDownChain.Count - 1 downto 0 do
      TMouseEvent(FMouseDownChain[cHandler])(Sender, Button, Shift, X, Y);
  finally
    FInMouseDown := False;
  end;
end;

procedure TSynEdit32KbdHandler.ExecuteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cHandler: Integer;
begin
  if FInMouseUp then
    Exit;
  FInMouseUp := True;
  try
    for cHandler := FMouseUpChain.Count - 1 downto 0 do
      TMouseEvent(FMouseUpChain[cHandler])(Sender, Button, Shift, X, Y);
  finally
    FInMouseUp := False;
  end;
end;

procedure TSynEdit32KbdHandler.ExecuteMouseCursor(Sender: TObject;
  const aLineCharPos: TSynEdit32BufferCoord; var aCursor: TCursor);
var
  cHandler: Integer;
begin
  if FInMouseCursor then
    Exit;
  FInMouseCursor := True;
  try
    for cHandler := FMouseCursorChain.Count - 1 downto 0 do
      TMouseCursorEvent(FMouseCursorChain[cHandler])(Sender, aLineCharPos, aCursor);
  finally
    FInMouseCursor := False;
  end;
end;

procedure TSynEdit32KbdHandler.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  FKeyDownChain.Remove(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  FKeyUpChain.Remove(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  FKeyPressChain.Remove(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  FMouseDownChain.Remove(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  FMouseUpChain.Remove(TMethod(aHandler));
end;

procedure TSynEdit32KbdHandler.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FMouseCursorChain.Remove(TMethod(aHandler));
end;

{ TSynEdit32MethodList }

procedure TSynEdit32MethodList.Add(aHandler: TMethod);
begin
  FData.Add(aHandler.Data);
  FData.Add(aHandler.Code);
end;

constructor TSynEdit32MethodList.Create;
begin
  FData := TList.Create;
end;

destructor TSynEdit32MethodList.Destroy;
begin
  FData.Free;
end;

function TSynEdit32MethodList.GetCount: Integer;
begin
  Result := FData.Count div 2;
end;

function TSynEdit32MethodList.GetItem(Index: Integer): TMethod;
begin
  Index := Index * 2;
  Result.Data := FData[Index];
  Result.Code := FData[Index + 1];
end;

procedure TSynEdit32MethodList.Remove(aHandler: TMethod);
var
  cPos: Integer;
begin
  cPos := FData.Count - 2;
  while cPos >= 0 do
  begin
    if (FData.List[cPos] = aHandler.Data) and (FData.List[cPos + 1] = aHandler.Code) then
    begin
      FData.Delete(cPos);
      FData.Delete(cPos);
      Exit;
    end;
    Dec(cPos, 2);
  end;
end;

end.
