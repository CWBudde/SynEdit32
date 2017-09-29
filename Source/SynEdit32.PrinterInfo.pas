{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrinterInfo.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
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

$Id: SynEditPrinterInfo.pas,v 1.4.2.2 2005/10/18 01:43:23 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Class retrieving info about selected printer and paper size.
-------------------------------------------------------------------------------}

unit SynEdit32.PrinterInfo;

{$I SynEdit32.inc}

interface

uses
  Windows,
  Printers;

type
  //Printer info class - getting dimensions of paper
  TSynEdit32PrinterInfo = class
  private
    FPhysicalWidth: Integer;
    FPhysicalHeight: Integer;
    FPrintableWidth: Integer;
    FPrintableHeight: Integer;
    FLeftGutter: Integer;
    FRightGutter: Integer;
    FTopGutter: Integer;
    FBottomGutter: Integer;
    FXPixPrInch: Integer;
    FYPixPrInch: Integer;
    FXPixPrmm: Single;
    FYPixPrmm: Single;
    FIsUpdated: Boolean;
    procedure FillDefault;
    function GetBottomGutter: Integer;
    function GetLeftGutter: Integer;
    function GetPhysicalHeight: Integer;
    function GetPhysicalWidth: Integer;
    function GetPrintableHeight: Integer;
    function GetPrintableWidth: Integer;
    function GetRightGutter: Integer;
    function GetTopGutter: Integer;
    function GetXPixPrInch: Integer;
    function GetYPixPrInch: Integer;
    function GetXPixPrmm: Single;
    function GetYPixPrmm: Single;
  public
    procedure UpdatePrinter;
    function PixFromLeft(mmValue: Double): Integer;
    function PixFromRight(mmValue: Double): Integer;
    function PixFromTop(mmValue: Double): Integer;
    function PixFromBottom(mmValue: Double): Integer;
    property PhysicalWidth: Integer read GetPhysicalWidth;
    property PhysicalHeight: Integer read GetPhysicalHeight;
    property PrintableWidth: Integer read GetPrintableWidth;
    property PrintableHeight: Integer read GetPrintableHeight;
    property LeftGutter: Integer read GetLeftGutter;
    property RightGutter: Integer read GetRightGutter;
    property TopGutter: Integer read GetTopGutter;
    property BottomGutter: Integer read GetBottomGutter;
    property XPixPrInch: Integer read GetXPixPrInch;
    property YPixPrInch: Integer read GetYPixPrInch;
    property XPixPrmm: Single read GetXPixPrmm;
    property YPixPrmm: Single read GetYPixPrmm;
  end;

implementation

{ TSynEdit32PrinterInfo }

function TSynEdit32PrinterInfo.PixFromBottom(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FYPixPrmm - FBottomGutter);
end;

function TSynEdit32PrinterInfo.PixFromLeft(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FXPixPrmm - FLeftGutter);
end;

function TSynEdit32PrinterInfo.PixFromRight(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FXPixPrmm - FRightGutter);
end;

function TSynEdit32PrinterInfo.PixFromTop(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FYPixPrmm - FTopGutter);
end;

procedure TSynEdit32PrinterInfo.FillDefault;
{In case of no printers installed this information is used
 (I think it's taken from a HP LaserJet III with A4 paper)}
begin
  FPhysicalWidth := 2481;
  FPhysicalHeight := 3507;
  FPrintableWidth := 2358;
  FPrintableHeight := 3407;
  FLeftGutter := 65;
  FRightGutter := 58;
  FTopGutter := 50;
  FBottomGutter := 50;
  FXPixPrInch := 300;
  FYPixPrInch := 300;
  FXPixPrmm := FXPixPrInch / 25.4;
  FYPixPrmm := FYPixPrInch / 25.4;
end;

function TSynEdit32PrinterInfo.GetBottomGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FBottomGutter;
end;

function TSynEdit32PrinterInfo.GetLeftGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FLeftGutter;
end;

function TSynEdit32PrinterInfo.GetPhysicalHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalHeight;
end;

function TSynEdit32PrinterInfo.GetPhysicalWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalWidth;
end;

function TSynEdit32PrinterInfo.GetPrintableHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableHeight;
end;

function TSynEdit32PrinterInfo.GetPrintableWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableWidth;
end;

function TSynEdit32PrinterInfo.GetRightGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FRightGutter;
end;

function TSynEdit32PrinterInfo.GetTopGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FTopGutter;
end;

function TSynEdit32PrinterInfo.GetXPixPrInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPrInch;
end;

function TSynEdit32PrinterInfo.GetXPixPrmm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPrmm;
end;

function TSynEdit32PrinterInfo.GetYPixPrInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPrInch;
end;

function TSynEdit32PrinterInfo.GetYPixPrmm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPrmm;
end;

procedure TSynEdit32PrinterInfo.UpdatePrinter;
begin
  FIsUpdated := True;
  Printer.Refresh;
  if Printer.Printers.Count <= 0 then
  begin
    FillDefault;
    Exit;
  end;
{$IFNDEF SYN_CLX}
  FPhysicalWidth := GetDeviceCaps(Printer.Handle, Windows.PhysicalWidth);
  FPhysicalHeight := GetDeviceCaps(Printer.Handle, Windows.PhysicalHeight);
{$ENDIF}
  FPrintableWidth := Printer.PageWidth; {or GetDeviceCaps(Printer.Handle, HorzRes);}
  FPrintableHeight := Printer.PageHeight; {or GetDeviceCaps(Printer.Handle, VertRes);}
{$IFNDEF SYN_CLX}
  FLeftGutter := GetDeviceCaps(Printer.Handle, PhysicalOffsetX);
  FTopGutter := GetDeviceCaps(Printer.Handle, PhysicalOffsetY);
{$ENDIF}
  FRightGutter := FPhysicalWidth - FPrintableWidth - FLeftGutter;
  FBottomGutter := FPhysicalHeight - FPrintableHeight - FTopGutter;
{$IFNDEF SYN_CLX}
  FXPixPrInch := GetDeviceCaps(Printer.Handle, LogPixelsX);
  FYPixPrInch := GetDeviceCaps(Printer.Handle, LogPixelsY);
{$ENDIF}
  FXPixPrmm := FXPixPrInch / 25.4;
  FYPixPrmm := FYPixPrInch / 25.4;
end;

end.

