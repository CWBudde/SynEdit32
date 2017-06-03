{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditRegexSearch.pas, released 2002-07-26.

Original Code by Eduardo Mauro, Gerald Nunn and Flávio Etrusco.
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

$Id: SynEditRegexSearch.pas,v 1.5.2.2 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEdit32.RegexSearch;

{$I SynEdit.inc}

interface

uses
  SynEdit32.Types,
  SynEdit32.RegExpr,
  SynEdit32.MiscClasses,
  SynEdit32.Unicode,
  Classes;

type
  TSynEdit32RegexSearch = class(TSynEdit32SearchCustom)
  private
    FRegex: TRegExpr;
    FPositions: TList;
    FLengths: TList;
  protected
    function GetPattern: UnicodeString; override;
    procedure SetPattern(const Value: UnicodeString); override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindAll(const NewText: UnicodeString): Integer; override;
    function Replace(const aOccurrence, aReplacement: UnicodeString): UnicodeString; override;
  end;

implementation

uses
  Consts;

{ TSynEdit32RegexSearch }

constructor TSynEdit32RegexSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegex := TRegExpr.Create;
  FPositions := TList.Create;
  FLengths := TList.Create;
end;

destructor TSynEdit32RegexSearch.Destroy;
begin
  inherited;
  FRegex.Free;
  FPositions.Free;
  FLengths.Free;
end;

function TSynEdit32RegexSearch.FindAll(const NewText: UnicodeString): Integer;

  procedure AddResult(const aPos, aLength: Integer);
  begin
    FPositions.Add(Pointer(aPos));
    FLengths.Add(Pointer(aLength));
  end;

begin
  FPositions.Clear;
  FLengths.Clear;
  if FRegex.Exec(NewText) then
  begin
    AddResult(FRegex.MatchPos[0], FRegex.MatchLen[0]);
    Result := 1;
    while FRegex.ExecNext do
    begin
      AddResult(FRegex.MatchPos[0], FRegex.MatchLen[0]);
      Inc(Result);
    end;
  end
  else
    Result := 0;
end;

function TSynEdit32RegexSearch.Replace(const aOccurrence, aReplacement: UnicodeString): UnicodeString;
begin
  Result := FRegex.Replace(aOccurrence, aReplacement, True);
end;   

function TSynEdit32RegexSearch.GetLength(Index: Integer): Integer;
begin
  Result := Integer(FLengths[Index]);
end;

function TSynEdit32RegexSearch.GetPattern: UnicodeString;
begin
  Result := FRegex.Expression;
end;

function TSynEdit32RegexSearch.GetResult(Index: Integer): Integer;
begin
  Result := Integer(FPositions[Index]);
end;

function TSynEdit32RegexSearch.GetResultCount: Integer;
begin
  Result := FPositions.Count;
end;

procedure TSynEdit32RegexSearch.SetOptions(const Value: TSynSearchOptions);
begin
  FRegex.ModifierI := not(ssoMatchCase in Value);
end;

procedure TSynEdit32RegexSearch.SetPattern(const Value: UnicodeString);
begin
  FRegex.Expression := Value;
end;

end.

