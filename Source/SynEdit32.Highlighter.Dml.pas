{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterDml.pas, released 2000-04-17.
The Original Code is based on the mwDmlSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Peter Adam.
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

$Id: SynHighlighterDml.pas,v 1.11.2.7 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - There are no metadata qualifiers.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Dml highlighter for SynEdit)
@author(Peter Adam)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterDml unit provides SynEdit with a Dml highlighter.
}

unit SynEdit32.Highlighter.Dml;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Unicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkBlock, tkComment, tkForm, tkFunction, tkIdentifier, tkKey,
    tkNull, tkNumber, tkQualifier, tkSpace, tkSpecial, tkString, tkSymbol,
    tkUnknown, tkVariable);

  TRangeState = (rsANil, rsAdd, rsFind, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynEdit32HighlighterDml = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FIdentFuncTable: array[0..2438] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    FFormAttri: TSynEdit32HighlighterAttributes;
    FBlockAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FQualiAttri: TSynEdit32HighlighterAttributes;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FFunctionAttri: TSynEdit32HighlighterAttributes;
    FVariableAttri: TSynEdit32HighlighterAttributes;
    FSpecialAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAbs(Index: Integer): TtkTokenKind;
    function FuncAbsolute_position(Index: Integer): TtkTokenKind;
    function FuncAccount(Index: Integer): TtkTokenKind;
    function FuncAcos(Index: Integer): TtkTokenKind;
    function FuncActual_break(Index: Integer): TtkTokenKind;
    function FuncAdd(Index: Integer): TtkTokenKind;
    function FuncAdd_form(Index: Integer): TtkTokenKind;
    function FuncAlternate_form(Index: Integer): TtkTokenKind;
    function FuncAscii(Index: Integer): TtkTokenKind;
    function FuncAsin(Index: Integer): TtkTokenKind;
    function FuncAtan(Index: Integer): TtkTokenKind;
    function FuncAtan2(Index: Integer): TtkTokenKind;
    function FuncAttributes(Index: Integer): TtkTokenKind;
    function FuncBack(Index: Integer): TtkTokenKind;
    function FuncBase(Index: Integer): TtkTokenKind;
    function FuncBatch(Index: Integer): TtkTokenKind;
    function FuncBegin_block(Index: Integer): TtkTokenKind;
    function FuncBegin_case(Index: Integer): TtkTokenKind;
    function FuncBegin_disable_trigger(Index: Integer): TtkTokenKind;
    function FuncBegin_row(Index: Integer): TtkTokenKind;
    function FuncBegin_signal_to_status(Index: Integer): TtkTokenKind;
    function FuncBell(Index: Integer): TtkTokenKind;
    function FuncBinary_to_poly(Index: Integer): TtkTokenKind;
    function FuncBottom_line(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncBreak0(Index: Integer): TtkTokenKind;
    function FuncCall(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCeil(Index: Integer): TtkTokenKind;
    function FuncCheck(Index: Integer): TtkTokenKind;
    function FuncCheck_domain(Index: Integer): TtkTokenKind;
    function FuncChr(Index: Integer): TtkTokenKind;
    function FuncClear_buffer(Index: Integer): TtkTokenKind;
    function FuncCli(Index: Integer): TtkTokenKind;
    function FuncClose(Index: Integer): TtkTokenKind;
    function FuncClose_text(Index: Integer): TtkTokenKind;
    function FuncCol(Index: Integer): TtkTokenKind;
    function FuncColumn_heading_row(Index: Integer): TtkTokenKind;
    function FuncColumn_headings(Index: Integer): TtkTokenKind;
    function FuncColumn_spacing(Index: Integer): TtkTokenKind;
    function FuncCommit(Index: Integer): TtkTokenKind;
    function FuncCommit_rate(Index: Integer): TtkTokenKind;
    function FuncCompile(Index: Integer): TtkTokenKind;
    function FuncCompress(Index: Integer): TtkTokenKind;
    function FuncCompress_all(Index: Integer): TtkTokenKind;
    function FuncConfirm(Index: Integer): TtkTokenKind;
    function FuncConnect(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncCos(Index: Integer): TtkTokenKind;
    function FuncCosh(Index: Integer): TtkTokenKind;
    function FuncCross_reference(Index: Integer): TtkTokenKind;
    function FuncDate(Index: Integer): TtkTokenKind;
    function FuncDate_seconds(Index: Integer): TtkTokenKind;
    function FuncDay_of_week(Index: Integer): TtkTokenKind;
    function FuncDays(Index: Integer): TtkTokenKind;
    function FuncDcl(Index: Integer): TtkTokenKind;
    function FuncDefault_tag(Index: Integer): TtkTokenKind;
    function FuncDelete(Index: Integer): TtkTokenKind;
    function FuncDelete_form(Index: Integer): TtkTokenKind;
    function FuncDescription(Index: Integer): TtkTokenKind;
    function FuncDir(Index: Integer): TtkTokenKind;
    function FuncDisconnect(Index: Integer): TtkTokenKind;
    function FuncDisplay(Index: Integer): TtkTokenKind;
    function FuncDisplay_length(Index: Integer): TtkTokenKind;
    function FuncDocumentation(Index: Integer): TtkTokenKind;
    function FuncDomain(Index: Integer): TtkTokenKind;
    function FuncEdit(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncElse_if(Index: Integer): TtkTokenKind;
    function FuncEnd_block(Index: Integer): TtkTokenKind;
    function FuncEnd_case(Index: Integer): TtkTokenKind;
    function FuncEnd_disable_trigger(Index: Integer): TtkTokenKind;
    function FuncEnd_execute(Index: Integer): TtkTokenKind;
    function FuncEnd_form(Index: Integer): TtkTokenKind;
    function FuncEnd_if(Index: Integer): TtkTokenKind;
    function FuncEnd_row(Index: Integer): TtkTokenKind;
    function FuncEnd_signal_to_status(Index: Integer): TtkTokenKind;
    function FuncEnd_while(Index: Integer): TtkTokenKind;
    function FuncErase(Index: Integer): TtkTokenKind;
    function FuncError(Index: Integer): TtkTokenKind;
    function FuncExecute(Index: Integer): TtkTokenKind;
    function FuncExit(Index: Integer): TtkTokenKind;
    function FuncExit_forward(Index: Integer): TtkTokenKind;
    function FuncExpand(Index: Integer): TtkTokenKind;
    function FuncExternal(Index: Integer): TtkTokenKind;
    function FuncFacility(Index: Integer): TtkTokenKind;
    function FuncFailure(Index: Integer): TtkTokenKind;
    function FuncFetch(Index: Integer): TtkTokenKind;
    function FuncFiles(Index: Integer): TtkTokenKind;
    function FuncFind(Index: Integer): TtkTokenKind;
    function FuncFind_form(Index: Integer): TtkTokenKind;
    function FuncFinish(Index: Integer): TtkTokenKind;
    function FuncFirst(Index: Integer): TtkTokenKind;
    function FuncFloor(Index: Integer): TtkTokenKind;
    function FuncFooting(Index: Integer): TtkTokenKind;
    function FuncFooting_form(Index: Integer): TtkTokenKind;
    function FuncForm(Index: Integer): TtkTokenKind;
    function FuncGenerate(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncGrouped_by(Index: Integer): TtkTokenKind;
    function FuncHeading(Index: Integer): TtkTokenKind;
    function FuncHeading_form(Index: Integer): TtkTokenKind;
    function FuncHeight(Index: Integer): TtkTokenKind;
    function FuncIdentifier(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncInput_block(Index: Integer): TtkTokenKind;
    function FuncInput_mask(Index: Integer): TtkTokenKind;
    function FuncInput_row_height(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncInvoke(Index: Integer): TtkTokenKind;
    function FuncItem(Index: Integer): TtkTokenKind;
    function FuncItem_block(Index: Integer): TtkTokenKind;
    function FuncItem_if(Index: Integer): TtkTokenKind;
    function FuncJoined_to(Index: Integer): TtkTokenKind;
    function FuncLeft(Index: Integer): TtkTokenKind;
    function FuncLen(Index: Integer): TtkTokenKind;
    function FuncLfooting(Index: Integer): TtkTokenKind;
    function FuncLheading(Index: Integer): TtkTokenKind;
    function FuncLine(Index: Integer): TtkTokenKind;
    function FuncLines_after(Index: Integer): TtkTokenKind;
    function FuncLines_before(Index: Integer): TtkTokenKind;
    function FuncList(Index: Integer): TtkTokenKind;
    function FuncLoad(Index: Integer): TtkTokenKind;
    function FuncLock(Index: Integer): TtkTokenKind;
    function FuncLog(Index: Integer): TtkTokenKind;
    function FuncLog10(Index: Integer): TtkTokenKind;
    function FuncLov(Index: Integer): TtkTokenKind;
    function FuncLov_auto_select(Index: Integer): TtkTokenKind;
    function FuncLov_col(Index: Integer): TtkTokenKind;
    function FuncLov_data(Index: Integer): TtkTokenKind;
    function FuncLov_first(Index: Integer): TtkTokenKind;
    function FuncLov_height(Index: Integer): TtkTokenKind;
    function FuncLov_noheading(Index: Integer): TtkTokenKind;
    function FuncLov_nosearch(Index: Integer): TtkTokenKind;
    function FuncLov_reduced_to(Index: Integer): TtkTokenKind;
    function FuncLov_row(Index: Integer): TtkTokenKind;
    function FuncLov_secondary(Index: Integer): TtkTokenKind;
    function FuncLov_selection(Index: Integer): TtkTokenKind;
    function FuncLov_sorted_by(Index: Integer): TtkTokenKind;
    function FuncLov_width(Index: Integer): TtkTokenKind;
    function FuncLov_with(Index: Integer): TtkTokenKind;
    function FuncLowercase(Index: Integer): TtkTokenKind;
    function FuncLtrim(Index: Integer): TtkTokenKind;
    function FuncMail(Index: Integer): TtkTokenKind;
    function FuncMenu(Index: Integer): TtkTokenKind;
    function FuncMenu_block(Index: Integer): TtkTokenKind;
    function FuncMenu_form(Index: Integer): TtkTokenKind;
    function FuncMessage(Index: Integer): TtkTokenKind;
    function FuncMid(Index: Integer): TtkTokenKind;
    function FuncMod(Index: Integer): TtkTokenKind;
    function FuncModify_form(Index: Integer): TtkTokenKind;
    function FuncNew(Index: Integer): TtkTokenKind;
    function FuncNo_domain(Index: Integer): TtkTokenKind;
    function FuncNobell(Index: Integer): TtkTokenKind;
    function FuncNoclear_buffer(Index: Integer): TtkTokenKind;
    function FuncNodeadlock_exit(Index: Integer): TtkTokenKind;
    function FuncNoerase(Index: Integer): TtkTokenKind;
    function FuncNoerror(Index: Integer): TtkTokenKind;
    function FuncNoexit_forward(Index: Integer): TtkTokenKind;
    function FuncNoheading(Index: Integer): TtkTokenKind;
    function FuncNolov_data(Index: Integer): TtkTokenKind;
    function FuncNorepeat(Index: Integer): TtkTokenKind;
    function FuncNostatus(Index: Integer): TtkTokenKind;
    function FuncNototals(Index: Integer): TtkTokenKind;
    function FuncNounderlines(Index: Integer): TtkTokenKind;
    function FuncNowait(Index: Integer): TtkTokenKind;
    function FuncOpen(Index: Integer): TtkTokenKind;
    function FuncOpen_text(Index: Integer): TtkTokenKind;
    function FuncOpt(Index: Integer): TtkTokenKind;
    function FuncOptions(Index: Integer): TtkTokenKind;
    function FuncOutput(Index: Integer): TtkTokenKind;
    function FuncOutput_block(Index: Integer): TtkTokenKind;
    function FuncOutput_mask(Index: Integer): TtkTokenKind;
    function FuncPause(Index: Integer): TtkTokenKind;
    function FuncPause_block(Index: Integer): TtkTokenKind;
    function FuncPerform(Index: Integer): TtkTokenKind;
    function FuncPoly_to_binary(Index: Integer): TtkTokenKind;
    function FuncPos(Index: Integer): TtkTokenKind;
    function FuncPrint(Index: Integer): TtkTokenKind;
    function FuncProcedure_form(Index: Integer): TtkTokenKind;
    function FuncPrompt(Index: Integer): TtkTokenKind;
    function FuncProtect(Index: Integer): TtkTokenKind;
    function FuncQuery(Index: Integer): TtkTokenKind;
    function FuncQuery_form(Index: Integer): TtkTokenKind;
    function FuncRandom(Index: Integer): TtkTokenKind;
    function FuncRead_line(Index: Integer): TtkTokenKind;
    function FuncRead_only(Index: Integer): TtkTokenKind;
    function FuncReceive(Index: Integer): TtkTokenKind;
    function FuncReceive_arguments(Index: Integer): TtkTokenKind;
    function FuncReceive_data(Index: Integer): TtkTokenKind;
    function FuncReceive_table(Index: Integer): TtkTokenKind;
    function FuncReduced_to(Index: Integer): TtkTokenKind;
    function FuncRelease(Index: Integer): TtkTokenKind;
    function FuncRemain(Index: Integer): TtkTokenKind;
    function FuncRepeat(Index: Integer): TtkTokenKind;
    function FuncReport(Index: Integer): TtkTokenKind;
    function FuncReport_form(Index: Integer): TtkTokenKind;
    function FuncReposition(Index: Integer): TtkTokenKind;
    function FuncRewind_text(Index: Integer): TtkTokenKind;
    function FuncRfooting(Index: Integer): TtkTokenKind;
    function FuncRheading(Index: Integer): TtkTokenKind;
    function FuncRight(Index: Integer): TtkTokenKind;
    function FuncRollback(Index: Integer): TtkTokenKind;
    function FuncRound(Index: Integer): TtkTokenKind;
    function FuncRow(Index: Integer): TtkTokenKind;
    function FuncRow_height(Index: Integer): TtkTokenKind;
    function FuncSearch(Index: Integer): TtkTokenKind;
    function FuncSecondary(Index: Integer): TtkTokenKind;
    function FuncSeconds(Index: Integer): TtkTokenKind;
    function FuncSelection(Index: Integer): TtkTokenKind;
    function FuncSend(Index: Integer): TtkTokenKind;
    function FuncSend_data(Index: Integer): TtkTokenKind;
    function FuncSend_message(Index: Integer): TtkTokenKind;
    function FuncSend_table(Index: Integer): TtkTokenKind;
    function FuncSequence(Index: Integer): TtkTokenKind;
    function FuncSeverity(Index: Integer): TtkTokenKind;
    function FuncSin(Index: Integer): TtkTokenKind;
    function FuncSinh(Index: Integer): TtkTokenKind;
    function FuncSorted_by(Index: Integer): TtkTokenKind;
    function FuncSource(Index: Integer): TtkTokenKind;
    function FuncSource_if(Index: Integer): TtkTokenKind;
    function FuncSqrt(Index: Integer): TtkTokenKind;
    function FuncStart_stream(Index: Integer): TtkTokenKind;
    function FuncStart_transaction(Index: Integer): TtkTokenKind;
    function FuncStatistic(Index: Integer): TtkTokenKind;
    function FuncStatus(Index: Integer): TtkTokenKind;
    function FuncStream_name(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncSuccess(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncSwitch_base(Index: Integer): TtkTokenKind;
    function FuncSystem(Index: Integer): TtkTokenKind;
    function FuncTable(Index: Integer): TtkTokenKind;
    function FuncTable_form(Index: Integer): TtkTokenKind;
    function FuncTable_search(Index: Integer): TtkTokenKind;
    function FuncTag(Index: Integer): TtkTokenKind;
    function FuncTag_length(Index: Integer): TtkTokenKind;
    function FuncTan(Index: Integer): TtkTokenKind;
    function FuncTanh(Index: Integer): TtkTokenKind;
    function FuncTarget(Index: Integer): TtkTokenKind;
    function FuncText(Index: Integer): TtkTokenKind;
    function FuncText_only(Index: Integer): TtkTokenKind;
    function FuncTitle(Index: Integer): TtkTokenKind;
    function FuncTo(Index: Integer): TtkTokenKind;
    function FuncTop_line(Index: Integer): TtkTokenKind;
    function FuncTotal(Index: Integer): TtkTokenKind;
    function FuncTransfer(Index: Integer): TtkTokenKind;
    function FuncTrigger(Index: Integer): TtkTokenKind;
    function FuncTrim(Index: Integer): TtkTokenKind;
    function FuncTsuppress(Index: Integer): TtkTokenKind;
    function FuncUnload(Index: Integer): TtkTokenKind;
    function FuncUppercase(Index: Integer): TtkTokenKind;
    function FuncUse_if(Index: Integer): TtkTokenKind;
    function FuncUser_key(Index: Integer): TtkTokenKind;
    function FuncUsing(Index: Integer): TtkTokenKind;
    function FuncUtilities(Index: Integer): TtkTokenKind;
    function FuncWait(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncWidth(Index: Integer): TtkTokenKind;
    function FuncWith(Index: Integer): TtkTokenKind;
    function FuncWrite(Index: Integer): TtkTokenKind;
    function FuncWrite_line(Index: Integer): TtkTokenKind;
    function FuncYesno_block(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure SymbolProc;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure RemProc;
    function IsQuali: Boolean;
    function IsSpecial: Boolean;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property BlockAttri: TSynEdit32HighlighterAttributes read FBlockAttri
      write FBlockAttri;
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property FormAttri: TSynEdit32HighlighterAttributes read FFormAttri
      write FFormAttri;
    property FunctionAttri: TSynEdit32HighlighterAttributes read FFunctionAttri
      write FFunctionAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property QualiAttri: TSynEdit32HighlighterAttributes read FQualiAttri
      write FQualiAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SpecialAttri: TSynEdit32HighlighterAttributes read FSpecialAttri
      write FSpecialAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property VariableAttri: TSynEdit32HighlighterAttributes read FVariableAttri
      write FVariableAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..263] of UnicodeString = (
    'abs', 'absolute_position', 'account', 'acos', 'actual_break', 'add', 
    'add_form', 'alternate_form', 'ascii', 'asin', 'atan', 'atan2', 
    'attributes', 'back', 'base', 'batch', 'begin_block', 'begin_case', 
    'begin_disable_trigger', 'begin_row', 'begin_signal_to_status', 'bell', 
    'binary_to_poly', 'bottom_line', 'break', 'break0', 'call', 'case', 'ceil', 
    'check', 'check_domain', 'chr', 'clear_buffer', 'cli', 'close', 
    'close_text', 'col', 'column_heading_row', 'column_headings', 
    'column_spacing', 'commit', 'commit_rate', 'compile', 'compress', 
    'compress_all', 'confirm', 'connect', 'continue', 'cos', 'cosh', 
    'cross_reference', 'date', 'date_seconds', 'day_of_week', 'days', 'dcl', 
    'default_tag', 'delete', 'delete_form', 'description', 'dir', 'disconnect', 
    'display', 'display_length', 'documentation', 'domain', 'edit', 'else', 
    'else_if', 'end_block', 'end_case', 'end_disable_trigger', 'end_execute', 
    'end_form', 'end_if', 'end_row', 'end_signal_to_status', 'end_while', 
    'erase', 'error', 'execute', 'exit', 'exit_forward', 'expand', 'external', 
    'facility', 'failure', 'fetch', 'files', 'find', 'find_form', 'finish', 
    'first', 'floor', 'footing', 'footing_form', 'form', 'generate', 'goto', 
    'grouped_by', 'heading', 'heading_form', 'height', 'identifier', 'if', 'in', 
    'input_block', 'input_mask', 'input_row_height', 'int', 'invoke', 'item', 
    'item_block', 'item_if', 'joined_to', 'left', 'len', 'lfooting', 'lheading', 
    'line', 'lines_after', 'lines_before', 'list', 'load', 'lock', 'log', 
    'log10', 'lov', 'lov_auto_select', 'lov_col', 'lov_data', 'lov_first', 
    'lov_height', 'lov_noheading', 'lov_nosearch', 'lov_reduced_to', 'lov_row', 
    'lov_secondary', 'lov_selection', 'lov_sorted_by', 'lov_width', 'lov_with', 
    'lowercase', 'ltrim', 'mail', 'menu', 'menu_block', 'menu_form', 'message', 
    'mid', 'mod', 'modify_form', 'new', 'no_domain', 'nobell', 'noclear_buffer', 
    'nodeadlock_exit', 'noerase', 'noerror', 'noexit_forward', 'noheading', 
    'nolov_data', 'norepeat', 'nostatus', 'nototals', 'nounderlines', 'nowait', 
    'open', 'open_text', 'opt', 'options', 'output', 'output_block', 
    'output_mask', 'pause', 'pause_block', 'perform', 'poly_to_binary', 'pos', 
    'print', 'procedure_form', 'prompt', 'protect', 'query', 'query_form', 
    'random', 'read_line', 'read_only', 'receive', 'receive_arguments', 
    'receive_data', 'receive_table', 'reduced_to', 'release', 'remain', 
    'repeat', 'report', 'report_form', 'reposition', 'rewind_text', 'rfooting', 
    'rheading', 'right', 'rollback', 'round', 'row', 'row_height', 'search', 
    'secondary', 'seconds', 'selection', 'send', 'send_data', 'send_message', 
    'send_table', 'sequence', 'severity', 'sin', 'sinh', 'sorted_by', 'source', 
    'source_if', 'sqrt', 'start_stream', 'start_transaction', 'statistic', 
    'status', 'stream_name', 'string', 'success', 'switch', 'switch_base', 
    'system', 'table', 'table_form', 'table_search', 'tag', 'tag_length', 'tan', 
    'tanh', 'target', 'text', 'text_only', 'title', 'to', 'top_line', 'total', 
    'transfer', 'trigger', 'trim', 'tsuppress', 'unload', 'uppercase', 'use_if', 
    'user_key', 'using', 'utilities', 'wait', 'while', 'width', 'with', 'write', 
    'write_line', 'yesno_block' 
  );

  KeyIndices: array[0..2438] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 261, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 230, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 217, -1, -1, -1, -1, -1, 183, -1, 246, -1, 134, -1, -1, -1, -1, 
    -1, 65, -1, -1, 223, -1, -1, -1, -1, -1, 213, -1, -1, -1, 46, -1, -1, 262, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 111, 157, -1, -1, -1, -1, -1, -1, 118, -1, -1, -1, -1, -1, -1, 
    -1, -1, 208, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 86, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 123, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 84, -1, 154, -1, 96, -1, -1, -1, 176, -1, -1, -1, 120, 178, -1, -1, -1, 
    -1, 74, -1, -1, -1, -1, 241, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 81, -1, 147, -1, -1, -1, 122, 
    -1, 58, -1, 87, 191, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 170, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 5, -1, -1, 194, -1, -1, -1, 243, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 52, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 248, -1, -1, 
    -1, 28, 77, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 255, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 239, -1, -1, -1, -1, 20, -1, -1, -1, -1, 79, 
    116, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 0, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 196, -1, -1, 85, -1, -1, -1, 104, -1, 103, -1, -1, 14, -1, -1, 
    131, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 105, -1, 
    -1, 6, -1, 182, -1, -1, 171, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 80, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 97, -1, -1, -1, -1, 41, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 204, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 185, -1, -1, -1, -1, 
    -1, -1, -1, 115, -1, -1, 108, -1, 150, -1, -1, 42, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 224, -1, -1, -1, 59, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 143, 166, -1, -1, 
    -1, -1, -1, -1, 225, -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, 92, -1, -1, 
    226, -1, 161, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, -1, -1, 237, -1, -1, -1, -1, 
    -1, 100, -1, -1, -1, -1, -1, -1, -1, -1, 214, -1, -1, -1, -1, -1, -1, -1, 
    -1, 151, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1, -1, 146, 210, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 23, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 245, -1, -1, -1, -1, -1, 68, -1, 
    231, -1, -1, -1, 126, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 57, 
    -1, -1, -1, 112, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 
    -1, -1, 75, -1, 252, 212, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    149, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 187, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 
    -1, -1, 47, -1, -1, -1, -1, -1, -1, -1, -1, 164, 35, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    234, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, -1, -1, 
    -1, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 240, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 39, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 72, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, -1, 229, 11, 
    -1, -1, 43, -1, -1, -1, -1, -1, 236, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 159, -1, -1, 238, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 181, -1, 139, -1, -1, -1, -1, -1, -1, 37, -1, -1, -1, 15, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 83, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 69, 
    258, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, -1, 153, 
    -1, -1, 36, -1, -1, 175, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, -1, -1, -1, 
    -1, -1, -1, -1, 48, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, 232, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 199, -1, -1, -1, -1, -1, 9, -1, -1, 140, 193, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 177, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 163, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 188, -1, -1, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1, 
    -1, -1, 228, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    19, -1, 66, -1, -1, -1, 24, -1, -1, -1, -1, 186, -1, -1, -1, -1, 99, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 253, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 54, -1, -1, -1, -1, 259, -1, 32, -1, -1, -1, -1, -1, 
    121, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 221, 209, 260, -1, 
    -1, -1, -1, -1, -1, -1, 76, 257, -1, -1, -1, -1, 211, -1, 90, -1, -1, -1, 
    -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, 70, 
    -1, -1, -1, -1, -1, 63, -1, -1, -1, 25, 207, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 174, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 167, -1, -1, -1, -1, -1, -1, -1, -1, 179, -1, 
    189, -1, -1, -1, 113, -1, -1, -1, 110, -1, 205, -1, 56, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 51, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 132, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1, -1, -1, 
    -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 180, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, -1, 254, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, -1, -1, 155, -1, -1, 
    235, -1, 34, -1, 218, -1, -1, -1, -1, -1, -1, 152, -1, -1, -1, -1, 220, -1, 
    -1, -1, 141, -1, -1, -1, -1, 195, -1, -1, -1, 137, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, 160, -1, 
    -1, -1, -1, 227, -1, -1, -1, -1, -1, -1, -1, 148, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 203, 
    -1, -1, -1, -1, -1, -1, -1, -1, 156, -1, -1, -1, -1, -1, -1, -1, -1, -1, 91, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 62, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 219, -1, -1, -1, -1, -1, 61, -1, -1, 30, -1, -1, 130, -1, -1, -1, 
    -1, -1, -1, -1, -1, 12, -1, 202, -1, -1, -1, -1, -1, 200, -1, -1, 169, -1, 
    -1, -1, -1, -1, -1, 16, -1, -1, -1, 172, -1, -1, -1, -1, -1, -1, 162, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 114, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 247, -1, -1, -1, -1, -1, -1, 242, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 198, -1, -1, -1, 251, -1, -1, -1, -1, -1, 
    -1, -1, -1, 216, -1, -1, -1, -1, 128, 27, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 18, -1, -1, -1, 
    -1, 158, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, -1, 173, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, 17, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 135, -1, -1, 190, -1, -1, -1, 222, 60, -1, -1, -1, -1, 
    -1, -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 98, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 136, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 168, -1, -1, -1, -1, -1, -1, 144, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 94, -1, 95, -1, -1, -1, -1, -1, 215, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 145, -1, 
    -1, -1, 10, 250, -1, -1, -1, 256, -1, -1, -1, -1, -1, -1, -1, -1, -1, 197, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 201, -1, -1, -1, 
    233, -1, -1, -1, -1, -1, 249, -1, -1, 184, -1, -1, -1, -1, -1, 263, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 26, 138, -1, -1, -1, -1, -1, -1, -1, -1, 64, 
    -1, -1, -1, 55, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, 127, 206, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 165, -1, 
    -1, 244, -1, -1, -1, -1, -1, -1, -1, -1 
  );

{$Q-}
function TSynEdit32HighlighterDml.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 798 + Ord(Str^) * 3;
    Inc(Str);
  end;
  Result := Result mod 2439;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterDml.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynEdit32HighlighterDml.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[435] := FuncAbs;
  FIdentFuncTable[41] := FuncAbsolute_position;
  FIdentFuncTable[13] := FuncAccount;
  FIdentFuncTable[2405] := FuncAcos;
  FIdentFuncTable[1707] := FuncActual_break;
  FIdentFuncTable[300] := FuncAdd;
  FIdentFuncTable[486] := FuncAdd_form;
  FIdentFuncTable[839] := FuncAlternate_form;
  FIdentFuncTable[735] := FuncAscii;
  FIdentFuncTable[1265] := FuncAsin;
  FIdentFuncTable[2300] := FuncAtan;
  FIdentFuncTable[1065] := FuncAtan2;
  FIdentFuncTable[1930] := FuncAttributes;
  FIdentFuncTable[1202] := FuncBack;
  FIdentFuncTable[464] := FuncBase;
  FIdentFuncTable[1118] := FuncBatch;
  FIdentFuncTable[1948] := FuncBegin_block;
  FIdentFuncTable[2124] := FuncBegin_case;
  FIdentFuncTable[2068] := FuncBegin_disable_trigger;
  FIdentFuncTable[1385] := FuncBegin_row;
  FIdentFuncTable[387] := FuncBegin_signal_to_status;
  FIdentFuncTable[1061] := FuncBell;
  FIdentFuncTable[166] := FuncBinary_to_poly;
  FIdentFuncTable[776] := FuncBottom_line;
  FIdentFuncTable[1391] := FuncBreak;
  FIdentFuncTable[1524] := FuncBreak0;
  FIdentFuncTable[2380] := FuncCall;
  FIdentFuncTable[2044] := FuncCase;
  FIdentFuncTable[337] := FuncCeil;
  FIdentFuncTable[644] := FuncCheck;
  FIdentFuncTable[1918] := FuncCheck_domain;
  FIdentFuncTable[1512] := FuncChr;
  FIdentFuncTable[1454] := FuncClear_buffer;
  FIdentFuncTable[1305] := FuncCli;
  FIdentFuncTable[1761] := FuncClose;
  FIdentFuncTable[908] := FuncClose_text;
  FIdentFuncTable[1179] := FuncCol;
  FIdentFuncTable[1114] := FuncColumn_heading_row;
  FIdentFuncTable[2183] := FuncColumn_headings;
  FIdentFuncTable[1007] := FuncColumn_spacing;
  FIdentFuncTable[697] := FuncCommit;
  FIdentFuncTable[521] := FuncCommit_rate;
  FIdentFuncTable[591] := FuncCompile;
  FIdentFuncTable[1068] := FuncCompress;
  FIdentFuncTable[1359] := FuncCompress_all;
  FIdentFuncTable[1637] := FuncConfirm;
  FIdentFuncTable[89] := FuncConnect;
  FIdentFuncTable[898] := FuncContinue;
  FIdentFuncTable[1200] := FuncCos;
  FIdentFuncTable[1747] := FuncCosh;
  FIdentFuncTable[954] := FuncCross_reference;
  FIdentFuncTable[1630] := FuncDate;
  FIdentFuncTable[320] := FuncDate_seconds;
  FIdentFuncTable[368] := FuncDay_of_week;
  FIdentFuncTable[1447] := FuncDays;
  FIdentFuncTable[2394] := FuncDcl;
  FIdentFuncTable[1583] := FuncDefault_tag;
  FIdentFuncTable[820] := FuncDelete;
  FIdentFuncTable[261] := FuncDelete_form;
  FIdentFuncTable[608] := FuncDescription;
  FIdentFuncTable[2142] := FuncDir;
  FIdentFuncTable[1915] := FuncDisconnect;
  FIdentFuncTable[1889] := FuncDisplay;
  FIdentFuncTable[1520] := FuncDisplay_length;
  FIdentFuncTable[2390] := FuncDocumentation;
  FIdentFuncTable[76] := FuncDomain;
  FIdentFuncTable[1387] := FuncEdit;
  FIdentFuncTable[1414] := FuncElse;
  FIdentFuncTable[801] := FuncElse_if;
  FIdentFuncTable[1158] := FuncEnd_block;
  FIdentFuncTable[1514] := FuncEnd_case;
  FIdentFuncTable[1734] := FuncEnd_disable_trigger;
  FIdentFuncTable[1043] := FuncEnd_execute;
  FIdentFuncTable[2119] := FuncEnd_form;
  FIdentFuncTable[224] := FuncEnd_if;
  FIdentFuncTable[842] := FuncEnd_row;
  FIdentFuncTable[1484] := FuncEnd_signal_to_status;
  FIdentFuncTable[338] := FuncEnd_while;
  FIdentFuncTable[893] := FuncErase;
  FIdentFuncTable[392] := FuncError;
  FIdentFuncTable[503] := FuncExecute;
  FIdentFuncTable[253] := FuncExit;
  FIdentFuncTable[1280] := FuncExit_forward;
  FIdentFuncTable[1146] := FuncExpand;
  FIdentFuncTable[206] := FuncExternal;
  FIdentFuncTable[455] := FuncFacility;
  FIdentFuncTable[176] := FuncFailure;
  FIdentFuncTable[263] := FuncFetch;
  FIdentFuncTable[2106] := FuncFiles;
  FIdentFuncTable[1191] := FuncFind;
  FIdentFuncTable[1492] := FuncFind_form;
  FIdentFuncTable[1868] := FuncFinish;
  FIdentFuncTable[651] := FuncFirst;
  FIdentFuncTable[2081] := FuncFloor;
  FIdentFuncTable[2267] := FuncFooting;
  FIdentFuncTable[2269] := FuncFooting_form;
  FIdentFuncTable[210] := FuncForm;
  FIdentFuncTable[516] := FuncGenerate;
  FIdentFuncTable[2196] := FuncGoto;
  FIdentFuncTable[1401] := FuncGrouped_by;
  FIdentFuncTable[711] := FuncHeading;
  FIdentFuncTable[1173] := FuncHeading_form;
  FIdentFuncTable[194] := FuncHeight;
  FIdentFuncTable[461] := FuncIdentifier;
  FIdentFuncTable[459] := FuncIf;
  FIdentFuncTable[483] := FuncIn;
  FIdentFuncTable[2151] := FuncInput_block;
  FIdentFuncTable[947] := FuncInput_mask;
  FIdentFuncTable[586] := FuncInput_row_height;
  FIdentFuncTable[420] := FuncInt;
  FIdentFuncTable[1579] := FuncInvoke;
  FIdentFuncTable[134] := FuncItem;
  FIdentFuncTable[824] := FuncItem_block;
  FIdentFuncTable[1575] := FuncItem_if;
  FIdentFuncTable[1988] := FuncJoined_to;
  FIdentFuncTable[583] := FuncLeft;
  FIdentFuncTable[393] := FuncLen;
  FIdentFuncTable[1698] := FuncLfooting;
  FIdentFuncTable[142] := FuncLheading;
  FIdentFuncTable[439] := FuncLine;
  FIdentFuncTable[218] := FuncLines_after;
  FIdentFuncTable[1460] := FuncLines_before;
  FIdentFuncTable[259] := FuncList;
  FIdentFuncTable[193] := FuncLoad;
  FIdentFuncTable[124] := FuncLock;
  FIdentFuncTable[2361] := FuncLog;
  FIdentFuncTable[807] := FuncLog10;
  FIdentFuncTable[2406] := FuncLov;
  FIdentFuncTable[2043] := FuncLov_auto_select;
  FIdentFuncTable[1806] := FuncLov_col;
  FIdentFuncTable[1921] := FuncLov_data;
  FIdentFuncTable[467] := FuncLov_first;
  FIdentFuncTable[1673] := FuncLov_height;
  FIdentFuncTable[1499] := FuncLov_noheading;
  FIdentFuncTable[70] := FuncLov_nosearch;
  FIdentFuncTable[2134] := FuncLov_reduced_to;
  FIdentFuncTable[2208] := FuncLov_row;
  FIdentFuncTable[1788] := FuncLov_secondary;
  FIdentFuncTable[2381] := FuncLov_selection;
  FIdentFuncTable[1107] := FuncLov_sorted_by;
  FIdentFuncTable[1268] := FuncLov_width;
  FIdentFuncTable[1779] := FuncLov_with;
  FIdentFuncTable[538] := FuncLowercase;
  FIdentFuncTable[631] := FuncLtrim;
  FIdentFuncTable[2233] := FuncMail;
  FIdentFuncTable[2296] := FuncMenu;
  FIdentFuncTable[743] := FuncMenu_block;
  FIdentFuncTable[255] := FuncMenu_form;
  FIdentFuncTable[1824] := FuncMessage;
  FIdentFuncTable[858] := FuncMid;
  FIdentFuncTable[588] := FuncMod;
  FIdentFuncTable[729] := FuncModify_form;
  FIdentFuncTable[1770] := FuncNew;
  FIdentFuncTable[1176] := FuncNo_domain;
  FIdentFuncTable[208] := FuncNobell;
  FIdentFuncTable[1756] := FuncNoclear_buffer;
  FIdentFuncTable[1858] := FuncNodeadlock_exit;
  FIdentFuncTable[135] := FuncNoerase;
  FIdentFuncTable[2073] := FuncNoerror;
  FIdentFuncTable[1092] := FuncNoexit_forward;
  FIdentFuncTable[1811] := FuncNoheading;
  FIdentFuncTable[656] := FuncNolov_data;
  FIdentFuncTable[1959] := FuncNorepeat;
  FIdentFuncTable[1319] := FuncNostatus;
  FIdentFuncTable[907] := FuncNototals;
  FIdentFuncTable[2427] := FuncNounderlines;
  FIdentFuncTable[632] := FuncNowait;
  FIdentFuncTable[1560] := FuncOpen;
  FIdentFuncTable[2226] := FuncOpen_text;
  FIdentFuncTable[1941] := FuncOpt;
  FIdentFuncTable[290] := FuncOptions;
  FIdentFuncTable[491] := FuncOutput;
  FIdentFuncTable[1952] := FuncOutput_block;
  FIdentFuncTable[2108] := FuncOutput_mask;
  FIdentFuncTable[1539] := FuncPause;
  FIdentFuncTable[1182] := FuncPause_block;
  FIdentFuncTable[214] := FuncPerform;
  FIdentFuncTable[1294] := FuncPoly_to_binary;
  FIdentFuncTable[219] := FuncPos;
  FIdentFuncTable[1569] := FuncPrint;
  FIdentFuncTable[1719] := FuncProcedure_form;
  FIdentFuncTable[1105] := FuncPrompt;
  FIdentFuncTable[488] := FuncProtect;
  FIdentFuncTable[66] := FuncQuery;
  FIdentFuncTable[2344] := FuncQuery_form;
  FIdentFuncTable[575] := FuncRandom;
  FIdentFuncTable[1396] := FuncRead_line;
  FIdentFuncTable[885] := FuncRead_only;
  FIdentFuncTable[1353] := FuncReceive;
  FIdentFuncTable[1571] := FuncReceive_arguments;
  FIdentFuncTable[2137] := FuncReceive_data;
  FIdentFuncTable[264] := FuncReceive_table;
  FIdentFuncTable[410] := FuncReduced_to;
  FIdentFuncTable[1269] := FuncRelease;
  FIdentFuncTable[303] := FuncRemain;
  FIdentFuncTable[1784] := FuncRepeat;
  FIdentFuncTable[452] := FuncReport;
  FIdentFuncTable[2315] := FuncReport_form;
  FIdentFuncTable[2025] := FuncReposition;
  FIdentFuncTable[1259] := FuncRewind_text;
  FIdentFuncTable[1938] := FuncRfooting;
  FIdentFuncTable[2331] := FuncRheading;
  FIdentFuncTable[1932] := FuncRight;
  FIdentFuncTable[1849] := FuncRollback;
  FIdentFuncTable[553] := FuncRound;
  FIdentFuncTable[1581] := FuncRow;
  FIdentFuncTable[2407] := FuncRow_height;
  FIdentFuncTable[1525] := FuncSearch;
  FIdentFuncTable[151] := FuncSecondary;
  FIdentFuncTable[1475] := FuncSeconds;
  FIdentFuncTable[744] := FuncSelection;
  FIdentFuncTable[1490] := FuncSend;
  FIdentFuncTable[845] := FuncSend_data;
  FIdentFuncTable[85] := FuncSend_message;
  FIdentFuncTable[720] := FuncSend_table;
  FIdentFuncTable[2275] := FuncSequence;
  FIdentFuncTable[2038] := FuncSeverity;
  FIdentFuncTable[60] := FuncSin;
  FIdentFuncTable[1763] := FuncSinh;
  FIdentFuncTable[1909] := FuncSorted_by;
  FIdentFuncTable[1775] := FuncSource;
  FIdentFuncTable[1474] := FuncSource_if;
  FIdentFuncTable[2141] := FuncSqrt;
  FIdentFuncTable[79] := FuncStart_stream;
  FIdentFuncTable[604] := FuncStart_transaction;
  FIdentFuncTable[639] := FuncStatistic;
  FIdentFuncTable[654] := FuncStatus;
  FIdentFuncTable[1816] := FuncStream_name;
  FIdentFuncTable[1368] := FuncString;
  FIdentFuncTable[1064] := FuncSuccess;
  FIdentFuncTable[39] := FuncSwitch;
  FIdentFuncTable[803] := FuncSwitch_base;
  FIdentFuncTable[1212] := FuncSystem;
  FIdentFuncTable[2335] := FuncTable;
  FIdentFuncTable[934] := FuncTable_form;
  FIdentFuncTable[1759] := FuncTable_search;
  FIdentFuncTable[1074] := FuncTag;
  FIdentFuncTable[705] := FuncTag_length;
  FIdentFuncTable[1095] := FuncTan;
  FIdentFuncTable[382] := FuncTanh;
  FIdentFuncTable[975] := FuncTarget;
  FIdentFuncTable[229] := FuncText;
  FIdentFuncTable[2007] := FuncText_only;
  FIdentFuncTable[307] := FuncTitle;
  FIdentFuncTable[2430] := FuncTo;
  FIdentFuncTable[795] := FuncTop_line;
  FIdentFuncTable[68] := FuncTotal;
  FIdentFuncTable[2000] := FuncTransfer;
  FIdentFuncTable[333] := FuncTrigger;
  FIdentFuncTable[2341] := FuncTrim;
  FIdentFuncTable[2301] := FuncTsuppress;
  FIdentFuncTable[2029] := FuncUnload;
  FIdentFuncTable[844] := FuncUppercase;
  FIdentFuncTable[1437] := FuncUse_if;
  FIdentFuncTable[1736] := FuncUser_key;
  FIdentFuncTable[353] := FuncUsing;
  FIdentFuncTable[2305] := FuncUtilities;
  FIdentFuncTable[1485] := FuncWait;
  FIdentFuncTable[1159] := FuncWhile;
  FIdentFuncTable[1452] := FuncWidth;
  FIdentFuncTable[1476] := FuncWith;
  FIdentFuncTable[27] := FuncWrite;
  FIdentFuncTable[92] := FuncWrite_line;
  FIdentFuncTable[2350] := FuncYesno_block;
end;

function TSynEdit32HighlighterDml.IsQuali: boolean;
begin
  Result:= False;
  if FRun > 0 then
    if FLine[FRun - 1] = '/' then Result:= True;
end;

function TSynEdit32HighlighterDml.IsSpecial: Boolean;
begin
  Result:= False;
  if FRun > 0 then
    if FLine[FRun - 1] = '%' then Result:= True;
end;

function TSynEdit32HighlighterDml.FuncAbs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAbsolute_position(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAccount(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAcos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncActual_break(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAdd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsSpecial then
      Result := tkSpecial
    else
    begin
      Result := tkKey;
      FRange := rsAdd;
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAdd_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAlternate_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAscii(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAsin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAtan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAtan2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncAttributes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBegin_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBegin_case(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBegin_disable_trigger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBegin_row(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBegin_signal_to_status(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBell(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBinary_to_poly(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBottom_line(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncBreak0(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCeil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCheck(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCheck_domain(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncChr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncClear_buffer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCli(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncClose(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncClose_text(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCol(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncColumn_heading_row(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncColumn_headings(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncColumn_spacing(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCommit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCommit_rate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCompile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCompress(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCompress_all(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncConfirm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncConnect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCosh(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncCross_reference(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDate_seconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDay_of_week(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDays(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDcl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDefault_tag(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDelete_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDescription(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDisconnect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDisplay(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDisplay_length(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDocumentation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncDomain(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEdit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncElse_if(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_case(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_disable_trigger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_execute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_if(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_row(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_signal_to_status(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncEnd_while(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncErase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncError(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncExecute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncExit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncExit_forward(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncExpand(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncExternal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFacility(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFailure(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFetch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFiles(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFind(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsFind;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFind_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFinish(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFirst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFloor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFooting(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncFooting_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncForm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsSpecial then
      Result := tkSpecial
    else
      Result := tkForm;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncGenerate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncGrouped_by(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncHeading(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncHeading_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncHeight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncIdentifier(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and (FRange = rsFind) then
  begin
    Result := tkKey;
    FRange := rsUnKnown;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncInput_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncInput_mask(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncInput_row_height(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncInvoke(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncItem(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncItem_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncItem_if(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncJoined_to(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLeft(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkFunction;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLfooting(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLheading(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLine(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLines_after(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLines_before(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncList(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLoad(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLog(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkFunction;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLog10(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_auto_select(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_col(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_data(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_first(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_height(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_noheading(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_nosearch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_reduced_to(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_row(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_secondary(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_selection(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_sorted_by(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_width(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLov_with(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLowercase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncLtrim(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncMail(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncMenu(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncMenu_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncMenu_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncMessage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncMid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncMod(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncModify_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNo_domain(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNobell(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNoclear_buffer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNodeadlock_exit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNoerase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNoerror(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNoexit_forward(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNoheading(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNolov_data(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNorepeat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNostatus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNototals(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNounderlines(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncNowait(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncOpen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncOpen_text(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncOpt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncOptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncOutput(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncOutput_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncOutput_mask(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncPause(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncPause_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncPerform(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncPoly_to_binary(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncPos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncPrint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncProcedure_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncPrompt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncProtect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncQuery(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncQuery_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRandom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRead_line(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRead_only(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReceive(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReceive_arguments(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReceive_data(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReceive_table(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReduced_to(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRelease(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRemain(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRepeat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReport_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncReposition(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRewind_text(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRfooting(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRheading(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRollback(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncRow_height(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSearch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSecondary(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSeconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSelection(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSend(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSend_data(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSend_message(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSend_table(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSequence(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSeverity(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSinh(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSorted_by(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSource(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSource_if(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSqrt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncStart_stream(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncStart_transaction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncStatistic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncStatus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncStream_name(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSuccess(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSwitch_base(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncSystem(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTable_form(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTable_search(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTag(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTag_length(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTanh(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTarget(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncText(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsSpecial then
      Result := tkSpecial
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncText_only(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTitle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and (FRange = rsAdd) then
  begin
    Result := tkKey;
    FRange := rsUnKnown;
  end
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTop_line(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTotal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTransfer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTrigger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTrim(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncTsuppress(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncUnload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncUppercase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncUse_if(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncUser_key(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncUsing(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncUtilities(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncWait(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncWidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncWith(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncWrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncWrite_line(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.FuncYesno_block(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterDml.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterDml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FFormAttri:= TSynEdit32HighlighterAttributes.Create(SYNS_AttrForm, SYNS_FriendlyAttrForm);
  FFormAttri.Style:= [fsBold];
  FFormAttri.Foreground:= clBlue;
  AddAttribute(FFormAttri);
  FBlockAttri:= TSynEdit32HighlighterAttributes.Create(SYNS_AttrBlock, SYNS_FriendlyAttrBlock);
  FBlockAttri.Style:= [fsBold];
  FBlockAttri.Foreground:= clGreen;
  AddAttribute(FBlockAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsBold];
  FCommentAttri.Foreground:= clRed;
  AddAttribute(FCommentAttri);
  FQualiAttri:= TSynEdit32HighlighterAttributes.Create(SYNS_AttrQualifier, SYNS_FriendlyAttrQualifier);
  FQualiAttri.Style:= [fsItalic];
  FQualiAttri.Foreground:= clGreen;
  AddAttribute(FQualiAttri);
  FFunctionAttri:= TSynEdit32HighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  FFunctionAttri.Style:= [fsItalic];
  FFunctionAttri.Foreground:= clBlack;
  AddAttribute(FFunctionAttri);
  FVariableAttri:= TSynEdit32HighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Style:= [fsBold, fsItalic];
  FVariableAttri.Foreground:= clBlack;
  AddAttribute(FVariableAttri);
  FSpecialAttri:= TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpecialVariable, SYNS_FriendlyAttrSpecialVariable);
  FSpecialAttri.Style:= [fsItalic];
  FSpecialAttri.Foreground:= clBlack;
  AddAttribute(FSpecialAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FRange := rsUnknown;

  fDefaultFilter := SYNS_FilterGembase;
end;

procedure TSynEdit32HighlighterDml.AddressOpProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '@' then Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.AsciiCharProc;

  function IsAsciiChar: Boolean;
  begin
     case FLine[FRun] of
       '_', '0'..'9', 'A'..'Z', 'a'..'z':
         Result := True;
       else
         Result := False;
     end;
  end;

begin
  // variables...
  FTokenID := tkVariable;
  repeat
    Inc(FRun);
  until not IsAsciiChar;
end;

procedure TSynEdit32HighlighterDml.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterDml.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun]= '=') or (FLine[FRun]= '>') then Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.NumberProc;
begin
  Inc(FRun);
  FTokenID := tkNumber;
  while CharInSet(FLine[FRun], ['0'..'9', '.']) do
  begin
    case FLine[FRun] of
      '.':
        if FLine[FRun + 1] = '.' then break;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterDml.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun]='.') or (FLine[FRun]=')') then Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.RemProc;
var
  p: PWideChar;
begin
  p := PWideChar(@FLine[FRun - 1]);
  while p >= FLine do
  begin
    if not CharInSet(p^, [#9, #32]) then
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
      exit;
    end;
    Dec(p);
  end;
  // it is a comment...
  FTokenID := tkComment;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterDml.SpaceProc;
begin
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = '"') and (FLine[FRun + 2] = '"') then Inc(FRun, 2);
  repeat
    Inc(FRun);
  until (FLine[FRun] = '"') or IsLineEnd(FRun);

  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterDml.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterDml.Next;
begin
  fTokenPos := FRun;
   case FLine[FRun] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32:
      SpaceProc;
    '#': AsciiCharProc;
    '"': StringProc;
    '0'..'9': NumberProc;
    'A'..'Z', 'a'..'z', '_':
      IdentProc;
    '{': SymbolProc;
    '}': SymbolProc;
    '!': RemProc;
    '.': PointProc;
    '<': LowerProc;
    '>': GreaterProc;
    '@': AddressOpProc;
    #39, '&', '('..'-', '/', ':', ';', '=', '?', '['..'^', '`', '~':
      SymbolProc;
  else
    UnknownProc;
  end;
  inherited;
end;

function TSynEdit32HighlighterDml.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynEdit32HighlighterDml.GetTokenID: TtkTokenKind;
begin
  Result:= FTokenID;
end;

function TSynEdit32HighlighterDml.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkForm: Result := FFormAttri;
    tkBlock: Result := FBlockAttri;
    tkKey: Result := FKeyAttri;
    tkComment: Result := FCommentAttri;
    tkQualifier: Result := FQualiAttri;
    tkFunction: Result := FFunctionAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkNumber: Result := FNumberAttri;
    tkSpecial: Result := FSpecialAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkVariable: Result := FVariableAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterDml.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynEdit32HighlighterDml.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynEdit32HighlighterDml.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynEdit32HighlighterDml.ResetRange;
begin
  FRange:= rsUnknown;
end;

function TSynEdit32HighlighterDml.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGembase;
end;

class function TSynEdit32HighlighterDml.GetLanguageName: string;
begin
  Result := SYNS_LangGembase;
end;

class function TSynEdit32HighlighterDml.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGembase;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterDml);
end.
