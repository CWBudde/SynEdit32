{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJScript.pas, released 2000-04-14.
The Original Code is based on the mwJScript.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony de Buys.
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

$Id: SynHighlighterJScript.pas,v 1.21.2.8 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a JavaScript/JScript highlighter for SynEdit)
@author(Tony De Buys [tony@lad.co.za], converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(December 1999, converted to SynEdit April 14, 2000)
@lastmod(2000-06-23)
The SynHighlighterJScript unit provides SynEdit with a JScript/JavaScript (.js) highlighter.
The highlighter formats JavaScript source code highlighting keywords, strings, numbers and characters.
}

unit SynEdit32.Highlighter.JScript;

{$I SynEdit32.Inc}

interface

uses
  Graphics, Registry, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Unicode;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown, tkNonReservedKey, tkEvent);

  TRangeState = (rsUnknown, rsANSI);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEdit32HighlighterJScript = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..5152] of TIdentFuncTableFunc;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNonReservedKeyAttri: TSynEdit32HighlighterAttributes;
    FEventAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAbs(Index: Integer): TtkTokenKind;
    function FuncAbstract(Index: Integer): TtkTokenKind;
    function FuncAcos(Index: Integer): TtkTokenKind;
    function FuncAction(Index: Integer): TtkTokenKind;
    function FuncAlert(Index: Integer): TtkTokenKind;
    function FuncAlign(Index: Integer): TtkTokenKind;
    function FuncAlinkcolor(Index: Integer): TtkTokenKind;
    function FuncAll(Index: Integer): TtkTokenKind;
    function FuncAnchor(Index: Integer): TtkTokenKind;
    function FuncAnchors(Index: Integer): TtkTokenKind;
    function FuncAppcodename(Index: Integer): TtkTokenKind;
    function FuncApplet(Index: Integer): TtkTokenKind;
    function FuncApplets(Index: Integer): TtkTokenKind;
    function FuncAppname(Index: Integer): TtkTokenKind;
    function FuncAppversion(Index: Integer): TtkTokenKind;
    function FuncArea(Index: Integer): TtkTokenKind;
    function FuncArguments(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncAsin(Index: Integer): TtkTokenKind;
    function FuncAtan(Index: Integer): TtkTokenKind;
    function FuncAtan2(Index: Integer): TtkTokenKind;
    function FuncBack(Index: Integer): TtkTokenKind;
    function FuncBackground(Index: Integer): TtkTokenKind;
    function FuncBgcolor(Index: Integer): TtkTokenKind;
    function FuncBig(Index: Integer): TtkTokenKind;
    function FuncBlink(Index: Integer): TtkTokenKind;
    function FuncBlur(Index: Integer): TtkTokenKind;
    function FuncBody(Index: Integer): TtkTokenKind;
    function FuncBold(Index: Integer): TtkTokenKind;
    function FuncBoolean(Index: Integer): TtkTokenKind;
    function FuncBoolean2(Index: Integer): TtkTokenKind;
    function FuncBorder(Index: Integer): TtkTokenKind;
    function FuncBottom(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncButton(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncCall(Index: Integer): TtkTokenKind;
    function FuncCallee(Index: Integer): TtkTokenKind;
    function FuncCaller(Index: Integer): TtkTokenKind;
    function FuncCaptureevents(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCatch(Index: Integer): TtkTokenKind;
    function FuncCeil(Index: Integer): TtkTokenKind;
    function FuncChar(Index: Integer): TtkTokenKind;
    function FuncCharat(Index: Integer): TtkTokenKind;
    function FuncCharcodeat(Index: Integer): TtkTokenKind;
    function FuncCheckbox(Index: Integer): TtkTokenKind;
    function FuncChecked(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncClear(Index: Integer): TtkTokenKind;
    function FuncClearinterval(Index: Integer): TtkTokenKind;
    function FuncCleartimeout(Index: Integer): TtkTokenKind;
    function FuncClick(Index: Integer): TtkTokenKind;
    function FuncClose(Index: Integer): TtkTokenKind;
    function FuncClosed(Index: Integer): TtkTokenKind;
    function FuncColor(Index: Integer): TtkTokenKind;
    function FuncComplete(Index: Integer): TtkTokenKind;
    function FuncConcat(Index: Integer): TtkTokenKind;
    function FuncConfirm(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncConstructor(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncCookie(Index: Integer): TtkTokenKind;
    function FuncCos(Index: Integer): TtkTokenKind;
    function FuncCurrent(Index: Integer): TtkTokenKind;
    function FuncDate(Index: Integer): TtkTokenKind;
    function FuncDebugger(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDefaultchecked(Index: Integer): TtkTokenKind;
    function FuncDefaultselected(Index: Integer): TtkTokenKind;
    function FuncDefaultstatus(Index: Integer): TtkTokenKind;
    function FuncDefaultvalue(Index: Integer): TtkTokenKind;
    function FuncDelete(Index: Integer): TtkTokenKind;
    function FuncDescription(Index: Integer): TtkTokenKind;
    function FuncDisplay(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDocument(Index: Integer): TtkTokenKind;
    function FuncDomain(Index: Integer): TtkTokenKind;
    function FuncDouble(Index: Integer): TtkTokenKind;
    function FuncE(Index: Integer): TtkTokenKind;
    function FuncElements(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEmbed(Index: Integer): TtkTokenKind;
    function FuncEmbeds(Index: Integer): TtkTokenKind;
    function FuncEnabledplugin(Index: Integer): TtkTokenKind;
    function FuncEncoding(Index: Integer): TtkTokenKind;
    function FuncEnum(Index: Integer): TtkTokenKind;
    function FuncEscape(Index: Integer): TtkTokenKind;
    function FuncEval(Index: Integer): TtkTokenKind;
    function FuncEvent(Index: Integer): TtkTokenKind;
    function FuncExp(Index: Integer): TtkTokenKind;
    function FuncExport(Index: Integer): TtkTokenKind;
    function FuncExtends(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFgcolor(Index: Integer): TtkTokenKind;
    function FuncFilename(Index: Integer): TtkTokenKind;
    function FuncFileupload(Index: Integer): TtkTokenKind;
    function FuncFinal(Index: Integer): TtkTokenKind;
    function FuncFinally(Index: Integer): TtkTokenKind;
    function FuncFind(Index: Integer): TtkTokenKind;
    function FuncFixed(Index: Integer): TtkTokenKind;
    function FuncFloat(Index: Integer): TtkTokenKind;
    function FuncFloat2(Index: Integer): TtkTokenKind;
    function FuncFloor(Index: Integer): TtkTokenKind;
    function FuncFocus(Index: Integer): TtkTokenKind;
    function FuncFontcolor(Index: Integer): TtkTokenKind;
    function FuncFontsize(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForm(Index: Integer): TtkTokenKind;
    function FuncForms(Index: Integer): TtkTokenKind;
    function FuncForward(Index: Integer): TtkTokenKind;
    function FuncFrame(Index: Integer): TtkTokenKind;
    function FuncFrames(Index: Integer): TtkTokenKind;
    function FuncFromcharcode(Index: Integer): TtkTokenKind;
    function FuncFunction(Index: Integer): TtkTokenKind;
    function FuncFunction2(Index: Integer): TtkTokenKind;    
    function FuncGetdate(Index: Integer): TtkTokenKind;
    function FuncGetday(Index: Integer): TtkTokenKind;
    function FuncGetelementbyid(Index: Integer): TtkTokenKind;
    function FuncGetfullyear(Index: Integer): TtkTokenKind;
    function FuncGethours(Index: Integer): TtkTokenKind;
    function FuncGetmilliseconds(Index: Integer): TtkTokenKind;
    function FuncGetminutes(Index: Integer): TtkTokenKind;
    function FuncGetmonth(Index: Integer): TtkTokenKind;
    function FuncGetseconds(Index: Integer): TtkTokenKind;
    function FuncGettime(Index: Integer): TtkTokenKind;
    function FuncGettimezoneoffset(Index: Integer): TtkTokenKind;
    function FuncGetutcdate(Index: Integer): TtkTokenKind;
    function FuncGetutcday(Index: Integer): TtkTokenKind;
    function FuncGetutcfullyear(Index: Integer): TtkTokenKind;
    function FuncGetutchours(Index: Integer): TtkTokenKind;
    function FuncGetutcmilliseconds(Index: Integer): TtkTokenKind;
    function FuncGetutcminutes(Index: Integer): TtkTokenKind;
    function FuncGetutcmonth(Index: Integer): TtkTokenKind;
    function FuncGetutcseconds(Index: Integer): TtkTokenKind;
    function FuncGetyear(Index: Integer): TtkTokenKind;
    function FuncGlobal(Index: Integer): TtkTokenKind;
    function FuncGo(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncHandleevent(Index: Integer): TtkTokenKind;
    function FuncHash(Index: Integer): TtkTokenKind;
    function FuncHeight(Index: Integer): TtkTokenKind;
    function FuncHidden(Index: Integer): TtkTokenKind;
    function FuncHistory(Index: Integer): TtkTokenKind;
    function FuncHome(Index: Integer): TtkTokenKind;
    function FuncHost(Index: Integer): TtkTokenKind;
    function FuncHostname(Index: Integer): TtkTokenKind;
    function FuncHref(Index: Integer): TtkTokenKind;
    function FuncHspace(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncImage(Index: Integer): TtkTokenKind;
    function FuncImages(Index: Integer): TtkTokenKind;
    function FuncImplements(Index: Integer): TtkTokenKind;
    function FuncImport(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncIndex(Index: Integer): TtkTokenKind;
    function FuncIndexof(Index: Integer): TtkTokenKind;
    function FuncInfinity(Index: Integer): TtkTokenKind;
    function FuncInnerheight(Index: Integer): TtkTokenKind;
    function FuncInnerwidth(Index: Integer): TtkTokenKind;
    function FuncInput(Index: Integer): TtkTokenKind;
    function FuncInstanceof(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncInterface(Index: Integer): TtkTokenKind;
    function FuncIsfinite(Index: Integer): TtkTokenKind;
    function FuncIsnan(Index: Integer): TtkTokenKind;
    function FuncItalics(Index: Integer): TtkTokenKind;
    function FuncJava(Index: Integer): TtkTokenKind;
    function FuncJavaenabled(Index: Integer): TtkTokenKind;
    function FuncJoin(Index: Integer): TtkTokenKind;
    function FuncLastindexof(Index: Integer): TtkTokenKind;
    function FuncLastmodified(Index: Integer): TtkTokenKind;
    function FuncLayer(Index: Integer): TtkTokenKind;
    function FuncLayers(Index: Integer): TtkTokenKind;
    function FuncLeft(Index: Integer): TtkTokenKind;
    function FuncLength(Index: Integer): TtkTokenKind;
    function FuncLink(Index: Integer): TtkTokenKind;
    function FuncLinkcolor(Index: Integer): TtkTokenKind;
    function FuncLinks(Index: Integer): TtkTokenKind;
    function FuncLn10(Index: Integer): TtkTokenKind;
    function FuncLn2(Index: Integer): TtkTokenKind;
    function FuncLocation(Index: Integer): TtkTokenKind;
    function FuncLocationbar(Index: Integer): TtkTokenKind;
    function FuncLog(Index: Integer): TtkTokenKind;
    function FuncLog10e(Index: Integer): TtkTokenKind;
    function FuncLog2e(Index: Integer): TtkTokenKind;
    function FuncLogon(Index: Integer): TtkTokenKind;
    function FuncLong(Index: Integer): TtkTokenKind;
    function FuncLowsrc(Index: Integer): TtkTokenKind;
    function FuncMatch(Index: Integer): TtkTokenKind;
    function FuncMath(Index: Integer): TtkTokenKind;
    function FuncMax(Index: Integer): TtkTokenKind;
    function FuncMax_value(Index: Integer): TtkTokenKind;
    function FuncMenubar(Index: Integer): TtkTokenKind;
    function FuncMethod(Index: Integer): TtkTokenKind;
    function FuncMimetype(Index: Integer): TtkTokenKind;
    function FuncMimetypes(Index: Integer): TtkTokenKind;
    function FuncMin(Index: Integer): TtkTokenKind;
    function FuncMin_value(Index: Integer): TtkTokenKind;
    function FuncMoveby(Index: Integer): TtkTokenKind;
    function FuncMoveto(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNan(Index: Integer): TtkTokenKind;
    function FuncNative(Index: Integer): TtkTokenKind;
    function FuncNavigator(Index: Integer): TtkTokenKind;
    function FuncNegative_infinity(Index: Integer): TtkTokenKind;
    function FuncNetscape(Index: Integer): TtkTokenKind;
    function FuncNew(Index: Integer): TtkTokenKind;
    function FuncNext(Index: Integer): TtkTokenKind;
    function FuncNull(Index: Integer): TtkTokenKind;
    function FuncNull2(Index: Integer): TtkTokenKind;
    function FuncNumber(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncOnabort(Index: Integer): TtkTokenKind;
    function FuncOnblur(Index: Integer): TtkTokenKind;
    function FuncOnchange(Index: Integer): TtkTokenKind;
    function FuncOnclick(Index: Integer): TtkTokenKind;
    function FuncOndblclick(Index: Integer): TtkTokenKind;
    function FuncOnerror(Index: Integer): TtkTokenKind;
    function FuncOnfocus(Index: Integer): TtkTokenKind;
    function FuncOnkeydown(Index: Integer): TtkTokenKind;
    function FuncOnkeypress(Index: Integer): TtkTokenKind;
    function FuncOnkeyup(Index: Integer): TtkTokenKind;
    function FuncOnload(Index: Integer): TtkTokenKind;
    function FuncOnmousedown(Index: Integer): TtkTokenKind;
    function FuncOnmousemove(Index: Integer): TtkTokenKind;
    function FuncOnmouseout(Index: Integer): TtkTokenKind;
    function FuncOnmouseover(Index: Integer): TtkTokenKind;
    function FuncOnmouseup(Index: Integer): TtkTokenKind;
    function FuncOnreset(Index: Integer): TtkTokenKind;
    function FuncOnselect(Index: Integer): TtkTokenKind;
    function FuncOnsubmit(Index: Integer): TtkTokenKind;
    function FuncOnunload(Index: Integer): TtkTokenKind;
    function FuncOpen(Index: Integer): TtkTokenKind;
    function FuncOpener(Index: Integer): TtkTokenKind;
    function FuncOption(Index: Integer): TtkTokenKind;
    function FuncOptions(Index: Integer): TtkTokenKind;
    function FuncOuterheight(Index: Integer): TtkTokenKind;
    function FuncOuterwidth(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncPackages(Index: Integer): TtkTokenKind;
    function FuncPagex(Index: Integer): TtkTokenKind;
    function FuncPagexoffset(Index: Integer): TtkTokenKind;
    function FuncPagey(Index: Integer): TtkTokenKind;
    function FuncPageyoffset(Index: Integer): TtkTokenKind;
    function FuncParent(Index: Integer): TtkTokenKind;
    function FuncParse(Index: Integer): TtkTokenKind;
    function FuncParsefloat(Index: Integer): TtkTokenKind;
    function FuncParseint(Index: Integer): TtkTokenKind;
    function FuncPassword(Index: Integer): TtkTokenKind;
    function FuncPathname(Index: Integer): TtkTokenKind;
    function FuncPersonalbar(Index: Integer): TtkTokenKind;
    function FuncPi(Index: Integer): TtkTokenKind;
    function FuncPlatform(Index: Integer): TtkTokenKind;
    function FuncPlugin(Index: Integer): TtkTokenKind;
    function FuncPlugins(Index: Integer): TtkTokenKind;
    function FuncPort(Index: Integer): TtkTokenKind;
    function FuncPositive_infinity(Index: Integer): TtkTokenKind;
    function FuncPow(Index: Integer): TtkTokenKind;
    function FuncPrevious(Index: Integer): TtkTokenKind;
    function FuncPrint(Index: Integer): TtkTokenKind;
    function FuncPrivate(Index: Integer): TtkTokenKind;
    function FuncPrompt(Index: Integer): TtkTokenKind;
    function FuncProtected(Index: Integer): TtkTokenKind;
    function FuncProtocol(Index: Integer): TtkTokenKind;
    function FuncPrototype(Index: Integer): TtkTokenKind;
    function FuncPublic(Index: Integer): TtkTokenKind;
    function FuncRadio(Index: Integer): TtkTokenKind;
    function FuncRandom(Index: Integer): TtkTokenKind;
    function FuncReferrer(Index: Integer): TtkTokenKind;
    function FuncRefresh(Index: Integer): TtkTokenKind;
    function FuncRegexp(Index: Integer): TtkTokenKind;
    function FuncReleaseevents(Index: Integer): TtkTokenKind;
    function FuncReload(Index: Integer): TtkTokenKind;
    function FuncReplace(Index: Integer): TtkTokenKind;
    function FuncReset(Index: Integer): TtkTokenKind;
    function FuncResizeby(Index: Integer): TtkTokenKind;
    function FuncResizeto(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncReverse(Index: Integer): TtkTokenKind;
    function FuncRight(Index: Integer): TtkTokenKind;
    function FuncRound(Index: Integer): TtkTokenKind;
    function FuncRouteevent(Index: Integer): TtkTokenKind;
    function FuncScreen(Index: Integer): TtkTokenKind;
    function FuncScroll(Index: Integer): TtkTokenKind;
    function FuncScrollbars(Index: Integer): TtkTokenKind;
    function FuncScrollby(Index: Integer): TtkTokenKind;
    function FuncScrollto(Index: Integer): TtkTokenKind;
    function FuncSearch(Index: Integer): TtkTokenKind;
    function FuncSelect(Index: Integer): TtkTokenKind;
    function FuncSelected(Index: Integer): TtkTokenKind;
    function FuncSelectedindex(Index: Integer): TtkTokenKind;
    function FuncSelf(Index: Integer): TtkTokenKind;
    function FuncSetdate(Index: Integer): TtkTokenKind;
    function FuncSetfullyear(Index: Integer): TtkTokenKind;
    function FuncSethours(Index: Integer): TtkTokenKind;
    function FuncSetinterval(Index: Integer): TtkTokenKind;
    function FuncSetmilliseconds(Index: Integer): TtkTokenKind;
    function FuncSetminutes(Index: Integer): TtkTokenKind;
    function FuncSetmonth(Index: Integer): TtkTokenKind;
    function FuncSetseconds(Index: Integer): TtkTokenKind;
    function FuncSettime(Index: Integer): TtkTokenKind;
    function FuncSettimeout(Index: Integer): TtkTokenKind;
    function FuncSetutcdate(Index: Integer): TtkTokenKind;
    function FuncSetutcfullyear(Index: Integer): TtkTokenKind;
    function FuncSetutchours(Index: Integer): TtkTokenKind;
    function FuncSetutcmilliseconds(Index: Integer): TtkTokenKind;
    function FuncSetutcminutes(Index: Integer): TtkTokenKind;
    function FuncSetutcmonth(Index: Integer): TtkTokenKind;
    function FuncSetutcseconds(Index: Integer): TtkTokenKind;
    function FuncSetyear(Index: Integer): TtkTokenKind;
    function FuncShort(Index: Integer): TtkTokenKind;
    function FuncSin(Index: Integer): TtkTokenKind;
    function FuncSlice(Index: Integer): TtkTokenKind;
    function FuncSmall(Index: Integer): TtkTokenKind;
    function FuncSort(Index: Integer): TtkTokenKind;
    function FuncSplit(Index: Integer): TtkTokenKind;
    function FuncSqrt(Index: Integer): TtkTokenKind;
    function FuncSqrt1_2(Index: Integer): TtkTokenKind;
    function FuncSqrt2(Index: Integer): TtkTokenKind;
    function FuncSrc(Index: Integer): TtkTokenKind;
    function FuncStart(Index: Integer): TtkTokenKind;
    function FuncStatic(Index: Integer): TtkTokenKind;
    function FuncStatus(Index: Integer): TtkTokenKind;
    function FuncStatusbar(Index: Integer): TtkTokenKind;
    function FuncStop(Index: Integer): TtkTokenKind;
    function FuncStrike(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStyle(Index: Integer): TtkTokenKind;
    function FuncSub(Index: Integer): TtkTokenKind;
    function FuncSubmit(Index: Integer): TtkTokenKind;
    function FuncSubstr(Index: Integer): TtkTokenKind;
    function FuncSubstring(Index: Integer): TtkTokenKind;
    function FuncSuffixes(Index: Integer): TtkTokenKind;
    function FuncSup(Index: Integer): TtkTokenKind;
    function FuncSuper(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncSynchronized(Index: Integer): TtkTokenKind;
    function FuncTags(Index: Integer): TtkTokenKind;
    function FuncTaint(Index: Integer): TtkTokenKind;
    function FuncTaintenabled(Index: Integer): TtkTokenKind;
    function FuncTan(Index: Integer): TtkTokenKind;
    function FuncTarget(Index: Integer): TtkTokenKind;
    function FuncText(Index: Integer): TtkTokenKind;
    function FuncTextarea(Index: Integer): TtkTokenKind;
    function FuncThis(Index: Integer): TtkTokenKind;
    function FuncThrow(Index: Integer): TtkTokenKind;
    function FuncThrows(Index: Integer): TtkTokenKind;
    function FuncTitle(Index: Integer): TtkTokenKind;
    function FuncTogmtstring(Index: Integer): TtkTokenKind;
    function FuncTolocalestring(Index: Integer): TtkTokenKind;
    function FuncTolowercase(Index: Integer): TtkTokenKind;
    function FuncToolbar(Index: Integer): TtkTokenKind;
    function FuncTop(Index: Integer): TtkTokenKind;
    function FuncTosource(Index: Integer): TtkTokenKind;
    function FuncTostring(Index: Integer): TtkTokenKind;
    function FuncTouppercase(Index: Integer): TtkTokenKind;
    function FuncToutcstring(Index: Integer): TtkTokenKind;
    function FuncTransient(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncTry(Index: Integer): TtkTokenKind;
    function FuncType(Index: Integer): TtkTokenKind;
    function FuncTypeof(Index: Integer): TtkTokenKind;
    function FuncUndefined(Index: Integer): TtkTokenKind;
    function FuncUnescape(Index: Integer): TtkTokenKind;
    function FuncUntaint(Index: Integer): TtkTokenKind;
    function FuncUnwatch(Index: Integer): TtkTokenKind;
    function FuncUrl(Index: Integer): TtkTokenKind;
    function FuncUseragent(Index: Integer): TtkTokenKind;
    function FuncUtc(Index: Integer): TtkTokenKind;
    function FuncValue(Index: Integer): TtkTokenKind;
    function FuncValueof(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function FuncVisibility(Index: Integer): TtkTokenKind;
    function FuncVlinkcolor(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncVspace(Index: Integer): TtkTokenKind;
    function FuncWatch(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncWidth(Index: Integer): TtkTokenKind;
    function FuncWindow(Index: Integer): TtkTokenKind;
    function FuncWith(Index: Integer): TtkTokenKind;
    function FuncWrite(Index: Integer): TtkTokenKind;
    function FuncWriteln(Index: Integer): TtkTokenKind;
    function FuncZindex(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure CommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
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
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NonReservedKeyAttri: TSynEdit32HighlighterAttributes read FNonReservedKeyAttri write FNonReservedKeyAttri;
    property EventAttri: TSynEdit32HighlighterAttributes read FEventAttri write FEventAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  KeyWords: array[0..398] of UnicodeString = (
    'abs', 'abstract', 'acos', 'action', 'alert', 'align', 'alinkColor', 'all',
    'All', 'anchor', 'Anchor', 'anchors', 'appCodeName', 'Applet', 'applets',
    'appName', 'appVersion', 'Area', 'arguments', 'Arguments', 'Array', 'asin',
    'atan', 'atan2', 'back', 'background', 'bgColor', 'big', 'blink', 'blur',
    'body', 'bold', 'boolean', 'Boolean', 'border', 'bottom', 'break', 'Button',
    'byte', 'call', 'callee', 'caller', 'captureEvents', 'case', 'catch',
    'ceil', 'char', 'charAt', 'charCodeAt', 'Checkbox', 'checked', 'class',
    'clear', 'clearInterval', 'clearTimeout', 'click', 'close', 'closed',
    'color', 'complete', 'concat', 'confirm', 'const', 'constructor',
    'continue', 'cookie', 'cos', 'current', 'Date', 'debugger', 'default',
    'defaultChecked', 'defaultSelected', 'defaultStatus', 'defaultValue',
    'delete', 'description', 'display', 'do', 'document', 'domain', 'double',
    'E', 'elements', 'else', 'Embed', 'embeds', 'enabledPlugin', 'encoding',
    'enum', 'escape', 'eval', 'event', 'exp', 'export', 'extends', 'false',
    'fgColor', 'filename', 'FileUpload', 'final', 'finally', 'find', 'fixed',
    'float', 'Float', 'floor', 'focus', 'fontcolor', 'fontsize', 'for', 'form',
    'Form', 'forms', 'forward', 'Frame', 'frames', 'fromCharCode', 'function',
    'Function', 'getDate', 'getDay', 'getElementById', 'getFullYear',
    'getHours', 'getMilliseconds', 'getMinutes', 'getMonth', 'getSeconds',
    'getTime', 'getTimezoneOffset', 'getUTCDate', 'getUTCDay', 'getUTCFullYear',
    'getUTCHours', 'getUTCMilliseconds', 'getUTCMinutes', 'getUTCMonth',
    'getUTCSeconds', 'getYear', 'Global', 'go', 'goto', 'handleEvent', 'hash',
    'height', 'Hidden', 'history', 'History', 'home', 'host', 'hostname',
    'href', 'hspace', 'if', 'Image', 'images', 'implements', 'import', 'in',
    'index', 'indexOf', 'Infinity', 'innerHeight', 'innerWidth', 'input',
    'instanceof', 'int', 'interface', 'isFinite', 'isNaN', 'italics', 'java',
    'javaEnabled', 'join', 'lastIndexOf', 'lastModified', 'Layer', 'layers',
    'left', 'length', 'link', 'Link', 'linkColor', 'links', 'LN10', 'LN2',
    'location', 'Location', 'locationbar', 'log', 'LOG10E', 'LOG2E', 'logon',
    'long', 'lowsrc', 'match', 'Math', 'max', 'MAX_VALUE', 'menubar', 'method',
    'MimeType', 'mimeTypes', 'min', 'MIN_VALUE', 'moveBy', 'moveTo', 'name',
    'NaN', 'native', 'navigator', 'Navigator', 'NEGATIVE_INFINITY', 'netscape',
    'new', 'next', 'null', 'Null', 'Number', 'Object', 'onAbort', 'onBlur',
    'onChange', 'onClick', 'onDblClick', 'onError', 'onFocus', 'onKeyDown',
    'onKeyPress', 'onKeyUp', 'onLoad', 'onMouseDown', 'onMouseMove',
    'onMouseOut', 'onMouseOver', 'onMouseUp', 'onReset', 'onSelect', 'onSubmit',
    'onUnload', 'open', 'opener', 'Option', 'options', 'outerHeight',
    'outerWidth', 'package', 'Packages', 'pageX', 'pageXOffset', 'pageY',
    'pageYOffset', 'parent', 'parse', 'parseFloat', 'parseInt', 'Password',
    'pathname', 'personalbar', 'PI', 'platform', 'Plugin', 'plugins', 'port',
    'POSITIVE_INFINITY', 'pow', 'previous', 'print', 'private', 'prompt',
    'protected', 'protocol', 'prototype', 'public', 'Radio', 'random',
    'referrer', 'refresh', 'RegExp', 'releaseEvents', 'reload', 'replace',
    'reset', 'Reset', 'resizeBy', 'resizeTo', 'return', 'reverse', 'right',
    'round', 'routeEvent', 'screen', 'scroll', 'scrollbars', 'scrollBy',
    'scrollTo', 'search', 'select', 'Select', 'selected', 'selectedIndex',
    'self', 'setDate', 'setFullYear', 'setHours', 'setInterval',
    'setMilliseconds', 'setMinutes', 'setMonth', 'setSeconds', 'setTime',
    'setTimeout', 'setUTCDate', 'setUTCFullYear', 'setUTCHours',
    'setUTCMilliseconds', 'setUTCMinutes', 'setUTCMonth', 'setUTCSeconds',
    'setYear', 'short', 'sin', 'slice', 'small', 'sort', 'split', 'sqrt',
    'SQRT1_2', 'SQRT2', 'src', 'start', 'static', 'status', 'statusbar', 'stop',
    'strike', 'String', 'style', 'sub', 'submit', 'Submit', 'substr',
    'substring', 'suffixes', 'sup', 'super', 'switch', 'synchronized', 'tags',
    'taint', 'taintEnabled', 'tan', 'target', 'text', 'Text', 'Textarea',
    'this', 'throw', 'throws', 'title', 'toGMTString', 'toLocaleString',
    'toLowerCase', 'toolbar', 'top', 'toSource', 'toString', 'toUpperCase',
    'toUTCString', 'transient', 'true', 'try', 'type', 'typeof', 'undefined',
    'Undefined', 'unescape', 'untaint', 'unwatch', 'URL', 'userAgent', 'UTC',
    'value', 'valueOf', 'var', 'visibility', 'vlinkColor', 'void', 'vspace',
    'watch', 'while', 'width', 'window', 'Window', 'with', 'write', 'writeln',
    'zIndex'
  );

  KeyIndices: array[0..5152] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 231, -1, -1, -1, -1, -1, 296, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 55,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 292, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 168, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 208, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 200, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 295, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 75, 351, -1, -1, -1, -1, -1, -1, 315, 37, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 239, -1, -1, -1, -1, -1, 326, -1, -1, -1, 31,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 143, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 339, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 241, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 235, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 145, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 204, -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1,
    -1, -1, 16, 52, 389, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    259, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 331, 30, -1, -1, -1, -1, -1, -1,
    -1, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 304, -1, 396, 2, -1, -1, 323, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 167,
    -1, -1, -1, -1, -1, -1, -1, 122, -1, -1, -1, -1, -1, -1, -1, -1, -1, 34, -1,
    -1, -1, -1, 203, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, 83, -1, -1,
    -1, -1, -1, 101, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    268, -1, -1, -1, -1, -1, -1, -1, -1, 182, -1, -1, -1, -1, -1, 246, 18, -1,
    -1, -1, -1, -1, 209, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 220, 161,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 134, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 332, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 229, -1, -1, -1, -1, -1, -1, -1, 157, 319, -1, 210, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 234, -1, -1, -1, -1, -1, -1, -1, -1, -1, 105,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 381, 78, -1,
    -1, -1, -1, -1, -1, -1, 257, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 219, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 196, -1, -1, -1, -1, -1, 379, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 363, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 309, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 386, 146, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 103, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 269, -1, -1, -1, 199, 172, -1, 15, 123, -1, -1, -1, -1, -1, -1, -1, 136,
    -1, -1, -1, 128, -1, -1, -1, -1, 366, -1, -1, 185, -1, -1, -1, -1, 153, -1,
    -1, -1, -1, 388, -1, -1, 165, -1, -1, -1, -1, -1, -1, 338, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 360, -1, -1,
    194, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 77, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 307, -1, -1, -1, -1, -1, -1, -1, 258, -1,
    -1, -1, 96, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 180, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 69, -1, -1, -1, -1, -1, -1, 129, -1, -1,
    -1, -1, -1, -1, -1, -1, 120, -1, -1, 95, -1, 233, -1, -1, -1, -1, -1, -1,
    -1, -1, 40, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 160, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 90, 282, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 341, 232, 121, 155, -1,
    -1, -1, -1, -1, 247, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 327, -1, -1, -1, -1, -1, -1, -1, -1, -1, 74, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 170, -1, -1, -1, -1, 298, -1,
    -1, -1, -1, -1, -1, -1, 114, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 94, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 271, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 324, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 70, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 197, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 91, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 106, -1, -1, 237, -1, -1, -1, -1, -1, 6,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 240, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 250, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 205, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 238, -1, -1, -1,
    -1, -1, -1, -1, -1, 275, -1, -1, -1, -1, -1, -1, -1, -1, -1, 287, -1, -1,
    -1, -1, -1, -1, -1, 227, -1, -1, 383, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    58, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 29, 148, 171, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 392, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 201, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 135, -1, -1, 212,
    -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, 272, -1, -1,
    -1, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 334,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 289, -1, -1, -1, -1, 312, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 385, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 51, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, -1, -1, -1, -1, 371, 76,
    -1, -1, 330, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 68, -1, -1, -1, -1, -1, -1, 225, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 13, -1, -1, -1, 156, -1, 23, -1, -1, -1, -1, -1, -1,
    -1, -1, 280, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 178, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 277, -1, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, -1, -1, 93, -1,
    -1, -1, -1, -1, -1, -1, 202, -1, 5, 343, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 255, -1, -1, -1, -1, -1, -1, -1,
    -1, 43, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 333, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, 139,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 320, -1, -1, -1, -1, -1, -1,
    214, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 152, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 278, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 302, 316, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 137, -1, -1, -1,
    254, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 86, -1, -1, -1, -1, -1, -1, -1, -1, 345, -1, -1, 144, -1, -1, -1, 7,
    -1, -1, 306, -1, -1, -1, -1, 113, -1, -1, -1, -1, -1, -1, 308, -1, -1, -1,
    -1, 357, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 361, -1, -1, -1, -1, -1, -1, -1, 195, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 387, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 169, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 376, -1, -1, -1, -1, 188, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 359, 98, -1, -1, -1,
    -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, 116, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 299, -1, -1, -1, 369, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 54, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 147, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 118, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    356, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 8, -1, 300, -1, -1, 228, 59, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 213, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 179, -1, -1, -1, -1, -1,
    -1, -1, 176, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 350, -1, -1, -1, -1,
    -1, -1, 284, -1, -1, -1, 256, -1, -1, 276, -1, -1, -1, -1, -1, -1, -1, -1,
    190, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 102, -1, 230, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 72, -1, 71, 26, -1, -1, -1, -1,
    -1, -1, 60, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 322, -1, -1, 175, -1, -1, 393, -1, 124, 85, -1, -1, -1, -1,
    -1, -1, -1, -1, 150, -1, 236, -1, -1, -1, -1, -1, -1, -1, -1, -1, 140, -1,
    -1, -1, -1, -1, -1, 183, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 20, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    42, 244, -1, -1, -1, -1, -1, -1, -1, 47, 313, -1, 41, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 64,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    294, -1, -1, -1, -1, -1, -1, -1, -1, 374, -1, -1, -1, -1, -1, -1, -1, 245,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 177, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 347, -1, -1, -1, -1, -1, -1, -1, 391, -1, -1, -1, -1, -1, -1, -1,
    217, -1, -1, -1, 87, -1, -1, -1, 329, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    39, -1, -1, -1, -1, -1, -1, -1, -1, 189, -1, -1, 222, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 174, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    274, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    382, -1, -1, -1, 138, 226, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 24, -1, -1, -1, -1, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, -1,
    -1, -1, 318, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 335,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 260, -1, -1, -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 288, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 342, -1, -1, -1, -1, -1, -1,
    61, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 377, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 132, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 158, -1, -1, 166, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, -1, -1, -1, 57,
    -1, -1, -1, 211, -1, -1, -1, -1, 243, -1, -1, -1, -1, -1, 264, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 321, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 207, -1, -1,
    216, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    149, -1, -1, -1, -1, -1, 89, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, 293,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1, -1, -1, 242, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 56,
    -1, 154, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 92, 193, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    325, 126, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 206, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 372, -1, -1, -1, 380, -1, -1,
    352, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 263, -1, -1, -1, -1, -1, -1, -1, 373, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 286, -1, 46, -1, -1, -1, -1, 184, -1, -1, -1, -1, -1, -1, 19,
    -1, -1, -1, 25, -1, -1, -1, -1, -1, -1, -1, 367, -1, -1, -1, -1, -1, 270,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 283,
    -1, -1, -1, -1, -1, -1, -1, -1, 151, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, -1, -1,
    -1, -1, -1, -1, 398, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 252,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 355, -1, -1, 365, -1, -1, -1,
    -1, -1, -1, -1, -1, 28, -1, -1, 378, -1, -1, -1, -1, 354, -1, -1, -1, -1,
    -1, -1, -1, -1, 349, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 97, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, -1, -1, 285,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 215, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 198, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 81, 394, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 358, -1, -1, -1, -1, -1, -1, -1, 173, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 224, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 181, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 375,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 9, -1, -1, -1, -1, -1, 305, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 141,
    281, 115, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 84, -1, -1, -1, -1, -1,
    -1, -1, 261, -1, -1, -1, -1, 265, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    273, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 362, -1, 290, -1, 66, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    131, -1, 279, -1, -1, -1, 249, -1, -1, -1, -1, -1, -1, 223, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 297, -1, -1, -1, -1,
    127, -1, -1, 142, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 364, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 88, -1, -1, -1, -1, -1, -1, 248, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 395, 251, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 45, -1, -1, -1, -1, -1, -1, -1, -1, 80, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 310, -1, 218, -1, -1, -1, -1, -1, -1, 187, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 130, 390, -1, -1, -1, -1, -1, -1, -1,
    328, -1, 221, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 336, -1, -1, -1, -1, -1, -1, 311, -1, -1, -1, -1,
    -1, -1, -1, -1, 303, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 344, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 337, -1, -1, -1, -1, -1, 262, -1, -1, -1, -1,
    -1, -1, -1, 267, -1, -1, -1, -1, -1, -1, -1, 253, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 397, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 162, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 346, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, 348, 159, -1, -1, -1, -1, -1, -1, -1,
    368, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 370, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 164, -1, 314, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 291, -1, -1, -1, -1, -1, -1, 384, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, -1, 340, -1, -1,
    -1, -1, -1, -1, 317, -1, 79, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1,
    353, -1, 301, -1, -1, -1, -1, -1, -1, 163, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 266, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 186, -1, -1, -1
  );

{$Q-}
function TSynEdit32HighlighterJScript.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 751 + Ord(Str^) * 148;
    Inc(Str);
  end;
  Result := Result mod 5153;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEdit32HighlighterJScript.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEdit32HighlighterJScript.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[4966] := FuncAbs;
  FIdentFuncTable[2170] := FuncAbstract;
  FIdentFuncTable[520] := FuncAcos;
  FIdentFuncTable[319] := FuncAction;
  FIdentFuncTable[4368] := FuncAlert;
  FIdentFuncTable[2070] := FuncAlign;
  FIdentFuncTable[1500] := FuncAlinkcolor;
  FIdentFuncTable[2362] := FuncAll;
  FIdentFuncTable[2706] := FuncAll;
  FIdentFuncTable[4383] := FuncAnchor;
  FIdentFuncTable[491] := FuncAnchor;
  FIdentFuncTable[2516] := FuncAnchors;
  FIdentFuncTable[2207] := FuncAppcodename;
  FIdentFuncTable[1993] := FuncApplet;
  FIdentFuncTable[1805] := FuncApplets;
  FIdentFuncTable[965] := FuncAppname;
  FIdentFuncTable[416] := FuncAppversion;
  FIdentFuncTable[2052] := FuncArea;
  FIdentFuncTable[618] := FuncArguments;
  FIdentFuncTable[3950] := FuncArguments;
  FIdentFuncTable[2987] := FuncArray;
  FIdentFuncTable[4131] := FuncAsin;
  FIdentFuncTable[5117] := FuncAtan;
  FIdentFuncTable[1999] := FuncAtan2;
  FIdentFuncTable[3356] := FuncBack;
  FIdentFuncTable[3954] := FuncBackground;
  FIdentFuncTable[2882] := FuncBgcolor;
  FIdentFuncTable[1824] := FuncBig;
  FIdentFuncTable[4067] := FuncBlink;
  FIdentFuncTable[1709] := FuncBlur;
  FIdentFuncTable[483] := FuncBody;
  FIdentFuncTable[243] := FuncBold;
  FIdentFuncTable[4200] := FuncBoolean;
  FIdentFuncTable[3265] := FuncBoolean2;
  FIdentFuncTable[563] := FuncBorder;
  FIdentFuncTable[2857] := FuncBottom;
  FIdentFuncTable[2410] := FuncBreak;
  FIdentFuncTable[223] := FuncButton;
  FIdentFuncTable[575] := FuncByte;
  FIdentFuncTable[3204] := FuncCall;
  FIdentFuncTable[1125] := FuncCallee;
  FIdentFuncTable[3049] := FuncCaller;
  FIdentFuncTable[3037] := FuncCaptureevents;
  FIdentFuncTable[2101] := FuncCase;
  FIdentFuncTable[2105] := FuncCatch;
  FIdentFuncTable[4662] := FuncCeil;
  FIdentFuncTable[3938] := FuncChar;
  FIdentFuncTable[3046] := FuncCharat;
  FIdentFuncTable[3724] := FuncCharcodeat;
  FIdentFuncTable[4522] := FuncCheckbox;
  FIdentFuncTable[2127] := FuncChecked;
  FIdentFuncTable[1908] := FuncClass;
  FIdentFuncTable[417] := FuncClear;
  FIdentFuncTable[4574] := FuncClearinterval;
  FIdentFuncTable[2626] := FuncCleartimeout;
  FIdentFuncTable[55] := FuncClick;
  FIdentFuncTable[3783] := FuncClose;
  FIdentFuncTable[3615] := FuncClosed;
  FIdentFuncTable[1688] := FuncColor;
  FIdentFuncTable[2712] := FuncComplete;
  FIdentFuncTable[2889] := FuncConcat;
  FIdentFuncTable[3503] := FuncConfirm;
  FIdentFuncTable[820] := FuncConst;
  FIdentFuncTable[3079] := FuncConstructor;
  FIdentFuncTable[3092] := FuncContinue;
  FIdentFuncTable[4022] := FuncCookie;
  FIdentFuncTable[4452] := FuncCos;
  FIdentFuncTable[1188] := FuncCurrent;
  FIdentFuncTable[1955] := FuncDate;
  FIdentFuncTable[1095] := FuncDebugger;
  FIdentFuncTable[1389] := FuncDefault;
  FIdentFuncTable[2881] := FuncDefaultchecked;
  FIdentFuncTable[2879] := FuncDefaultselected;
  FIdentFuncTable[3607] := FuncDefaultstatus;
  FIdentFuncTable[1234] := FuncDefaultvalue;
  FIdentFuncTable[214] := FuncDelete;
  FIdentFuncTable[1929] := FuncDescription;
  FIdentFuncTable[1046] := FuncDisplay;
  FIdentFuncTable[748] := FuncDo;
  FIdentFuncTable[5075] := FuncDocument;
  FIdentFuncTable[4671] := FuncDomain;
  FIdentFuncTable[4176] := FuncDouble;
  FIdentFuncTable[5059] := FuncE;
  FIdentFuncTable[581] := FuncElements;
  FIdentFuncTable[4413] := FuncElse;
  FIdentFuncTable[2919] := FuncEmbed;
  FIdentFuncTable[2346] := FuncEmbeds;
  FIdentFuncTable[3190] := FuncEnabledplugin;
  FIdentFuncTable[4627] := FuncEncoding;
  FIdentFuncTable[3716] := FuncEnum;
  FIdentFuncTable[1147] := FuncEscape;
  FIdentFuncTable[1465] := FuncEval;
  FIdentFuncTable[3807] := FuncEvent;
  FIdentFuncTable[2060] := FuncExp;
  FIdentFuncTable[1298] := FuncExport;
  FIdentFuncTable[1114] := FuncExtends;
  FIdentFuncTable[1069] := FuncFalse;
  FIdentFuncTable[4097] := FuncFgcolor;
  FIdentFuncTable[2508] := FuncFilename;
  FIdentFuncTable[271] := FuncFileupload;
  FIdentFuncTable[3365] := FuncFinal;
  FIdentFuncTable[587] := FuncFinally;
  FIdentFuncTable[2843] := FuncFind;
  FIdentFuncTable[931] := FuncFixed;
  FIdentFuncTable[1921] := FuncFloat;
  FIdentFuncTable[730] := FuncFloat2;
  FIdentFuncTable[1491] := FuncFloor;
  FIdentFuncTable[4111] := FuncFocus;
  FIdentFuncTable[4774] := FuncFontcolor;
  FIdentFuncTable[4932] := FuncFontsize;
  FIdentFuncTable[407] := FuncFor;
  FIdentFuncTable[2968] := FuncForm;
  FIdentFuncTable[4469] := FuncForm;
  FIdentFuncTable[2370] := FuncForms;
  FIdentFuncTable[1279] := FuncForward;
  FIdentFuncTable[4402] := FuncFrame;
  FIdentFuncTable[2522] := FuncFrames;
  FIdentFuncTable[3737] := FuncFromcharcode;
  FIdentFuncTable[2666] := FuncFunction;
  FIdentFuncTable[1982] := FuncFunction2;
  FIdentFuncTable[1111] := FuncGetdate;
  FIdentFuncTable[1176] := FuncGetday;
  FIdentFuncTable[553] := FuncGetelementbyid;
  FIdentFuncTable[966] := FuncGetfullyear;
  FIdentFuncTable[2918] := FuncGethours;
  FIdentFuncTable[1735] := FuncGetmilliseconds;
  FIdentFuncTable[3823] := FuncGetminutes;
  FIdentFuncTable[4549] := FuncGetmonth;
  FIdentFuncTable[978] := FuncGetseconds;
  FIdentFuncTable[1102] := FuncGettime;
  FIdentFuncTable[4707] := FuncGettimezoneoffset;
  FIdentFuncTable[4493] := FuncGetutcdate;
  FIdentFuncTable[3536] := FuncGetutcday;
  FIdentFuncTable[5080] := FuncGetutcfullyear;
  FIdentFuncTable[671] := FuncGetutchours;
  FIdentFuncTable[1795] := FuncGetutcmilliseconds;
  FIdentFuncTable[974] := FuncGetutcminutes;
  FIdentFuncTable[2302] := FuncGetutcmonth;
  FIdentFuncTable[3282] := FuncGetutcseconds;
  FIdentFuncTable[2212] := FuncGetyear;
  FIdentFuncTable[2940] := FuncGlobal;
  FIdentFuncTable[4400] := FuncGo;
  FIdentFuncTable[4552] := FuncGoto;
  FIdentFuncTable[269] := FuncHandleevent;
  FIdentFuncTable[2358] := FuncHash;
  FIdentFuncTable[380] := FuncHeight;
  FIdentFuncTable[911] := FuncHidden;
  FIdentFuncTable[2645] := FuncHistory;
  FIdentFuncTable[1710] := FuncHistory;
  FIdentFuncTable[3710] := FuncHome;
  FIdentFuncTable[2928] := FuncHost;
  FIdentFuncTable[3996] := FuncHostname;
  FIdentFuncTable[2246] := FuncHref;
  FIdentFuncTable[991] := FuncHspace;
  FIdentFuncTable[3785] := FuncIf;
  FIdentFuncTable[1177] := FuncImage;
  FIdentFuncTable[1997] := FuncImages;
  FIdentFuncTable[706] := FuncImplements;
  FIdentFuncTable[3582] := FuncImport;
  FIdentFuncTable[4969] := FuncIn;
  FIdentFuncTable[1137] := FuncIndex;
  FIdentFuncTable[656] := FuncIndexof;
  FIdentFuncTable[4918] := FuncInfinity;
  FIdentFuncTable[5096] := FuncInnerheight;
  FIdentFuncTable[5008] := FuncInnerwidth;
  FIdentFuncTable[999] := FuncInput;
  FIdentFuncTable[3585] := FuncInstanceof;
  FIdentFuncTable[545] := FuncInt;
  FIdentFuncTable[124] := FuncInterface;
  FIdentFuncTable[2465] := FuncIsfinite;
  FIdentFuncTable[1266] := FuncIsnan;
  FIdentFuncTable[1711] := FuncItalics;
  FIdentFuncTable[963] := FuncJava;
  FIdentFuncTable[4225] := FuncJavaenabled;
  FIdentFuncTable[3229] := FuncJoin;
  FIdentFuncTable[2913] := FuncLastindexof;
  FIdentFuncTable[2778] := FuncLastmodified;
  FIdentFuncTable[3139] := FuncLayer;
  FIdentFuncTable[2021] := FuncLayers;
  FIdentFuncTable[2770] := FuncLeft;
  FIdentFuncTable[1083] := FuncLength;
  FIdentFuncTable[4263] := FuncLink;
  FIdentFuncTable[611] := FuncLink;
  FIdentFuncTable[2947] := FuncLinkcolor;
  FIdentFuncTable[3943] := FuncLinks;
  FIdentFuncTable[986] := FuncLn10;
  FIdentFuncTable[5149] := FuncLn2;
  FIdentFuncTable[4694] := FuncLocation;
  FIdentFuncTable[2489] := FuncLocation;
  FIdentFuncTable[3213] := FuncLocationbar;
  FIdentFuncTable[2812] := FuncLog;
  FIdentFuncTable[3420] := FuncLog10e;
  FIdentFuncTable[3346] := FuncLog2e;
  FIdentFuncTable[3808] := FuncLogon;
  FIdentFuncTable[1030] := FuncLong;
  FIdentFuncTable[2430] := FuncLowsrc;
  FIdentFuncTable[830] := FuncMatch;
  FIdentFuncTable[1454] := FuncMath;
  FIdentFuncTable[4163] := FuncMax;
  FIdentFuncTable[962] := FuncMax_value;
  FIdentFuncTable[165] := FuncMenubar;
  FIdentFuncTable[1767] := FuncMethod;
  FIdentFuncTable[2068] := FuncMimetype;
  FIdentFuncTable[568] := FuncMimetypes;
  FIdentFuncTable[398] := FuncMin;
  FIdentFuncTable[1580] := FuncMin_value;
  FIdentFuncTable[3868] := FuncMoveby;
  FIdentFuncTable[3688] := FuncMoveto;
  FIdentFuncTable[147] := FuncName;
  FIdentFuncTable[624] := FuncNan;
  FIdentFuncTable[709] := FuncNative;
  FIdentFuncTable[3619] := FuncNavigator;
  FIdentFuncTable[1798] := FuncNavigator;
  FIdentFuncTable[2749] := FuncNegative_infinity;
  FIdentFuncTable[2232] := FuncNetscape;
  FIdentFuncTable[4150] := FuncNew;
  FIdentFuncTable[3691] := FuncNext;
  FIdentFuncTable[3186] := FuncNull;
  FIdentFuncTable[4687] := FuncNull2;
  FIdentFuncTable[811] := FuncNumber;
  FIdentFuncTable[655] := FuncObject;
  FIdentFuncTable[4718] := FuncOnabort;
  FIdentFuncTable[3216] := FuncOnblur;
  FIdentFuncTable[4506] := FuncOnchange;
  FIdentFuncTable[4236] := FuncOnclick;
  FIdentFuncTable[1962] := FuncOndblclick;
  FIdentFuncTable[3283] := FuncOnerror;
  FIdentFuncTable[1618] := FuncOnfocus;
  FIdentFuncTable[2711] := FuncOnkeydown;
  FIdentFuncTable[698] := FuncOnkeypress;
  FIdentFuncTable[2845] := FuncOnkeyup;
  FIdentFuncTable[9] := FuncOnload;
  FIdentFuncTable[1175] := FuncOnmousedown;
  FIdentFuncTable[1116] := FuncOnmousemove;
  FIdentFuncTable[720] := FuncOnmouseout;
  FIdentFuncTable[356] := FuncOnmouseover;
  FIdentFuncTable[2930] := FuncOnmouseup;
  FIdentFuncTable[1494] := FuncOnreset;
  FIdentFuncTable[1591] := FuncOnselect;
  FIdentFuncTable[233] := FuncOnsubmit;
  FIdentFuncTable[1527] := FuncOnunload;
  FIdentFuncTable[309] := FuncOpen;
  FIdentFuncTable[3742] := FuncOpener;
  FIdentFuncTable[3624] := FuncOption;
  FIdentFuncTable[3038] := FuncOptions;
  FIdentFuncTable[3129] := FuncOuterheight;
  FIdentFuncTable[617] := FuncOuterwidth;
  FIdentFuncTable[1183] := FuncPackage;
  FIdentFuncTable[4634] := FuncPackages;
  FIdentFuncTable[4499] := FuncPagex;
  FIdentFuncTable[1543] := FuncPagexoffset;
  FIdentFuncTable[4647] := FuncPagey;
  FIdentFuncTable[4043] := FuncPageyoffset;
  FIdentFuncTable[4818] := FuncParent;
  FIdentFuncTable[2306] := FuncParse;
  FIdentFuncTable[2092] := FuncParsefloat;
  FIdentFuncTable[2800] := FuncParseint;
  FIdentFuncTable[756] := FuncPassword;
  FIdentFuncTable[1065] := FuncPathname;
  FIdentFuncTable[433] := FuncPersonalbar;
  FIdentFuncTable[3413] := FuncPi;
  FIdentFuncTable[4421] := FuncPlatform;
  FIdentFuncTable[4802] := FuncPlugin;
  FIdentFuncTable[3917] := FuncPlugins;
  FIdentFuncTable[3630] := FuncPort;
  FIdentFuncTable[4426] := FuncPositive_infinity;
  FIdentFuncTable[5137] := FuncPow;
  FIdentFuncTable[4810] := FuncPrevious;
  FIdentFuncTable[602] := FuncPrint;
  FIdentFuncTable[958] := FuncPrivate;
  FIdentFuncTable[3968] := FuncPrompt;
  FIdentFuncTable[1326] := FuncProtected;
  FIdentFuncTable[1815] := FuncProtocol;
  FIdentFuncTable[4437] := FuncPrototype;
  FIdentFuncTable[3260] := FuncPublic;
  FIdentFuncTable[1600] := FuncRadio;
  FIdentFuncTable[2803] := FuncRandom;
  FIdentFuncTable[2045] := FuncReferrer;
  FIdentFuncTable[2270] := FuncRefresh;
  FIdentFuncTable[4495] := FuncRegexp;
  FIdentFuncTable[2008] := FuncReleaseevents;
  FIdentFuncTable[4401] := FuncReload;
  FIdentFuncTable[1148] := FuncReplace;
  FIdentFuncTable[3987] := FuncReset;
  FIdentFuncTable[2796] := FuncReset;
  FIdentFuncTable[4116] := FuncResizeby;
  FIdentFuncTable[3936] := FuncResizeto;
  FIdentFuncTable[1610] := FuncReturn;
  FIdentFuncTable[3457] := FuncReverse;
  FIdentFuncTable[1857] := FuncRight;
  FIdentFuncTable[4450] := FuncRound;
  FIdentFuncTable[5041] := FuncRouteevent;
  FIdentFuncTable[100] := FuncScreen;
  FIdentFuncTable[3727] := FuncScroll;
  FIdentFuncTable[3112] := FuncScrollbars;
  FIdentFuncTable[195] := FuncScrollby;
  FIdentFuncTable[15] := FuncScrollto;
  FIdentFuncTable[4544] := FuncSearch;
  FIdentFuncTable[1271] := FuncSelect;
  FIdentFuncTable[2532] := FuncSelect;
  FIdentFuncTable[2708] := FuncSelected;
  FIdentFuncTable[5089] := FuncSelectedindex;
  FIdentFuncTable[2283] := FuncSelf;
  FIdentFuncTable[4756] := FuncSetdate;
  FIdentFuncTable[517] := FuncSetfullyear;
  FIdentFuncTable[4389] := FuncSethours;
  FIdentFuncTable[2365] := FuncSetinterval;
  FIdentFuncTable[1057] := FuncSetmilliseconds;
  FIdentFuncTable[2377] := FuncSetminutes;
  FIdentFuncTable[867] := FuncSetmonth;
  FIdentFuncTable[4685] := FuncSetseconds;
  FIdentFuncTable[4747] := FuncSettime;
  FIdentFuncTable[1862] := FuncSettimeout;
  FIdentFuncTable[3047] := FuncSetutcdate;
  FIdentFuncTable[5010] := FuncSetutcfullyear;
  FIdentFuncTable[222] := FuncSetutchours;
  FIdentFuncTable[2284] := FuncSetutcmilliseconds;
  FIdentFuncTable[5073] := FuncSetutcminutes;
  FIdentFuncTable[3374] := FuncSetutcmonth;
  FIdentFuncTable[707] := FuncSetutcseconds;
  FIdentFuncTable[2225] := FuncSetyear;
  FIdentFuncTable[3661] := FuncShort;
  FIdentFuncTable[2910] := FuncSin;
  FIdentFuncTable[523] := FuncSlice;
  FIdentFuncTable[1345] := FuncSmall;
  FIdentFuncTable[3822] := FuncSort;
  FIdentFuncTable[239] := FuncSplit;
  FIdentFuncTable[1224] := FuncSqrt;
  FIdentFuncTable[4716] := FuncSqrt1_2;
  FIdentFuncTable[3194] := FuncSqrt2;
  FIdentFuncTable[1932] := FuncSrc;
  FIdentFuncTable[482] := FuncStart;
  FIdentFuncTable[684] := FuncStatic;
  FIdentFuncTable[2201] := FuncStatus;
  FIdentFuncTable[1836] := FuncStatusbar;
  FIdentFuncTable[3389] := FuncStop;
  FIdentFuncTable[4740] := FuncStrike;
  FIdentFuncTable[4796] := FuncString;
  FIdentFuncTable[1006] := FuncStyle;
  FIdentFuncTable[283] := FuncSub;
  FIdentFuncTable[5066] := FuncSubmit;
  FIdentFuncTable[1174] := FuncSubmit;
  FIdentFuncTable[3496] := FuncSubstr;
  FIdentFuncTable[2071] := FuncSubstring;
  FIdentFuncTable[4785] := FuncSuffixes;
  FIdentFuncTable[2355] := FuncSup;
  FIdentFuncTable[4953] := FuncSuper;
  FIdentFuncTable[3170] := FuncSwitch;
  FIdentFuncTable[4968] := FuncSynchronized;
  FIdentFuncTable[4084] := FuncTags;
  FIdentFuncTable[2789] := FuncTaint;
  FIdentFuncTable[215] := FuncTaintenabled;
  FIdentFuncTable[3896] := FuncTan;
  FIdentFuncTable[5087] := FuncTarget;
  FIdentFuncTable[4075] := FuncText;
  FIdentFuncTable[4055] := FuncText;
  FIdentFuncTable[2681] := FuncTextarea;
  FIdentFuncTable[2382] := FuncThis;
  FIdentFuncTable[4217] := FuncThrow;
  FIdentFuncTable[2507] := FuncThrows;
  FIdentFuncTable[1027] := FuncTitle;
  FIdentFuncTable[2422] := FuncTogmtstring;
  FIdentFuncTable[4448] := FuncTolocalestring;
  FIdentFuncTable[857] := FuncTolowercase;
  FIdentFuncTable[4611] := FuncToolbar;
  FIdentFuncTable[4058] := FuncTop;
  FIdentFuncTable[983] := FuncTosource;
  FIdentFuncTable[3962] := FuncTostring;
  FIdentFuncTable[4977] := FuncTouppercase;
  FIdentFuncTable[2536] := FuncToutcstring;
  FIdentFuncTable[4990] := FuncTransient;
  FIdentFuncTable[1928] := FuncTrue;
  FIdentFuncTable[3889] := FuncTry;
  FIdentFuncTable[3925] := FuncType;
  FIdentFuncTable[3121] := FuncTypeof;
  FIdentFuncTable[4305] := FuncUndefined;
  FIdentFuncTable[2484] := FuncUndefined;
  FIdentFuncTable[3518] := FuncUnescape;
  FIdentFuncTable[4070] := FuncUntaint;
  FIdentFuncTable[836] := FuncUnwatch;
  FIdentFuncTable[3893] := FuncUrl;
  FIdentFuncTable[747] := FuncUseragent;
  FIdentFuncTable[3278] := FuncUtc;
  FIdentFuncTable[1621] := FuncValue;
  FIdentFuncTable[5048] := FuncValueof;
  FIdentFuncTable[1890] := FuncVar;
  FIdentFuncTable[910] := FuncVisibility;
  FIdentFuncTable[2454] := FuncVlinkcolor;
  FIdentFuncTable[996] := FuncVoid;
  FIdentFuncTable[418] := FuncVspace;
  FIdentFuncTable[4708] := FuncWatch;
  FIdentFuncTable[3178] := FuncWhile;
  FIdentFuncTable[1729] := FuncWidth;
  FIdentFuncTable[2916] := FuncWindow;
  FIdentFuncTable[4177] := FuncWindow;
  FIdentFuncTable[4646] := FuncWith;
  FIdentFuncTable[519] := FuncWrite;
  FIdentFuncTable[4841] := FuncWriteln;
  FIdentFuncTable[4030] := FuncZindex;
end;

function TSynEdit32HighlighterJScript.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAbs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAbstract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAcos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAlert(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAlign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAlinkcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAnchor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAnchors(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAppcodename(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncApplet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncApplets(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAppname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAppversion(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncArea(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncArguments(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAsin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAtan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncAtan2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBackground(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBgcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBlink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBlur(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBody(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBold(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBoolean(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBoolean2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;


function TSynEdit32HighlighterJScript.FuncBorder(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBottom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncButton(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCallee(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCaller(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCaptureevents(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCeil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncChar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCharat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCharcodeat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCheckbox(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncChecked(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncClear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncClearinterval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCleartimeout(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncClick(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncClose(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncClosed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncColor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncComplete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncConcat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncConfirm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncConstructor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCookie(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncCurrent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDebugger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDefaultchecked(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDefaultselected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDefaultstatus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDefaultvalue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDescription(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDisplay(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDocument(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDomain(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncDouble(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncE(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncElements(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEmbed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEmbeds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEnabledplugin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEncoding(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEnum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEscape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncEvent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncExp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncExport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncExtends(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFgcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFilename(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFileupload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFinal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFinally(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFind(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFixed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFloat2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFloor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFocus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFontcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFontsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncForm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncForms(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncForward(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFrame(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFrames(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFromcharcode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFunction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncFunction2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetday(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetelementbyid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGethours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGettime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGettimezoneoffset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutcdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutcday(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutcfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutchours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutcmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutcminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutcmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetutcseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGetyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGlobal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHandleevent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHash(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHeight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHidden(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHistory(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHome(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHost(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHostname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHref(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncHspace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncImage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncImages(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncImplements(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncImport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncIndex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncIndexof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncInfinity(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncInnerheight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncInnerwidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncInput(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncInstanceof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncInterface(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncIsfinite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncIsnan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncItalics(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncJava(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncJavaenabled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncJoin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLastindexof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLastmodified(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLayer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLayers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLeft(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLength(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLinkcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLinks(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLn10(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLn2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLocation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLocationbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLog(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLog10e(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLog2e(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLogon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLong(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncLowsrc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMath(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMax(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMax_value(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMenubar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMethod(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMimetype(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMimetypes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMin_value(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMoveby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncMoveto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncName(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNative(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNavigator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNegative_infinity(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNetscape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNull(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNull2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncNumber(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnabort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnblur(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnchange(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnclick(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOndblclick(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnerror(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnfocus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnkeydown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnkeypress(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnkeyup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnmousedown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnmousemove(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnmouseout(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnmouseover(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnmouseup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnreset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnselect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnsubmit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOnunload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOpen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOpener(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOption(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOuterheight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncOuterwidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPackages(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPagex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPagexoffset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPagey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPageyoffset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncParent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncParse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncParsefloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncParseint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPassword(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPathname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPersonalbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPi(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPlatform(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPlugin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPlugins(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPositive_infinity(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPrevious(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPrint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPrivate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPrompt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncProtected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncProtocol(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPrototype(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncPublic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncRadio(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncRandom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncReferrer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncRefresh(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncRegexp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncReleaseevents(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncReload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncReplace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncReset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncResizeby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncResizeto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncReverse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncRight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncRound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncRouteevent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncScreen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncScroll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncScrollbars(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncScrollby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncScrollto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSearch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSelect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSelected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSelectedindex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSelf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSethours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetinterval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSettime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSettimeout(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetutcdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetutcfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetutchours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetutcmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetutcminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetutcmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetutcseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSetyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncShort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSlice(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSmall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSplit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSqrt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSqrt1_2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSqrt2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSrc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncStart(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncStatic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncStatus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncStatusbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncStop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncStrike(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncStyle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSub(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSubmit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSubstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSubstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSuffixes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSuper(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncSynchronized(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTags(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTaint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTaintenabled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTarget(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncText(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTextarea(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncThis(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncThrow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncThrows(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTitle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTogmtstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTolocalestring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTolowercase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncToolbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTosource(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTostring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTouppercase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncToutcstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTransient(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncType(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncTypeof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncUndefined(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncUnescape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncUntaint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncUnwatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncUrl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncUseragent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncUtc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncValue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncValueof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncVisibility(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncVlinkcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncVspace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncWatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncWidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncWindow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncWith(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncWrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncWriteln(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynEdit32HighlighterJScript.FuncZindex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterJScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FNonReservedKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  AddAttribute(FNonReservedKeyAttri);
  FEventAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrEvent, SYNS_FriendlyAttrEvent);
  AddAttribute(FEventAttri);
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
  FDefaultFilter := SYNS_FilterJScript;
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterJScript.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '&']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.CommentProc;
begin
  if FLine[FRun] = #0 then
    NullProc
  else
  begin
    FTokenID := tkComment;
    repeat
      if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
      begin
        FRange := rsUnKnown;
        Inc(FRun, 2);
        break;
      end;
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterJScript.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.MinusProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '-', '>']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.ModSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  isHex: Boolean;
begin
  FTokenID := tkNumber;
  isHex := False;
  idx1 := FRun;
  Inc(FRun);
  while IsNumberChar do
  begin
    case FLine[FRun] of
      '.':
        if FLine[Succ(FRun)] = '.' then
          Break;
      'a'..'f', 'A'..'F':
        if not isHex then
          Break;
      'x', 'X':
        begin
          if (FLine[idx1] <> '0') or (FRun > Succ(idx1)) then
            Break;
          if not IsHexChar(Succ(FRun)) then
            Break;
          isHex := True;
        end;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterJScript.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '|']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '+']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] = '.') and (FLine[FRun + 1] = '.') then Inc(FRun, 2);
end;

procedure TSynEdit32HighlighterJScript.SlashProc;
begin
  Inc(FRun);
  case FLine[FRun] of
    '/': begin
           FTokenID := tkComment;
           repeat
             Inc(FRun);
           until IsLineEnd(FRun);
         end;
    '*': begin
           FTokenID := tkComment;
           FRange := rsAnsi;
           repeat
             Inc(FRun);
             if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
             begin
               FRange := rsUnKnown;
               Inc(FRun, 2);
               break;
             end;
           until IsLineEnd(FRun);
         end;
    '=': begin
           Inc(FRun);
           FTokenID := tkSymbol;
         end;
    else
      FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterJScript.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.StarProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.StringProc;
var
  l_strChar: UnicodeString;
begin
  FTokenID := tkString;
  l_strChar := FLine[FRun];   // We could have '"' or #39
  if (FLine[FRun + 1] = l_strChar) and (FLine[FRun + 2] = l_strChar) then Inc(FRun, 2);
  repeat
    if IsLineEnd(FRun) then break;
    Inc(FRun);
  until (FLine[FRun] = l_strChar) and (FLine[Pred(FRun)] <> '\');
  if not IsLineEnd(FRun) then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterJScript.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterJScript.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterJScript.Next;
begin
  fTokenPos := FRun;
  if FRange = rsANSI then
    CommentProc
  else
    case FLine[FRun] of
      '&': AndSymbolProc;
      #13: CRProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      #10: LFProc;
      '-': MinusProc;
      '%': ModSymbolProc;
      #0: NullProc;
      '0'..'9': NumberProc;
      '|': OrSymbolProc;
      '+': PlusProc;
      '.': PointProc;
      '/': SlashProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '*': StarProc;
      '"', #39: StringProc;
      '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=':
        SymbolProc;
      else UnknownProc;
    end;
  inherited;
end;

function TSynEdit32HighlighterJScript.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterJScript.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterJScript.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterJScript.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNonReservedKey: Result := FNonReservedKeyAttri;
    tkEvent: Result := FEventAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterJScript.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterJScript.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEdit32HighlighterJScript.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterJScript.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterJScript;
end;

class function TSynEdit32HighlighterJScript.GetLanguageName: string;
begin
  Result := SYNS_LangJScript;
end;

function TSynEdit32HighlighterJScript.GetSampleSource: UnicodeString;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  var number = 1234;'#13#10+
            '  var x;'#13#10+
            '  document.write("The number is " + number);'#13#10+
            '  for (var i = 0; i <= number; i++)'#13#10+
            '  {'#13#10+
            '    x++;'#13#10+
            '    x--;'#13#10+
            '    x += 1.0;'#13#10+
            '  }'#13#10+
            '  i += @; // illegal character'#13#10+
            '}'#13#10+
            'body.onLoad = printNumber;';
end;

class function TSynEdit32HighlighterJScript.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangJScript;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterJScript);
end.
