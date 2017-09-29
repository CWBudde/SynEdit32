{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterASM.pas, released 2000-04-18.
The Original Code is based on the nhAsmSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Nick Hoddinott.
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

$Id: SynHighlighterAsmMASM.pas,v 1.0 2017/02/12 tjaeger Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides Microsoft Macro Assembler (MASM) highlighter for SynEdit)
@author(Thomas Jaeger <thomasjaeger@gmail.com>)
@created(February 12th, 2017)
@lastmod(February 12th, 2017)
The SynHighlighterASM unit provides SynEdit with a Microsoft Macro Assembler (MASM) highlighter.
The highlighter supports all MASM features including directives and macros.

May experience out of memory when compiling package. Folow instructions to
compile externally until I move the API functions externally into JSON file.
}

unit SynEdit32.Highlighter.AsmMASM;

{$I SynEdit32.Inc}

interface

uses
  Graphics, Classes, SysUtils, IOUtils,
  SynEdit32.Types,
  SynEdit32.Highlighter,
  SynEdit32.Highlighter.HashEntries,
  SynEdit32.Unicode,
  SynEdit32.Memo;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown, tkDirectives, tkRegister, tkApi, tkInclude,
    tkOperator);

type
  TSynEdit32HighlighterAsmMASM = class(TSynEdit32CustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIncludeAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;
    FKeywords: TSynEdit32HashEntryList;
    FDirectivesKeywords: TSynEdit32HashEntryList;
    FDirectivesAttri: TSynEdit32HighlighterAttributes;
    FRegisterKeywords: TSynEdit32HashEntryList;
    FRegisterAttri: TSynEdit32HighlighterAttributes;
    FApiKeywords: TSynEdit32HashEntryList;
    FApiAttri: TSynEdit32HighlighterAttributes;
    FOperatorKeywords: TSynEdit32HashEntryList;
    FOperatorAttri: TSynEdit32HighlighterAttributes;
    FApis: UnicodeString;
    function HashKey(Str: PWideChar): Cardinal;
    procedure CommentProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure IncludeProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SingleQuoteStringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
    procedure DoAddDirectivesKeyword(AKeyword: UnicodeString; AKind: integer);
    procedure DoAddRegisterKeyword(AKeyword: UnicodeString; AKind: integer);
    procedure DoAddApiKeyword(AKeyword: UnicodeString; AKind: integer);
    procedure DoAddOperatorKeyword(AKeyword: UnicodeString; AKind: integer);
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
      override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynEdit32HighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynEdit32HighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property DirectivesAttri: TSynEdit32HighlighterAttributes read FDirectivesAttri write FDirectivesAttri;
    property RegisterAttri: TSynEdit32HighlighterAttributes read FRegisterAttri write FRegisterAttri;
    property ApiAttri: TSynEdit32HighlighterAttributes read FApiAttri write FApiAttri;
    property IncludeAttri: TSynEdit32HighlighterAttributes read FIncludeAttri write FIncludeAttri;
    property OperatorAttri: TSynEdit32HighlighterAttributes read FOperatorAttri write FOperatorAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  Mnemonics: UnicodeString =
    'aaa,aad,aam,adc,add,and,arpl,bound,bsf,bsr,bswap,bt,btc,' +
    'btr,bts,call,cbw,cdq,clc,cld,cli,clts,cmc,cmp,cmps,cmpsb,cmpsd,cmpsw,' +
    'cmpxchg,cwd,cwde,daa,das,dec,div,emms,enter,f2xm1,fabs,fadd,faddp,fbld,' +
    'fbstp,fchs,fclex,fcmovb,fcmovbe,fcmove,fcmovnb,fcmovnbe,fcmovne,fcmovnu,' +
    'fcmovu,fcom,fcomi,fcomip,fcomp,fcompp,fcos,fdecstp,fdiv,fdivp,fdivr,' +
    'fdivrp,femms,ffree,fiadd,ficom,ficomp,fidiv,fidivr,fild,fimul,fincstp,' +
    'finit,fist,fistp,fisub,fisubr,fld,fld1,fldcw,fldenv,fldl2e,fldl2t,fldlg2,' +
    'fldln2,fldpi,fldz,fmul,fmulp,fnclex,fninit,fnop,fnsave,fnstcw,fnstenv,' +
    'fnstsw,fpatan,fprem1,fptan,frndint,frstor,fsave,fscale,fsin,fsincos,' +
    'fsqrt,fst,fstcw,fstenv,fstp,fstsw,fsub,fsubp,fsubr,fsubrp,ftst,' +
    'fucom,fucomi,fucomip,fucomp,fucompp,fwait,fxch,fxtract,fyl2xp1,hlt,idiv,' +
    'imul,in,inc,ins,insb,insd,insw,int,into,invd,invlpg,iret,iretd,iretw,' +
    'ja,jae,jb,jbe,jc,jcxz,je,jecxz,jg,jge,jl,jle,jmp,jna,jnae,jnb,jnbe,jnc,' +
    'jne,jng,jnge,jnl,jnle,jno,jnp,jns,jnz,jo,jp,jpe,jpo,js,jz,lahf,lar,lds,' +
    'lea,leave,les,lfs,lgdt,lgs,lidt,lldt,lmsw,lock,lods,lodsb,lodsd,lodsw,' +
    'loop,loope,loopne,loopnz,loopz,lsl,lss,ltr,mov,movd,movq, movs,movsb,' +
    'movsd,movsw,movsx,movzx,mul,neg,nop,not,or,out,outs,outsb,outsd,outsw,' +
    'packssdw,packsswb,packuswb,paddb,paddd,paddsb,paddsw,paddusb,paddusw,' +
    'paddw,pand,pandn,pavgusb,pcmpeqb,pcmpeqd,pcmpeqw,pcmpgtb,pcmpgtd,pcmpgtw,' +
    'pf2id,pfacc,pfadd,pfcmpeq,pfcmpge,pfcmpgt,pfmax,pfmin,pfmul,pfrcp,' +
    'pfrcpit1,pfrcpit2,pfrsqit1,pfrsqrt,pfsub,pfsubr,pi2fd,pmaddwd,pmulhrw,' +
    'pmulhw,pmullw,pop,popa,popad,popaw,popf,popfd,popfw,por,prefetch,prefetchw,' +
    'pslld,psllq,psllw,psrad,psraw,psrld,psrlq,psrlw,psubb,psubd,psubsb,' +
    'psubsw,psubusb,psubusw,psubw,punpckhbw,punpckhdq,punpckhwd,punpcklbw,' +
    'punpckldq,punpcklwd,push,pusha,pushad,pushaw,pushf,pushfd,pushfw,pxor,' +
    'rcl,rcr,rep,repe,repne,repnz,repz,ret,rol,ror,sahf,sal,sar,sbb,scas,' +
    'scasb,scasd,scasw,seta,setae,setb,setbe,setc,sete,setg,setge,setl,setle,' +
    'setna,setnae,setnb,setnbe,setnc,setne,setng,setnge,setnl,setnle,setno,' +
    'setnp,setns,setnz,seto,setp,setpo,sets,setz,sgdt,shl,shld,shr,shrd,sidt,' +
    'sldt,smsw,stc,std,sti,stos,stosb,stosd,stosw,str,sub,test,verr,verw,' +
    'wait,wbinvd,xadd,xchg,xlat,xlatb,xor';

  Registers: UnicodeString =
    'ah,al,ax,bh,bl,bx,ch,cl,cs,cx,dh,di,dl,ds,dx,'+
    'eax,ebp,ebx,ecx,edi,edx,es,esi,esp,fs,gs,ip,eip,'+
    'rax,rcx,rdx,rbx,rsp,rbp,rsi,rdisi,ss,'+
    'r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,'+
    'r0D,r1D,r2D,r3D,r4D,r5D,r6D,r7D,r8D,r9D,r10D,r11D,r12D,r13D,r14D,r15D,'+
    'r0W,r1W,r2W,r3W,r4W,r5W,r6W,r7W,r8W,r9W,r10W,r11W,r12W,r13W,r14W,r15W,'+
    'r0L,r1L,r2L,r3L,r4L,r5L,r6L,r7L,r8L,r9L,r10L,r11L,r12L,r13L,r14L,r15L';

  Operators: UnicodeString =
    '+,-,*,/,==,!=,>,>=,<,<=,||,&&,&,!,carry?,overflow?,'+
    'parity?,sign?,zero?,%,&&,abs,addr,and,dup,eq,ge,'+
    'gt,high,high32,highword,imagerel,le,length,lengthof,' +
    'low,low32,lowword,lroffset,lt,mask,mod,ne,not,offset,' +
    'opattr,or,ptr,seg,shl,.type,sectionrel,short,shr,' +
    'size,sizeof,this,type,width,xor';

  Directives: UnicodeString =
    '=,.386,.386p,.387,.486,.486p,.586,.586p,.686,.686p,alias,align,.allocstack,'+
    '.alpha,assume,.break,byte,catstr,.code,comm,comment,.const,.continue,.cref,'+
    '.data,.data?,db,dd,df,.dosseg,dosseg,dq,dt,dw,dword,echo,.else,else,elseif,'+
  	'.elseif,'+
    'elseif2,end,.endif,endm,endp,.endprolog,ends,.endw,equ,.err,.err2,.errb,'+
    '.errdef,.errdif[[i]],.erre,.erridn[[i]],.errnb,.errndef,.errnz,even,.exit,'+
    'exitm,extern,externdef,extrn,.fardata,.fardata?,for,forc,.fpo,fword,goto,'+
    'group,.if,if,if2,ifb,ifdef,ifdif[[i]],ife,ifidn[[i]],ifnb,ifndef,include,'+
    'includelib,instr,invoke,irp,irpc,.k3d,label,.lall,.lfcond,.list,.listall,'+
    '.listif,.listmacro,.listmacroall,local,macro,mmword,.mmx,.model,name,'+
    '.nocref,.nollist,.nolistif,.nolistmacro,offset,option,org,%out,oword,page,'+
    'popcontext,proc,proto,public,purge,pushcontext,.pushframe,.pushreg,qword,'+
    '.radix,real10,real4,real8,record,.repeat,repeat,rept,.safeseh,.sall,'+
    '.savereg,.savexmm128,sbyte,sdword,segment,.seq,.setframe,.sfcond,sizestr,'+
    'sqword,.stack,.startup,struc,struct,substr,subtitle,subttl,sword,tbyte,'+
    'textequ,.tfcond,title,typedef,union,.until,.untilcxz,.while,while,word,'+
    '.xall,.xcref,.xlist,.xmm,xmmword,ymmword,'+
    'tiny,small,compact,medium,large,huge,flat,nearstack,farstack';

procedure TSynEdit32HighlighterAsmMASM.DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Cardinal;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

procedure TSynEdit32HighlighterAsmMASM.DoAddDirectivesKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Cardinal;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FDirectivesKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

procedure TSynEdit32HighlighterAsmMASM.DoAddRegisterKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Cardinal;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FRegisterKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

procedure TSynEdit32HighlighterAsmMASM.DoAddApiKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Cardinal;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FApiKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

procedure TSynEdit32HighlighterAsmMASM.DoAddOperatorKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Cardinal;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FOperatorKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

//{$Q-}
function TSynEdit32HighlighterAsmMASM.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 197 + Ord(Str^) * 14;
    Inc(Str);
  end;
  Result := Result mod 4561;
  FStringLen := Str - FToIdent;
end;
//{$Q+}

function TSynEdit32HighlighterAsmMASM.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynEdit32HashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;

  // THJ
  Entry := FDirectivesKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;

  // THJ
  Entry := FRegisterKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;

  // THJ
  Entry := FApiKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;

  Entry := FOperatorKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;

  Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterAsmMASM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeywords := TSynEdit32HashEntryList.Create;
  FDirectivesKeywords := TSynEdit32HashEntryList.Create;
  FRegisterKeywords := TSynEdit32HashEntryList.Create;
  FApiKeywords := TSynEdit32HashEntryList.Create;
  FOperatorKeywords := TSynEdit32HashEntryList.Create;

  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clRed;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FDirectivesAttri   := TSynEdit32HighlighterAttributes.Create('Directives', 'Directives');
  FDirectivesAttri.Foreground := $008CFF;
  FDirectivesAttri.Style := [fsBold];
  AddAttribute(FDirectivesAttri);

  FRegisterAttri := TSynEdit32HighlighterAttributes.Create('Register', 'Register');
  FRegisterAttri.Foreground := $32CD32;
  FRegisterAttri.Style := [fsBold];
  AddAttribute(FRegisterAttri);

  FApiAttri := TSynEdit32HighlighterAttributes.Create('Api', 'Api');
  FApiAttri.Foreground := clYellow;
  FApiAttri.Style := [fsBold];
  AddAttribute(FApiAttri);

  FIncludeAttri := TSynEdit32HighlighterAttributes.Create('Include', 'Include');
  FIncludeAttri.Foreground := clMoneyGreen;
  FIncludeAttri.Style := [fsBold];
  AddAttribute(FIncludeAttri);

  FOperatorAttri := TSynEdit32HighlighterAttributes.Create('Operator', 'Operator');
  FOperatorAttri.Foreground := clLime;
  FOperatorAttri.Style := [fsBold];
  AddAttribute(FOperatorAttri);

  EnumerateKeywords(Ord(tkKey), Mnemonics, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkDirectives), Directives, IsIdentChar, DoAddDirectivesKeyword);
  EnumerateKeywords(Ord(tkRegister), Registers, IsIdentChar, DoAddRegisterKeyword);

  if FileExists('WinAPIInsertList.txt') then
    FApis := TFile.ReadAllText('WinAPIInsertList.txt');
  EnumerateKeywords(Ord(tkApi), FApis, IsIdentChar, DoAddApiKeyword);

  EnumerateKeywords(Ord(tkOperator), Operators, IsIdentChar, DoAddOperatorKeyword);

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter      := SYNS_FilterX86Assembly;
end;

destructor TSynEdit32HighlighterAsmMASM.Destroy;
begin
  FKeywords.Free;
  FDirectivesKeywords.Free;
  FRegisterKeywords.Free;
  FApiKeywords.Free;
  FOperatorKeywords.Free;
  inherited Destroy;
end;

procedure TSynEdit32HighlighterAsmMASM.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.GreaterProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.LowerProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
  if CharInSet(FLine[FRun], ['=', '>']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', 'a'..'f', 'h', 'A'..'F', 'H': Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(FRun);
  FTokenID := tkNumber;
  while IsNumberChar do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.SlashProc;
begin
  Inc(FRun);
  if FLine[FRun] = '/' then begin
    FTokenID := tkComment;
    repeat
      Inc(FRun);
    until IsLineEnd(FRun);
  end else
    FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterAsmMASM.IncludeProc;
begin
  FTokenID := tkInclude;
  repeat
    Inc(FRun);
  until IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(FRun);
  until (FLine[FRun] > #32) or IsLineEnd(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.StringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #34) and (FLine[FRun + 2] = #34) then
    Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: break;
    end;
    Inc(FRun);
  until FLine[FRun] = #34;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.SingleQuoteStringProc;
begin
  FTokenID := tkString;
  if (FLine[FRun + 1] = #39) and (FLine[FRun + 2] = #39) then
    Inc(FRun, 2);
  repeat
    case FLine[FRun] of
      #0, #10, #13: break;
    end;
    Inc(FRun);
  until FLine[FRun] = #39;
  if FLine[FRun] <> #0 then Inc(FRun);
end;

procedure TSynEdit32HighlighterAsmMASM.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterAsmMASM.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkIdentifier;
end;

procedure TSynEdit32HighlighterAsmMASM.Next;
begin
  fTokenPos := FRun;
  case FLine[FRun] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #34: StringProc;
    #39: SingleQuoteStringProc;
    '>': GreaterProc;
    '<': LowerProc;
    '/': SlashProc;
    '\': IncludeProc;
    //'A'..'Z', 'a'..'z', '_': IdentProc;
    'A'..'Z', 'a'..'z', '_', '.', '?', '[', ']': IdentProc;   // THJ
    '0'..'9': NumberProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '#', ';': CommentProc;
    //'.', ':', '&', '{', '}', '=', '^', '-', '+', '(', ')', '*': SymbolProc;
    ':', '&', '{', '}', '^', '-', '+', '(', ')', '*': SymbolProc;
    else
      UnknownProc;
  end;
  inherited;
end;

function TSynEdit32HighlighterAsmMASM.GetDefaultAttribute(Index: integer): TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterAsmMASM.GetTokenAttribute: TSynEdit32HighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    tkDirectives: Result := FDirectivesAttri;
    tkRegister: Result := FRegisterAttri;
    tkApi: Result := FApiAttri;
    tkInclude: Result := FIncludeAttri;
    tkOperator: Result := FOperatorAttri;
    else Result := nil;
  end;
end;

function TSynEdit32HighlighterAsmMASM.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEdit32HighlighterAsmMASM.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

class function TSynEdit32HighlighterAsmMASM.GetLanguageName: string;
begin
  Result := SYNS_LangMASM;
end;

function TSynEdit32HighlighterAsmMASM.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterX86Assembly;
end;

function TSynEdit32HighlighterAsmMASM.GetSampleSource: UnicodeString;
begin
  Result := '; x86 assembly sample source'#13#10 +
            '  CODE	SEGMENT	BYTE PUBLIC'#13#10 +
            '    ASSUME	CS:CODE'#13#10 +
            #13#10 +
            '    PUSH SS'#13#10 +
            '    POP DS'#13#10 +
            '    MOV AX, AABBh'#13#10 +
            '    MOV	BYTE PTR ES:[DI], 255'#13#10 +
            '    JMP SHORT AsmEnd'#13#10 +
            #13#10 +
            '  welcomeMsg DB ''Hello World'', 0'#13#10 +
            #13#10 +
            '  AsmEnd:'#13#10 +
            '    MOV AX, 0'#13#10 +
            #13#10 +
            '  CODE	ENDS'#13#10 +
            'END';
end;

class function TSynEdit32HighlighterAsmMASM.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangMASM;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterAsmMASM);
end.

