{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVrml.pas, released 2002-10-21.
The Original Code is based on: SynHighlighterJScript.pas, released 2000-04-14.
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

$Id: SynHighlighterVrml97.pas,v 1.6.2.8 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Vrml97/X3D/JavaScript highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it)
@created(november 2002 [December 1999, converted to SynEdit April 14, 2000])
@lastmod(2002-11-03)
The SynHighlighterVrml97 unit provides SynEdit with a Vrml97/X3D/JavaScript (.wrl;*.x3d) highlighter.
The highlighter formats Vrml97/X3D source code highlighting keywords, strings, numbers and characters.
}

{ TODO: The Ansi version kept unclear to the status of following tokens:

  Token       Ambiguity
  =====       =========
  bottom      tkVrmlAttribute or tkNonReservedKey
  description tkVrmlAttribute or tkNonReservedKey
  height      tkVrmlAttribute or tkNonReservedKey
  location    tkVrmlAttribute or tkNonReservedKey
  style       tkVrmlAttribute or tkNonReservedKey
  type        tkVrmlAttribute or tkNonReservedKey

  NULL        tkVrmlParameter or tkVrmlProto
  FALSE       tkVrmlParameter or tkVrmlProto
  
  Text        tkVrmlShape or tkNonReservedKey

  I took always the first one as this produces the same results as in the
  Ansi-version, because the other cases were never reached (due to the way
  the if construct was used).
}

unit SynEdit32.Highlighter.Vrml97;

{$I SynEdit32.Inc}

interface

uses
  Windows, Messages, Registry, Controls, Graphics, SysUtils, Classes,
  SynEdit32.Types, SynEdit32.Highlighter, SynEdit32.Highlighter.HashEntries,
  SynEdit32.Unicode;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown,
    tkNonReservedKey,
    tkEvent,
    tkVrmlAppearance,
    tkVrmlAttribute,
    tkVrmlDefinition,
    tkVrmlEvent,
    tkVrmlGrouping,
    tkVrmlInterpolator,
    tkVrmlLight,
    tkVrmlNode,
    tkVrmlParameter,
    tkVrmlproto,
    tkVrmlSensor,
    tkVrmlShape,
    tkVrmlShape_Hint,
    tkVrmlTime_dependent,
    tkVrmlViewpoint,
    tkVrmlWorldInfo,
    tkX3DDocType,
    tkX3DHeader);

  TRangeState = (rsNormalText, rsComment, rsX3DHeader, rsX3DDocType);

type
  TSynEdit32HighlighterVrml97 = class(TSynEdit32CustomHighlighter)
  private
    FRange: TRangeState;
    FIsDoctype: boolean;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynEdit32HighlighterAttributes;
    FIdentifierAttri: TSynEdit32HighlighterAttributes;
    FKeyAttri: TSynEdit32HighlighterAttributes;
    FNonReservedKeyAttri: TSynEdit32HighlighterAttributes;
    FEventAttri: TSynEdit32HighlighterAttributes;
    FNumberAttri: TSynEdit32HighlighterAttributes;
    FSpaceAttri: TSynEdit32HighlighterAttributes;
    FStringAttri: TSynEdit32HighlighterAttributes;
    FSymbolAttri: TSynEdit32HighlighterAttributes;

    FVrmlAppearanceAttri: TSynEdit32HighlighterAttributes;
    FVrmlAttributeAttri: TSynEdit32HighlighterAttributes;
    FVrmlDefinitionAttri: TSynEdit32HighlighterAttributes;
    FVrmlEventAttri: TSynEdit32HighlighterAttributes;
    FVrmlGroupingAttri: TSynEdit32HighlighterAttributes;
    FVrmlInterpolatorAttri: TSynEdit32HighlighterAttributes;
    FVrmlLightAttri: TSynEdit32HighlighterAttributes;
    FVrmlNodeAttri: TSynEdit32HighlighterAttributes;
    FVrmlParameterAttri: TSynEdit32HighlighterAttributes;
    FVrmlprotoAttri: TSynEdit32HighlighterAttributes;
    FVrmlSensorAttri: TSynEdit32HighlighterAttributes;
    FVrmlShapeAttri: TSynEdit32HighlighterAttributes;
    FVrmlShape_HintAttri: TSynEdit32HighlighterAttributes;
    FVrmlTime_dependentAttri: TSynEdit32HighlighterAttributes;
    FVrmlViewpointAttri: TSynEdit32HighlighterAttributes;
    FVrmlWorldInfoAttri: TSynEdit32HighlighterAttributes;
    FX3DDocTypeAttri: TSynEdit32HighlighterAttributes;
    FX3DHeaderAttri: TSynEdit32HighlighterAttributes;

    FKeywords: TSynEdit32HashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AndSymbolProc;
    procedure CommentProc;
    procedure DiesisCommentProc;
    procedure X3DDocTypeOpenProc;
    procedure X3DDocTypeProc;
    procedure X3DHeaderOpenProc;
    procedure X3DHeaderProc;
    procedure InCommentProc;
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
    function NextTokenIs(T: UnicodeString) :Boolean;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index :integer) :TSynEdit32HighlighterAttributes;
      override;
    function GetEol :Boolean; override;
    function GetRange :Pointer; override;
    function GetTokenID :TtkTokenKind;
    function GetTokenAttribute :TSynEdit32HighlighterAttributes; override;
    function GetTokenKind :integer; override;
    procedure Next; override;
    procedure SetRange(Value :Pointer); override;
    procedure ResetRange; override;
  published
    property NonReservedKeyAttri :TSynEdit32HighlighterAttributes read FNonReservedKeyAttri write FNonReservedKeyAttri;
    property NumberAttri :TSynEdit32HighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri :TSynEdit32HighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri :TSynEdit32HighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri :TSynEdit32HighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property CommentAttri :TSynEdit32HighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri :TSynEdit32HighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property EcmaScriptKeyAttri :TSynEdit32HighlighterAttributes read FKeyAttri write FKeyAttri;
    property EcmaScriptEventAttri :TSynEdit32HighlighterAttributes read FEventAttri write FEventAttri;

    property VrmlAppearanceAttri :TSynEdit32HighlighterAttributes read FVrmlAppearanceAttri write FVrmlAppearanceAttri;
    property VrmlAttributeAttri :TSynEdit32HighlighterAttributes read FVrmlAttributeAttri write FVrmlAttributeAttri;
    property VrmlDefinitionAttri :TSynEdit32HighlighterAttributes read FVrmlDefinitionAttri write FVrmlDefinitionAttri;
    property VrmlEventAttri :TSynEdit32HighlighterAttributes read FVrmlEventAttri write FVrmlEventAttri;
    property VrmlGroupingAttri :TSynEdit32HighlighterAttributes read FVrmlGroupingAttri write FVrmlGroupingAttri;
    property VrmlInterpolatorAttri :TSynEdit32HighlighterAttributes read FVrmlInterpolatorAttri write FVrmlInterpolatorAttri;
    property VrmlLightAttri :TSynEdit32HighlighterAttributes read FVrmlLightAttri write FVrmlLightAttri;
    property VrmlNodeAttri :TSynEdit32HighlighterAttributes read FVrmlNodeAttri write FVrmlNodeAttri;
    property VrmlParameterAttri :TSynEdit32HighlighterAttributes read FVrmlParameterAttri write FVrmlParameterAttri;
    property VrmlprotoAttri :TSynEdit32HighlighterAttributes read FVrmlprotoAttri write FVrmlprotoAttri;
    property VrmlSensorAttri :TSynEdit32HighlighterAttributes read FVrmlSensorAttri write FVrmlSensorAttri;
    property VrmlShapeAttri :TSynEdit32HighlighterAttributes read FVrmlShapeAttri write FVrmlShapeAttri;
    property VrmlShape_HintAttri :TSynEdit32HighlighterAttributes read FVrmlShape_HintAttri write FVrmlShape_HintAttri;
    property VrmlTime_dependentAttri :TSynEdit32HighlighterAttributes read FVrmlTime_dependentAttri write FVrmlTime_dependentAttri;
    property VrmlViewpointAttri :TSynEdit32HighlighterAttributes read FVrmlViewpointAttri write FVrmlViewpointAttri;
    property VrmlWorldInfoAttri :TSynEdit32HighlighterAttributes read FVrmlWorldInfoAttri write FVrmlWorldInfoAttri;
    property X3DDocTypeAttri :TSynEdit32HighlighterAttributes read FX3DDocTypeAttri write FX3DDocTypeAttri;
    property X3DHeaderAttri :TSynEdit32HighlighterAttributes read FX3DHeaderAttri write FX3DHeaderAttri;
  end;

implementation

uses
  SynEdit32.StrConst;

const
  Events: UnicodeString =
    'onAbort, onBlur, onChange, onClick, onDblClick, onError, onFocus, ' +
    'onKeyDown, onKeyPress, onKeyUp, onLoad, onMouseDown, onMouseMove, ' +
    'onMouseOut, onMouseOver, onMouseUp, onReset, onSelect, onSubmit, ' +
    'onUnload';

  KeyWords: UnicodeString =
    'abstract, boolean, break, byte, callee, case, catch, char, class, ' +
    'const, constructor, continue, debugger, default, delete, do, DOCTYPE, ' +
    'double, else, enum, export, extends, false, final, finally, float, for, ' +
    'function, goto, head, if, implements, import, in, instanceof, int, ' +
    'interface, long, meta, NaN, native, new, null, package, private, ' +
    'protected, prototype, public, PUBLIC, return, short, start, static, ' +
    'super, switch, synchronized, this, throw, throws, transient, true, try, ' +
    'typeof, var, void, while, with, xml';

  NonReservedKeys: UnicodeString =
    'abs, acos, action, alert, align, alinkColor, all, All, anchor, anchors, ' +
    'appCodeName, Applet, applets, appName, appVersion, Area, arguments, ' +
    'Arguments, Array, asin, atan, atan2, back, background, bgColor, big, ' +
    'blink, blur, body, bold, Boolean, border, Button, call, caller, ' +
    'captureEvents, ceil, charAt, charCodeAt, Checkbox, checked, clear, ' +
    'clearInterval, clearTimeout, click, close, closed, complete, concat, ' +
    'confirm, content, cookie, cos, current, Date, defaultChecked, ' +
    'defaultSelected, defaultStatus, defaultValue, display, document, ' +
    'domain, E, elements, Embed, embeds, enabledPlugin, encoding, escape, ' +
    'eval, event, exp, fgColor, filename, FileUpload, find, fixed, Float, ' +
    'floor, focus, fontcolor, fontsize, form, Form, forms, forward, Frame, ' +
    'frames, fromCharCode, Function, getDate, getDay, getElementById, ' +
    'getFullYear, getHours, getMilliseconds, getMinutes, getMonth, ' +
    'getSeconds, getTime, getTimezoneOffset, getUTCDate, getUTCDay, ' +
    'getUTCFullYear, getUTCHours, getUTCMilliseconds, getUTCMinutes, ' +
    'getUTCMonth, getUTCSeconds, getYear, Global, go, handleEvent, hash, ' +
    'Hidden, history, History, home, host, hostname, href, hspace, Image, ' +
    'images, index, indexOf, Infinity, innerHeight, innerWidth, input, ' +
    'isFinite, isNaN, italics, java, javaEnabled, join, lastIndexOf, ' +
    'lastModified, Layer, layers, left, link, Link, linkColor, links, LN10, ' +
    'LN2, Location, locationbar, log, LOG10E, LOG2E, logon, lowsrc, match, ' +
    'Math, max, MAX_VALUE, menubar, method, MimeType, mimeTypes, min, ' +
    'MIN_VALUE, moveBy, moveTo, name, navigator, Navigator, ' +
    'NEGATIVE_INFINITY, netscape, next, Null, Number, Object, open, opener, ' +
    'Option, options, outerHeight, outerWidth, Packages, pageX, pageXOffset, ' +
    'pageY, pageYOffset, parent, parse, parseFloat, parseInt, Password, ' +
    'pathname, personalbar, PI, platform, Plugin, plugins, port, ' +
    'POSITIVE_INFINITY, pow, previous, print, prompt, protocol, Radio, ' +
    'random, referrer, refresh, RegExp, releaseEvents, reload, replace, ' +
    'reset, Reset, resizeBy, resizeTo, reverse, right, round, routeEvent, ' +
    'screen, scroll, scrollbars, scrollBy, scrollTo, search, select, Select, ' +
    'selected, selectedIndex, self, setDate, setFullYear, setHours, ' +
    'setInterval, setMilliseconds, setMinutes, setMonth, setSeconds, ' +
    'setTime, setTimeout, setUTCDate, setUTCFullYear, setUTCHours, ' +
    'setUTCMilliseconds, setUTCMinutes, setUTCMonth, setUTCSeconds, setYear, ' +
    'sin, slice, small, sort, split, sqrt, SQRT1_2, SQRT2, src, status, ' +
    'statusbar, stop, strike, String, sub, submit, Submit, substr, ' +
    'substring, suffixes, sup, tags, taint, taintEnabled, tan, target, text, ' +
    'Textarea, title, toGMTString, toLocaleString, toLowerCase, toolbar, ' +
    'toSource, toString, toUpperCase, toUTCString, undefined, Undefined, ' +
    'unescape, untaint, unwatch, URL, userAgent, UTC, value, valueOf, ' +
    'version, visibility, vlinkColor, vspace, watch, width, window, Window, ' +
    'write, writeln, zIndex';

  VrmlAppearances: UnicodeString =
    'Appearance, ImageTexture, Material, NurbsTextureSurface, PixelTexture, ' +
    'TextureBackground, TextureCoordinate, TextureCoordinateGenerator, ' +
    'TextureTransform';

  VrmlAttributes: UnicodeString =
    'addChildren, ambientIntensity, appearance, attenuation, autoOffset, ' +
    'avatarSize, axisOfRotation, backUrl, bboxCenter, bboxSize, beamWidth, ' +
    'beginCap, bindTime, bottom, bottomRadius, bottomUrl, ccw, center, children, ' +
    'choice, collide, collideTime, color, colorIndex, colorPerVertex, ' +
    'ColorRGBA, convex, coord, coordIndex, creaseAngle, crossSection, ' +
    'cutOffAngle, cycleInterval, cycleTime, description, diffuseColor, direction, ' +
    'directOutput, diskAngle, duration_changed, emissiveColor, enabled, ' +
    'endCap, enterTime, eventName, eventType, exitTime, family, fieldName, ' +
    'fieldOfView, fieldType, FillProperties, fogType, fontStyle, ' +
    'fraction_changed, frontUrl, GeoCoordinate, GeoElevationGrid, ' +
    'GeoLocation, GeoLOD, GeoMetadata, geometry, GeoOrigin, groundAngle, ' +
    'groundColor, headlight, height, hitNormal_changed, hitPoint_changed, ' +
    'hitTexCoord_changed, horizontal, image, info, intensity, isActive, ' +
    'isBound, isOver, jump, justify, key, keyValue, language, leftToRight, ' +
    'leftUrl, length, level, LineProperties, location, loop, material, maxAngle, ' +
    'maxBack, maxExtent, maxFront, maxPosition, minAngle, minBack, minFront, ' +
    'minPosition, MultiTexture, MultiTextureCoordinate, mustEvaluate, ' +
    'normal, normalIndex, normalPerVertex, offset, on, orientation, ' +
    'orientation_changed, parameter, pitch, point, position, ' +
    'position_changed, priority, proxy, radius, range, removeChildren, ' +
    'repeatS, repeatT, rightUrl, rotation, rotation_changed, scale, ' +
    'scaleOrientation, set_bind, set_colorIndex, set_coordIndex, ' +
    'set_crossSection, set_fraction, set_height, set_normalIndex, ' +
    'set_orientation, set_scale, set_spine, set_texCoordIndex, shininess, ' +
    'side, size, skyAngle, skyColor, solid, source, spacing, spatialize, ' +
    'specularColor, speed, spine, startTime, stopTime, string, style, texCoord, ' +
    'texCoordIndex, texture, textureTransform, time, top, topToBottom, ' +
    'topUrl, touchTime, trackPoint_changed, translation, ' +
    'translation_changed, transparency, type, url, value_changed, vector, ' +
    'visibilityLimit, visibilityRange, whichChoice, xDimension, xSpacing, ' +
    'zDimension, zSpacing';

  VrmlDefinitions: UnicodeString =
    'MFColor, MFFloat, MFInt32, MFNode, MFRotation, MFString, MFTime, ' +
    'MFVec2f, MFVec3f, SFBool, SFColor, SFFloat, SFImage, SFInt32, SFNode, ' +
    'SFRotation, SFString, SFTime, SFVec2f, SFVec3f';

  VrmlEvents: UnicodeString =
    'eventIn, eventOut, exposedField, field';

  VrmlGroupings: UnicodeString =
    'Anchor, Billboard, Collision, ESPDUTransform, Group, Inline, LOD, ' +
    'NurbsGroup, ReceiverPdu, SignalPdu, StaticGroup, Switch, Transform, ' +
    'Transform2D, TransmitterPdu';

  VrmlInterpolators: UnicodeString =
    'ColorInterpolator, CoordinateInterpolator, CoordinateInterpolator2D, ' +
    'GeoPositionInterpolator, NormalInterpolator, NurbsPositionInterpolator, ' +
    'OrientationInterpolator, PositionInterpolator, PositionInterpolator2D, ' +
    'ScalarInterpolator';

  VrmlLights: UnicodeString =
    'DirectionalLight, PointLight, SpotLight';

  VrmlNodes: UnicodeString =
    'Background, Color, Coordinate, CoordinateDeformer, Fog, FontStyle, ' +
    'Joint, NavigationInfo, Normal, Script, Site, Sound';

  VrmlParameters: UnicodeString =
    'ALL, AUTO, BINDINGS, BOLD, BOTTOM, CENTER, CLAMP, CLOCKWISE, CONVEX, ' +
    'COUNTERCLOCKWISE, CULLING, DEFAULT, DEFAULTS, Displacer, ENUMS, FACE, FALSE, ' +
    'FAMILY, FILE, FORMAT, ITALIC, JUSTIFICATION, LEFT, NONE, NULL, OFF, ON, ' +
    'OVERALL, PARTS, PER_FACE, PER_FACE_INDEXED, PER_PART, PER_PART_INDEXED, ' +
    'PER_VERTEX, PER_VERTEX_INDEXED, REPEAT, RIGHT, SHAPE, SIDES, SOLID, ' +
    'STYLE, TRUE, TYPE, UNKNOWN_FACE_TYPE, UNKNOWN_ORDERING, ' +
    'UNKNOWN_SHAPE_TYPE, WRAP';

  VrmlProtos: UnicodeString =
    'DEF, EXTERNPROTO, IS, PROTO, ROUTE, Scene, TO, USE, VRML, X3D, ' +
    'X3DAppearanceNode, X3DAppearanceChildNode, X3DBackgroundNode, X3DBindableNode, ' +
    'X3DBoundedObject, X3DChildNode, X3DColorNode, X3DComposedGeometryNode, ' +
    'X3DCoordinateNode, X3DDragSensorNode, X3DEnvironmentalSensorNode, ' +
    'X3DFontStyleNode, X3DGeometry2DNode, X3DGeometry3DNode, ' +
    'X3DGeometryNode, X3DGeometryPropertyNode, X3DGroupingNode, ' +
    'X3DInterpolatorNode, X3DKeyDeviceSensorNode, X3DLightNode, ' +
    'X3DMaterialNode, X3DNetworkSensorNode, X3DNode, X3DNormalNode, ' +
    'X3DParametricGeometryNode, X3DPointingDeviceSensorNode, ' +
    'X3DPrototypeInstance, X3DScriptNode, X3DSensorNode, X3DSequencerNode, ' +
    'X3DShapeNode, X3DSoundNode, X3DSoundSourceNode, X3DTexture2DNode, ' +
    'X3DTextureCoordinateNode, X3DTextureNode, X3DTextureTransform2DNode, ' +
    'X3DTextureTransformNode, X3DTimeDependentNode, X3DTouchSensorNode, ' +
    'X3DTriggerNode, X3DUrlObject';

  VrmlSensors: UnicodeString =
    'BooleanFilter, BooleanSequencer, BooleanToggle, BooleanTrigger, ' +
    'CylinderSensor, GeoTouchSensor, IntegerTrigger, KeySensor, LoadSensor, ' +
    'PlaneSensor, ProximitySensor, SphereSensor, StringSensor, TimeSensor, ' +
    'TouchSensor, VisibilitySensor';

  VrmlShapes: UnicodeString =
    'Arc2D, ArcClose2D, Box, Circle2D, Cone, Contour2D, ContourPolyline2D, ' +
    'Cylinder, Disk2D, ElevationGrid, Humanoid, NurbsCurve, NurbsCurve2D, ' +
    'NurbsSurface, PointSet, Polyline2D, Polypoint2D, Rectangle2D, Segment, ' +
    'Shape, Shape2D, Sphere, Text, TriangleFanSet, TriangleSet, TriangleSet2D, ' +
    'TriangleStripSet, TrimmedSurface';

  VrmlShape_Hints: UnicodeString =
    'Extrusion, IndexedFaceSet, IndexedLineSet';

  VrmlTime_dependents: UnicodeString =
    'AudioClip, IntegerSequencer, MovieTexture, TimeTrigger';

  VrmlViewpoints: UnicodeString =
    'GeoViewpoint, Viewpoint';

  VrmlWorldInfos: UnicodeString =
    'WorldInfo';


procedure TSynEdit32HighlighterVrml97.DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynEdit32HashEntry.Create(AKeyword, AKind);
end;

function TSynEdit32HighlighterVrml97.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 27 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 54 + Ord(Str^) - Ord('0');
      '_': Result := 53;
      else Result := 0;
    end
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
{$IFOPT Q-}
    Result := 7 * Result + GetOrd;
{$ELSE}
    Result := (7 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
  end;
  Result := Result and $FF; // 255
  FStringLen := Str - FToIdent;
end;

function TSynEdit32HighlighterVrml97.IdentKind(MayBe: PWideChar): TtkTokenKind;
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
        Exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSynEdit32HighlighterVrml97.Create(AOwner :TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FKeywords := TSynEdit32HashEntryList.Create;
  FIsDoctype := False;
  FCommentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  FCommentAttri.Background := clGray;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FIdentifierAttri.Style := [];
  FIdentifierAttri.Foreground := clNavy;
  FIdentifierAttri.Background := clWhite;
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clRed;
  FKeyAttri.Background := clWhite;
  AddAttribute(FKeyAttri);

  FNonReservedKeyAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  FNonReservedKeyAttri.Style := [fsItalic];
  FNonReservedKeyAttri.Foreground := clBlack;
  FNonReservedKeyAttri.Background := clWhite;
  AddAttribute(FNonReservedKeyAttri);

  FEventAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrEvent, SYNS_FriendlyAttrEvent);
  FEventAttri.Style := [fsItalic];
  FEventAttri.Foreground := clNavy;
  FEventAttri.Background := clWhite;
  AddAttribute(FEventAttri);

  FNumberAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FEventAttri.Style := [fsItalic];
  FEventAttri.Foreground := clNavy;
  FEventAttri.Background := clWhite;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  FSpaceAttri.Style := [fsItalic];
  FSpaceAttri.Foreground := clNavy;
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Style := [fsItalic];
  FStringAttri.Foreground := clNavy;
  FStringAttri.Background := clWhite;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsItalic];
  FSymbolAttri.Foreground := clNavy;
  FSymbolAttri.Background := clWhite;
  AddAttribute(FSymbolAttri);
  //-- vrml
  FVrmlAppearanceAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlAppearance, SYNS_FriendlyAttrVrmlAppearance);
  FVrmlAppearanceAttri.Style := [fsItalic];
  FVrmlAppearanceAttri.Foreground := clNavy;
  FVrmlAppearanceAttri.Background := clWhite;
  AddAttribute(FVrmlAppearanceAttri);

  FVrmlAttributeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlAttribute, SYNS_FriendlyAttrVrmlAttribute);
  FVrmlAttributeAttri.Style := [fsItalic];
  FVrmlAttributeAttri.Foreground := clNavy;
  FVrmlAttributeAttri.Background := clGray;
  AddAttribute(FVrmlAttributeAttri);

  FVrmlDefinitionAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlDefinition, SYNS_FriendlyAttrVrmlDefinition);
  FVrmlDefinitionAttri.Style := [fsItalic];
  FVrmlDefinitionAttri.Foreground := clNavy;
  FVrmlDefinitionAttri.Background := clRed;
  AddAttribute(FVrmlDefinitionAttri);

  FVrmlEventAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlEvent, SYNS_FriendlyAttrVrmlEvent);
  FVrmlEventAttri.Style := [fsBold];
  FVrmlEventAttri.Foreground := clRed;
  FVrmlEventAttri.Background := clWhite;
  AddAttribute(FVrmlEventAttri);

  FVrmlGroupingAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlGrouping, SYNS_FriendlyAttrVrmlGrouping);
  FVrmlGroupingAttri.Style := [fsBold];
  FVrmlGroupingAttri.Foreground := clNavy;
  FVrmlGroupingAttri.Background := clWhite;
  AddAttribute(FVrmlGroupingAttri);

  FVrmlInterpolatorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlInterpolator, SYNS_FriendlyAttrVrmlInterpolator);
  FVrmlInterpolatorAttri.Style := [fsItalic];
  FVrmlInterpolatorAttri.Foreground := clLime;
  FVrmlInterpolatorAttri.Background := clWhite;
  AddAttribute(FVrmlInterpolatorAttri);

  FVrmlLightAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlLight, SYNS_FriendlyAttrVrmlLight);
  FVrmlLightAttri.Style := [fsItalic];
  FVrmlLightAttri.Foreground := clTeal;
  FVrmlLightAttri.Background := clWhite;
  AddAttribute(FVrmlLightAttri);

  FVrmlNodeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlNode, SYNS_FriendlyAttrVrmlNode);
  FVrmlNodeAttri.Style := [fsItalic, fsBold];
  FVrmlNodeAttri.Foreground := clGreen;
  FVrmlNodeAttri.Background := clWhite;
  AddAttribute(FVrmlNodeAttri);

  FVrmlParameterAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlParameter, SYNS_FriendlyAttrVrmlParameter);
  FVrmlParameterAttri.Style := [fsBold];
  FVrmlParameterAttri.Foreground := $F0CAA6; //clSkyBlue
  FVrmlParameterAttri.Background := clWhite;
  AddAttribute(FVrmlParameterAttri);

  FVrmlprotoAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlProto, SYNS_FriendlyAttrVrmlProto);
  FVrmlprotoAttri.Style := [fsBold];
  FVrmlprotoAttri.Foreground := clRed;
  FVrmlprotoAttri.Background := clWhite;
  AddAttribute(FVrmlprotoAttri);

  FVrmlSensorAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlSensor, SYNS_FriendlyAttrVrmlSensor);
  FVrmlSensorAttri.Style := [fsBold];
  FVrmlSensorAttri.Foreground := clOlive;
  FVrmlSensorAttri.Background := clWhite;
  AddAttribute(FVrmlSensorAttri);

  FVrmlShapeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlShape, SYNS_FriendlyAttrVrmlShape);
  FVrmlShapeAttri.Style := [fsBold];
  FVrmlShapeAttri.Foreground := clPurple;
  FVrmlShapeAttri.Background := clWhite;
  AddAttribute(FVrmlShapeAttri);

  FVrmlShape_HintAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlShape_Hint, SYNS_FriendlyAttrVrmlShape_Hint);
  FVrmlShape_HintAttri.Style := [fsItalic];
  FVrmlShape_HintAttri.Foreground := clPurple;
  FVrmlShape_HintAttri.Background := clWhite;
  AddAttribute(FVrmlShape_HintAttri);

  FVrmlTime_dependentAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlTime_dependent, SYNS_FriendlyAttrVrmlTime_dependent);
  FVrmlTime_dependentAttri.Style := [fsItalic];
  FVrmlTime_dependentAttri.Foreground := clOlive;
  FVrmlTime_dependentAttri.Background := clWhite;
  AddAttribute(FVrmlTime_dependentAttri);

  FVrmlViewpointAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlViewpoint, SYNS_FriendlyAttrVrmlViewpoint);
  FVrmlViewpointAttri.Style := [fsItalic];
  FVrmlViewpointAttri.Foreground := clGreen;
  FVrmlViewpointAttri.Background := clWhite;
  AddAttribute(FVrmlViewpointAttri);

  FVrmlWorldInfoAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrVrmlWorldInfo, SYNS_FriendlyAttrVrmlWorldInfo);
  FVrmlWorldInfoAttri.Style := [fsItalic];
  FVrmlWorldInfoAttri.Foreground := clMaroon;
  FVrmlWorldInfoAttri.Background := clWhite;
  AddAttribute(FVrmlWorldInfoAttri);

  FX3DDocTypeAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrX3DDocType, SYNS_FriendlyAttrX3DDocType);
  FX3DDocTypeAttri.Style := [fsItalic];
  FX3DDocTypeAttri.Foreground := clMaroon;
  FX3DDocTypeAttri.Background := clWhite;
  AddAttribute(FX3DDocTypeAttri);

  FX3DHeaderAttri := TSynEdit32HighlighterAttributes.Create(SYNS_AttrX3DHeader, SYNS_FriendlyAttrX3DHeader);
  FX3DHeaderAttri.Style := [fsItalic];
  FX3DHeaderAttri.Foreground := clMaroon;
  FX3DHeaderAttri.Background := clWhite;
  AddAttribute(FX3DHeaderAttri);
  SetAttributesOnChange(DefHighlightChange);

  EnumerateKeywords(Ord(tkEvent), Events, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), KeyWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkNonReservedKey), NonReservedKeys, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlAppearance), VrmlAppearances, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlAttribute), VrmlAttributes, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlDefinition), VrmlDefinitions, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlEvent), VrmlEvents, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlGrouping), VrmlGroupings, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlInterpolator), VrmlInterpolators, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlLight), VrmlLights, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlNode), VrmlNodes, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlParameter), VrmlParameters, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlproto), VrmlProtos, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlSensor), VrmlSensors, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlShape), VrmlShapes, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlShape_Hint), VrmlShape_Hints, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlTime_dependent), VrmlTime_dependents, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlViewpoint), VrmlViewpoints, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlWorldInfo), VrmlWorldInfos, IsIdentChar, DoAddKeyword);

  fDefaultFilter := SYNS_FilterVrml97;
  FRange := rsNormalText;
end;

destructor TSynEdit32HighlighterVrml97.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynEdit32HighlighterVrml97.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '&']) then Inc(FRun);
end;

function TSynEdit32HighlighterVrml97.NextTokenIs(T: UnicodeString): Boolean;
var
  I, Len: Integer;
begin
  Result := True;
  Len := Length(T);
  for I := 1 to Len do
    if (FLine[FRun + I] <> T[I]) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TSynEdit32HighlighterVrml97.InCommentProc;
begin
  if (FLine[FRun + 1] = '-') and (FLine[FRun + 2] = '-') then
  begin
    Inc(FRun);
    FTokenID := tkComment;
    FRange := rsComment;
    Inc(FRun, 2);
    repeat
      Inc(FRun);
      if (FLine[FRun] = '-') and (FLine[FRun + 1] = '-') then
        begin
          FRange := rsNormalText;
          Inc(FRun, 2);
          break;
        end;
    until IsLineEnd(FRun);
    Exit;
  end;
end;

procedure TSynEdit32HighlighterVrml97.DiesisCommentProc;
begin
  if FLine[FRun] = #0 then
    NullProc
  else
  begin
    FTokenID := tkComment;
    repeat
      Inc(FRun);
    until IsLineEnd(FRun);
  end;
end;

procedure TSynEdit32HighlighterVrml97.X3DHeaderOpenProc;
begin
  Inc(FRun);
  FRange := rsX3DHeader;
  X3DHeaderProc;
  FTokenID := tkX3DHeader;
end;

procedure TSynEdit32HighlighterVrml97.X3DHeaderProc;
begin
  case FLine[FRun] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
    begin
      FTokenID := tkX3DHeader;
      repeat
        if (FLine[FRun] = '?') then
        begin
          Inc(FRun, 1);
          FRange := rsNormalText;
          Break;
        end;
        if not IsLineEnd(FRun) then
          Inc(FRun);
      until IsLineEnd(FRun);
    end;
  end;
end;

procedure TSynEdit32HighlighterVrml97.X3DDocTypeOpenProc;
begin
  if NextTokenIs('DOCTYPE') then
  begin
    FRange := rsX3DDocType;
    X3DDocTypeProc;
    FTokenID := tkX3DDocType;
  end
  else
    if NextTokenIs('--') then
    begin
      FRange := rsComment;
      InCommentProc;
      FTokenID := tkComment;
    end
    else
    begin
      FTokenID := tkSymbol;
      Inc(FRun);
    end;
end;

procedure TSynEdit32HighlighterVrml97.X3DDocTypeProc;
begin
  case FLine[FRun] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
      begin
        FTokenID := tkX3DDocType;
        repeat
          if (FLine[FRun + 1] = '>') then
          begin
            Inc(FRun, 1);
            FRange := rsNormalText;
            Break;
          end;
          if not IsLineEnd(FRun) then
            Inc(FRun);
        until IsLineEnd(FRun);
      end;
  end;
end;

procedure TSynEdit32HighlighterVrml97.CommentProc;
begin
  if FLine[FRun] = #0 then
    NullProc
  else
    begin
      FTokenID := tkComment;
      repeat
        if ((FLine[FRun] = '*') and (FLine[FRun + 1] = '/'))
          or
          ((FLine[FRun] = '-') and (FLine[FRun + 1] = '-')) then
          begin
            FRange := rsNormalText;
            Inc(FRun, 2);
            break;
          end;
        Inc(FRun);
      until IsLineEnd(FRun);
    end;
end;

procedure TSynEdit32HighlighterVrml97.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.IdentProc;
begin
  FTokenID := IdentKind(FLine + FRun);
  Inc(FRun, FStringLen);
  while IsIdentChar(FLine[FRun]) do
    Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.LFProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.MinusProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '-', '>']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.ModSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.NullProc;
begin
  FTokenID := tkNull;
  Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[FRun] of
      '0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X':
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
      '.' :
        if FLine[Succ(FRun)] = '.' then
          Break;
      'a'..'f', 'A'..'F' :
        if not isHex then
          Break;
      'x', 'X' :
        begin
          if (FLine[idx1] <> '0') or (FRun > Succ(idx1)) then
            Break;
          if not CharInSet(FLine[Succ(FRun)], ['0'..'9', 'a'..'f', 'A'..'F']) then
          begin
            Break;
          end;
          isHex := True;
        end;
    end;
    Inc(FRun);
  end;
end;

procedure TSynEdit32HighlighterVrml97.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '|']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if CharInSet(FLine[FRun], ['=', '+']) then Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] = '.') and (FLine[FRun + 1] = '.') then Inc(FRun, 2);
end;

procedure TSynEdit32HighlighterVrml97.SlashProc;
begin
  Inc(FRun);
  case FLine[FRun] of
    '/' :
      begin
        FTokenID := tkComment;
        repeat
          Inc(FRun);
        until IsLineEnd(FRun);
      end;
    '*' :
      begin
        FTokenID := tkComment;
        FRange := rsComment;
        repeat
          Inc(FRun);
          if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
            begin
              FRange := rsNormalText;
              Inc(FRun, 2);
              break;
            end;
        until IsLineEnd(FRun);
      end;
    '=' :
      begin
        Inc(FRun);
        FTokenID := tkSymbol;
      end;
    else
      FTokenID := tkSymbol;
  end;
end;

procedure TSynEdit32HighlighterVrml97.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while (FLine[FRun] <= #32) and not IsLineEnd(FRun) do Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.StarProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] = '=' then Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.StringProc;
var
  l_strChar: UnicodeString;
begin
  FTokenID := tkString;
  l_strChar := FLine[FRun]; // We could have '"' or #39
  if (FLine[FRun + 1] = l_strChar) and (FLine[FRun + 2] = l_strChar) then Inc(FRun, 2);
  repeat
    if IsLineEnd(FRun) then break;
    Inc(FRun);
  until (FLine[FRun] = l_strChar) and (FLine[Pred(FRun)] <> '\');
  if not IsLineEnd(FRun) then
    Inc(FRun);
end;

procedure TSynEdit32HighlighterVrml97.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TSynEdit32HighlighterVrml97.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TSynEdit32HighlighterVrml97.Next;
begin
  fTokenPos := FRun;
  case FRange of
    rsX3DHeader: X3DHeaderProc;
    rsX3DDocType: X3DDocTypeProc;
    rsComment: CommentProc;
    else
      case FLine[FRun] of
        '&': AndSymbolProc;
        #13: CRProc;
        '#': DiesisCommentProc;
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
        '?': X3DHeaderOpenProc;
        '!': X3DDocTypeOpenProc;
        '~', '{', '}', ',', '(', ')', '[', ']', ':', ';', '=', '<', '>': SymbolProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynEdit32HighlighterVrml97.GetDefaultAttribute(Index :integer) :TSynEdit32HighlighterAttributes;
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

function TSynEdit32HighlighterVrml97.GetEol :Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynEdit32HighlighterVrml97.GetRange :Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEdit32HighlighterVrml97.GetTokenID :TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEdit32HighlighterVrml97.GetTokenAttribute :TSynEdit32HighlighterAttributes;
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
    // vrml
    tkVrmlAppearance: Result := FVrmlAppearanceAttri;
    tkVrmlAttribute: Result := FVrmlAttributeAttri;
    tkVrmlDefinition: Result := FVrmlDefinitionAttri;
    tkVrmlEvent: Result := FVrmlEventAttri;
    tkVrmlGrouping: Result := FVrmlGroupingAttri;
    tkVrmlInterpolator: Result := FVrmlInterpolatorAttri;
    tkVrmlLight: Result := FVrmlLightAttri;
    tkVrmlNode: Result := FVrmlNodeAttri;
    tkVrmlParameter: Result := FVrmlParameterAttri;
    tkVrmlproto: Result := FVrmlprotoAttri;
    tkVrmlSensor: Result := FVrmlSensorAttri;
    tkVrmlShape: Result := FVrmlShapeAttri;
    tkVrmlShape_Hint: Result := FVrmlShape_HintAttri;
    tkVrmlTime_dependent: Result := FVrmlTime_dependentAttri;
    tkVrmlViewpoint: Result := FVrmlViewpointAttri;
    tkVrmlWorldInfo: Result := FVrmlWorldInfoAttri;
    tkX3DDocType: Result := FX3DDocTypeAttri;
    tkX3DHeader: Result := FX3DHeaderAttri;
    //--
    else
      Result := nil;
  end;
end;

function TSynEdit32HighlighterVrml97.GetTokenKind :integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynEdit32HighlighterVrml97.ResetRange;
begin
  FRange := rsNormalText;
end;

procedure TSynEdit32HighlighterVrml97.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEdit32HighlighterVrml97.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterVrml97;
end;

class function TSynEdit32HighlighterVrml97.GetLanguageName: string;
begin
  Result := SYNS_LangVrml97;
end;

function TSynEdit32HighlighterVrml97.GetSampleSource: UnicodeString;
begin
  Result :=
    '#VRML V2.0 utf8'#13#10 +
    'Transform {'#13#10 +
    '  children ['#13#10 +
    '    NavigationInfo { headlight FALSE } # We''ll add our own light'#13#10 +
    ''#13#10 +
    '    DirectionalLight {        # First child'#13#10 +
    '        direction 0 0 -1      # Light illuminating the scene'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    Transform {               # Second child - a red sphere'#13#10 +
    '      translation 3 0 1'#13#10 +
    '      children ['#13#10 +
    '        Shape {'#13#10 +
    '          geometry Sphere { radius 2.3 }'#13#10 +
    '          appearance Appearance {'#13#10 +
    '            material Material { diffuseColor 1 0 0 }   # Red'#13#10 +
    '         }'#13#10 +
    '        }'#13#10 +
    '      ]'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    Transform {               # Third child - a blue box '#13#10 +
    '      translation -2.4 .2 1'#13#10 +
    '      rotation     0 1 1  .9'#13#10 +
    '      children ['#13#10 +
    '        Shape {'#13#10 +
    '          geometry Box {}'#13#10 +
    '          appearance Appearance {'#13#10 +
    '            material Material { diffuseColor 0 0 1 }  # Blue'#13#10 +
    '         }'#13#10 +
    '        }'#13#10 +
    '      ]'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '  ] # end of children for world'#13#10 +
    '}'#13#10 +
    'DEF Example_2 Script {'#13#10 +
    '    field   SFNode myself USE Example_2'#13#10 +
    '    field   SFNode root USE ROOT_TRANSFORM'#13#10 +
    '    field   MFString url "foo.wrl"'#13#10 +
    '    eventIn MFNode   nodesLoaded'#13#10 +
    '    eventIn SFBool   trigger_event'#13#10 +
    ''#13#10 +
    '    url "javascript:'#13#10 +
    '        function trigger_event(value, ts){'#13#10 +
    '            // do something and then fetch values'#13#10 +
    '            Browser.createVRMLFromURL(url, myself, ''nodesLoaded'');'#13#10 +
    '        }'#13#10 +
    ''#13#10 +
    '        function nodesLoaded(value, timestamp){'#13#10 +
    '            if (value.length > 5) {'#13#10 +
    '                 // do something more than 5 nodes in this MFNode...'#13#10 +
    '            }'#13#10 +
    '            root.addChildren = value;'#13#10 +
    '        }"'#13#10 +
    '}';
end;

class function TSynEdit32HighlighterVrml97.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangVrml97;
end;

initialization
  RegisterPlaceableHighlighter(TSynEdit32HighlighterVrml97);
end.
