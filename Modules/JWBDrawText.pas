unit JWBDrawText;
{ Text painting routines. }
{
FONTS 101.

PIXELS, INCHES AND POINTS:
  Pixels  = pixels on the logical screen
  Inches    can contain different number of pixels, depending on the pixel density
  PXI/PPI = Pixels Per Inch, variable
  Points  = universal font size measurement
  PTI     = Points Per Inch = 72

"Logical units" or "logical height" usually means "logical" pixels. Device units
usually means inches or "physical pixels" (you rarely meet this) depending on
the context.

https://support.microsoft.com/en-us/help/74299/info-calculating-the-logical-height-and-point-size-of-a-font


FONT METRICS AND LAYOUT:
Different fonts align their characters differently and leave different spacing,
even when they're the same size.

Characters from different fonts are aligned together on a single line by their
baselines. This is how text is normally drawn (Word, browsers).

Line height        = Ascent + Descent
Top                = Baseline + Ascent
Bottom             = Baseline - Descent
Cell height        = Line height
Character height   = Line height - Internal leading
Internal leading   = Space at the top of the character for diacritics etc.
                     Included in the Ascent.
External leading   = Apps can leave additional space between lines. User-defined.

https://msdn.microsoft.com/en-us/library/windows/desktop/dd145122(v=vs.85).aspx


LINE HEIGHT AND CHARACTER SIZE
Fonts with the same height can have different character size:
  Line/cell height  Full height of the line
  Character height  Line Height - Internal leading (variable)
Some fonts have no internal leading, making these equal.

CreateFont accepts the font height in pixels in two ways:
  +Height   set the line height        (use to display a line of text)
  -Height   set the character height   (use to fit a character to given rectangle)

TFont provides two properties:
  Height    directly translate to CreateFont's Height
  Size      is a size in font-points, correctly converted to height


FONT SUBSTITUTION
When a font doesn't have glyphs for some characters, Windows automatically uses
a different one which has these.
This only works with some of the system drawing functions.


HALFWIDTH AND FULLWIDTH FORMS
Classic CJK fonts paint latin chars in exactly half-width of CJK chars. Modern
draw them normally (variable width).
Unicode has additional full-width latin which is always equal to CJK char. Most
fonts, classic and modern, implement these.
https://en.wikipedia.org/wiki/Halfwidth_and_fullwidth_forms


CJK FONT FAMILIES AND METRICS
There are broadly two classes of CJK fonts on Windows:

I. Classic fonts (MS Gothic/MS Mincho/SimSun/MingLiU):
 - No internal leading (height == size)
 - Some hardcoded internal leading so that characters are positioned in the
   center of the cell
 - Baseline is in the same place
 - Latin chars are half-width

II. Modern fonts (Yu Gothic/Yu Mincho/Meiryo and others)
 - Wildly varied internal leading
 - Wildly varied character positions (some fonts can go 1/3 over the top of the cell)
   No hardcoded internal leading.
 - Baseline is roughly in the same place, but different fonts place
   the characters slightly differently.
 - Latin chars are arbitrary width
}

interface
uses Graphics, Windows, JWBStrings;

{
Currently selected fonts for the application
}

var
  FontStrokeOrder,
  FontChinese,
  FontChineseGB,
  FontChineseGrid,
  FontChineseGridGB,
  FontJapaneseGrid,
  FontJapanese,
  FontSmall,
  FontRadical,
  FontEnglish,
  FontPinYin:string;


{
Wakan employs 2 methods of painting text

1. Normal/Line painting - paint lines of arbitrary text composed from characters
  potentially in different languages.
  This is how text is normally drawn.
}
procedure DrawUnicodeText(c:TCanvas; x,y,fh:integer; const ch:FString; const fontface:string);

{
2. CJK/Char painting - paint single CJK characters or strings of CJK characters.
  CJK chars are always square cells so Wakan ignores the internal leading and
  tries to fill all the available space.
}
procedure DrawCJK(c:TCanvas; x,y,fh:integer; const ch:FString; const fontface:string);
procedure DrawCJKChar(c:TCanvas; rect:TRect; fh:integer; const ch:FString; const fontface:string);


{
Draw registry
To support text selection, Wakan keeps a list of all text lines it has drawn
with the help of DrawCJK.
Each time a control is redrawn, all cells related to it are cleared.
}
type
  TFindDrawRegFlag = (
    ffHalfCharRounding  //if more than half char is covered, return next char --
      //like it's usually done with text selection.
  );
  TFindDrawRegFlags = set of TFindDrawRegFlag;

procedure BeginDrawReg(p:TCanvas);
procedure EndDrawReg;
function FindDrawReg(p:TCanvas;x,y:integer;flags:TFindDrawRegFlags;
  out id,cx,cy:integer; out fontface: string;out fs:integer):FString;
procedure AddDrawReg(p:TCanvas;const fontface: string;fs:integer;const rect:TRect;
  const s:FString);
function GetDrawReg(id:integer; out cx,cy: integer; out fontface:string;
  out fs:integer):FString;
function CalcStrWidth(c:TCanvas;const fontface:string;fs:integer;
  const w:UnicodeString): integer; forward;
function GetCoveredCharNo(c:TCanvas;const fontface:string;fs:integer;
  const ch:FString;x:integer;halfCharRounding:boolean): integer; forward;



implementation
uses SysUtils;

var
  curpbox:TCanvas;

{
We sometimes want to draw CJK characters precisely in the middle of the box,
filling it. Unfortunately, different fonts have different measurements,

The best shot at this is by aligning characters by their baseline, since this is
how multiple fonts are aligned together normally.

Unfortunately, different fonts place characters a bit differently. The offset is
almost guaranteed to be acceptable anyway, but for most likely fonts we want
to offer a perfect experience.

This is a table of adjustments to baselines for some well-known fonts. We align
their characters to fit old-style fonts with the same baseline.

Notably, there are classical fonts which:
1. Ignore internal leading entirely (it is set to 0)
2. Have hardcoded internal leading by placing the character precisely in the center
  of the character cell and leaving some space to the sides.
3. All have the baseline in the same place.
These fonts are therefore entirely interchangeable and don't need any treatment.
We use them as a reference when adjusting other fonts.

These fonts are any variations of: MS Gothic, MS Mincho, SimSun and MingLiU,
with the exception that MingLiU fonts have broken baseline (no #3).

Many modern fonts are also similar in that they all put characters roughly
2% higher above the baseline than the classical fonts.
}
type
  TFontAdjustment = record
    n: string;
    bl: integer; //adjustment to baseline, in %
  end;

var
  FontAdjustments: array[0..34] of TFontAdjustment = (
  {
   //These classic reference fonts need no adjustments:

   //Gothic family
   (n:'MS Gothic'; bl: +0),
   (n:'MS PGothic'; bl: +0),
   (n:'MS UI Gothic'; bl: +0),
   (n:'ＭＳ ゴシック'; bl: +0),
   (n:'ＭＳ Pゴシック'; bl: +0),
   (n:'ＭＳ UI ゴシック'; bl: +0),

   //Mincho family
   (n:'MS Mincho'; bl: +0),
   (n:'MS PMincho'; bl: +0),
   (n:'ＭＳ 明朝'; bl: +0),
   (n:'ＭＳ P明朝'; bl: +0),

   //SimSun family looks like it _might_ benefit from an adjustment, but not really
   (n:'SimSun'; bl: +0),
   (n:'SimSun-ExtB'; bl: +0),
   (n:'NSimSun'; bl: +0),
  }

   //MingLiU is a classic font family too, but it's baseline is broken
   (n:'MingLiU'; bl: -5),
   (n:'MingLiU-ExtB'; bl: -5),
   (n:'MingLiU_HKSCS'; bl: -5),
   (n:'MingLiU_HKSCS-ExtB'; bl: -5),
   (n:'PMingLiU'; bl: -5),
   (n:'PMingLiU-ExtB'; bl: -5),
   (n:'PMingLiU_HKSCS'; bl: -5),
   (n:'PMingLiU_HKSCS-ExtB'; bl: -5),
   (n:'細明體'; bl: -5),
   (n:'細明體-ExtB'; bl: -5),
   (n:'細明體_HKSCS'; bl: -5),
   (n:'細明體_HKSCS-ExtB'; bl: -5),
   (n:'P細明體'; bl: -5),
   (n:'P細明體-ExtB'; bl: -5),
   (n:'P細明體_HKSCS'; bl: -5),
   (n:'P細明體_HKSCS-ExtB'; bl: -5),

   //Arial Unicode MS: +2
   (n:'Arial Unicode MS'; bl: +2),

   //Meiryo font family: +3
   (n:'Meiryo'; bl: +3),
   (n:'メイリオ'; bl: +3),
   (n:'Meiryo UI'; bl: +3),
   (n:'メイリオ UI'; bl: +3),

   //All Yu Gothic/Yu Mincho fonts are positioned slightly higher than classic
   (n:'Yu Gothic'; bl: +2),
   (n:'Yu Gothic Light'; bl: +2),
   (n:'Yu Gothic Medium'; bl: +2),
   (n:'Yu Gothic UI'; bl: +2),
   (n:'Yu Gothic UI Light'; bl: +2),
   (n:'Yu Gothic UI Semibold'; bl: +2),
   (n:'Yu Gothic UI Semilight'; bl: +2),
   (n:'游ゴシック'; bl: +2),
   (n:'Yu Mincho'; bl: +2),
   (n:'Yu Mincho Demibold'; bl: +2),
   (n:'Yu Mincho Light'; bl: +2),
   (n:'游明朝'; bl: +2),

   //Microsoft JhengHei: +3
   (n:'Microsoft JhengHei'; bl: +3),

   //Microsoft YaHei:
   //It's hard to tell, but let's leave at 0 for now

   (n:''; bl: +0)
  );


function GetFontLeadingFix(c: TCanvas; const fontface: string): integer;
var tm: TTextMetric;
  bl: integer;
  i: integer;
  fh: integer;
begin
  GetTextMetrics(c.Handle, tm); //we can cache these if they turn out to be slow
  fh := c.Font.Height;
  if fh < 0 then fh := -fh;

  bl := 86;

  for i := Low(FontAdjustments) to High(FontAdjustments) do
    if SysUtils.SameStr(FontAdjustments[i].n, fontface) then begin
      bl := bl + FontAdjustments[i].bl;
      break;
    end;

  Result := - Trunc(bl*fh/100) + tm.tmAscent;
end;


{
Draws a single continuous line of a random CJK and non-CJK mixed text in the most
normal and compatible way.
Use this when you simply want to paint text of unknown origin (clipboard content and so on).

Does not add this to the drawing registry (it doesn't support non-CJK selection anyway).

fh: Font height in pixels. This includes internal leading, making characters
  smaller but more in line with western characters which need leading.
}
procedure DrawUnicodeText(c:TCanvas; x,y,fh:integer; const ch:FString; const fontface:string);
var w:UnicodeString;
  r: TRect;
begin
  if ch='' then exit;
  SetBkMode(c.Handle,TRANSPARENT);
  c.Font.Name:=fontface;
  c.Font.Height:=fh; //no "-" => fit by cell height
  w := fstrtouni(ch);
  r.Left := x;
  r.Top := y;
  r.Right := x;
  r.Bottom := y;
  DrawText(c.Handle,PWideChar(w),length(w),r,DT_LEFT or DT_TOP or DT_NOCLIP);
end;




{
Draws a single continuous line of CJK unicode characters and adds it to the drawing registry.
This is tailored for CJK only.

x, y: Where to draw.
fh: Char height in pixels
ch: Text
}
procedure DrawCJK(c:TCanvas; x,y,fh:integer; const ch:FString; const fontface:string);
var w:UnicodeString;
  r: TRect;
  il: integer;
begin
  if ch='' then exit;
  SetBkMode(c.Handle,TRANSPARENT);
  c.Font.Name:=fontface;
  c.Font.Height:=-fh; //"-" => fit by char height
  w := fstrtouni(ch);
  il := GetFontLeadingFix(c, fontface);
  r.Left := x;
  r.Top := y - il;
  r.Right := x;
  r.Bottom := y - il;
 //We need the rect for AddDrawReg and CALCRECT does not draw
  DrawText(c.Handle,PWideChar(w),length(w),r,DT_LEFT or DT_TOP or DT_CALCRECT);
  DrawText(c.Handle,PWideChar(w),length(w),r,DT_LEFT or DT_TOP or DT_NOCLIP);
  r.Top := r.Top + il; //exclude il
  if curpbox<>nil then
    AddDrawReg(curpbox,fontface,fh,r,ch);
end;

{
Draws ONE character in a given rectangle, centered or left-aligned.
If the given character cannot be drawn with the given font, draws its character code instead.

fh: Char height in pixels.
}
procedure DrawCJKChar(c:TCanvas; rect:TRect; fh:integer; const ch:FString;
  const fontface:string);
var w: UnicodeString;
  w_ind: word;
  ws: string;
  il: integer;
begin
  if ch='' then exit;
  SetBkMode(c.Handle,TRANSPARENT);
 { Some glyphs may be outright impossible to draw -- no suitable fonts, even with substitution }
  w := fstrtouni(ch);
  c.Font.Name:=fontface;
  c.Font.Height:=-fh;
  il := GetFontLeadingFix(c, fontface);
  if GetGlyphIndices(c.Handle,PChar(w),1,@w_ind, GGI_MARK_NONEXISTING_GLYPHS)=GDI_ERROR then
    RaiseLastOsError();
  if w_ind<>$FFFF then begin
    rect.Top := rect.Top - il;
    DrawText(c.Handle,PWideChar(w),length(w),rect, DT_CENTER or DT_SINGLELINE or DT_NOCLIP);
  end else begin
   //Draw unicode index instead
    ws := IntToHex(Utf16ToUnicodeIndex(fgetch(ch,1)),4);
    c.Font.Name:=FontEnglish;
    c.Font.Height:=Trunc(fh*0.44);
    DrawText(c.Handle,PChar(ws),Length(ws),rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;




{
Draw registry.
}

type
  TIntTextInfo=record
    act:boolean;
    p:TCanvas;
    fontface:string;
    fs:integer;
    rect:TRect;
    s:FString;
  end;

const
  MAX_INTTEXTINFO = 4000;
var
  itt:array[1..MAX_INTTEXTINFO] of TIntTextInfo;

procedure BeginDrawReg(p:TCanvas);
var i:integer;
begin
  for i:=1 to MAX_INTTEXTINFO do if itt[i].p=p then itt[i].act:=false;
  curpbox:=p;
end;

procedure EndDrawReg;
begin
  curpbox:=nil;
end;

{ Retrieves a subblock of a block of text drawn with DrawUnicode.
p,x,y: position to probe.
Result: substring of a block of text under mouse starting at mouse position, inclusive.
id: index of a draw call, if these are equal, it's the same line
cx,cy: position of a first character of a returned subblock on a control.
fs: font size (character height) for this string.
flags: see TFindDrawRegFlags description }
function FindDrawReg(p:TCanvas;x,y:integer;flags:TFindDrawRegFlags;
  out id,cx,cy:integer; out fontface: string;out fs:integer):FString;
var i,j:integer;
begin
  id:=-1;
  result:='';
  for i:=1 to MAX_INTTEXTINFO do
    if (itt[i].p=p)
    and (y>=itt[i].rect.Top) and (y<=itt[i].rect.Top+itt[i].fs)
    and (x>=itt[i].rect.Left) and (x<=itt[i].rect.Right) then begin
      id:=i;
      cy:=itt[i].rect.Top;
      fs:=itt[i].fs;
      fontface:=itt[i].fontface;

      j:=GetCoveredCharNo(p,fontface,fs,itt[i].s,x-itt[i].rect.Left,
        {halfCharRounding=}ffHalfCharRounding in flags);
      if not (ffHalfCharRounding in flags) then
        Dec(j); //without hcr we receive the character UNDER mouse, want BEFORE it
      if j<0 then continue; //should not happen
      if j>flength(itt[i].s) then continue;
      cx:=itt[i].rect.Left+CalcStrWidth(p,fontface,fs,copy(itt[i].s,1,j));
      result:=fcopy(itt[i].s,j+1,flength(itt[i].s)-j);
      break;
    end;
end;

function GetDrawReg(id:integer; out cx,cy: integer; out fontface:string; out fs:integer):FString;
begin
  cx:=itt[id].rect.Left;
  cy:=itt[id].rect.Top;
  fs:=itt[id].fs;
  fontface:=itt[id].fontface;
  Result:=itt[id].s;
end;

procedure AddDrawReg(p:TCanvas;const fontface: string;fs:integer;const rect:TRect;const s:FString);
var i: integer;
begin
  for i:=1 to MAX_INTTEXTINFO do
    if not itt[i].act then begin
      itt[i].act:=true;
      itt[i].p:=p;
      itt[i].fontface:=fontface;
      itt[i].fs:=fs;
      itt[i].rect:=rect;
      itt[i].s:=s;
      break;
    end;
end;

{ Returns the width of a string if drawn like specified.
 Hint: pass a copy(str,start,end) to measure the width of only some characters. }
function CalcStrWidth(c:TCanvas;const fontface:string;fs:integer;const w:UnicodeString): integer;
var r:TRect;
begin
  if w='' then begin
    Result := 0;
    exit;
  end;
  c.Font.Name:=fontface;
  c.Font.Height:=-fs;
  r.Left := 0;
  r.Top := 0;
  r.Right := 0;
  r.Bottom := 0;
  if DrawText(c.Handle,PWideChar(w),length(w),r,DT_LEFT or DT_TOP or DT_CALCRECT or DT_NOCLIP)=0 then
    Result := 0
  else
    Result := r.Right-r.Left;
end;

{ Returns the number of characters in the string which are selection-covered
 when the mouse is in X pixels from the left edge of the text.
 A less-than-half-covered character is considered not covered.
  -1: mouse is over the left edge
  n+1: mouse is over the right edge }
function GetCoveredCharNo(c:TCanvas;const fontface:string;fs:integer;
  const ch:FString;x:integer;halfCharRounding:boolean): integer;
var w: UnicodeString;
  chwPrev, chwNext: integer;
begin
  if x<0 then begin
    Result := -1;
    exit;
  end;

  w := fstrtouni(ch);
  if Length(w)<=0 then begin
    Result := 1;
    exit;
  end;

 { Measure chunks of 1..i chars. We could have measured only one char each turn,
  but we'd be missing inter-character space. }
  Result := 0;
  chwPrev := 0;
  chwNext := CalcStrWidth(c,fontface,fs,w[1]); //get first char width
  while x>chwNext do begin
    Inc(Result);
    if Result>=Length(w) then begin
      Inc(Result);
      exit;
    end;
    chwPrev := chwNext;
    chwNext := CalcStrWidth(c,fontface,fs,copy(w,1,1+Result));
  end;

  if (x>chwPrev+(chwNext-chwPrev) div 2) or not halfCharRounding then
    Inc(Result);
end;

var
  i:integer;

initialization
  CurPBox:=nil;
  for i:=1 to MAX_INTTEXTINFO do itt[i].act:=false;

end.
