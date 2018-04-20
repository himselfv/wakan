unit FontsTest_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFontTestForm = class(TForm)
    PaintBox: TPaintBox;
    cbFonts: TComboBox;
    edtText: TEdit;
    lblFont: TLabel;
    lblText: TLabel;
    lblSize: TLabel;
    cbSize: TComboBox;
    mmFontInfo: TMemo;
    lblBoxSize: TLabel;
    cbBoxSize: TComboBox;
    Label1: TLabel;
    cbLayoutMode: TComboBox;
    cbDrawMetrics: TCheckBox;
    cbJISFontsOnly: TCheckBox;
    Label2: TLabel;
    cbShiftTop: TComboBox;
    cbDrawTicks: TCheckBox;
    mmPanose: TMemo;
    procedure FormShow(Sender: TObject);
    procedure cbFontsChange(Sender: TObject);
    procedure edtTextChange(Sender: TObject);
    procedure cbSizeChange(Sender: TObject);
    procedure cbBoxSizeChange(Sender: TObject);
    procedure cbCenteredClick(Sender: TObject);
    procedure cbJISFontsOnlyClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure cbDrawMetricsClick(Sender: TObject);
    procedure cbShiftTopChange(Sender: TObject);
  public
    procedure ReloadFonts;

  protected
    procedure RepaintChar(Canvas: TCanvas; CanvasRect: TRect); overload;
    procedure DrawMetrics(Canvas: TCanvas; DrawRect: TRect; Centered: boolean);

  protected
    procedure AddInfo(const ATitle: string; const AValue: string); overload;
    procedure AddInfo(const ATitle: string; const AValue: integer); overload;
    procedure ReloadFontInfo;
    procedure AddPanose(const ATitle: string; const AValue: integer); overload;
    procedure ReloadFontPanose;

  end;

var
  FontTestForm: TFontTestForm;

implementation
uses System.Types;

{$R *.dfm}

procedure TFontTestForm.FormShow(Sender: TObject);
begin
  ReloadFonts;
  Paintbox.Invalidate;
end;

procedure TFontTestForm.cbJISFontsOnlyClick(Sender: TObject);
begin
  ReloadFonts;
end;

function ListFonts_Callback(lpelfe: PLogFont; lpntme: PTextMetric; FontType: dword;
  param: LParam): integer; stdcall;
begin
  Result := 1; //continue
  if lpelfe.lfFaceName[0]='@' then
    exit; //sideways fonts
  TStringList(param).AddObject(lpelfe.lfFaceName, TObject(lpelfe.lfCharSet));
end;

function ListFonts(const face: string; charset: integer): TStringList;
var DC: HDC;
  LFont: TLogFont;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := true;
  DC := GetDC(0);
  try
    FillChar(LFont, SizeOf(LFont), 00);
    LFont.lfCharSet := charset;
    if face<>'' then
      strplcopy(@LFont.lfFaceName[0], face, Length(LFont.lfFaceName)-1); //also copies null
    EnumFontFamiliesEx(DC, LFont, @ListFonts_Callback, LParam(Result), DEFAULT_CHARSET);
   { DEFAULT_CHARSET in this context is a wildcard to receive all charsets for all fonts. }
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TFontTestForm.ReloadFonts;
var fonts: TStringList;
begin
  cbFonts.Items.Clear;
  if cbJISFontsOnly.Checked then
    fonts := ListFonts('', SHIFTJIS_CHARSET)
  else
    fonts := ListFonts('', DEFAULT_CHARSET);
  try
    cbFonts.Items.Assign(fonts);
  finally
    FreeAndNil(fonts);
  end;
end;

procedure TFontTestForm.cbFontsChange(Sender: TObject);
begin
  ReloadFontInfo;
  Paintbox.Invalidate;
end;

procedure TFontTestForm.cbSizeChange(Sender: TObject);
begin
  ReloadFontInfo;
  Paintbox.Invalidate;
end;

procedure TFontTestForm.ReloadFontInfo;
var fontSize: integer;
  tm: TTextMetric;
begin
  mmFontInfo.Lines.BeginUpdate;
  try
    mmFontInfo.Clear;
    if cbFonts.Text = '' then exit;
    if not TryStrToInt(cbSize.Text, fontSize) then
      exit;
    Paintbox.Canvas.Font.Name := cbFonts.Text;
    Paintbox.Canvas.Font.Size := fontSize;

    GetTextMetrics(Paintbox.Canvas.Handle, tm);
    AddInfo('Height', tm.tmHeight);
    AddInfo('Ascent', tm.tmAscent);
    AddInfo('Descent', tm.tmDescent);
    AddInfo('InternalLeading', tm.tmInternalLeading);
    AddInfo('ExternalLeading', tm.tmExternalLeading);
    AddInfo('AveCharWidth', tm.tmAveCharWidth);
    AddInfo('MaxCharWidth', tm.tmMaxCharWidth);
    AddInfo('Weight', tm.tmWeight);
    AddInfo('Overhang', tm.tmOverhang);
    AddInfo('DigitizedAspectX', tm.tmDigitizedAspectX);
    AddInfo('DigitizedAspectY', tm.tmDigitizedAspectY);
    AddInfo('FirstChar', tm.tmFirstChar);
    AddInfo('LastChar', tm.tmLastChar);
    AddInfo('DefaultChar', tm.tmDefaultChar);
    AddInfo('BreakChar', tm.tmBreakChar);
    AddInfo('Italic', tm.tmItalic);
    AddInfo('Underlined', tm.tmUnderlined);
    AddInfo('StruckOut', tm.tmStruckOut);
    AddInfo('PitchAndFamily', tm.tmPitchAndFamily);
    AddInfo('CharSet', tm.tmCharSet);
  finally
    mmFontInfo.Lines.EndUpdate;
  end;
  ReloadFontPanose;
end;

procedure TFontTestForm.AddInfo(const ATitle: string; const AValue: string);
begin
  mmFontInfo.Lines.Add(ATitle + ' = ' + AValue);
end;

procedure TFontTestForm.AddInfo(const ATitle: string; const AValue: integer);
begin
  AddInfo(ATitle, IntToStr(AValue));
end;

procedure TFontTestForm.ReloadFontPanose;
var fontSize: integer;
  tm: TOutlineTextMetric;
  tmsize: cardinal;
begin
  mmPanose.Lines.BeginUpdate;
  try
    mmPanose.Clear;
    if cbFonts.Text = '' then exit;
    if not TryStrToInt(cbSize.Text, fontSize) then
      exit;
    Paintbox.Canvas.Font.Name := cbFonts.Text;
    Paintbox.Canvas.Font.Size := fontSize;

    GetOutlineTextMetrics(Paintbox.Canvas.Handle, sizeof(tm), @tm);

    AddPanose('bFamilyType', tm.otmPanoseNumber.bFamilyType);
    AddPanose('bSerifStyle', tm.otmPanoseNumber.bSerifStyle);
    AddPanose('bWeight', tm.otmPanoseNumber.bWeight);
    AddPanose('bProportion', tm.otmPanoseNumber.bProportion);
    AddPanose('bContrast', tm.otmPanoseNumber.bContrast);
    AddPanose('bStrokeVariation', tm.otmPanoseNumber.bStrokeVariation);
    AddPanose('bArmStyle', tm.otmPanoseNumber.bArmStyle);
    AddPanose('bLetterform', tm.otmPanoseNumber.bLetterform);
    AddPanose('bMidline', tm.otmPanoseNumber.bMidline);
    AddPanose('bXHeight', tm.otmPanoseNumber.bXHeight);
  finally
    mmPanose.Lines.EndUpdate;
  end;
end;

procedure TFontTestForm.AddPanose(const ATitle: string; const AValue: integer);
begin
  mmPanose.Lines.Add(ATitle + ' = ' + IntToStr(AValue));
end;

procedure TFontTestForm.edtTextChange(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TFontTestForm.cbBoxSizeChange(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TFontTestForm.cbCenteredClick(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TFontTestForm.cbDrawMetricsClick(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TFontTestForm.cbShiftTopChange(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TFontTestForm.PaintBoxPaint(Sender: TObject);
begin
  RepaintChar(Paintbox.Canvas, Paintbox.ClientRect);
end;


type
  TAutoadjustRecord = record
    n: string;
    a: integer;
  end;

const
  FONT_AUTOADJUST: array[0..4] of TAutoAdjustRecord = (
    (n:'Meiryo';                a:32),
    (n:'Meiryo UI';             a:8),
    (n:'Microsoft JhengHei';    a:14),
    (n:'Microsoft YaHei';       a:9),
    (n:'Yu Gothic';             a:10)
  );
  {
  No adjustment needed:
  - MingLiU (all versions)
  - SimSun (all versions)
  - MS Gothic (all versions)
  - MS Mincho (all versions)
  }

function GetAutoadjustLeading(const FontName: string; const Height: integer): integer;
var i: integer;
begin
  Result := 0;
  for i := Low(FONT_AUTOADJUST) to High(FONT_AUTOADJUST) do
    if SameText(Fontname, FONT_AUTOADJUST[i].n) then begin
      Result := Trunc(FONT_AUTOADJUST[i].a * Height / 100);
      break;
    end;
end;

procedure TFontTestForm.RepaintChar(Canvas: TCanvas; CanvasRect: TRect);
var boxSize: integer;
  rc, rcd: TRect;
  flags: integer;
  bCentered: boolean;
  tm: TTextMetric;
  shiftTop: integer;
  tick: integer;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(CanvasRect);
  if edtText.Text = '' then exit;
  if not TryStrToInt(cbBoxSize.Text, boxSize) then
    exit;

  //Font name
  if cbFonts.Text = '' then exit;
  Canvas.Font.Name := cbFonts.Text;

  //Font size
  case cbLayoutMode.ItemIndex of
    0: Canvas.Font.Height := boxSize; //simple selection by full line height
    1, 2, 3, 4, 5: Canvas.Font.Height := -boxSize; //simple selection by ignoring the internal leading
  end;

  //Font metrics
  GetTextMetrics(Paintbox.Canvas.Handle, tm);

  bCentered := cbLayoutMode.ItemIndex in [2];

  //Nominal character rect for a SINGLE character
  //If we have a line of characters, the rest should go outside of this rect,
  //so that we can see both how characters line up, and how we can fit a single one
  rc := CanvasRect;
  //Vertically always in the middle
  rc.Top := (rc.Height - boxSize) div 2;
  rc.Bottom := rc.Top + boxSize;
  //Depending on the mode, start either at the left side or at the horizontal center
  if bCentered then begin
    rc.Left := (rc.Width - boxSize) div 2;
    rc.Right := rc.Left + boxSize;
  end else begin
    rc.Left := rc.Left + 20;
    rc.Right := rc.Left + boxSize;
  end;

  flags := 0;
  if bCentered then
    flags := flags or DT_CENTER or DT_VCENTER or DT_SINGLELINE //CENTER won't work without SINGLELINE
  else
    flags := flags or DT_LEFT or DT_TOP;

  //Actual drawing rect (may be adjusted in some layout modes)
  rcd := rc;
  if cbLayoutMode.ItemIndex in [3, 4] then
    //Raise by internal leading
    rcd.Top := rcd.Top - tm.tmInternalLeading;
  case cbLayoutMode.ItemIndex of
    4:  //Raise by autoadjust
        rcd.Top := rcd.Top + GetAutoadjustLeading(Canvas.Font.Name, -Canvas.Font.Height);
    5:  //Raise to have the baseline at the predefined level of 11% from the bottom
        //This is where the baseline is in all the old-school CJK fonts.
        rcd.Top := rcd.Top + Trunc(0.86*boxSize) - tm.tmAscent;
  end;
  if not TryStrToInt(cbShiftTop.Text, shiftTop) then
    exit;
  rcd.Top := rcd.Top + shiftTop;

  DrawText(Canvas.Handle, edtText.Text, Length(edtText.Text), rcd, flags or DT_NOCLIP or DT_SINGLELINE);
  //We want to know the final rect too:
  DrawText(Canvas.Handle, edtText.Text, Length(edtText.Text), rcd, flags or DT_NOCLIP or DT_SINGLELINE or DT_CALCRECT);
  if bCentered then
    //In CENTER mode CALCRECT does not adjust the rect.Left, so we need to:
    rcd.Offset(- (rcd.Width div 2) + (rc.Width div 2), 0);

  if cbDrawMetrics.Checked then
    DrawMetrics(Canvas, rcd, bCentered);

  //Paint a rectangle to show where the character needs to sit (paint the nominal rect)
  Canvas.Pen.Color := clRed;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(rc);

  if cbDrawTicks.Checked then begin
    tick := rc.Top + 10;
    while tick < rc.Bottom do begin
      Canvas.PenPos := TPoint.Create(rc.Left, tick);
      Canvas.LineTo(rc.Left+4, tick);
      Inc(tick, 10);
    end;
  end;
end;

procedure TFontTestForm.DrawMetrics(Canvas: TCanvas; DrawRect: TRect; Centered: boolean);
var tm: TTextMetric;
  lineTop, cellTop, baseline, cellBottom: integer;
begin
  GetTextMetrics(Paintbox.Canvas.Handle, tm);

  //Adjust starting point
  if Centered then
    lineTop := DrawRect.Top + DrawRect.Height div 2 //DrawRect center
      - tm.tmHeight div 2 //line top
  else
    lineTop := DrawRect.Top;

  //Line top
  Canvas.Pen.Color := clLime;
  Canvas.PenPos := TPoint.Create(DrawRect.Left, lineTop);
  Canvas.LineTo(DrawRect.Right + 20, lineTop);

  //External leading, if present - outside of the line
  { //we don't care; it just complicates things
  if tm.tmExternalLeading <> 0 then begin
    Canvas.Pen.Color := clSkyBlue;
    Canvas.PenPos := TPoint.Create(DrawRect.Left, lineTop-tm.tmExternalLeading);
    Canvas.LineTo(DrawRect.Right + 20, lineTop-tm.tmExternalLeading);
  end;
  }

  //Cell top
  cellTop := lineTop + tm.tmInternalLeading;
  if cellTop <> lineTop then begin
    Canvas.Pen.Color := clLime;
    Canvas.PenPos := TPoint.Create(DrawRect.Left, cellTop);
    Canvas.LineTo(DrawRect.Right + 20, cellTop);
  end;

  //Baseline - ascent includes the internal leading
  baseline := lineTop + tm.tmAscent;
  Canvas.Pen.Color := clBlue;
  Canvas.PenPos := TPoint.Create(DrawRect.Left, baseline);
  Canvas.LineTo(DrawRect.Right + 10, baseline);

  //Cell bottom
  cellBottom := baseline + tm.tmDescent;
  Canvas.Pen.Color := clLime;
  Canvas.PenPos := TPoint.Create(DrawRect.Left, cellBottom);
  Canvas.LineTo(DrawRect.Right + 20, cellBottom);

  {
  +tm.tmHeight
  +tm.tmAscent
  +tm.tmDescent
  +tm.tmInternalLeading
  tm.tmExternalLeading
  tm.tmAveCharWidth
  tm.tmMaxCharWidth
  tm.tmOverhang
  }
end;


end.
