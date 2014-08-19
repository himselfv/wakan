unit SpeedBtn;
{
TWinSpeedButton

Similar to TSpeedButton but based on windowed TButton, with nicer
ImageList-based images and platform-independent Down / DropDownList support.

Supports:
* Split button (with dropdown arrow) even without platform support
* Classic, system-themed and delphi-themed rendering
* Flat versions of all of the above

Most of the code is modelled after TSpeedButton, TBitBtn.

TODO:
  - Perhaps override standard DrawControlText instead of own DoDrawText?
  - "Default" drawing for Flat/Classic/ClassicFlat cases
  - Glyph on flat themed buttons in some styles

Possible improvements:
  - Use normal Button fading mechanics (try moving focus away and watch)
  - Use normal Button focus mechanics (when you focus by clicking, there's
   highlight but no focus rect. Only keyboard brings focus rect)
  - Draw in DrawItem() handler, this allows for ODS_FOCUS handling too (see
   how BitBtn.DrawItem does it, with no success though)
  - Write custom TImageIndex property editor which checks own .Images first
   before going to GetOwner.Images() as builtin TPersistentImageIndexProperty
   Editor does.
}

interface
uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, Messages, Windows,
  Themes, ImgList, UITypes;

type
  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TButtonState = (bsUp, bsDisabled, bsDown, bsExclusive);
  TButtonFrameState = (bfUp, bfDown, bfExclusive, bfHot, bfDefault, bfDisabled);
  TMousePosition = (mpOutside, mpBody, mpDropBtn);
  TDragState = (dsNone, dsBody, dsDropBtn);

  //TCustomButton with TCustomControl additions over it.
  TCustomPaintedButton = class(TCustomButton)
  protected
    FCanvas: TCanvas;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  {$IF NOT DEFINED(CLR)}
    property Canvas: TCanvas read FCanvas;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IF DEFINED(CLR)}
    function get_Canvas: TCanvas;
    property Canvas: TCanvas read get_Canvas;
  {$ENDIF}
  end;

  TWinSpeedButton = class;
  TWinSpeedButtonStyleHook = class;

  TWinSpeedButtonActionLink = class(TPushButtonActionLink)
  protected
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetGroupIndex(Value: Integer); override;
  end;

  TSplitterStyle = (
    ssHidden,  // single button
    ssClassic, // classic colored splitter
    ssVista,   // vista colored splitter
    ssButtons, // two separate buttons
    ssNative   // whatever fits the theme
  );
  TDropButtonSettings = class(TPersistent)
  protected
    FButton: TWinSpeedButton;
    FSize: integer;
    FSplitterStyle: TSplitterStyle;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FHotImageIndex: TImageIndex;
    FPressedImageIndex: TImageIndex;
    FDisabledImageIndex: TImageIndex;
    FSelectedImageIndex: TImageIndex;
    FStylusHotImageIndex: TImageIndex;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
    function GetImages: TCustomImageList;
    function GetOwner: TPersistent; override;
    function GetParentImages: TCustomImageList;
    function IsImagesStored: boolean;
    procedure SetSize(const Value: integer);
    procedure SetSplitterStyle(const Value: TSplitterStyle);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetHotImageIndex(const Value: TImageIndex);
    procedure SetPressedImageIndex(const Value: TImageIndex);
    procedure SetDisabledImageIndex(const Value: TImageIndex);
    procedure SetSelectedImageIndex(const Value: TImageIndex);
    procedure SetStylusHotImageIndex(const Value: TImageIndex);
  public
    constructor Create(AButton: TWinSpeedButton);
  published
    property Size: integer read FSize write SetSize default 16;
    property SplitterStyle: TSplitterStyle read FSplitterStyle write SetSplitterStyle default ssNative;
    property Images: TCustomImageList read GetImages write SetImages stored IsImagesStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property HotImageIndex: TImageIndex read FHotImageIndex write SetHotImageIndex default -1;
    property PressedImageIndex: TImageIndex read FPressedImageIndex write SetPressedImageIndex default -1;
    property DisabledImageIndex: TImageIndex read FDisabledImageIndex write SetDisabledImageIndex default -1;
    property SelectedImageIndex: TImageIndex read FSelectedImageIndex write SetSelectedImageIndex default -1;
    property StylusHotImageIndex: TImageIndex read FStylusHotImageIndex write SetStylusHotImageIndex default -1;
  end;

  TWinSpeedButton = class(TCustomPaintedButton)
  strict private
    class constructor Create;
    class destructor Destroy;
  protected
    FAllowAllUp: Boolean;
    FDown: Boolean;
    FDragState: TDragState;
    FFocusable: Boolean;
    FFlat: Boolean;
    FGroupIndex: Integer;
    FLayout: TButtonLayout;
    FMargin: Integer; //distance from the anchor border to the glyph
    FMousePosition: TMousePosition;
    FPainter: TWinSpeedButtonStyleHook;
    FDropButtonSettings: TDropButtonSettings;
    FSpacing: Integer; //distance from the glyph to the caption
    FState: TButtonState;
    FDropBtnState: TButtonState; //only bsUp or bsDown
    FTransparent: Boolean;
    FFakeFocus: boolean; //see WndProc->WM_LBUTTONDOWN
    FForceClassicLook: boolean;
    procedure Paint; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetBodyRect: TRect;
    function GetDropBtnRect: TRect;
    procedure DoDropDownClick; virtual;
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetDown(Value: Boolean);
    procedure SetDropButtonSettings(Value: TDropButtonSettings);
    procedure SetFocusable(Value: Boolean);
    procedure SetForceClassicLook(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure WndProc(var Message: TMessage); override;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure EndDrag(X, Y: Integer);
    procedure ButtonPressed(Group: Integer; Button: TWinSpeedButton);
    procedure UpdateExclusive;
    procedure UpdateTracking;
    property BodyRect: TRect read GetBodyRect;
    property DropBtnRect: TRect read GetDropBtnRect;
    property MousePosition: TMousePosition read FMousePosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    function CanFocus: Boolean; override;
    procedure SetFocus; override;
    function Focused: boolean; override;
    function IsInDropButton(const APoint: TPoint): boolean;
  published
    property Action;
    property Align;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property Caption;
    property Constraints;
    property Default;
    property DisabledImageIndex;
    property DoubleBuffered;
    property Down: Boolean read FDown write SetDown default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropButtonSettings: TDropButtonSettings read FDropButtonSettings
      write SetDropButtonSettings;
    property DropDownMenu;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Focusable: Boolean read FFocusable write SetFocusable default True;
    property ForceClassicLook: Boolean read FForceClassicLook
      write SetForceClassicLook default False;
    property Font;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HotImageIndex;
    property ImageIndex;
    property ImageMargins;
    property Images;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft; //?
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property PressedImageIndex;
    property SelectedImageIndex;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Style;
    property StylusHotImageIndex;
    property TabOrder;
    property TabStop;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property WordWrap;
    property StyleElements;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDownClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  //Paints WinSpeedButton contents (glyph + caption) on a given canvas
  //Different implementations support themed or classic look
  TWinSpeedButtonStyleHook = class(TMouseTrackControlStyleHook)
  protected
    FButton: TWinSpeedButton;
    procedure Paint(Canvas: TCanvas); override;
    function GetBodyFrameState: TButtonFrameState;
    function GetDropFrameState: TButtonFrameState;
    procedure DrawSplitter(Canvas: TCanvas; Rect: TRect; Color1, Color2: TColor);
    procedure DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
      const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
      ImageIndex: TImageIndex; Transparent: boolean); virtual;
    function ImageIndexFromState(State: TButtonState): integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; Flags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; State: TButtonState; var GlyphPos: TPoint;
      var TextBounds: TRect; BiDiFlags: Longint);
    procedure DrawBackground(Canvas: TCanvas); virtual;
    procedure DrawDropButton(Canvas: TCanvas); virtual;
    procedure DrawDropGlyph(Canvas: TCanvas; State: TButtonFrameState;
      PaintRect: TRect; Details: TThemedElementDetails); virtual;
    procedure DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
      out PaintRect: TRect; out Offset: TPoint); virtual;
    procedure DoDrawText(Canvas: TCanvas; State: TButtonState; const Text: UnicodeString;
      var TextRect: TRect; TextFlags: Cardinal); virtual;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TThemedSpeedBtnStyleHook = class(TWinSpeedButtonStyleHook)
  protected
    FPaintOnGlass: Boolean;
    FThemeDetails: TThemedElementDetails;
    FThemeTextColor: Boolean;
    procedure Paint(Canvas: TCanvas); override;
    procedure DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
      const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
      ImageIndex: TImageIndex; Transparent: boolean); override;
    procedure DrawButtonFrame(Canvas: TCanvas; State: TButtonFrameState;
      PaintRect, ClipRect: TRect; out Details: TThemedElementDetails;
      out ContentRect: TRect);
    procedure DrawBackground(Canvas: TCanvas); override;
    procedure DrawDropButton(Canvas: TCanvas); override;
    procedure DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
      out PaintRect: TRect; out Offset: TPoint); override;
    procedure DoDrawText(Canvas: TCanvas; State: TButtonState; const Text: UnicodeString;
      var TextRect: TRect; TextFlags: Cardinal); override;
  end;

  TClassicSpeedBtnStyleHook = class(TWinSpeedButtonStyleHook)
  protected
    procedure DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
      const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
      ImageIndex: TImageIndex; Transparent: boolean); override;
    procedure DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
      out PaintRect: TRect; out Offset: TPoint); override;
    procedure DrawDropButton(Canvas: TCanvas); override;
    procedure DrawButtonFrame(Canvas: TCanvas; State: TButtonFrameState;
      PaintRect, ClipRect: TRect; out Details: TThemedElementDetails;
      out ContentRect: TRect);
    procedure DoDrawText(Canvas: TCanvas; State: TButtonState; const Text: UnicodeString;
      var TextRect: TRect; TextFlags: Cardinal); override;
  end;

procedure Register;

implementation
uses Types, Forms, ActnList, UxTheme, CommCtrl;

procedure Register;
begin
  RegisterComponents('Wakan', [TWinSpeedButton]);
end;

{ TCustomPaintedButton }

constructor TCustomPaintedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TCustomPaintedButton.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

{$IF DEFINED(CLR)}
[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeSubWindows)]
function TCustomPaintedButton.get_Canvas: TCanvas;
begin
  Result := FCanvas;
end;
{$ENDIF}

procedure TCustomPaintedButton.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TCustomPaintedButton.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TCustomPaintedButton.Paint;
begin
end;


{ TWinSpeedButtonActionLink }

function TWinSpeedButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked
    and (TWinSpeedButton(FClient).GroupIndex <> 0)
    and TWinSpeedButton(FClient).AllowAllUp
    and (TWinSpeedButton(FClient).Down = TCustomAction(Action).Checked);
end;

function TWinSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := inherited IsGroupIndexLinked
    and (TWinSpeedButton(FClient).GroupIndex = TCustomAction(Action).GroupIndex);
end;

procedure TWinSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TWinSpeedButton(FClient).Down := Value;
end;

procedure TWinSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TWinSpeedButton(FClient).GroupIndex := Value;
end;



{ TDropButtonSettings }

constructor TDropButtonSettings.Create(AButton: TWinSpeedButton);
begin
  inherited Create();
  FButton := AButton;
  FSize := 16;
  FSplitterStyle := ssNative;
  FImageIndex := -1;
  FHotImageIndex := -1;
  FPressedImageIndex := -1;
  FDisabledImageIndex := -1;
  FSelectedImageIndex := -1;
  FStylusHotImageIndex := -1;
end;

procedure TDropButtonSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TDropButtonSettings then begin
    TDropButtonSettings(Dest).FSize := Self.FSize;
    TDropButtonSettings(Dest).FSplitterStyle := Self.FSplitterStyle;
    TDropButtonSettings(Dest).FImages := Self.FImages;
    TDropButtonSettings(Dest).FImageIndex := Self.FImageIndex;
    TDropButtonSettings(Dest).FHotImageIndex := Self.FHotImageIndex;
    TDropButtonSettings(Dest).FPressedImageIndex := Self.FPressedImageIndex;
    TDropButtonSettings(Dest).FDisabledImageIndex := Self.FDisabledImageIndex;
    TDropButtonSettings(Dest).FSelectedImageIndex := Self.FSelectedImageIndex;
    TDropButtonSettings(Dest).FStylusHotImageIndex := Self.FStylusHotImageIndex;
    TDropButtonSettings(Dest).Changed;
  end else
    inherited;
end;

procedure TDropButtonSettings.Changed;
begin
  if FButton.Style = bsSplitButton then
    FButton.Invalidate;
end;

function TDropButtonSettings.GetImages: TCustomImageList;
begin
  if FImages <> nil then
    Result := FImages
  else
    Result := GetParentImages;
end;

function TDropButtonSettings.GetOwner: TPersistent;
begin
  Result := FButton;
end;

function TDropButtonSettings.GetParentImages: TCustomImageList;
begin
  if FButton <> nil then
    Result := FButton.Images
  else
    Result := nil;
end;

function TDropButtonSettings.IsImagesStored: boolean;
begin
  Result := (FImages<>nil);
end;

procedure TDropButtonSettings.SetSize(const Value: integer);
begin
  FSize := Value;
  Changed;
end;

procedure TDropButtonSettings.SetSplitterStyle(const Value: TSplitterStyle);
begin
  FSplitterStyle := Value;
  Changed;
end;

procedure TDropButtonSettings.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Changed;
end;

procedure TDropButtonSettings.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
  Changed;
end;

procedure TDropButtonSettings.SetHotImageIndex(const Value: TImageIndex);
begin
  FHotImageIndex := Value;
  Changed;
end;

procedure TDropButtonSettings.SetPressedImageIndex(const Value: TImageIndex);
begin
  FPressedImageIndex := Value;
  Changed;
end;

procedure TDropButtonSettings.SetDisabledImageIndex(const Value: TImageIndex);
begin
  FDisabledImageIndex := Value;
  Changed;
end;

procedure TDropButtonSettings.SetSelectedImageIndex(const Value: TImageIndex);
begin
  FSelectedImageIndex := Value;
  Changed;
end;

procedure TDropButtonSettings.SetStylusHotImageIndex(const Value: TImageIndex);
begin
  FStylusHotImageIndex := Value;
  Changed;
end;


{ TWinSpeedButton }

class constructor TWinSpeedButton.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TWinSpeedButton, TWinSpeedButtonStyleHook);
end;

class destructor TWinSpeedButton.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TWinSpeedButton, TWinSpeedButtonStyleHook);
end;

constructor TWinSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropButtonSettings := TDropButtonSettings.Create(Self);
  FPainter := nil;
  FFocusable := true;
  FLayout := blGlyphLeft;
  FMargin := -1;
  FSpacing := 4;
  FTransparent := True;
end;

destructor TWinSpeedButton.Destroy;
begin
  FreeAndNil(FPainter);
  FreeAndNil(FDropButtonSettings);
  inherited;
end;

function TWinSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TWinSpeedButtonActionLink;
end;

function TWinSpeedButton.GetBodyRect: TRect;
begin
  Result := ClientRect;
  if Self.Style = bsSplitButton then
    Result.Right := Result.Right - DropButtonSettings.Size;
end;

function TWinSpeedButton.GetDropBtnRect: TRect;
begin
  Result := ClientRect;
  if Self.Style = bsSplitButton then
    Result.Left := Result.Right - DropButtonSettings.Size
  else
    Result.Left := Result.Right;
end;

procedure TWinSpeedButton.DoDropDownClick;
var Pt: TPoint;
begin
  if Assigned(OnDropDownClick) then
    OnDropDownClick(Self);
  if Assigned(DropDownMenu) then begin
    Pt := ClientToScreen(Point(0, Self.Height));
    DropDownMenu.Popup(Pt.X, Pt.Y);
  end;
  { Popup or other code could have eaten WM_MOUSEUP, so repost it
   Just calling EndDrag / posting WM_MOUSEUP is not good enough, what if there
   was no handler? We end up un-pressing the button the same second. }
  if (GetCapture() <> Self.Handle) or (GetKeyState(VK_LBUTTON) = 0) then
    Self.EndDrag(0, 0);
end;

procedure TWinSpeedButton.Paint;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDropBtnState := bsUp;
    FDragState := dsNone;
  end
  else if FState = bsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;

  //Update painter
  if ThemeControl(Self) and not FForceClassicLook then begin
    if (FPainter = nil) or not (FPainter is TThemedSpeedBtnStyleHook) then begin
      FreeAndNil(FPainter);
      FPainter := TThemedSpeedBtnStyleHook.Create(Self);
    end;
  end else begin
    if (FPainter = nil) or not (FPainter is TClassicSpeedBtnStyleHook) then begin
      FreeAndNil(FPainter);
      FPainter := TClassicSpeedBtnStyleHook.Create(Self);
    end;
  end;

  FPainter.Paint(Canvas);
end;


{ StyleHooks }

constructor TWinSpeedButtonStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  FButton := AControl as TWinSpeedButton;
{
  OverridePaint := True;
  OverrideEraseBkgnd := True;
  DoubleBuffered := True;
  FDropDown := False;
  FOldMouseInControl := False;
}
end;

procedure TWinSpeedButtonStyleHook.Paint(Canvas: TCanvas);
var
  PaintRect: TRect;
  GlyphPos: TPoint;
  Offset: TPoint;
  Details: TThemedElementDetails;
  TextBounds: TRect;
  BiDiFlags: integer;
begin
  Canvas.Font := FButton.Font;
  Self.DrawBackground(Canvas);

  Self.DrawBody(Canvas, Details, PaintRect, Offset);
  if FButton.Style = bsSplitButton then
    Self.DrawDropButton(Canvas);

  BiDiFlags := FButton.DrawTextBiDiModeFlags(0);

  Canvas.Font := FButton.Font;
  Self.CalcButtonLayout(Canvas, PaintRect, Offset, FButton.Caption, FButton.FLayout,
    FButton.FMargin, FButton.FSpacing, FButton.FState, GlyphPos, TextBounds,
    BiDiFlags);
  Self.DrawButtonGlyph(Canvas, GlyphPos, FButton.FState, FButton.Transparent);
  Self.DrawButtonText(Canvas, FButton.Caption, TextBounds, FButton.FState, BiDiFlags);

  if FButton.Focused and (not (Self is TThemedSpeedBtnStyleHook) or StyleServices.IsSystemStyle) then begin
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Color := clBtnFace;
    DrawFocusRect(Canvas.Handle, PaintRect);
  end;
end;

function TWinSpeedButtonStyleHook.GetBodyFrameState: TButtonFrameState;
begin
  if not FButton.Enabled then
    Result := bfDisabled
  else
    if FButton.FState in [bsDown, bsExclusive] then
      Result := bfDown
    else
      if FButton.FMousePosition in [mpBody] then
        Result := bfHot
      else
        if FButton.Focused or FButton.Default then
          Result := bfDefault
        else
          Result := bfUp;
end;

function TWinSpeedButtonStyleHook.GetDropFrameState: TButtonFrameState;
begin
  if not FButton.Enabled then
    Result := bfDisabled
  else
    if FButton.FDropBtnState in [bsDown] then
      Result := bfDown
    else
      if FButton.FMousePosition in [mpBody, mpDropBtn] then
        Result := bfHot
      else
        if FButton.Focused or FButton.Default then
          Result := bfDefault
        else
          Result := bfUp;
end;

procedure TWinSpeedButtonStyleHook.DrawBackground(Canvas: TCanvas);
begin
end;

procedure TWinSpeedButtonStyleHook.DrawDropButton(Canvas: TCanvas);
begin
end;

procedure TWinSpeedButtonStyleHook.DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
  out PaintRect: TRect; out Offset: TPoint);
begin
end;

procedure TWinSpeedButtonStyleHook.DrawSplitter(Canvas: TCanvas; Rect: TRect;
  Color1, Color2: TColor);
begin
  Canvas.Pen.Color := StyleServices.GetSystemColor(Color1);
  Canvas.MoveTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Left, Rect.Bottom);
  Inc(Rect.Left, 1);
  if FButton.Enabled then
    Canvas.Pen.Color := StyleServices.GetSystemColor(Color2)
  else
    Canvas.Pen.Color := FButton.Font.Color;
  Canvas.MoveTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Left, Rect.Bottom);
  Inc(Rect.Right, 1);
end;

procedure TWinSpeedButtonStyleHook.DrawDropGlyph(Canvas: TCanvas; State: TButtonFrameState;
  PaintRect: TRect; Details: TThemedElementDetails);
var ImgList: TCustomImageList;
  ImgIndex: integer;
  GlyphPos: TPoint;
begin
  case State of
    bfDown,
    bfExclusive: ImgIndex := FButton.DropButtonSettings.PressedImageIndex;
    bfHot: ImgIndex := FButton.DropButtonSettings.HotImageIndex;
    bfDefault: ImgIndex := FButton.DropButtonSettings.FSelectedImageIndex;
    bfDisabled: ImgIndex := FButton.DropButtonSettings.DisabledImageIndex;
  else ImgIndex := FButton.DropButtonSettings.ImageIndex;
  end;
  if ImgIndex<0 then
    ImgIndex := FButton.DropButtonSettings.ImageIndex; //fallback for other cases

  ImgList := FButton.DropButtonSettings.Images;
  if ImgList = nil then
    ImgList := FButton.Images;

  PaintRect.Right := FButton.ClientRect.Right; //ignore border, centers nicer

  if (ImgIndex < 0) or (ImgList = nil) then begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := 'Webdings';
    Canvas.Font.Size := -15;
    Canvas.Font.Quality := fqAntialiased;
    Self.DoDrawText(Canvas, bsUp, #$36 {alt: '▼' but this fits better},
      PaintRect, DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end else begin
    GlyphPos.X := PaintRect.Left + (PaintRect.Width - ImgList.Width) div 2;
    GlyphPos.Y := PaintRect.Top + (PaintRect.Height - ImgList.Height) div 2;

    DrawGlyph(Canvas, Details, GlyphPos, FButton.ImageMargins, ImgList, ImgIndex,
      FButton.Transparent);
  end;
end;

procedure TWinSpeedButtonStyleHook.DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
  const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
  ImageIndex: TImageIndex; Transparent: boolean);
begin
end;

procedure TWinSpeedButtonStyleHook.DoDrawText(Canvas: TCanvas; State: TButtonState;
  const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
begin
end;

function TWinSpeedButtonStyleHook.ImageIndexFromState(State: TButtonState): integer;
begin
  case State of
    bsUp: Result := FButton.ImageIndex;
    bsDown: Result := FButton.PressedImageIndex;
    bsExclusive: Result := FButton.HotImageIndex;
    bsDisabled: Result := FButton.DisabledImageIndex;
  else Result := -1;
  end;
  if Result < 0 then //fallback
    Result := FButton.ImageIndex;
end;

procedure TWinSpeedButtonStyleHook.DrawButtonGlyph(Canvas: TCanvas;
  const GlyphPos: TPoint; State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
  Details: TThemedElementDetails;
  Button: TThemedButton;
begin
  if FButton.Images = nil then Exit;
  if (FButton.Images.Width = 0) or (FButton.Images.Height = 0) then Exit;

  Index := ImageIndexFromState(State);
  if Index < 0 then exit;

  if Self is TThemedSpeedBtnStyleHook then begin
    case State of
      bsDisabled:
        Button := tbPushButtonDisabled;
      bsDown,
      bsExclusive:
        Button := tbPushButtonPressed;
    else
      // bsUp
      Button := tbPushButtonNormal;
    end;
    Details := StyleServices.GetElementDetails(Button);
  end else
    Details.Element := teButton;

  DrawGlyph(Canvas, Details, GlyphPos, FButton.ImageMargins, FButton.Images,
    Index, FButton.Transparent or (State = bsExclusive));
end;

procedure TWinSpeedButtonStyleHook.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; Flags: LongInt);
begin
  if FButton.WordWrap then
    Flags := Flags or DT_WORDBREAK;
  Canvas.Brush.Style := bsClear;
  if (State = bsDisabled) and not (Self is TThemedSpeedBtnStyleHook) then
  begin
    OffsetRect(TextBounds, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DoDrawText(Canvas, State, Caption, TextBounds, DT_NOCLIP or DT_CENTER
      or DT_VCENTER or Flags);
    OffsetRect(TextBounds, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DoDrawText(Canvas, State, Caption, TextBounds, DT_NOCLIP or DT_CENTER
      or DT_VCENTER or Flags);
  end
  else
    DoDrawText(Canvas, State, Caption, TextBounds, DT_NOCLIP or DT_CENTER
      or DT_VCENTER or Flags);
end;

procedure TWinSpeedButtonStyleHook.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; State: TButtonState; var GlyphPos: TPoint;
  var TextBounds: TRect; BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if (FButton.Images<>nil) and (ImageIndexFromState(State)>=0) then
    GlyphSize := Point(
      FButton.Images.Width+FButton.ImageMargins.Left+FButton.ImageMargins.Right,
      FButton.Images.Height+FButton.ImageMargins.Top+FButton.ImageMargins.Bottom)
  else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    if FButton.WordWrap then
      BiDiFlags := BiDiFlags or DT_WORDBREAK;
    DrawText(Canvas.Handle, Caption, Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left + Offset.X);
  Inc(GlyphPos.Y, Client.Top + Offset.Y);

  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
end;



{ Themed painter }

procedure TThemedSpeedBtnStyleHook.Paint(Canvas: TCanvas);

  function DoGlassPaint: Boolean;
  var
    LParent: TWinControl;
  begin
    Result := csGlassPaint in FButton.ControlState;
    if Result then
    begin
      LParent := FButton.Parent;
      while (LParent <> nil) and not LParent.DoubleBuffered do
        LParent := LParent.Parent;
      Result := (LParent = nil) or not LParent.DoubleBuffered or (LParent is TCustomForm);
    end;
  end;

begin
  Self.FPaintOnGlass := DoGlassPaint();
  inherited Paint(Canvas);
end;

procedure TThemedSpeedBtnStyleHook.DrawBackground(Canvas: TCanvas);
begin
  if not FPaintOnGlass then
    if FButton.Transparent then
      StyleServices.DrawParentBackground(0, Canvas.Handle, nil, True)
    else
      PerformEraseBackground(FButton, Canvas.Handle)
  else
    FillRect(Canvas.Handle, FButton.ClientRect, GetStockObject(BLACK_BRUSH));
end;

procedure TThemedSpeedBtnStyleHook.DrawBody(Canvas: TCanvas;
  out Details: TThemedElementDetails; out PaintRect: TRect; out Offset: TPoint);
var State: TButtonFrameState;
  ASplitterStyle: TSplitterStyle;
begin
  State := GetBodyFrameState;

  ASplitterStyle := Self.FButton.DropButtonSettings.SplitterStyle;
  if ASplitterStyle = ssNative then
    if StyleServices.IsSystemStyle then
      ASplitterStyle := ssVista
    else
      ASplitterStyle := ssClassic; //custom themes look better with classic

  if ASplitterStyle = ssButtons then
    DrawButtonFrame(Canvas, State, FButton.BodyRect, FButton.BodyRect,
      Details, PaintRect)
  else
    DrawButtonFrame(Canvas, State, FButton.ClientRect, FButton.BodyRect,
      Details, PaintRect);

  if (FButton.Style = bsSplitButton) and (ASplitterStyle <> ssHidden) then
    Dec(PaintRect.Right, 1);

  Offset := Point(0, 0);
  if (State in [bfDown, bfExclusive]) and FButton.FFlat then
  begin
    // A pressed "flat" speed button has white text in XP, but the Themes
    // API won't render it as such, so we need to hack it.
    if not CheckWin32Version(6) then
      Canvas.Font.Color := clHighlightText
    else
      Offset := Point(1, 0);
  end;

  Self.FThemeDetails := Details;
  Self.FThemeTextColor := seFont in FButton.StyleElements;
end;

procedure TThemedSpeedBtnStyleHook.DrawDropButton(Canvas: TCanvas);
var State: TButtonFrameState;
  Details: TThemedElementDetails;
  PaintRect: TRect;
  ASplitterStyle: TSplitterStyle;
begin
  State := GetDropFrameState;

  ASplitterStyle := Self.FButton.DropButtonSettings.SplitterStyle;
  if ASplitterStyle = ssNative then
    if StyleServices.IsSystemStyle then
      ASplitterStyle := ssVista
    else
      ASplitterStyle := ssClassic; //custom themes look better with classic

  if ASplitterStyle = ssButtons then
    DrawButtonFrame(Canvas, State, FButton.DropBtnRect, FButton.DropBtnRect,
      Details, PaintRect)
  else
    DrawButtonFrame(Canvas, State, FButton.ClientRect, FButton.DropBtnRect,
      Details, PaintRect);

  //Paint glyph
  DrawDropGlyph(Canvas, State, PaintRect, Details);

  //Paint splitter
  case ASplitterStyle of
    ssClassic: begin
      DrawSplitter(Canvas, PaintRect, clBtnShadow, clBtnHighlight);
      Inc(PaintRect.Left, 2);
    end;
    ssVista: begin
      DrawSplitter(Canvas, PaintRect, clActiveBorder, cl3DDkShadow);
      Inc(PaintRect.Left, 2);
    end;
  end;

end;


//Paints themed button frame and returns theme details according to current
//control styling
//Non-themed styling is handled separately
procedure TThemedSpeedBtnStyleHook.DrawButtonFrame(Canvas: TCanvas;
  State: TButtonFrameState; PaintRect, ClipRect: TRect;
  out Details: TThemedElementDetails; out ContentRect: TRect);
var
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  OldClipRgn: HRGN;
begin
  //Themes based on Delphi's TCustomTheme ignore DrawElement's ClipRect when
  //drawing, so have to set it globally
  OldClipRgn := 0;
  GetClipRgn(Canvas.Handle, OldClipRgn);
  IntersectClipRect(Canvas.Handle, ClipRect.Left, ClipRect.Top, ClipRect.Right,
    ClipRect.Bottom);

  if not FButton.FFlat then begin

    case State of
      bfDown,
      bfExclusive: Button := tbPushButtonPressed;
      bfHot: Button := tbPushButtonHot;
      bfDefault: Button := tbPushButtonDefaulted;
      bfDisabled: Button := tbPushButtonDisabled;
    else Button := tbPushButtonNormal;
    end;

    Details := StyleServices.GetElementDetails(Button);
    StyleServices.DrawElement(Canvas.Handle, Details, PaintRect, ClipRect);

  end else begin

    case State of
      bfDown,
      bfExclusive: ToolButton := ttbButtonPressed;
      bfHot: ToolButton := ttbButtonHot;
      bfDefault: ToolButton := ttbButtonNormal;
      bfDisabled: ToolButton := ttbButtonDisabled;
    else ToolButton := ttbButtonNormal;
    end;

    Details := StyleServices.GetElementDetails(ToolButton);
    if not TStyleManager.IsCustomStyleActive then
    begin
      StyleServices.DrawElement(Canvas.Handle, Details, PaintRect, ClipRect);
      // Windows theme services doesn't paint disabled toolbuttons
      // with grayed text (as it appears in an actual toolbar). To workaround,
      // retrieve Details for a disabled button for drawing the caption.
      if ToolButton = ttbButtonDisabled then
        Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
    end
    else
    begin
      // Special case for flat speedbuttons with custom styles. The assumptions
      // made about the look of ToolBar buttons may not apply, so only paint
      // the hot and pressed states, leaving normal/disabled to appear flat.
      if not FButton.FFlat or (State in [bfDown, bfExclusive, bfHot]) then
        StyleServices.DrawElement(Canvas.Handle, Details, PaintRect, ClipRect);
    end;

  end;

  SelectClipRgn(Canvas.Handle, OldClipRgn);

  StyleServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, ContentRect);

  //Some themes (notably all of Delphi's custom ones) and some items (Toolbar
  //Button in W7 default theme) do not really exclude borders from content rect.
  //Test for that case and make amends (better fixed border than nothing)
  if PaintRect=ContentRect then
    InflateRect(ContentRect, -3, -3);

  //Intersect content rect with clip rect
  if ContentRect.Left < ClipRect.Left then
    ContentRect.Left := ClipRect.Left;
  if ContentRect.Top < ClipRect.Top then
    ContentRect.Top := ClipRect.Top;
  if ContentRect.Right > ClipRect.Right then
    ContentRect.Right := ClipRect.Right;
  if ContentRect.Bottom > ClipRect.Bottom then
    ContentRect.Bottom := ClipRect.Bottom;
end;

procedure TThemedSpeedBtnStyleHook.DoDrawText(Canvas: TCanvas; State: TButtonState;
  const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
var
  LColor: TColor;
  LFormats: TTextFormat;
begin
  if (State = bsDisabled) or (not StyleServices.IsSystemStyle and FThemeTextColor) then
  begin
    if not StyleServices.GetElementColor(FThemeDetails, ecTextColor, LColor) or (LColor = clNone) then
      LColor := Canvas.Font.Color;
  end
  else
    LColor := Canvas.Font.Color;

  LFormats := TTextFormatFlags(TextFlags);
  if FPaintOnGlass then
    Include(LFormats, tfComposited);

  StyleServices.DrawText(Canvas.Handle, FThemeDetails, Text, TextRect, LFormats, LColor);
end;


{ Classic painter }

const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);

procedure TClassicSpeedBtnStyleHook.DrawButtonFrame(Canvas: TCanvas;
  State: TButtonFrameState; PaintRect, ClipRect: TRect;
  out Details: TThemedElementDetails; out ContentRect: TRect);
var DrawFlags: Integer;
  OldClipRgn: HRGN;
begin
  //Themes based on Delphi's TCustomTheme ignore DrawElement's ClipRect when
  //drawing, so have to set it globally
  OldClipRgn := 0;
  GetClipRgn(Canvas.Handle, OldClipRgn);
  IntersectClipRect(Canvas.Handle, ClipRect.Left, ClipRect.Top, ClipRect.Right,
    ClipRect.Bottom);

  if not FButton.FFlat then
  begin
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if State in [bfDown, bfExclusive] then
      DrawFlags := DrawFlags or DFCS_PUSHED;
    DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
  end
  else
  begin
    if (State in [bfDown, bfExclusive]) or
      ((FButton.FMousePosition in [mpBody, mpDropBtn]) and (State <> bfDisabled)) or
      (csDesigning in FButton.ComponentState) then
      DrawEdge(Canvas.Handle, PaintRect, DownStyles[State in [bfDown, bfExclusive]],
        FillStyles[FButton.Transparent] or BF_RECT)
    else if not FButton.Transparent then
    begin
      Canvas.Brush.Color := FButton.Color;
      Canvas.FillRect(PaintRect);
    end;
    InflateRect(PaintRect, -1, -1);
  end;

  if (State = bfExclusive) and (not FButton.FFlat or (FButton.FMousePosition in [mpOutside])) then
  begin
    Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
    Canvas.FillRect(PaintRect);
  end;

  SelectClipRgn(Canvas.Handle, OldClipRgn);

  ContentRect := PaintRect;

  //Intersect content rect with clip rect
  if ContentRect.Left < ClipRect.Left then
    ContentRect.Left := ClipRect.Left;
  if ContentRect.Top < ClipRect.Top then
    ContentRect.Top := ClipRect.Top;
  if ContentRect.Right > ClipRect.Right then
    ContentRect.Right := ClipRect.Right;
  if ContentRect.Bottom > ClipRect.Bottom then
    ContentRect.Bottom := ClipRect.Bottom;
end;

procedure TClassicSpeedBtnStyleHook.DrawBody(Canvas: TCanvas;
  out Details: TThemedElementDetails; out PaintRect: TRect; out Offset: TPoint);
var State: TButtonFrameState;
  ASplitterStyle: TSplitterStyle;
begin
  PaintRect := FButton.BodyRect;
  State := GetBodyFrameState;

  ASplitterStyle := Self.FButton.DropButtonSettings.SplitterStyle;
  if (ASplitterStyle=ssButtons) or ((ASplitterStyle=ssNative)
    and ((State in [bfDown, bfExclusive]) or (GetDropFrameState in [bfDown, bfExclusive]))) then
    DrawButtonFrame(Canvas, State, FButton.BodyRect, FButton.BodyRect,
      Details, PaintRect)
  else begin
    DrawButtonFrame(Canvas, State, FButton.ClientRect, FButton.BodyRect,
      Details, PaintRect);

    if (FButton.Style = bsSplitButton) and (FButton.DropButtonSettings.SplitterStyle<>ssHidden) then
      Dec(PaintRect.Right, 1);
  end;

  if State in [bfDown, bfExclusive] then
  begin
    Offset.X := 1;
    Offset.Y := 1;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  InflateRect(PaintRect, -1, -1);
end;

procedure TClassicSpeedBtnStyleHook.DrawDropButton(Canvas: TCanvas);
var State: TButtonFrameState;
  Details: TThemedElementDetails;
  PaintRect: TRect;
  ASplitterStyle: TSplitterStyle;
begin
  PaintRect := FButton.DropBtnRect;
  State := GetDropFrameState;

  ASplitterStyle := Self.FButton.DropButtonSettings.SplitterStyle;
  if (ASplitterStyle=ssButtons) or ((ASplitterStyle=ssNative)
    and ((State in [bfDown, bfExclusive]) or (GetDropFrameState in [bfDown, bfExclusive]))) then
    DrawButtonFrame(Canvas, State, FButton.DropBtnRect, FButton.DropBtnRect,
      Details, PaintRect)
  else
    DrawButtonFrame(Canvas, State, FButton.ClientRect, FButton.DropBtnRect,
      Details, PaintRect);

  //Paint glyph
  DrawDropGlyph(Canvas, State, PaintRect, Details);

  case ASplitterStyle of
    ssClassic: begin
      DrawSplitter(Canvas, PaintRect, clBtnShadow, clBtnHighlight);
      Inc(PaintRect.Left, 2);
    end;
    ssVista: begin
      DrawSplitter(Canvas, PaintRect, clActiveBorder, cl3DDkShadow);
      Inc(PaintRect.Left, 2);
    end;
    ssNative:
      if not ((GetBodyFrameState in [bfDown, bfExclusive])
        or (GetDropFrameState in [bfDown, bfExclusive])) then begin
        DrawSplitter(Canvas, PaintRect, clBtnShadow, clBtnHighlight);
        Inc(PaintRect.Left, 2);
      end;
  end;
end;

procedure TClassicSpeedBtnStyleHook.DoDrawText(Canvas: TCanvas; State: TButtonState;
  const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
begin
  Windows.DrawText(Canvas.Handle, Text, Length(Text), TextRect, TextFlags);
end;



procedure TThemedSpeedBtnStyleHook.DrawGlyph(Canvas: TCanvas;
  Details: TThemedElementDetails; const GlyphPos: TPoint; Margins: TImageMargins;
  Images: TCustomImageList; ImageIndex: TImageIndex; Transparent: boolean);
var R: TRect;
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  if Images = nil then exit;
  if (Images.Width = 0) or (Images.Height = 0) then exit;
  if ImageIndex < 0 then exit;

  R.Left := GlyphPos.X + Margins.Left;
  R.Top := GlyphPos.Y + Margins.Top;
  R.Right := R.Left + Images.Width;
  R.Bottom := R.Top + Images.Height;

  if FPaintOnGlass then
  begin
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, R, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      StyleServices.DrawIcon(MemDC, Details, R, Images.Handle, ImageIndex);
      BufferedPaintMakeOpaque(PaintBuffer, R);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end
  else
    StyleServices.DrawIcon(Canvas.Handle, Details, R, Images.Handle, ImageIndex);
end;

procedure TClassicSpeedBtnStyleHook.DrawGlyph(Canvas: TCanvas;
  Details: TThemedElementDetails; const GlyphPos: TPoint; Margins: TImageMargins;
  Images: TCustomImageList; ImageIndex: TImageIndex; Transparent: boolean);
var R: TRect;
begin
  if Images = nil then exit;
  if (Images.Width = 0) or (Images.Height = 0) then exit;
  if ImageIndex < 0 then exit;

  R.Left := GlyphPos.X + Margins.Left;
  R.Top := GlyphPos.Y + Margins.Top;
  R.Right := R.Left + Images.Width;
  R.Bottom := R.Top + Images.Height;

  if Transparent then
  begin
    ImageList_DrawEx(Images.Handle, ImageIndex, Canvas.Handle, R.Left, R.Top, 0, 0,
      clNone, clNone, ILD_Transparent)
  end
  else
    ImageList_DrawEx(Images.Handle, ImageIndex, Canvas.Handle, R.Left, R.Top, 0, 0,
      ColorToRGB(clBtnFace), clNone, ILD_Normal);
end;




//Notifies other SpeedButtons with the same GroupIndex if we have been pressed
procedure TWinSpeedButton.UpdateExclusive;
var
  I: Integer;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    for I := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[I] is TWinSpeedButton then
        TWinSpeedButton(Parent.Controls[I]).ButtonPressed(FGroupIndex, Self);
   //^ This is the CLR way of notifying the siblings.
   //TSpeedButton has a message-based version of this for non-CLR platforms,
   //but otherwise it is identical, and existing message handlers assume
   //the sender is TSpeedButton so it's risky to reuse. Let's stick to CLR way.
  end;
end;

//Called by siblings when other SpeedButton with the same parent have been pressed.
procedure TWinSpeedButton.ButtonPressed(Group: Integer; Button: TWinSpeedButton);
begin
  if (Group = FGroupIndex) and (Button <> Self) then
  begin
    if Button.Down and FDown then
    begin
      FDown := False;
      FState := bsUp;
      if (Action is TCustomAction) then
        TCustomAction(Action).Checked := False;
      Invalidate;
    end;
    FAllowAllUp := Button.AllowAllUp;
  end;
end;

procedure TWinSpeedButton.UpdateTracking;
var
  P: TPoint;
begin
  if not Enabled then begin
    FMousePosition := mpOutside;
    exit;
  end;

  GetCursorPos(P);
  if FindDragTarget(P, True) <> Self then begin
    if FMousePosition <> mpOutside then begin
      FMousePosition := mpOutside;
      Invalidate;
    end;
    exit;
  end;

  P := Self.ScreenToClient(P);
  if (Self.Style=bsSplitButton) and PtInRect(Self.DropBtnRect, P) then begin
    if FMousePosition <> mpDropBtn then begin
      FMousePosition := mpDropBtn;
      Invalidate;
    end;
  end else begin
    if FMousePosition <> mpBody then begin
      FMousePosition := mpBody;
      Invalidate;
    end;
  end;
end;

procedure TWinSpeedButton.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
      if not Focusable then
        FFakeFocus := true; //fake Focused() to avoid SetFocus() in parent WndProc
      inherited WndProc(Message);
      FFakeFocus := false; //can be released earlier in MouseDown
    end;
  else
    inherited WndProc(Message);
  end;
end;

procedure TWinSpeedButton.DefaultHandler(var Message);
begin
  case TMessage(Message).Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
     //Default handler sets focus (undesirable if Focusable==false) and handles
     //clicking on separate button parts (we do it differently).
     //So just don't call it.
    end;

  else
    inherited DefaultHandler(Message);
  end;
end;

procedure TWinSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  //Release fake focus which WndProc sets. The code it hacks around has already
  //happened and the handlers below may want to know real Focus.
  FFakeFocus := false;
  inherited MouseDown(Button, Shift, X, Y);
  SetCaptureControl(Self);
  if (Button = mbLeft) and Enabled then
  begin
    if IsInDropButton(Point(X, Y)) then begin
      FDragState := dsDropBtn;
      FDropBtnState := bsDown;
      Repaint;
      DoDropDownClick(); //drag can even be finished inside
    end else begin
      FDragState := dsBody;
      if not FDown then begin
        FState := bsDown;
        Invalidate;
      end;
    end;
  end;
end;

procedure TWinSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState, NewDropBtnState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  case FDragState of
    dsBody: begin
      if not FDown then
        NewState := bsUp
      else
        NewState := bsExclusive;
      if PtInRect(BodyRect, Point(X, Y)) then
        if FDown then NewState := bsExclusive else NewState := bsDown;
      if NewState <> FState then begin
        FState := NewState;
        Invalidate;
      end;
    end;

    dsDropBtn: begin
      if PtInRect(DropBtnRect, Point(X, Y)) then
        NewDropBtnState := bsDown
      else
        NewDropBtnState := bsUp;
      if NewDropBtnState <> FDropBtnState then begin
        FDropBtnState := NewDropBtnState;
        Invalidate;
      end;
    end;

  else UpdateTracking;
  end;
end;

procedure TWinSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  SetCaptureControl(nil);
  EndDrag(X, Y);
end;

procedure TWinSpeedButton.EndDrag(X, Y: Integer);
var
  DoClick: Boolean;
  OldDragState: TDragState;
begin
  OldDragState := FDragState;
  FDragState := dsNone;
  case OldDragState of
    dsBody: begin
      DoClick := PtInRect(BodyRect, Point(X, Y));
      if FGroupIndex = 0 then
      begin
        { Redraw face in-case mouse is captured }
        FState := bsUp;
        FMousePosition := mpOutside;
        if DoClick and not (FState in [bsExclusive, bsDown]) then
          Invalidate;
      end
      else
        if DoClick then
        begin
          SetDown(not FDown);
          if FDown then Repaint;
        end
        else
        begin
          if FDown then FState := bsExclusive;
          Repaint;
        end;
      if DoClick then Click;
      UpdateTracking;
    end;

    dsDropBtn: begin
      //DropDownClick() was called on press
      if FDropBtnState = bsDown then begin
        FDropBtnState := bsUp;
        Invalidate;
      end;
      UpdateTracking;
    end;
  end;
end;

procedure TWinSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TWinSpeedButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
begin
  UpdateTracking;
  Repaint;
end;

procedure TWinSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
 //Similar to inherited TCustomButton's, but it used CanFocus and this can
 //be non-focusable
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TWinSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TWinSpeedButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TWinSpeedButton.CMSysColorChange(var Message: TMessage);
begin
  Invalidate;
end;

procedure TWinSpeedButton.CMMouseEnter(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  NeedRepaint := FFlat and (FMousePosition = mpOutside) and Enabled
    and (DragMode <> dmAutomatic) and (GetCapture = 0);

  { Windows XP introduced hot states also for non-flat buttons. }
  if (NeedRepaint or StyleServices.Enabled) and not (csDesigning in ComponentState) then
  begin
    UpdateTracking;
    if Enabled then
      Repaint;
  end;
end;

procedure TWinSpeedButton.CMMouseLeave(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  NeedRepaint := FFlat and (FMousePosition<>mpOutside) and Enabled and (FDragState=dsNone);
  { Windows XP introduced hot states also for non-flat buttons. }
  if NeedRepaint or StyleServices.Enabled then
  begin
    FMousePosition := mpOutside;
    if Enabled then
      Repaint;
  end;
end;

function TWinSpeedButton.CanFocus: Boolean;
begin
  Result := FFocusable and inherited;
end;

procedure TWinSpeedButton.SetFocus;
begin
  if FFocusable then inherited; //else nothing!
end;

function TWinSpeedButton.Focused: boolean;
begin
 //Only way to stop parent TButtonControl.WndProc's WM_LBUTTONDOWN processing
 //from setting Focus: tell that it's already set.
  if FFakeFocus then
    Result := true
  else
    Result := inherited;
end;

function TWinSpeedButton.IsInDropButton(const APoint: TPoint): boolean;
begin
  Result := (Self.Style = bsSplitButton)
    and PtInRect(DropBtnRect, APoint);
end;

procedure TWinSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TWinSpeedButton.SetDropButtonSettings(Value: TDropButtonSettings);
begin
  FDropButtonSettings.Assign(Value);
end;

procedure TWinSpeedButton.SetFocusable(Value: Boolean);
begin
  if Value <> FFocusable then
  begin
    FFocusable := Value;
    Invalidate;
  end;
end;

procedure TWinSpeedButton.SetForceClassicLook(Value: Boolean);
begin
  if Value <> FForceClassicLook then
  begin
    FForceClassicLook := Value;
    Invalidate;
  end;
end;

procedure TWinSpeedButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TWinSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TWinSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TWinSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TWinSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TWinSpeedButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TWinSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

end.
