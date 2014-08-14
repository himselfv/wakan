unit SimpleControls;

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Messages, Windows,
  Themes, ImgList, UITypes;

type
 { Adds Autosize like in TLabel }
  TJwbCheckbox = class(TCheckBox)
  protected
    FAutoSize: Boolean;
    procedure AdjustBounds; dynamic;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
  end;

 { Label which opens URL when clicked; blue and hand-pointed by default }
  TUrlLabel = class(TLabel)
  protected
    FURL: string;
    procedure Click; override;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged);
      message CM_PARENTFONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property URL: string read FURL write FURL;
  end;

 { TPanel with some routines for popup-menu like support }
  TPopupPanel = class(TPanel)
  protected
    procedure CMCancelMode(var message: TCMCancelMode); message CM_CANCELMODE;
    procedure WndProc(var Message: TMessage); override;
  public
    procedure Popup;
  end;

 {  TWinSpeedButton

  Similar to TSpeedButton but based on windowed TButton, with nicer
  ImageList-based images and platform-independent Down / DropDownList support.

  Most of the code is based on TSpeedButton

  TODO:
    - DropDown rendering for classic UI case (incl. splitter, glyph)
    - Make DropButtonSettings.*ImageIndex params behave as ImageIndex params in
      ObjectInspector
    - Render splitter through theming mechanics in themed case

  Possible improvements:
    - Use normal Button fading mechanics (try moving focus away and watch)
    - Use normal Button focus mechanics (when you focus by clicking, there's
     highlight but no focus rect. Only keyboard brings focus rect)
    - Draw in DrawItem() handler, this allows for ODS_FOCUS handling too (see
     how BitBtn.DrawItem does it, with no success though)
  }

  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TButtonState = (bsUp, bsDisabled, bsDown, bsExclusive);
  TDropButtonState = (dbsUp, dbsDown);
  TButtonFrameState = (bfUp, bfDown, bfHot, bfDefault, bfDisabled);
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
  TWinSpeedButtonPainter = class;

  TWinSpeedButtonActionLink = class(TPushButtonActionLink)
  protected
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetGroupIndex(Value: Integer); override;
  end;

  TDropButtonSettings = class(TPersistent)
  protected
    FButton: TWinSpeedButton;
    FSize: integer;
    FShowSplit: boolean;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FHotImageIndex: TImageIndex;
    FPressedImageIndex: TImageIndex;
    FDisabledImageIndex: TImageIndex;
    FSelectedImageIndex: TImageIndex;
    FStylusHotImageIndex: TImageIndex;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
    procedure SetSize(const Value: integer);
    procedure SetShowSplit(const Value: boolean);
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
    property ShowSplit: boolean read FShowSplit write SetShowSplit default true;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property HotImageIndex: TImageIndex read FHotImageIndex write SetHotImageIndex default -1;
    property PressedImageIndex: TImageIndex read FPressedImageIndex write SetPressedImageIndex default -1;
    property DisabledImageIndex: TImageIndex read FDisabledImageIndex write SetDisabledImageIndex default -1;
    property SelectedImageIndex: TImageIndex read FSelectedImageIndex write SetSelectedImageIndex default -1;
    property StylusHotImageIndex: TImageIndex read FStylusHotImageIndex write SetStylusHotImageIndex default -1;
  end;

  TWinSpeedButton = class(TCustomPaintedButton)
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
    FPainter: TWinSpeedButtonPainter;
    FDropButtonSettings: TDropButtonSettings;
    FSpacing: Integer; //distance from the glyph to the caption
    FState: TButtonState;
    FDropState: TDropButtonState;
    FTransparent: Boolean;
    FFakeFocus: boolean; //see WndProc->WM_LBUTTONDOWN
    procedure Paint; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetBodyRect: TRect;
    function GetDropBtnRect: TRect;
    procedure DoDropDownClick; virtual;
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetDown(Value: Boolean);
    procedure SetDropButtonSettings(Value: TDropButtonSettings);
    procedure SetFocusable(Value: Boolean);
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
  //Different implementations support themed or classic and normal/flat look
  TWinSpeedButtonPainter = class
  protected
    FButton: TWinSpeedButton;
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
    procedure DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
      out PaintRect: TRect; out Offset: TPoint); virtual;
    procedure DoDrawText(Canvas: TCanvas; State: TButtonState; const Text: UnicodeString;
      var TextRect: TRect; TextFlags: Cardinal); virtual;
  public
    constructor Create(AButton: TWinSpeedButton);

  end;

  TThemedPainter = class(TWinSpeedButtonPainter)
  protected
    FPaintOnGlass: Boolean;
    FThemeDetails: TThemedElementDetails;
    FThemeTextColor: Boolean;
    procedure DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
      const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
      ImageIndex: TImageIndex; Transparent: boolean); override;
    procedure DrawButtonFrame(Canvas: TCanvas; State: TButtonFrameState;
      PaintRect, ClipRect: TRect; out Details: TThemedElementDetails;
      out ContentRect: TRect);
    procedure DrawDropGlyph(Canvas: TCanvas; State: TButtonFrameState;
      PaintRect: TRect; Details: TThemedElementDetails);
    procedure DrawBackground(Canvas: TCanvas); override;
    procedure DrawDropButton(Canvas: TCanvas); override;
    procedure DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
      out PaintRect: TRect; out Offset: TPoint); override;
    procedure DoDrawText(Canvas: TCanvas; State: TButtonState; const Text: UnicodeString;
      var TextRect: TRect; TextFlags: Cardinal); override;
  end;

  TClassicPainter = class(TWinSpeedButtonPainter)
  protected
    procedure DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
      const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
      ImageIndex: TImageIndex; Transparent: boolean); override;
    procedure DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
      out PaintRect: TRect; out Offset: TPoint); override;
    procedure DoDrawText(Canvas: TCanvas; State: TButtonState; const Text: UnicodeString;
      var TextRect: TRect; TextFlags: Cardinal); override;
  end;

procedure Register;

implementation
uses Types, Forms, ActnList, UxTheme, CommCtrl,
{$IFDEF MSWINDOWS}
  ShellAPI
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib
{$ENDIF POSIX};

procedure Register;
begin
  RegisterComponents('Wakan', [TJwbCheckbox]);
  RegisterComponents('Wakan', [TUrlLabel]);
  RegisterComponents('Wakan', [TPopupPanel]);
  RegisterComponents('Wakan', [TWinSpeedButton]);
end;


{ JwbCheckbox }

constructor TJwbCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
end;

//See TCustomLabel.SetAutoSize
procedure TJwbCheckbox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

procedure TJwbCheckbox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;

//See TCustomLabel.AdjustBounds
procedure TJwbCheckbox.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  X, W: Integer;
  Canvas: TControlCanvas;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    X := Left;
    Canvas := TControlCanvas.Create;
    try
      Canvas.Control := Self;
      Canvas.Font := Self.Font;
      W := GetSystemMetrics(SM_CXMENUCHECK) + 4 + Canvas.TextWidth(Self.Caption);
      SetBounds(X, Top, W, Height);
    finally
      FreeAndNil(Canvas);
    end;
  end;
end;


{ URL label }

constructor TUrlLabel.Create(AOwner: TComponent);
begin
  inherited;
  Self.Cursor := crHandPoint;
  Self.Font.Style := Self.Font.Style + [fsBold, fsUnderline];
  Self.Font.Color := clHighlight;
end;

procedure TUrlLabel.CMParentFontChanged(var Message: TCMParentFontChanged);
var ChangeEvent: TNotifyEvent;
begin
  inherited; //copies font from parent; sets FParentFont := true;
  if Self.ParentFont then begin
   //Auto-upgrade font but try not to break FParentFont==true
    ChangeEvent := Self.Font.OnChange;
    Self.Font.OnChange := nil;
    try
      Self.Font.Style := Self.Font.Style + [fsBold, fsUnderline];
      Self.Font.Color := clHighlight;
    finally
      Self.Font.OnChange := ChangeEvent;
    end;
  end;
end;

procedure ShellOpen(const sCommand: string; const sParams: string = '');
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(sCommand), PChar(sParams), '', SW_SHOW);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  if sParams<>'' then
    _system(PAnsiChar('open ' + AnsiString(sCommand)+' '+AnsiString(sParams)))
  else
    _system(PAnsiChar('open ' + AnsiString(sCommand)));
{$ENDIF POSIX}
end;

procedure TUrlLabel.Click;
begin
  if URL<>'' then
    ShellOpen(URL);
  inherited;
end;


{ TPopupPanel }

function IsChildrenOf(AControl, AParent: TControl): boolean;
begin
  while AControl<>nil do begin
    if AControl=AParent then begin
      Result := true;
      exit;
    end;
    AControl := AControl.Parent;
  end;
  Result := false;
end;

function MsgToStr(const AMsg: integer): string;
begin
  case AMsg of
    WM_ACTIVATE: Result := 'WM_ACTIVATE';
    WM_ACTIVATEAPP: Result := 'WM_ACTIVATEAPP';
    WM_SETFOCUS: Result := 'WM_SETFOCUS';
    WM_KILLFOCUS: Result := 'WM_KILLFOCUS';
    WM_LBUTTONDOWN: Result := 'WM_LBUTTONDOWN';
    WM_LBUTTONUP: Result := 'WM_LBUTTONUP';
    WM_LBUTTONDBLCLK: Result := 'WM_LBUTTONDBLCLK';
    WM_RBUTTONDOWN: Result := 'WM_RBUTTONDOWN';
    WM_RBUTTONUP: Result := 'WM_RBUTTONUP';
    WM_RBUTTONDBLCLK: Result := 'WM_RBUTTONDBLCLK';
    WM_MBUTTONDOWN: Result := 'WM_MBUTTONDOWN';
    WM_MBUTTONUP: Result := 'WM_MBUTTONUP';
    WM_MBUTTONDBLCLK: Result := 'WM_MBUTTONDBLCLK';
    WM_XBUTTONDOWN: Result := 'WM_XBUTTONDOWN';
    WM_XBUTTONUP: Result := 'WM_XBUTTONUP';
    WM_XBUTTONDBLCLK: Result := 'WM_XBUTTONDBLCLK';
    WM_KEYDOWN: Result := 'WM_KEYDOWN';
    WM_KEYUP: Result := 'WM_KEYUP';
    WM_CANCELMODE: Result := 'WM_CANCELMODE';
    WM_GETTEXTLENGTH: Result := 'WM_GETTEXTLENGTH';
    WM_ERASEBKGND: Result := 'WM_ERASEBKGND';
    WM_CTLCOLOREDIT: Result := 'WM_CTLCOLOREDIT';
    WM_COMMAND: Result := 'WM_COMMAND';
    WM_IME_NOTIFY: Result := 'WM_IME_NOTIFY';
    WM_IME_SETCONTEXT: Result := 'WM_IME_SETCONTEXT';
    WM_NCHITTEST: Result := 'WM_NCHITTEST';
    WM_PRINTCLIENT: Result := 'WM_PRINTCLIENT';
    WM_SETCURSOR: Result := 'WM_SETCURSOR';
    WM_TIMER: Result := 'WM_TIMER';
    WM_MOUSEMOVE: Result := 'WM_MOUSEMOVE';
    WM_NCMOUSEMOVE: Result := 'WM_NCMOUSEMOVE';
    WM_PAINT: Result := 'WM_PAINT';
    CM_ACTIVATE: Result := 'CM_ACTIVATE';
    CM_DEACTIVATE: Result := 'CM_DEACTIVATE';
    CM_TEXTCHANGED: Result := 'CM_TEXTCHANGED';
    CM_MOUSEENTER: Result := 'CM_MOUSEENTER';
    CM_MOUSELEAVE: Result := 'CM_MOUSELEAVE';
    CM_HINTSHOW: Result := 'CM_HINTSHOW';
    CM_HINTSHOWPAUSE: Result := 'CM_HINTSHOWPAUSE';
  else Result := IntToStr(AMsg);
  end;
end;

function IsHiddenMsg(const AMsg: integer): boolean;
begin
  case AMsg of
    WM_TIMER,
    WM_MOUSEMOVE,
    WM_NCMOUSEMOVE,
    WM_NCHITTEST,
    WM_PAINT,
    WM_SETCURSOR,
    WM_CTLCOLOREDIT,
    CM_HINTSHOWPAUSE:
      Result := true;
  else Result := false;
  end;
end;

procedure TPopupPanel.Popup;
var Msg: TMsg;
begin
  try
    Self.Show;
    Self.SetFocus;
    Self.SelectFirst;
    repeat
      try
        if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
{          if not IsHiddenMsg(Msg.message) then
            OutputDebugString(PChar('Msg: '+MsgToStr(Msg.message))); }

          if (Msg.message=WM_ACTIVATEAPP) and (Msg.wParam=0) then
            break;
          if (Msg.message=WM_ACTIVATE) and (Msg.wParam=WA_INACTIVE) and IsChildrenOf(Self, FindControl(Msg.hwnd)) then
            break;
          if (Msg.message=WM_KILLFOCUS) and IsChildrenOf(FindControl(msg.hwnd), Self)
          and ((Msg.wParam=0) or not IsChildrenOf(FindControl(Msg.wParam), Self)) then
            break;
          if ((Msg.message=WM_LBUTTONDOWN) or (Msg.message=WM_LBUTTONDBLCLK)
          or (Msg.message=WM_RBUTTONDOWN) or (Msg.message=WM_RBUTTONDBLCLK)
          or (Msg.message=WM_MBUTTONDOWN) or (Msg.message=WM_MBUTTONDBLCLK)
          or (Msg.message=WM_XBUTTONDOWN) or (Msg.message=WM_XBUTTONDBLCLK))
          and not IsChildrenOf(FindControl(Msg.hwnd), Self) then
            break;
          if (Msg.message=WM_KEYDOWN) and (Msg.lParam=VK_ESCAPE) then
            break;
          if (Msg.message=WM_CANCELMODE) and IsChildrenOf(FindControl(Msg.hwnd), Self) then
            break;

          Application.HandleMessage;
        end;
      except
        Application.HandleException(Self);
      end;
    until Application.Terminated;
  finally
    Self.Hide;
  end;
end;

procedure TPopupPanel.CMCancelMode(var message: TCMCancelMode);
begin
  if not IsChildrenOf(message.Sender, Self) then
    Self.Hide;
end;

procedure TPopupPanel.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
{  if not IsHiddenMsg(message.Msg) then
    OutputDebugString(PChar('Panel msg: '+MsgToStr(Message.Msg))); }
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
  FShowSplit := true;
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
    TDropButtonSettings(Dest).FShowSplit := Self.FShowSplit;
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

procedure TDropButtonSettings.SetSize(const Value: integer);
begin
  FSize := Value;
  Changed;
end;

procedure TDropButtonSettings.SetShowSplit(const Value: boolean);
begin
  FShowSplit := Value;
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

constructor TWinSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropButtonSettings := TDropButtonSettings.Create(Self);
  FPainter := TWinSpeedButtonPainter.Create(Self);
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
  Self.EndDrag(0, 0); //Popup or other code could have eaten WM_MOUSEUP
end;

const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);

procedure TWinSpeedButton.Paint;

  function DoGlassPaint: Boolean;
  var
    LParent: TWinControl;
  begin
    Result := csGlassPaint in ControlState;
    if Result then
    begin
      LParent := Parent;
      while (LParent <> nil) and not LParent.DoubleBuffered do
        LParent := LParent.Parent;
      Result := (LParent = nil) or not LParent.DoubleBuffered or (LParent is TCustomForm);
    end;
  end;

var
  PaintRect: TRect;
  GlyphPos: TPoint;
  Offset: TPoint;
  Details: TThemedElementDetails;
  TextBounds: TRect;
  BiDiFlags: integer;

begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDropState := dbsUp;
    FDragState := dsNone;
  end
  else if FState = bsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  Canvas.Font := Self.Font;

  //Update painter
  if ThemeControl(Self) then begin
    if not (FPainter is TThemedPainter) then begin
      FreeAndNil(FPainter);
      FPainter := TThemedPainter.Create(Self);
      TThemedPainter(FPainter).FPaintOnGlass := DoGlassPaint();
    end;
  end else begin
    if not (FPainter is TClassicPainter) then begin
      FreeAndNil(FPainter);
      FPainter := TClassicPainter.Create(Self);
    end;
  end;

  FPainter.DrawBackground(Canvas);

  if Self.Style = bsSplitButton then
    FPainter.DrawDropButton(Canvas);
  FPainter.DrawBody(Canvas, Details, PaintRect, Offset);

  BiDiFlags := Self.DrawTextBiDiModeFlags(0);

  FPainter.CalcButtonLayout(Canvas, PaintRect, Offset, Self.Caption, Self.FLayout,
    Self.FMargin, Self.FSpacing, Self.FState, GlyphPos, TextBounds,
    BiDiFlags);
  FPainter.DrawButtonGlyph(Canvas, GlyphPos, Self.FState, Self.Transparent);
  FPainter.DrawButtonText(Canvas, Self.Caption, TextBounds, Self.FState, BiDiFlags);

  if Self.Focused and (not (FPainter is TThemedPainter) or StyleServices.IsSystemStyle) then begin
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Color := clBtnFace;
    DrawFocusRect(Canvas.Handle, PaintRect);
  end;

end;


{ Painters }

constructor TWinSpeedButtonPainter.Create(AButton: TWinSpeedButton);
begin
  inherited Create;
  Self.FButton := AButton;
end;

procedure TWinSpeedButtonPainter.DrawBackground(Canvas: TCanvas);
begin
end;

procedure TWinSpeedButtonPainter.DrawDropButton(Canvas: TCanvas);
begin
end;

procedure TWinSpeedButtonPainter.DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
  out PaintRect: TRect; out Offset: TPoint);
begin
end;

procedure TWinSpeedButtonPainter.DoDrawText(Canvas: TCanvas; State: TButtonState;
  const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
begin
end;

procedure TThemedPainter.DrawBackground(Canvas: TCanvas);
begin
  if not FPaintOnGlass then
    if FButton.Transparent then
      StyleServices.DrawParentBackground(0, Canvas.Handle, nil, True)
    else
      PerformEraseBackground(FButton, Canvas.Handle)
  else
    FillRect(Canvas.Handle, FButton.ClientRect, GetStockObject(BLACK_BRUSH));
end;

procedure TThemedPainter.DrawDropButton(Canvas: TCanvas);
var State: TButtonFrameState;
  Details: TThemedElementDetails;
  PaintRect: TRect;
begin
  if not FButton.Enabled then
    State := bfDisabled
  else
    if FButton.FDropState in [dbsDown] then
      State := bfDown
    else
      if FButton.FMousePosition in [mpBody, mpDropBtn] then
        State := bfHot
      else
        if FButton.Focused or FButton.Default then
          State := bfDefault
        else
          State := bfUp;

  DrawButtonFrame(Canvas, State, FButton.ClientRect, FButton.DropBtnRect,
    Details, PaintRect);

  //Paint glyph
  DrawDropGlyph(Canvas, State, PaintRect, Details);

  if FButton.DropButtonSettings.ShowSplit then begin
    //Paint splitter
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clSilver;
    Canvas.MoveTo(PaintRect.Left, PaintRect.Top);
    Canvas.LineTo(PaintRect.Left, PaintRect.Bottom);
    Inc(PaintRect.Left, 1);
    Canvas.Pen.Color := clGray;
    Canvas.MoveTo(PaintRect.Left, PaintRect.Top);
    Canvas.LineTo(PaintRect.Left, PaintRect.Bottom);
    Inc(PaintRect.Right, 1);
  end;
end;

procedure TThemedPainter.DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
  out PaintRect: TRect; out Offset: TPoint);
var State: TButtonFrameState;
begin
  if not FButton.Enabled then
    State := bfDisabled
  else
    if FButton.FState in [bsDown, bsExclusive] then
      State := bfDown
    else
      if FButton.FMousePosition in [mpBody] then
        State := bfHot
      else
        if FButton.Focused or FButton.Default then
          State := bfDefault
        else
          State := bfUp;

  DrawButtonFrame(Canvas, State, FButton.ClientRect, FButton.BodyRect,
    Details, PaintRect);

  Offset := Point(0, 0);
  if (State = bfDown) and FButton.FFlat then
  begin
    // A pressed "flat" speed button has white text in XP, but the Themes
    // API won't render it as such, so we need to hack it.
    if not CheckWin32Version(6) then
      Canvas.Font.Color := clHighlightText
    else
      Offset := Point(1, 0);
  end;

  if (FButton.Style = bsSplitButton) and FButton.DropButtonSettings.ShowSplit then
    Dec(PaintRect.Right, 2);

  Self.FThemeDetails := Details;
  Self.FThemeTextColor := seFont in FButton.StyleElements;
end;

procedure TThemedPainter.DoDrawText(Canvas: TCanvas; State: TButtonState;
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

procedure TClassicPainter.DrawBody(Canvas: TCanvas; out Details: TThemedElementDetails;
  out PaintRect: TRect; out Offset: TPoint);
var DrawFlags: Integer;
begin
  PaintRect := FButton.ClientRect;

  if not FButton.FFlat then
  begin
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if FButton.FState in [bsDown, bsExclusive] then
      DrawFlags := DrawFlags or DFCS_PUSHED;
    DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
  end
  else
  begin
    if (FButton.FState in [bsDown, bsExclusive]) or
      ((FButton.FMousePosition in [mpBody, mpDropBtn]) and (FButton.FState <> bsDisabled)) or
      (csDesigning in FButton.ComponentState) then
      DrawEdge(Canvas.Handle, PaintRect, DownStyles[FButton.FState in [bsDown, bsExclusive]],
        FillStyles[FButton.Transparent] or BF_RECT)
    else if not FButton.Transparent then
    begin
      Canvas.Brush.Color := FButton.Color;
      Canvas.FillRect(PaintRect);
    end;
    InflateRect(PaintRect, -1, -1);
  end;
  if FButton.FState in [bsDown, bsExclusive] then
  begin
    if (FButton.FState = bsExclusive) and (not FButton.FFlat or (FButton.FMousePosition in [mpOutside])) then
    begin
      Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
      Canvas.FillRect(PaintRect);
    end;
    Offset.X := 1;
    Offset.Y := 1;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;
end;

procedure TClassicPainter.DoDrawText(Canvas: TCanvas; State: TButtonState;
  const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
begin
  Windows.DrawText(Canvas.Handle, Text, Length(Text), TextRect, TextFlags);
end;




//Paints themed button frame and returns theme details according to current
//control styling
//Non-themed styling is handled separately
procedure TThemedPainter.DrawButtonFrame(Canvas: TCanvas;
  State: TButtonFrameState; PaintRect, ClipRect: TRect;
  out Details: TThemedElementDetails; out ContentRect: TRect);
var
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
begin
  if not FButton.FFlat then begin

    case State of
      bfDown: Button := tbPushButtonPressed;
      bfHot: Button := tbPushButtonHot;
      bfDefault: Button := tbPushButtonDefaulted;
      bfDisabled: Button := tbPushButtonDisabled;
    else Button := tbPushButtonNormal;
    end;

    Details := StyleServices.GetElementDetails(Button);
    StyleServices.DrawElement(Canvas.Handle, Details, PaintRect, ClipRect);

  end else begin

    case State of
      bfDown: ToolButton := ttbButtonPressed;
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
      // the hot and pressed states , leaving normal/disabled to appear flat.
      if not FButton.FFlat or ((State = bfDown) or (State = bfHot)) then
        StyleServices.DrawElement(Canvas.Handle, Details, PaintRect, ClipRect);
    end;

  end;

  StyleServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, ContentRect);

  //Toolbar content rect is returned with borders, which is bad
  if FButton.FFlat then
    InflateRect(ContentRect, -2, -2);

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

procedure TThemedPainter.DrawDropGlyph(Canvas: TCanvas; State: TButtonFrameState;
  PaintRect: TRect; Details: TThemedElementDetails);
var ImgList: TCustomImageList;
  ImgIndex: integer;
  GlyphPos: TPoint;
begin
  case State of
    bfDown: ImgIndex := FButton.DropButtonSettings.PressedImageIndex;
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

  if (ImgIndex < 0) or (ImgList = nil) then begin
    Canvas.Brush.Style := bsClear;
    GlyphPos.X := PaintRect.Right - Canvas.TextWidth('▼');
    GlyphPos.Y := PaintRect.Top + (PaintRect.Height - Canvas.TextHeight('▼')) div 2;
    Canvas.TextOut(GlyphPos.X, GlyphPos.Y, '▼');
  end else begin
    GlyphPos.X := PaintRect.Left + (PaintRect.Width - ImgList.Width) div 2;
    GlyphPos.Y := PaintRect.Top + (PaintRect.Height - ImgList.Height) div 2;

    DrawGlyph(Canvas, Details, GlyphPos, FButton.ImageMargins, ImgList, ImgIndex,
      FButton.Transparent);

    ImageList_DrawEx(ImgList.Handle, ImgIndex, Canvas.Handle, GlyphPos.X,
      GlyphPos.Y, 0, 0, clNone, clNone, ILD_Transparent);
  end;

(*
  Button: TThemedButton;
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

  Transparent  {or (State = bsExclusive)}
*)
end;

procedure TWinSpeedButtonPainter.DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
  const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
  ImageIndex: TImageIndex; Transparent: boolean);
begin
end;



procedure TThemedPainter.DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
  const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
  ImageIndex: TImageIndex; Transparent: boolean);
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

procedure TClassicPainter.DrawGlyph(Canvas: TCanvas; Details: TThemedElementDetails;
  const GlyphPos: TPoint; Margins: TImageMargins; Images: TCustomImageList;
  ImageIndex: TImageIndex; Transparent: boolean);
var R: TRect;
begin
  if Transparent then
  begin
    ImageList_DrawEx(Images.Handle, ImageIndex, Canvas.Handle, R.Left, R.Top, 0, 0,
      clNone, clNone, ILD_Transparent)
  end
  else
    ImageList_DrawEx(Images.Handle, ImageIndex, Canvas.Handle, R.Left, R.Top, 0, 0,
      ColorToRGB(clBtnFace), clNone, ILD_Normal);
end;

function TWinSpeedButtonPainter.ImageIndexFromState(State: TButtonState): integer;
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

procedure TWinSpeedButtonPainter.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
  Details: TThemedElementDetails;
  Button: TThemedButton;
begin
  if FButton.Images = nil then Exit;
  if (FButton.Images.Width = 0) or (FButton.Images.Height = 0) then Exit;

  Index := ImageIndexFromState(State);
  if Index < 0 then exit;

  if Self is TThemedPainter then begin
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

procedure TWinSpeedButtonPainter.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; Flags: LongInt);
begin
  Canvas.Brush.Style := bsClear;
  if (State = bsDisabled) and not (Self is TThemedPainter) then
  begin
    OffsetRect(TextBounds, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DoDrawText(Canvas, State, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
    OffsetRect(TextBounds, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DoDrawText(Canvas, State, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
  end
  else
    DoDrawText(Canvas, State, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
end;

procedure TWinSpeedButtonPainter.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
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
      FFakeFocus := false;
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
     //clicking on different button parts (we do it differently).
     //So just don't call it.
    end;

  else
    inherited DefaultHandler(Message);
  end;
end;

procedure TWinSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if IsInDropButton(Point(X, Y)) then begin
      FDragState := dsDropBtn;
      FDropState := dbsDown;
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
  NewState: TButtonState;
  NewDropState: TDropButtonState;
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
        NewDropState := dbsDown
      else
        NewDropState := dbsUp;
      if NewDropState <> FDropState then begin
        FDropState := NewDropState;
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
      //DropDownClick() called on press
      if FDropState = dbsDown then begin
        FDropState := dbsUp;
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
