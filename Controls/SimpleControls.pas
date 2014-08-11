unit SimpleControls;

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Messages, Windows,
  Themes;

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
    - DropDown button and it's handling
    - Line between dropdown and normal parts
    - DropDown rendering for classic UI case
    - DropDown click handling (make it down, call DoDropDown, OnDropDown, bring
      up menu, turn it up)

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
  TMousePosition = (mpOutside, mpBody, mpDropBtn);
  TButtonFrameState = (bfUp, bfDown, bfHot, bfDefault, bfDisabled);

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

  TWinSpeedButton = class(TCustomPaintedButton)
  protected
    FAllowAllUp: Boolean;
    FDown: Boolean;
    FDragging: Boolean;
    FFocusable: Boolean;
    FFlat: Boolean;
    FGroupIndex: Integer;
    FLayout: TButtonLayout;
    FMargin: Integer; //distance from the anchor border to the glyph
    FMousePosition: TMousePosition;
    FPainter: TWinSpeedButtonPainter;
    FSpacing: Integer; //distance from the glyph to the caption
    FState: TButtonState;
    FDropState: TDropButtonState;
    FTransparent: Boolean;
    FFakeFocus: boolean; //see WndProc->WM_LBUTTONDOWN
    procedure UpdateExclusive;
    procedure UpdateTracking;
    procedure ButtonPressed(Group: Integer; Button: TWinSpeedButton);
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetDown(Value: Boolean);
    procedure SetFocusable(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure Paint; override;
    procedure PaintButtonFrame(State: TButtonFrameState; PaintRect, ClipRect: TRect;
      out Details: TThemedElementDetails; out ContentRect: TRect);
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
    function BodyRect: TRect;
    function DropBtnRect: TRect;
    property MousePosition: TMousePosition read FMousePosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    procedure SetFocus; override;
    function Focused: boolean; override;
    procedure DefaultHandler(var Message); override;
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

    //Dubious
    property CommandLinkHint;
    property ElevationRequired;
  end;

  //Paints WinSpeedButton contents (glyph + caption) on a given canvas
  //Analogous to Buttons.TButtonGlyph, but does not host the glyph itself.
  TWinSpeedButtonPainter = class
  protected
    FButton: TWinSpeedButton;
    FPaintOnGlass: Boolean;
    FThemeDetails: TThemedElementDetails;
    FThemesEnabled: Boolean;
    FThemeTextColor: Boolean;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; Flags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: Longint);
  public
    constructor Create(AButton: TWinSpeedButton);
    function Draw(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
      BiDiFlags: LongInt): TRect;
  end;


procedure Register;

implementation
uses Types, UITypes, Forms, ActnList, UxTheme, CommCtrl,
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



{ TWinSpeedButton }

constructor TWinSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  inherited;
end;

function TWinSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TWinSpeedButtonActionLink;
end;

function TWinSpeedButton.BodyRect: TRect;
begin
  Result := ClientRect;
  if Self.Style = bsSplitButton then
    Result.Right := Result.Right - 15;
end;

function TWinSpeedButton.DropBtnRect: TRect;
begin
  Result := ClientRect;
  if Self.Style = bsSplitButton then
    Result.Left := Result.Right - 15
  else
    Result.Left := Result.Right;
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
  DrawFlags: Integer;
  Offset: TPoint;
  LGlassPaint: Boolean;
  Button: TButtonFrameState;
  Details: TThemedElementDetails;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDropState := dbsUp;
    FDragging := False;
  end
  else if FState = bsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  Canvas.Font := Self.Font;


  if ThemeControl(Self) then
  begin
    //Background
    LGlassPaint := DoGlassPaint;
    if not LGlassPaint then
      if Transparent then
        StyleServices.DrawParentBackground(0, Canvas.Handle, nil, True)
      else
        PerformEraseBackground(Self, Canvas.Handle)
    else
      FillRect(Canvas.Handle, ClientRect, GetStockObject(BLACK_BRUSH));

    //Paint DropBtn
    if Self.Style = bsSplitButton then begin
      if not Enabled then
        Button := bfDisabled
      else
        if FDropState in [dbsDown] then
          Button := bfDown
        else
          if FMousePosition in [mpBody, mpDropBtn] then
            Button := bfHot
          else
            if Self.Focused or Self.Default then
              Button := bfDefault
            else
              Button := bfUp;

      PaintButtonFrame(Button, ClientRect, DropBtnRect, Details, PaintRect);
      Canvas.Brush.Style := bsClear;
      PaintRect.Top := PaintRect.Top + (PaintRect.Height - Canvas.TextHeight('▼')) div 2;
      Canvas.TextOut(PaintRect.Left, PaintRect.Top, '▼');

    end;

    //Paint button body
    if not Enabled then
      Button := bfDisabled
    else
      if FState in [bsDown, bsExclusive] then
        Button := bfDown
      else
        if FMousePosition in [mpBody] then
          Button := bfHot
        else
          if Self.Focused or Self.Default then
            Button := bfDefault
          else
            Button := bfUp;

    PaintButtonFrame(Button, ClientRect, BodyRect, Details, PaintRect);

    Offset := Point(0, 0);
    if (Button = bfDown) and FFlat then
    begin
      // A pressed "flat" speed button has white text in XP, but the Themes
      // API won't render it as such, so we need to hack it.
      if not CheckWin32Version(6) then
        Canvas.Font.Color := clHighlightText
      else
        Offset := Point(1, 0);
    end;

    FPainter.FPaintOnGlass := LGlassPaint;
    FPainter.FThemeDetails := Details;
    FPainter.FThemesEnabled := True;
    FPainter.FThemeTextColor := seFont in StyleElements;
    FPainter.Draw(Canvas, PaintRect, Offset, Caption, FLayout,
      FMargin, FSpacing, FState, Transparent, DrawTextBiDiModeFlags(0));

    if Self.Focused and StyleServices.IsSystemStyle then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, PaintRect);
    end;

  end
  else
  begin
    PaintRect := ClientRect;

    if not FFlat then
    begin
      DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if FState in [bsDown, bsExclusive] then
        DrawFlags := DrawFlags or DFCS_PUSHED;
      DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end
    else
    begin
      if (FState in [bsDown, bsExclusive]) or
        ((FMousePosition in [mpBody, mpDropBtn]) and (FState <> bsDisabled)) or
        (csDesigning in ComponentState) then
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[Transparent] or BF_RECT)
      else if not Transparent then
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(PaintRect);
      end;
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [bsDown, bsExclusive] then
    begin
      if (FState = bsExclusive) and (not FFlat or (FMousePosition in [mpOutside])) then
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

    FPainter.FThemesEnabled := StyleServices.Enabled;
    FPainter.Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, FState, Transparent, DrawTextBiDiModeFlags(0));

    if Self.Focused then begin
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Brush.Color := clBtnFace;
      Canvas.DrawFocusRect(PaintRect);
    end;

  end;

end;

//Paints themed button frame and returns theme details according to current
//control styling
//Non-themed styling is handled separately
procedure TWinSpeedButton.PaintButtonFrame(State: TButtonFrameState;
  PaintRect, ClipRect: TRect; out Details: TThemedElementDetails;
  out ContentRect: TRect);
var
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
begin
  if not FFlat then begin

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
      if not FFlat or ((State = bfDown) or (State = bfHot)) then
        StyleServices.DrawElement(Canvas.Handle, Details, PaintRect, ClipRect);
    end;

  end;

  StyleServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, ContentRect);

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

constructor TWinSpeedButtonPainter.Create(AButton: TWinSpeedButton);
begin
  inherited Create;
  Self.FButton := AButton;
end;

//Paints glyph and text contents inside the provided rectangle
function TWinSpeedButtonPainter.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: LongInt): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;

procedure TWinSpeedButtonPainter.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
  Details: TThemedElementDetails;
  R: TRect;
  Button: TThemedButton;
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  if FButton.Images = nil then Exit;
  if (FButton.Images.Width = 0) or (FButton.Images.Height = 0) then Exit;

  case State of
    bsUp: Index := FButton.ImageIndex;
    bsDown: begin
      Index := FButton.PressedImageIndex;
      if Index<0 then
        Index := FButton.ImageIndex;
    end;
    bsExclusive: begin
      Index := FButton.HotImageIndex;
      if Index<0 then
        Index := FButton.ImageIndex;
    end;
    bsDisabled: begin
      Index := FButton.DisabledImageIndex;
      if Index<0 then
        Index := FButton.ImageIndex;
    end
  else Index := -1;
  end;
  if Index < 0 then exit;

  R.Left := GlyphPos.X + FButton.ImageMargins.Left;
  R.Top := GlyphPos.Y + FButton.ImageMargins.Top;
  R.Right := R.Left + FButton.Images.Width;
  R.Bottom := R.Top + FButton.Images.Height;
  if FThemesEnabled then
  begin
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

    if FPaintOnGlass then
    begin
      PaintBuffer := BeginBufferedPaint(Canvas.Handle, R, BPBF_TOPDOWNDIB, nil, MemDC);
      try
        StyleServices.DrawIcon(MemDC, Details, R, FButton.Images.Handle, Index);
        BufferedPaintMakeOpaque(PaintBuffer, R);
      finally
        EndBufferedPaint(PaintBuffer, True);
      end;
    end
    else
      StyleServices.DrawIcon(Canvas.Handle, Details, R, FButton.Images.Handle, Index);
  end
  else
    if Transparent or (State = bsExclusive) then
    begin
      ImageList_DrawEx(FButton.Images.Handle, Index, Canvas.Handle, R.Left, R.Top, 0, 0,
        clNone, clNone, ILD_Transparent)
    end
    else
      ImageList_DrawEx(FButton.Images.Handle, Index, Canvas.Handle, R.Left, R.Top, 0, 0,
        ColorToRGB(clBtnFace), clNone, ILD_Normal);
end;

procedure TWinSpeedButtonPainter.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; Flags: LongInt);

  procedure DoDrawText(DC: HDC; const Text: UnicodeString;
    var TextRect: TRect; TextFlags: Cardinal);
  var
    LColor: TColor;
    LFormats: TTextFormat;
  begin
    if FThemesEnabled then
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
      StyleServices.DrawText(DC, FThemeDetails, Text, TextRect, LFormats, LColor);
    end
    else
      Windows.DrawText(DC, Text, Length(Text), TextRect, TextFlags);
  end;

begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if (State = bsDisabled) and not FThemesEnabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DoDrawText(Handle, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DoDrawText(Handle, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
    end
    else
      DoDrawText(Handle, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
  end;
end;

procedure TWinSpeedButtonPainter.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt);
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

  if (FButton.Images<>nil) and (FButton.ImageIndex>=0) then
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

procedure TWinSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TWinSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := bsUp
    else NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else
    UpdateTracking;
end;

procedure TWinSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
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
end;

procedure TWinSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  if FDown then DblClick;
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
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if Focusable then
        inherited DefaultHandler(Message)
      else begin
       //Do not do default handling which sets focus.
       //If there's anything else going on, reimplement it here.
      end
  else
    inherited DefaultHandler(Message);
  end;
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
  NeedRepaint := FFlat and (FMousePosition<>mpOutside) and Enabled and not FDragging;
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
