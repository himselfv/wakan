
{ -------------------------------------------------------
		Background Component   Copyright (r) by DreamFactory
    Version : 1.75   Author : William Yang
		Last Update 10 - Sep - 97
  ------------------------------------------------------ }

unit Backgnd;

interface

uses
  SysUtils, Windows, Messages, Classes, DrawMan,
  	Graphics, NumMan, Controls, ColorMan, Gradient, DFClasses;

type
  TBackStyle = (bgSingleColor, bgGradient, bgSingleBitmap ,bgTileBitmap);

  TBackgndObj = class(TPersistent)
  private
    fPicture : TBitmap;
    fColor: TColor;
    fBackStyle : TBackStyle;
    fGradient: TGradient;
    fOnChange: TNotifyEvent;

    procedure SetStyle(Value : TBackStyle);
    procedure SetPicture(Value : TBitmap);
    procedure SetGradient(Value: TGradient);
    procedure SetColor(Value: TColor);
    procedure GradChanged(Sender: TObject);
	public
  	constructor Create;
    destructor Destroy; override;

    procedure Assign(Value: TBackgndObj);
    procedure Changed;

 	published
		property Gradient: TGradient read fGradient write SetGradient;
    property Picture : TBitmap read FPicture write SetPicture;
    property BackStyle : TBackStyle read FBackStyle write SetStyle;
    property Color: TColor read fColor write SetColor;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TBackground = class(TDFGraphicControl)
  private
    { Private Declarations }
    fBackObj: TBackgndObj;
    procedure SetBackObj(Value: TBackgndObj);
    procedure BackObjChanged(Sender: TObject);
  protected
    { Protected Declarations }
    procedure PaintBackup; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
  	property BackObj: TBackgndObj read fBackObj write SetBackObj;
   	property Align;
    property Visible;
    property DragCursor;
    property DragMode;
    property Enabled;
    property PopupMenu;
    property ShowHint;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

  end;

procedure PaintBackObj(Canvas: TCanvas; Rect: TRect; BackObj: TBackgndObj);
procedure Register;

implementation

{ TBackgnd Object }
constructor TBackgndObj.Create;
begin
	inherited Create;
	fBackStyle := bgSingleColor;
  fGradient := TGradient.Create;
  fGradient.OnChange := GradChanged;
  fPicture := TBitmap.Create;
end;

destructor TBackgndObj.Destroy;
begin
	fGradient.Free;
	fPicture.Free;
end;

procedure TBackgndObj.Assign(Value: TBackgndObj);
begin
	fPicture.Assign(Value.Picture);
  fGradient.Assign(Value.Gradient);
  fBackStyle := Value.Backstyle;
  Changed;
end;

procedure TBackgndObj.Changed;
begin
	if Assigned(fOnChange) then fOnChange(Self);
end;

procedure TBackgndObj.GradChanged(Sender: TObject);
begin
	Changed;
end;

procedure TBackgndObj.SetGradient(Value: TGradient);
begin
  if fGradient <> Value then
  begin
  	fGradient.Assign(Value);
  	Changed;
  end;
end;

procedure TBackgndObj.SetStyle(Value: TBackStyle);
begin
  if fBackStyle <> Value then
  begin
  	fBackStyle := Value;
  	Changed;
  end;
end;

procedure TBackgndObj.SetPicture(Value: TBitmap);
begin
	if fPicture <> Value then
  begin
  	fPicture.Assign(Value);
  	Changed;
  end;
end;

procedure TBackgndObj.SetColor(Value: TColor);
begin
	if fColor <> Value then
  begin
  	fColor := Value;
    Changed;
  end;
end;

{ TBackground }

constructor TBackground.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBackObj := TBackgndObj.Create;
  fBackObj.OnChange := BackObjChanged;
  Setbounds(Left, Top, 144, 144);
end;

destructor TBackground.Destroy;
begin
  fBackObj.Free;
  inherited Destroy;
end;

procedure TBackground.Paintbackup;
begin
	inherited Paintbackup;
	PaintBackObj(Backup.Canvas, ClientRect, fBackObj);
end;

procedure TBackground.SetBackObj(Value: TBackgndObj);
begin
	if fBackObj <> Value then
  begin
  	fBackObj.Assign(Value);
    Paintbackup;
    Invalidate;
  end;
end;

procedure TBackground.BackObjChanged(Sender: TObject);
begin
	Paintbackup;
  Invalidate;
end;

procedure PaintBackObj(Canvas: TCanvas; Rect: TRect; BackObj: TBackgndObj);

	procedure DrawSinglePic;
  begin
		Canvas.CopyRect(Rect, BackObj.Picture.Canvas, Rect);
  end;

	procedure DrawTilePic;
	begin
   	if BackObj.Picture <> nil then
			MultiClipPaint(Canvas, BackObj.Picture, Rect, Rect);
  end; {Procedure}

  procedure DrawOneColor;
  begin
  	Canvas.Brush.Color := BackObj.Color;
  	Canvas.FillRect(Rect)
  end;

  procedure DrawGradient;
  var
  	Tmp: TBitmap;
  begin
  	Tmp := TBitmap.Create;
    Tmp.Width := Rect.Right-Rect.Left;
    Tmp.Height := Rect.Bottom-Rect.Top;
  	PaintGradient(Tmp.Canvas, Classes.Rect(0,0,Tmp.Width,Tmp.Height), BackObj.Gradient);
    Canvas.Draw(Rect.Left, Rect.Top, Tmp);
    Tmp.Free;
  end;
begin
	case BackObj.BackStyle of
		bgSingleColor : DrawOneColor;
  	bgGradient : DrawGradient;
		bgSingleBitmap : DrawSinglePic;
  	bgTileBitmap : DrawTilePic;
  end;
end;

procedure Register;
begin
  RegisterComponents('Grafix', [TBackground]);
end;

end.
