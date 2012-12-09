unit JWBStrokeOrder;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, rxAnimate, StdCtrls, ComCtrls, rxGIFCtrl;

type
  TfStrokeOrder = class(TForm)
    RxGIFAnimator1: TRxGIFAnimator;
    Bevel1: TBevel;
    SpeedButton3: TSpeedButton;
    Timer1: TTimer;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  public
    strokenum:integer;
    procedure GoUp;
    procedure GoDown;
  end;

var
  fStrokeOrder: TfStrokeOrder;

implementation

uses JWBKanji;

{$R *.DFM}

procedure TfStrokeOrder.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.SpeedButton4.Down:=false;
end;

procedure TfStrokeOrder.SpeedButton1Click(Sender: TObject);
begin
  GoDown;
end;

procedure TfStrokeOrder.SpeedButton2Click(Sender: TObject);
begin
  GoUp;
end;

procedure TfStrokeOrder.SpeedButton3Click(Sender: TObject);
begin
  Timer1.Enabled:=SpeedButton3.Down;
  TrackBar1.Enabled:=not SpeedButton3.Down;
end;

procedure TfStrokeOrder.Timer1Timer(Sender: TObject);
begin
  if TrackBar1.Max>0 then GoUp;
end;

procedure TfStrokeOrder.GoUp;
begin
  if RxGIFAnimator1.FrameIndex=strokenum then RxGIFAnimator1.FrameIndex:=0 else
    RxGIFAnimator1.FrameIndex:=RxGIFAnimator1.FrameIndex+1;
  TrackBar1.Position:=RxGIFAnimator1.FrameIndex;
end;

procedure TfStrokeOrder.GoDown;
begin
  if RxGIFAnimator1.FrameIndex=0 then RxGIFAnimator1.FrameIndex:=strokenum else
    RxGIFAnimator1.FrameIndex:=RxGIFAnimator1.FrameIndex-1;
  TrackBar1.Position:=RxGIFAnimator1.FrameIndex;
end;

procedure TfStrokeOrder.TrackBar1Change(Sender: TObject);
begin
  RxGIFAnimator1.FrameIndex:=TrackBar1.Position;
end;

end.
