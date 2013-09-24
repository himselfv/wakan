unit WindowPlacementTest_Control;
{ See comments in WindowPlacementTest.dpr }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    mmPla: TMemo;
    Button18: TButton;
    Button19: TButton;
    Label1: TLabel;
    lblStatus: TLabel;
    Label2: TLabel;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
  protected
    FLastPlacement: TWindowPlacement;
  end;

var
  Form1: TForm1;

implementation
uses WindowPlacementTest_Target;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TargetForm := TTargetForm.Create(nil);
  Button5Click(nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FreeAndNil(TargetForm);
  Button5Click(nil);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TargetForm.Show;
  Button5Click(nil);
end;

procedure TForm1.Button18Click(Sender: TObject);
begin
  TargetForm.Visible := true;
  Button5Click(nil);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TargetForm.Hide;
  Button5Click(nil);
end;

procedure TForm1.Button19Click(Sender: TObject);
begin
  TargetForm.BoundsRect := FLastPlacement.rcNormalPosition;
  Button5Click(nil);
end;

procedure TForm1.Button20Click(Sender: TObject);
begin
  TargetForm.WindowState := wsMaximized;
  Button5Click(nil);
end;

procedure TForm1.Button21Click(Sender: TObject);
begin
  TargetForm.WindowState := wsMinimized;
  Button5Click(nil);
end;

procedure TForm1.Button22Click(Sender: TObject);
begin
  TargetForm.WindowState := wsNormal;
  Button5Click(nil);
end;

procedure TForm1.Button5Click(Sender: TObject);
var stat: string;
begin
  if TargetForm=nil then begin
    lblStatus.Caption := 'nil';
    exit;
  end;

  if not TargetForm.HandleAllocated then
    stat := 'no handle'
  else
    stat := '0x'+IntToHex(TargetForm.Handle, 8);

  stat := stat + ', ';

  if not TargetForm.Visible then
    stat := stat + 'not Visible'
  else
    stat := stat + 'Visible';

  case TargetForm.WindowState of
    wsMaximized: stat := stat + ', Maximized';
    wsMinimized: stat := stat + ', Minimized';
  else
    stat := stat + ', Restored';
  end;

  lblStatus.Caption := stat;
end;

procedure TForm1.Button6Click(Sender: TObject);
var w: WINDOWPLACEMENT;
begin
  FillChar(w, SizeOf(w), 0);
  w.length := SizeOf(w);
  if not GetWindowPlacement(TargetForm.Handle, @w) then
    RaiseLastOsError;

  FLastPlacement := w;

  mmPla.Clear;

  mmPla.Lines.Add('flags: '+IntToStr(w.flags));
  mmPla.Lines.Add('showcmd: '+IntToStr(w.showCmd));
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FLastPlacement.showCmd := (Sender as TControl).Tag;
  SetWindowPlacement(TargetForm.Handle, @FLastPlacement);
end;

end.
