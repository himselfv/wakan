unit APIMan;

interface

uses Windows, Forms, Controls, SysUtils, NumMan,
	Classes, Messages, MMSystem, DrawMan;

//Wave can be a Filename or a resouce ID
function PlayWave(Wave: String): Boolean;
procedure DragForm(Handle: Integer);
procedure PopupForm(Form: TForm; Owner: TControl);
function PlayWaveWait(Wave: String): Boolean;

implementation

procedure PopupForm(Form: TForm; Owner: TControl);
var
  P: TPoint;
  Y: Integer;
begin
	if IsWindowVisible(Form.Handle) then
  	Form.Close
  else
  begin
    P := Owner.Parent.ClientToScreen(Point(Owner.Left, Owner.Top));
    Y := P.Y + Owner.Height;
    if Y + Form.Height > Screen.Height then
      Y := P.Y - Form.Height;
    Dec(P.X, Form.Width - Owner.Width);
    if P.X < 0 then Inc(P.X, Form.Width - Owner.Width);
    Form.Left := P.X;
    Form.Top := Y;
		Form.Show;
  end;
end;

procedure DragForm(Handle: Integer);
begin
	Releasecapture;
  SendMessage(Handle, WM_NCLButtonDown, 2, 0);
end;

function PlayWave(Wave: String): Boolean;
begin
	Result := True;
	if FileExists(Wave) then
		PlaySound(PChar(Wave), 0, SND_FILENAME or SND_ASYNC)
  else if FindResource(HINSTANCE, PChar(Wave), 'RT_RCDATA') <> 0 then
		PlaySound(PChar(Wave), 0, SND_RESOURCE or SND_ASYNC)
  else
  	Result := False;
end;

function PlayWaveWait(Wave: String): Boolean;
begin
	Result := True;
	if FileExists(Wave) then
		PlaySound(PChar(Wave), 0, SND_FILENAME or SND_SYNC)
  else if FindResource(HINSTANCE, PChar(Wave), 'RT_RCDATA') <> 0 then
		PlaySound(PChar(Wave), 0, SND_RESOURCE or SND_SYNC)
  else
  	Result := False;
end;

end.
