unit FormPlacemnt;
{
Simple form placement storage.
}

interface
uses SysUtils, Classes, Types, IniFiles, Registry;

type
  TFormPlacementOption = (foPosition, foVisibility);
  TFormPlacementOptions = set of TFormPlacementOption;

  TRestoreOption = (
    roActivate,   { position AND activate the form. When you position the main
      form while starting the application, if you don't activate it you might
      lose activation permission }
    roJustWrite   { do not call SetWindowPlacement, just write settings to the
      Delphi form object. If the form is yet to be created, it will be created
      with the new settings }
  );
  TRestoreOptions = set of TRestoreOption;

  TFormPlacement = class(TComponent)
  protected
    FUseRegistry: boolean;
    FIniFileName: string;
    FIniSection: string;
    FOptions: TFormPlacementOptions;
    FPlacementRestored: boolean; {
    Set to true on each successful RestorePlacement, set to false manually
    to track when form placement is valid and should be saved }
    function OpenIniFile: TCustomIniFile;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveFormPlacement;
    procedure RestoreFormPlacement(AOptions: TRestoreOptions);
    property PlacementRestored: boolean read FPlacementRestored write FPlacementRestored;
  published
    property UseRegistry: boolean read FUseRegistry write FUseRegistry default true;
    property IniFileName: string read FIniFileName write FIniFileName;
    property IniSection: string read FIniSection write FIniSection;
    property Options: TFormPlacementOptions read FOptions write FOptions default [foPosition, foVisibility];
  end;

procedure Register;

implementation
uses Forms, Windows;

constructor TFormPlacement.Create(AOwner: TComponent);
begin
  inherited;
  FUseRegistry := true;
  FIniFileName := '';
  FIniSection := '';
  FOptions := [foPosition, foVisibility];
end;

function TFormPlacement.OpenIniFile: TCustomIniFile;
begin
  if FUseRegistry then
    Result := TRegistryIniFile.Create(FIniFileName)
  else
    Result := TMemIniFile.Create(FIniFileName);
end;

type
  TStringArray = array of string;

function SplitStr(s: string; ch: char=','): TStringArray;
var i: integer;
begin
  SetLength(Result, 0);
  i := pos(ch,s);
  while i>0 do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := copy(s, 1, i-1);
    delete(s, 1, i);
    i := pos(ch,s);
  end;
  if s<>'' then begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := s;
  end;
end;

type
  TIniHelper = class helper for TCustomIniFile
  public
    function TryReadRect(const Section, Param: string; out Value: TRect): boolean;
    procedure WriteRect(const Section, Param: string; const Value: TRect);
  end;

function TIniHelper.TryReadRect(const Section, Param: string; out Value: TRect): boolean;
var str: string;
  parts: TStringArray;
begin
  Result := false;

  str := Self.ReadString(Section,'NormPos','');
  if str='' then exit;

  parts := SplitStr(str,',');
  if Length(parts)<>4 then exit;

  if not TryStrToInt(parts[0],Value.Left)
  or not TryStrToInt(parts[1],Value.Top)
  or not TryStrToInt(parts[2],Value.Right)
  or not TryStrToInt(parts[3],Value.Bottom) then exit;

  Result := true;
end;

procedure TIniHelper.WriteRect(const Section, Param: string; const Value: TRect);
begin
  Self.WriteString(Section,Param,
     Format('%d,%d,%d,%d',[Value.Left,Value.Top,Value.Right,Value.Bottom])
  );
end;

{ GetWindowPlacement does not return SW_HIDE when the form is hidden,
 it always returns SW_SHOW*SOMETHING (source: MSDN).
 Use this function to adjust showCmd returned by GetWindowPlacement to properly
 reflect "what needs to be done with the window to bring it to current state".
 Activation is handled separately. }
function GetRealShowCmd(const AShowCmd: integer; AForm: TCustomForm): integer;
begin
  Result := AShowCmd;
  if not TForm(AForm).Visible then
    Result := SW_HIDE
  else //Delphi says it's visible, let's believe Delphi
    if Result<>SW_HIDE then
     //We could've just chosen SW_SHOWNORMAL for all cases but whatever
      case TForm(AForm).WindowState of
        wsMaximized: Result := SW_SHOWMAXIMIZED;
        wsMinimized: Result := SW_SHOWMINIMIZED;
      else Result := SW_SHOWNORMAL;
      end;
end;

procedure TFormPlacement.SaveFormPlacement;
var ini: TCustomIniFile;
  form: TCustomForm;
  wp: TWindowPlacement;
  rc: TRect;
begin
  form := self.Owner as TCustomForm;
  ini := OpenIniFile;
  try
    GetWindowPlacement(form.Handle, wp);
    ini.WriteRect(FIniSection,'NormPos',wp.rcNormalPosition);
    rc.TopLeft := wp.ptMinPosition;
    rc.BottomRight := wp.ptMaxPosition;
    ini.WriteRect(FIniSection,'MinMaxPos',rc);
    wp.showCmd := GetRealShowCmd(wp.showCmd, form); //adjust showCmd to cover invisibility too
    ini.WriteInteger(FIniSection,'ShowCmd',wp.showCmd);
    ini.WriteInteger(FIniSection,'Flags',wp.flags);
    ini.UpdateFile;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TFormPlacement.RestoreFormPlacement(AOptions: TRestoreOptions);
var ini: TCustomIniFile;
  form: TCustomForm;
  wp: TWindowPlacement;
  rc: TRect;
begin
  form := self.Owner as TCustomForm;
  ini := OpenIniFile;
  try
    if not GetWindowPlacement(form.Handle, wp) then
      RaiseLastOsError();

    if foPosition in self.FOptions then begin
      if ini.TryReadRect(FIniSection,'NormPos',wp.rcNormalPosition) then begin
       //nothing
      end;
      if ini.TryReadRect(FIniSection,'MinMaxPos', rc) then begin
        wp.ptMinPosition := rc.TopLeft;
        wp.ptMaxPosition := rc.BottomRight;
      end;
    end;

    if foVisibility in Self.FOptions then begin
      wp.flags := ini.ReadInteger(FIniSection,'Flags',wp.flags);
     { Only one flag is supported, RESTORETOMAXIMIZED. No special treatment
      is needed when setting it. }
      wp.showCmd := ini.ReadInteger(FIniSection,'ShowCmd',wp.showCmd);
    end else
      wp.showCmd := GetRealShowCmd(wp.showCmd, form); //adjust showCmd to cover invisibility too

   { Issue 187: showCmd values returned by GetWindowPlacement have to be adjusted
    to not activate the window on applying }
    if not form.Active and not (roActivate in AOptions) then
     { If it's active we MUST NOT deflect to non-active version, the activation
      may have yet to be applied, such as on creation. }
      case wp.showCmd of
        //SW_HIDE: begin end; //hiding does not change activation unless neccessary
        SW_SHOWNORMAL: wp.showCmd := SW_SHOWNOACTIVATE;
        SW_SHOWMINIMIZED: wp.showCmd := SW_SHOWMINNOACTIVE;
        //SW_SHOWMAXIMIZED: begin end; //there's no way to maximize and not activate
        //SW_SHOWNOACTIVATE: begin end; //already safe
        SW_SHOW: wp.showCmd := SW_SHOWNA;
      end;

    if roJustWrite in AOptions then begin
      TForm(form).BoundsRect := wp.rcNormalPosition;
    end else begin
      SetWindowPlacement(form.Handle, wp);

     { Issue 187: Visibility changes are not synchronized directly back to Delphi.
      We have to do it explicitly. Visible:=true activates the window, but not
      when we just did SW_SHOWNA. }
     { We don't check foVisibility because setting Visible does no harm anyway }
      case wp.showCmd of
        SW_HIDE: TForm(form).Visible := false;
      else //basically everything else
        TForm(form).Visible := true;
      end;
    end;

    FPlacementRestored := true;
  finally
    FreeAndNil(ini);
  end;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TFormPlacement]);
end;

end.
