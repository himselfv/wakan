unit FormPlacemnt;
{
Simple form placement storage.
}

interface

uses
  SysUtils, Classes, IniFiles, Registry;

type
  TFormPlacementOption = (foPosition);
  TFormPlacementOptions = set of TFormPlacementOption;

  TFormPlacement = class(TComponent)
  protected
    FUseRegistry: boolean;
    FIniFileName: string;
    FIniSection: string;
    FOptions: TFormPlacementOptions;
    FPlacementRestored: boolean;
   { Set to true on each successful RestorePlacement, set to false manually
    to track when form placement is valid and should be saved }
    function OpenIniFile: TCustomIniFile;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveFormPlacement;
    procedure RestoreFormPlacement;
    property PlacementRestored: boolean read FPlacementRestored write FPlacementRestored;
  published
    property UseRegistry: boolean read FUseRegistry write FUseRegistry default true;
    property IniFileName: string read FIniFileName write FIniFileName;
    property IniSection: string read FIniSection write FIniSection;
    property Options: TFormPlacementOptions read FOptions write FOptions default [foPosition];
  end;

procedure Register;

implementation
uses Types, Forms, Windows;

constructor TFormPlacement.Create(AOwner: TComponent);
begin
  inherited;
  FUseRegistry := true;
  FIniFileName := '';
  FIniSection := '';
  FOptions := [foPosition];
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
    ini.WriteInteger(FIniSection,'ShowCmd',wp.showCmd);
    ini.WriteInteger(FIniSection,'Flags',wp.flags);
    ini.UpdateFile;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TFormPlacement.RestoreFormPlacement;
var ini: TCustomIniFile;
  form: TCustomForm;
  wp: TWindowPlacement;
  rc: TRect;
begin
  form := self.Owner as TCustomForm;
  ini := OpenIniFile;
  try
    GetWindowPlacement(form.Handle, wp);
    ini.TryReadRect(FIniSection,'NormPos',wp.rcNormalPosition);
    if ini.TryReadRect(FIniSection,'MinMaxPos', rc) then begin
      wp.ptMinPosition := rc.TopLeft;
      wp.ptMaxPosition := rc.BottomRight;
    end;
    wp.showCmd := ini.ReadInteger(FIniSection,'ShowCmd',wp.showCmd);
    wp.flags := ini.ReadInteger(FIniSection,'Flags',wp.flags);
    SetWindowPlacement(form.Handle, wp);
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
