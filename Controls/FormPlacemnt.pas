unit FormPlacemnt;
{
Simple form placement storage.
}

interface

uses
  System.SysUtils, System.Classes, IniFiles, Registry;

type
  TFormPlacementOption = (foPosition);
  TFormPlacementOptions = set of TFormPlacementOption;

  TFormPlacement = class(TComponent)
  protected
    FUseRegistry: boolean;
    FIniFileName: string;
    FIniSection: string;
    FOptions: TFormPlacementOptions;
    function OpenIniFile: TCustomIniFile;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveFormPlacement;
    procedure RestoreFormPlacement;
  published
    property UseRegistry: boolean read FUseRegistry write FUseRegistry default true;
    property IniFileName: string read FIniFileName write FIniFileName;
    property IniSection: string read FIniSection write FIniSection;
    property Options: TFormPlacementOptions read FOptions write FOptions default [foPosition];
  end;

procedure Register;

implementation
uses Types, Forms;

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

procedure TFormPlacement.SaveFormPlacement;
var ini: TCustomIniFile;
  form: TCustomForm;
  r: TRect;
begin
  form := self.Owner as TCustomForm;
  ini := OpenIniFile;
  try
    r := form.BoundsRect;
    ini.WriteString(FIniSection,'NormPos',
       Format('%d,%d,%d,%d',[r.Left,r.Top,r.Bottom,r.Right])
    );
  finally
    FreeAndNil(ini);
  end;
end;

type
  TStringArray = array of string;

function SplitStr(s: string; ch: char=','): TStringArray;
var i: integer;
begin
  SetLength(Result, 0);
  i := pos(s, ch);
  while i>0 do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := copy(s, 1, i-1);
    delete(s, 1, i);
    i := pos(s, ch);
  end;
  if s<>'' then begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := s;
  end;
end;

procedure TFormPlacement.RestoreFormPlacement;
var ini: TCustomIniFile;
  form: TCustomForm;
  str: string;
  parts: TStringArray;
  r: TRect;
begin
  form := self.Owner as TCustomForm;
  ini := OpenIniFile;
  try
    str := ini.ReadString(FIniSection,'NormPos','');
    if str='' then exit;

    parts := SplitStr(str,',');
    if Length(parts)<>4 then exit;

    if not TryStrToInt(parts[0],r.Left)
    or not TryStrToInt(parts[1],r.Top)
    or not TryStrToInt(parts[2],r.Right)
    or not TryStrToInt(parts[3],r.Bottom) then exit;

    form.BoundsRect := r;
  finally
    FreeAndNil(ini);
  end;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TFormPlacement]);
end;

end.
