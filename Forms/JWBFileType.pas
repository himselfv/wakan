unit JWBFileType;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, JWBIO, JwbForms;

type
  TfFileType = class(TJwbForm)
    rgType: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormShow(Sender: TObject);
  end;

function Conv_ChooseType(AChinese:boolean; ADefault: CEncoding): CEncoding;

implementation

{$R *.DFM}

function Conv_ChooseType(AChinese:boolean; ADefault: CEncoding): CEncoding;
var fFileType: TfFileType;
begin
  Result := nil;
  fFileType := TfFileType.Create(nil);
  try
    if AChinese then
    begin
      fFileType.rgType.Items.Clear;
      fFileType.rgType.Items.Add('Unicode (UCS2)');
      fFileType.rgType.Items.Add('UTF-8');
      fFileType.rgType.Items.Add('Big5');
      fFileType.rgType.Items.Add('GB2312');
      fFileType.rgType.Items.Add('Unicode (UCS2) reversed bytes');
      if ADefault=TUTF16LEEncoding then
        fFileType.rgType.ItemIndex:=0
      else
      if ADefault=TUTF8Encoding then
        fFileType.rgType.ItemIndex:=1
      else
      if ADefault=TBIG5Encoding then
        fFileType.rgType.ItemIndex:=2
      else
      if ADefault=TGBEncoding then
        fFileType.rgType.ItemIndex:=3
      else
      if ADefault=TUTF16BEEncoding then
        fFileType.rgType.ItemIndex:=4
      else
        fFileType.rgType.ItemIndex:=0;
      if fFileType.ShowModal=mrOK then
      case fFileType.rgType.ItemIndex of
        0: Result:=TUTF16LEEncoding;
        1: Result:=TUTF8Encoding;
        2: Result:=TBIG5Encoding;
        3: Result:=TUTF16BEEncoding;
        4: Result:=TUTF16BEEncoding;
      end else Result:=nil;
    end else
    begin
      fFileType.rgType.Items.Clear;
      fFileType.rgType.Items.Add('Unicode (UCS2)');
      fFileType.rgType.Items.Add('UTF-8');
      fFileType.rgType.Items.Add('Shift-JIS');
      fFileType.rgType.Items.Add('JIS');
      fFileType.rgType.Items.Add('Old JIS');
      fFileType.rgType.Items.Add('NEC JIS');
      fFileType.rgType.Items.Add('EUC');
      fFileType.rgType.Items.Add('Unicode (UCS2) reversed bytes');
      if ADefault=TUTF16LEEncoding then
        fFileType.rgType.ItemIndex:=0
      else
      if ADefault=TUTF8Encoding then
        fFileType.rgType.ItemIndex:=1
      else
      if ADefault=TSJISEncoding then
        fFileType.rgType.ItemIndex:=2
      else
      if ADefault=TJISEncoding then
        fFileType.rgType.ItemIndex:=3
      else
      if ADefault=TOldJISEncoding then
        fFileType.rgType.ItemIndex:=4
      else
      if ADefault=TNECJISEncoding then
        fFileType.rgType.ItemIndex:=5
      else
      if ADefault=TEUCEncoding then
        fFileType.rgType.ItemIndex:=6
      else
      if ADefault=TUTF16BEEncoding then
        fFileType.rgType.ItemIndex:=7
      else
        fFileType.rgType.ItemIndex:=0;
      if fFileType.ShowModal=mrOK then
      case fFileType.rgType.ItemIndex of
        0: Result:=TUTF16LEEncoding;
        1: Result:=TUTF8Encoding;
        2: Result:=TSJISEncoding;
        3: Result:=TJISEncoding;
        4: Result:=TOldJISEncoding;
        5: Result:=TNECJISEncoding;
        6: Result:=TEUCEncoding;
        7: Result:=TUTF16BEEncoding;
      end;
    end;
  finally
    FreeAndNil(fFileType);
  end;
end;

procedure TfFileType.FormShow(Sender: TObject);
begin
  rgType.SetFocus;
end;

end.
