unit JWBFileType;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, JwbForms;

type
  TfFileType = class(TJwbForm)
    rgType: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormShow(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TfFileType.FormShow(Sender: TObject);
begin
  rgType.SetFocus;
end;

end.
