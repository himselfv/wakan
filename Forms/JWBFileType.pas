unit JWBFileType;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfFileType = class(TForm)
    rgType: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormShow(Sender: TObject);
  end;

var
  fFileType: TfFileType;

implementation

{$R *.DFM}

procedure TfFileType.FormShow(Sender: TObject);
begin
  rgType.SetFocus;
end;

end.
