unit JWBSplash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ArtLabel, ExtCtrls, ComCtrls, StdCtrls, Buttons;

type
  TfSplash = class(TForm)
    Panel1: TPanel;
    Label4: TLabel;
    Image1: TImage;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    BitBtn1: TBitBtn;
    Memo1: TMemo;
    Shape1: TShape;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fSplash: TfSplash;

implementation

{$R *.DFM}

end.
