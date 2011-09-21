unit DGPAll;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ColorBtns, ShadowButton, Backgnd, ArtLabel, Apples, ExtCtrls, ClrPanel,
  ToggleImage, BCBitmap, Gradient, DFCtrls, DFClasses;

type
  TForm1 = class(TForm)
    ColorPanel1: TColorPanel;
    Apples1: TApples;
    ArtLabel1: TArtLabel;
    ShadowButton1: TShadowButton;
    Color95Button1: TColor95Button;
    Color95Button2: TColor95Button;
    Color95Button3: TColor95Button;
    ColorPanel2: TColorPanel;
    BCBitmap1: TBCBitmap;
    ArtLabel2: TArtLabel;
    Color95Button6: TColor95Button;
    Color95Button7: TColor95Button;
    Color95Button4: TColor95Button;
    Color95Button5: TColor95Button;
    procedure Color95Button4Click(Sender: TObject);
    procedure Color95Button5Click(Sender: TObject);
    procedure Color95Button6Click(Sender: TObject);
    procedure Color95Button7Click(Sender: TObject);
    procedure ShadowButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Color95Button4Click(Sender: TObject);
begin
	ArtLabel2.Caption := 'Art label can contain images.';
end;

procedure TForm1.Color95Button5Click(Sender: TObject);
begin
	ArtLabel2.Caption := 'Color buttons have different styles.';
end;

procedure TForm1.Color95Button6Click(Sender: TObject);
begin
	ArtLabel2.Caption := 'Apples box shows a imagginable sence.';
end;

procedure TForm1.Color95Button7Click(Sender: TObject);
begin
	ArtLabel2.Caption := 'Color panels make you a colourful enviroment.';
end;

procedure TForm1.ShadowButton1Click(Sender: TObject);
begin
	Close;
end;

end.
