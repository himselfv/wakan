unit ColorBtnEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ColorBtns, ExtCtrls, DrawMan, ColorMan;

function EditColorButton(Source: TColor95Button): Boolean;

implementation

{$R ColorBtnEdit.DFM}
type
  TForm1 = class(TForm)
    ColorButton: TColor95Button;
    btnColor: TSpeedButton;
    txtCaption: TEdit;
    Button1: TButton;
    Button2: TButton;
    rgStyles: TRadioGroup;
    Button3: TButton;
    ColorDlg: TColorDialog;
    FontDlg: TFontDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure txtCaptionChange(Sender: TObject);
    procedure rgStylesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure	CreateColor(Color: TColor);
  public
    { Public declarations }
		function Execute(AColorButton: TColor95Button): Boolean;
  end;

function EditColorButton(Source: TColor95Button): Boolean;
begin
  with TForm1.Create(Application) do
  begin
  	try
    	Result := Execute(Source);
    finally
    	Free;
    end;
  end;
end;
  

function TForm1.Execute(AColorButton: TColor95Button): Boolean;
begin
  ColorButton.Color := AColorButton.Color;
  ColorButton.Caption := AColorButton.Caption;
  txtCaption.Text := AColorButton.Caption;
  ColorButton.Style := AColorButton.Style;
  if Showmodal = mrOK then
  begin
  	AColorButton.Color := ColorButton.Color;
  	AColorButton.Caption := ColorButton.Caption;
  	AColorButton.Style := ColorButton.Style;
    Result := True;
  end
  else
  	Result := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
	Custom: array[1..16] of Integer = ($5CC646, $4E89BE, $45C7BA, $26BFE6,
  	$263FE6, $BA9B52, $9F5FAD, $2F6BDD, $D94033, $B8E12B,
		$EE99F2, $9A9A9A, $9898BC, $92549A, $3478A5,$D3E547);
var
	i : Byte;
begin
  for i := 1 to 16 do
  	ColorDlg.CustomColors.Add('Color' + Chr(Ord('A')+i-1) + '=' + IntToHex(Custom[i], 6));
	CreateColor(clBtnFace);
end;

procedure TForm1.btnColorClick(Sender: TObject);
begin
	ColorDlg.Color := ColorButton.Color;
	if ColorDlg.Execute then
  begin
  	CreateColor(ColorDlg.Color);
  end;
end;

procedure	TForm1.CreateColor(Color: TColor);
begin
	ColorButton.Color := Color;
end;

procedure TForm1.txtCaptionChange(Sender: TObject);
begin
	ColorButton.Caption := txtCaption.Text;
end;

procedure TForm1.rgStylesClick(Sender: TObject);
const
	Styles : array[0..5] of TClr95ButtonStyle = (bsWin31, bsNew, bsGlass, bsTab,
  	bsSideDownTab, bsEllipse);
begin
	ColorButton.Style := Styles[rgStyles.ItemIndex];
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
	FontDlg.Font := ColorButton.Font;
	if FontDlg.Execute then
  	ColorButton.Font := FontDlg.Font;
end;

end.
