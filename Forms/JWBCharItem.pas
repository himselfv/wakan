unit JWBCharItem;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfCharItem = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    RadioGroup4: TRadioGroup;
    CheckBox2: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  public
    inputs:string;
    results:string;
    function GetDet(j:integer):string;
  end;

var
  fCharItem: TfCharItem;

implementation

uses JWBMenu;

{$R *.DFM}

procedure TfCharItem.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  results:='';
  results:=results+GetCharPropType(ComboBox1.ItemIndex,0)+';';
  case RadioGroup1.ItemIndex of
    0:results:=results+'L;';
    1:results:=results+'C;';
    2:results:=results+'W;';
  end;
  case RadioGroup2.ItemIndex of
    0:results:=results+'L;';
    1:results:=results+'W;';
    2:results:=results+'N;';
  end;
  case RadioGroup3.ItemIndex of
    0:results:=results+'B;';
    1:results:=results+'J;';
    2:results:=results+'C;';
  end;
  if CheckBox1.Checked then results:=results+'Y;'else results:=results+'N;';
  case RadioGroup4.ItemIndex of
    0:results:=results+'S;';
    1:results:=results+'M;';
    2:results:=results+'B;';
  end;
  if CheckBox2.Checked then results:=results+edit1.text;
end;

function TfCharItem.GetDet(j:integer):string;
var s:string;
begin
  s:=inputs;
  while j>0 do
  begin
    delete(s,1,pos(';',s));
    dec(j);
  end;
  delete(s,pos(';',s),length(s)-pos(';',s)+1);
  result:=s;
end;

procedure TfCharItem.FormShow(Sender: TObject);
var i:integer;
begin
  if inputs='' then inputs:='0;L;L;B;Y;M;';
  ComboBox1.Items.Clear;
  for i:=0 to CharPropTypes.Count-1 do
    ComboBox1.Items.Add(_l('^e'+GetCharPropType(i,4)));
  i := FindCharPropType(GetDet(0));
  if i<0 then
    ComboBox1.ItemIndex:=0
  else
    ComboBox1.ItemIndex:=i;
  if GetDet(1)='L'then RadioGroup1.ItemIndex:=0;
  if GetDet(1)='C'then RadioGroup1.ItemIndex:=1;
  if GetDet(1)='W'then RadioGroup1.ItemIndex:=2;
  if GetDet(2)='L'then RadioGroup2.ItemIndex:=0;
  if GetDet(2)='W'then RadioGroup2.ItemIndex:=1;
  if GetDet(2)='N'then RadioGroup2.ItemIndex:=2;
  if GetDet(3)='B'then RadioGroup3.ItemIndex:=0;
  if GetDet(3)='J'then RadioGroup3.ItemIndex:=1;
  if GetDet(3)='C'then RadioGroup3.ItemIndex:=2;
  if GetDet(5)='S'then RadioGroup4.ItemIndex:=0;
  if GetDet(5)='M'then RadioGroup4.ItemIndex:=1;
  if GetDet(5)='B'then RadioGroup4.ItemIndex:=2;
  CheckBox1.Checked:=GetDet(4)='Y';
  CheckBox2.Checked:=GetDet(6)<>'';
  Edit1.Enabled:=CheckBox2.Checked;
  Edit1.Text:=GetDet(6);
  Combobox1Change(Combobox1);
end;

procedure TfCharItem.CheckBox2Click(Sender: TObject);
begin
  Edit1.Enabled:=CheckBox2.Checked;
end;

procedure TfCharItem.ComboBox1Change(Sender: TObject);
begin
  Label2.Caption:=GetCharPropType(ComboBox1.ItemIndex,6);
  if GetCharPropType(ComboBox1.ItemIndex,1)='D'then
    Label5.Caption:='EDICT -> '+GetCharPropType(ComboBox1.ItemIndex,2)
  else
    Label5.Caption:='UNIHAN -> '+GetCharPropType(ComboBox1.ItemIndex,2);
end;

end.
