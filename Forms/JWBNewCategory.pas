unit JWBNewCategory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TEditCategoryFlag = (
    efFixedName     //don't let user change the name
  );
  TEditCategoryFlags = set of TEditCategoryFlag;

  TfNewCategory = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  public
    function EditCategory(var catname: string; flags: TEditCategoryFlags=[]): boolean; overload;
    function EditCategory(var catname: string; var cattype: char; flags: TEditCategoryFlags=[]): boolean; overload;
  end;

var
  fNewCategory: TfNewCategory;

implementation
uses JWBMenu, JWBUserData, JWBCategories;

{$R *.DFM}

//For kanji categories -- no type change allowed
function TfNewCategory.EditCategory(var catname: string; flags: TEditCategoryFlags=[]): boolean;
begin
  Edit1.Text:=catname;
  Edit1.Enabled := not (efFixedName in flags);
  RadioGroup1.Enabled:=false;
  RadioGroup1.ItemIndex:=0;
  Result := (ShowModal=idOK);
  if Result then
    catname := Edit1.Text;
end;

//For word categories -- type change allowed
function TfNewCategory.EditCategory(var catname: string; var cattype: char; flags: TEditCategoryFlags=[]): boolean;
begin
  Edit1.Text:=catname;
  Edit1.Enabled := not (efFixedName in flags);
  RadioGroup1.Enabled:=true;
  case cattype of
    'L':RadioGroup1.ItemIndex:=0;
    'G':RadioGroup1.ItemIndex:=1;
    'T':RadioGroup1.ItemIndex:=2;
  else
    RadioGroup1.ItemIndex:=0;
  end;
  Result := (ShowModal=idOK);
  if Result then begin
    catname := Edit1.Text;
    case fNewCategory.RadioGroup1.ItemIndex of
      0:cattype:='L';
      1:cattype:='G';
      2:cattype:='T';
    end;
  end;
end;

end.
