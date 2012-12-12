unit JWBNewCategory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TEditCategoryFlag = (
    efAddCategory,  //we're adding and not editing
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

function NeedCategory(category: string; cattype: char; silent: boolean): integer;

implementation
uses JWBMenu, JWBCategories;

{$R *.DFM}

function TfNewCategory.EditCategory(var catname: string; flags: TEditCategoryFlags=[]): boolean;
begin
  if efAddCategory in flags then
    Caption:=_l('^eNew category')
  else
    Caption:=_l('^eEdit category');
  Edit1.Text:=catname;
  Edit1.Enabled := not (efFixedName in flags);
  RadioGroup1.Enabled:=false;
  RadioGroup1.ItemIndex:=0;
  Result := (ShowModal=idOK);
  if Result then
    catname := Edit1.Text;
end;

function TfNewCategory.EditCategory(var catname: string; var cattype: char; flags: TEditCategoryFlags=[]): boolean;
begin
  if efAddCategory in flags then
    Caption:=_l('^eNew category')
  else
    Caption:=_l('^eEdit category');
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

//Finds a category by name, or creates a new one asking user for details.
//Returns category id. If user cancels the operation, returns -1.
//  category: category name
//  cattype: category type
//  silent: do not update user interface after adding. (Do it manually later!)
function NeedCategory(category: string; cattype: char; silent: boolean): integer;
var catname: string;
begin
  if TUserCat.Locate('Name',category,false) then Result:=TUserCat.Int(TUserCatIndex) else
  begin
    if cattype='?' then
    begin
      catname := StripCatName(category);
      if not fNewCategory.EditCategory(catname, cattype, [efAddCategory]) then begin
        Result := -1;
        exit;
      end;
      category:=curlang+'~'+catname;
    end;
    if TUserCat.Locate('Name',category,false) then Result:=TUserCat.Int(TUserCatIndex) else
    begin
      Inc(MaxCategoryIndex);
      TUserCat.Insert([inttostr(MaxCategoryIndex),category,inttostr(ord(cattype)),FormatDateTime('yyyymmdd',now)]);
      if not silent then begin
        fMenu.RefreshCategory;
        fMenu.ChangeUserData;
      end;
      Result:=MaxCategoryIndex;
    end;
  end;
end;

end.
