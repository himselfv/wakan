unit JWBCategoryMgr;

interface

//TODO: Localize everything here
//TODO: Also aCategoryManager in JWBMenu

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls;

type
  TfCategoryMgr = class(TForm)
    pcPages: TTabControl;
    lbList: TListBox;
    btnClose: TBitBtn;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn3: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
  public
    procedure ReloadList;
  end;

var
  fCategoryMgr: TfCategoryMgr;

implementation
uses JWBCategories, JWBUserData, JWBMenu;

{$R *.dfm}

procedure TfCategoryMgr.FormShow(Sender: TObject);
begin
  ReloadList;
end;

procedure TfCategoryMgr.pcPagesChange(Sender: TObject);
begin
  ReloadList;
end;

procedure TfCategoryMgr.ReloadList;
var s: string;
  lc: char;
  ct: char;
  skip: boolean;
begin
  lbList.Clear;
  lbList.Perform(WM_SETREDRAW,0,0);
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    s:=TUserCat.Str(TUserCatName);
    lc:=GetCatPrefix(s);
    if lc='?' then begin
      lc := 'j';
      TUserCat.Edit([TUserCatName],['j~'+s])
    end;
    s:=StripCatName(s);
    ct:=Chr(TUserCat.Int(TUserCatType));
    skip:=false;
    case pcPages.TabIndex of
      0: if (lc<>curlang) or (ct<>'L') then skip:=true;
      1: if (lc<>curlang) or (ct<>'G') then skip:=true;
      2: if (lc<>curlang) or (ct<>'T') then skip:=true;
      3: if (lc<>curlang) or (ct<>'W') then skip:=true;
      4: if lc<>'k' then skip:=true;
    else skip:=true;
    end;

    if not skip then
      lbList.AddItem(s,TObject(TUserCat.Int(TUserCatIndex)));
    TUserCat.Next;
  end;
  lbList.Perform(WM_SETREDRAW,1,0);
end;

end.
