unit JWBWordCategory;
{ Displays statistical information for the currently selected word in
 dictionary lookup results.
 Not used at this time. }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfWordCategory = class(TForm)
    Label30: TLabel;
    Label32: TLabel;
    Label34: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label54: TLabel;
    RxLabel9: TLabel;
    Label55: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Bevel1: TBevel;
  public
    procedure Clear;
    procedure SetWord(const AUserIndex: integer);
  end;

var
  fWordCategory: TfWordCategory;

implementation
uses JWBMenu, JWBUnit, JWBUserData, JWBCategories;

{$R *.DFM}

procedure TfWordCategory.Clear;
begin
  RxLabel9.Caption:='-';
  Label55.Caption:='-';
  Label11.Caption:='-';
  Label12.Caption:='-';
  Label13.Caption:='-';
  Label14.Caption:='-';
end;

procedure TfWordCategory.SetWord(const AUserIndex: integer);
var sl: TStringList;
  i: integer;
  s: string;
begin
  if AUserIndex<0 then begin
    RxLabel9.Caption:=_l('#00677^eNot in vocabulary');
    exit;
  end;

  TUser.Locate('Index',AUserIndex);
  Label11.Caption:=DateForm(TUser.Str(TUserAdded));
  Label12.Caption:=DateForm(TUser.Str(TUserLearned));
  Label13.Caption:=DateForm(TUser.Str(TUserMastered));
  Label14.Caption:=DateForm(TUser.Str(TUserPrinted));
  if Label13.Caption<>'-'then Label13.Caption:=Label13.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
  RxLabel9.Caption:=StateStr(TUser.Int(TUserScore));
  sl:=TStringList.Create;
  try
    sl.Clear;
    ListWordCategories(AUserIndex,sl);
    s:='';
    for i:=0 to sl.Count-1 do if s='' then s:=sl[i] else s:=s+', '+sl[i];
    Label55.Caption:=s;
  finally
    FreeAndNil(sl);
  end;
end;

end.
