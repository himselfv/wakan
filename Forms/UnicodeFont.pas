unit UnicodeFont;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, JWBStrings;

type
  TfSelectFont = class(TForm)
    StringGrid1: TStringGrid;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormShow(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
  public
    teststring:FString;
    deffont,selfont:string;
    selcoding:string;
  end;

var
  fSelectFont: TfSelectFont;

const
 {$IFNDEF UNICODE}
  FS_JAPANESE_CHARTEST:FString = '3042305A308C30AE30DD30C34EBA99AC9FA0571281D3';
  FS_ENGLISH_CHARTEST:FString = '00410042004300440045004600470048004900610062';
  FS_CHINESE_CHARTEST:FString = '8C66699E659658EC793A5C7158389FA057129F98';
  FS_PINYIN_CHARTEST:FString = '01010113012B014D016B01D600F300F2014D01D201DA01D601DC01D8';
  FS_RADICAL_CHARTEST:FString = '4E004E284E854EBB51AB9CE59E1F';
  FS_CHINESEGB_CHARTEST:FString = '4E2A4EA962D772F87C9884E695089F99';
 {$ELSE}
  FS_JAPANESE_CHARTEST:FString = #$3042#$305A#$308C#$30AE#$30DD#$30C3#$4EBA#$99AC#$9FA0#$5712#$81D3;
  FS_ENGLISH_CHARTEST:FString = #$0041#$0042#$0043#$0044#$0045#$0046#$0047#$0048#$0049#$0061#$0062;
  FS_CHINESE_CHARTEST:FString = #$8C66#$699E#$6596#$58EC#$793A#$5C71#$5838#$9FA0#$5712#$9F98;
  FS_PINYIN_CHARTEST:FString = #$0101#$0113#$012B#$014D#$016B#$01D6#$00F3#$00F2#$014D#$01D2#$01DA#$01D6#$01DC#$01D8;
  FS_RADICAL_CHARTEST:FString = #$4E00#$4E28#$4E85#$4EBB#$51AB#$9CE5#$9E1F;
  FS_CHINESEGB_CHARTEST:FString = #$4E2A#$4EA9#$62D7#$72F8#$7C98#$84E6#$9508#$9F99;
 {$ENDIF}


function ChooseFont(charsets:array of TFontCharset;teststring:FString;
  var supportedsets:string;defaultfont:string;selectfirst:boolean):string;

implementation
uses JWBMenu, JWBUnit;

{$R *.DFM}

var
  fontlist: TStringList;

procedure TfSelectFont.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  selfont:=StringGrid1.Cells[0,ARow];
  selcoding:=StringGrid1.Cells[1,ARow];
  PaintBox3.Invalidate;
end;

procedure TfSelectFont.FormShow(Sender: TObject);
var b:boolean;
    i:integer;
begin
  for i:=1 to StringGrid1.RowCount-1 do
    if uppercase(StringGrid1.Cells[0,i])=uppercase(deffont) then
    begin
      StringGrid1SelectCell(self,0,i,b);
      StringGrid1.Row:=i;
    end;  
end;

procedure TfSelectFont.PaintBox3Paint(Sender: TObject);
begin
  PaintBox3.Canvas.Brush.Color:=clWindow;
  PaintBox3.Canvas.Pen.Color:=clWindow;
  PaintBox3.Canvas.Rectangle(0,0,PaintBox3.Width,PaintBox3.Height);
  PaintBox3.Canvas.Pen.Color:=clWindowText;
  DrawUnicode(PaintBox3.Canvas,1,1,48,teststring,selfont);
end;

function EnumProc(lpelf:pointer;lpnt:pointer;FontType:integer;lParam:integer):integer; stdcall;
var p:^ENUMLOGFONTEX;
begin
  p:=lpelf;
  fontlist.Add(AnsiString(p^.elfLogFont.lfFaceName)+#9+inttostr(p^.elfLogFont.lfCharset));
end;

function ChooseFont(charsets:array of TFontCharset;teststring:FString;
  var supportedsets:string;defaultfont:string;selectfirst:boolean):string;
var lf:LOGFONT;
    i:integer;
    curfont:string;
    csets:string;
    s:string;
    ci:integer;
    y:integer;
begin
  fillchar(lf,sizeof(lf),0);
  fontlist:=TStringList.Create;
{  for i:=0 to High(charsets) do
  begin
    lf.lfCharset:=charsets[i];
    EnumFontFamiliesEx(fMenu.Canvas.Handle,lf,@EnumProc,0,0);
  end;}
  lf.lfCharset:=ANSI_CHARSET;
//  EnumFontFamiliesEx(fMenu.Canvas.Handle,lf,@EnumProc,0,0);
  for i:=0 to Screen.Fonts.Count-1 do
    fontlist.Add(Screen.Fonts[i]+#9+inttostr(ANSI_CHARSET));
  fontlist.Sort;
  fSelectFont.StringGrid1.RowCount:=2;
  fSelectFont.StringGrid1.Cells[0,0]:=_l('#00636^eFont name');
  fSelectFont.StringGrid1.Cells[1,0]:=_l('#00637^eCharsets');
  curfont:='';
  y:=1;
  for i:=0 to fontlist.Count-1 do
  begin
    s:=fontlist[i];
    if (length(s)>0) and (s[1]<>'@') then
    begin
      ci:=strtoint(copy(s,pos(#9,s)+1,length(s)-pos(#9,s)));
      delete(s,pos(#9,s),length(s)-pos(#9,s)+1);
      if (curfont<>s) and (curfont<>'') then
      begin
        if length(csets)>0 then delete(csets,length(csets),1);
        fSelectFont.StringGrid1.Cells[0,y]:=curfont;
        fSelectFont.StringGrid1.Cells[1,y]:=csets;
        csets:='';
        curfont:=s;
        inc(y);
        fSelectFont.StringGrid1.RowCount:=y+1;
      end;
      curfont:=s;
      case ci of
        ANSI_CHARSET:csets:=csets+'ANSI,';
        ARABIC_CHARSET:csets:=csets+'Arabic,';
        BALTIC_CHARSET:csets:=csets+'Baltic,';
        DEFAULT_CHARSET:csets:=csets+'Def,';
        EASTEUROPE_CHARSET:csets:=csets+'EastEurope,';
        GB2312_CHARSET:csets:=csets+'GB2312,';
        GREEK_CHARSET:csets:=csets+'Greek,';
        HANGEUL_CHARSET:csets:=csets+'Hangeul,';
        HEBREW_CHARSET:csets:=csets+'Hebrew,';
        CHINESEBIG5_CHARSET:csets:=csets+'Big5,';
        JOHAB_CHARSET:csets:=csets+'Johab,';
        MAC_CHARSET:csets:=csets+'Mac,';
        OEM_CHARSET:csets:=csets+'OEM,';
        RUSSIAN_CHARSET:csets:=csets+'Russian,';
        SHIFTJIS_CHARSET:csets:=csets+'Shift-JIS,';
        SYMBOL_CHARSET:csets:=csets+'Symbol,';
        THAI_CHARSET:csets:=csets+'Thai,';
        TURKISH_CHARSET:csets:=csets+'Turkish,';
      end;
    end;
  end;
  fSelectFont.teststring:=teststring;
  fSelectFont.deffont:=defaultfont;
  fSelectFont.StringGrid1.RowCount:=y;
  s:='';
  fontlist.Free;
  if selectfirst then
  begin
    if y>1 then result:=curfont else result:='!';
    exit;
  end;
  if fSelectFont.ShowModal<>idOK then result:=defaultfont else result:=fSelectFont.selfont;
  supportedsets:=fSelectFont.selcoding;
end;

end.
