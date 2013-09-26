unit UnicodeFont;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, JWBStrings, JwbForms, WakanPaintbox;

type
  TfSelectFont = class(TJwbForm)
    StringGrid1: TStringGrid;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    PaintBox3: TWakanPaintbox;
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormShow(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject; Canvas: TCanvas);
  public
    teststring:FString;
    deffont,selfont:string;
    selcoding:string;
  end;

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

function FindFont(const face: string; charset: integer): string;
function ListFonts(const face: string; charset: integer): TStringList;

implementation
uses JWBMenu, JWBUnit;

{$R *.DFM}

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

procedure TfSelectFont.PaintBox3Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  Canvas.Pen.Color:=clWindow;
  Canvas.Rectangle(0,0,PaintBox3.Width,PaintBox3.Height);
  Canvas.Pen.Color:=clWindowText;
  DrawUnicode(Canvas,1,1,48,teststring,selfont);
end;

function FindFont_Callback(lpelfe: PLogFont; lpntme: PTextMetric; FontType: dword;
  param: LParam): integer; stdcall;
begin
  PString(param)^ := lpelfe.lfFaceName;
  Result := 0;
end;

function FindFont(const face: string; charset: integer): string;
var DC: HDC;
  LFont: TLogFont;
begin
  Result := '';
  DC := GetDC(0);
  try
    FillChar(LFont, SizeOf(LFont), 00);
    LFont.lfCharSet := charset;
    if face<>'' then
      strplcopy(@LFont.lfFaceName[0], face, Length(LFont.lfFaceName)-1); //also copies null
    EnumFontFamiliesEx(DC, LFont, @FindFont_Callback, LParam(@Result), 0);
  finally
    ReleaseDC(0, DC);
  end;
end;

function ListFonts_Callback(lpelfe: PLogFont; lpntme: PTextMetric; FontType: dword;
  param: LParam): integer; stdcall;
begin
 { Unfortunately, most of the times Unicode fonts report itself simply as "Unicode",
  and Windows returns those with the default system charset, usually ANSI.
  TODO: Use NEWTEXTMETRICEX(lpntme).ntmFontSig.fsUsb bit field to study font unicode coverage,
    where available.
    Older fonts wont have this, but they have Charset field filled. }
  TStringList(param).AddObject(lpelfe.lfFaceName, TObject(lpelfe.lfCharSet));
  Result := 1; //continue
end;

function ListFonts(const face: string; charset: integer): TStringList;
var DC: HDC;
  LFont: TLogFont;
begin
  Result := TStringList.Create;
  DC := GetDC(0);
  try
    FillChar(LFont, SizeOf(LFont), 00);
    LFont.lfCharSet := charset;
    if face<>'' then
      strplcopy(@LFont.lfFaceName[0], face, Length(LFont.lfFaceName)-1); //also copies null
    EnumFontFamiliesEx(DC, LFont, @ListFonts_Callback, LParam(Result), DEFAULT_CHARSET);
   { DEFAULT_CHARSET in this context is a wildcard to receive all charsets for all fonts. }
  finally
    ReleaseDC(0, DC);
  end;
end;

function ChooseFont(charsets:array of TFontCharset;teststring:FString;
  var supportedsets:string;defaultfont:string;selectfirst:boolean):string;
var fSelectFont: TfSelectFont;
  fonts: TStringList;
  i:integer;
  curfont:string;
  csets:string;
  s:string;
  ci:integer;
  y:integer;

 //If one font handles several character sets, it appears several times.
 //So we store data and commit it when the font name changes.
  procedure CommitFont;
  begin
    if length(csets)>0 then delete(csets,length(csets),1);
    fSelectFont.StringGrid1.RowCount:=y+1;
    fSelectFont.StringGrid1.Cells[0,y]:=curfont;
    fSelectFont.StringGrid1.Cells[1,y]:=csets;
    csets:='';
    curfont:='';
    inc(y);
  end;

begin
  fSelectFont := TfSelectFont.Create(Application);
  try
    fonts := ListFonts('',0);
    try
      fonts.Sort;

      fSelectFont.StringGrid1.RowCount:=2;
      fSelectFont.StringGrid1.Cells[0,0]:=_l('#00636^eFont name');
      fSelectFont.StringGrid1.Cells[1,0]:=_l('#00637^eCharsets');

      curfont:='';
      y:=0;
      for i:=0 to fonts.Count-1 do
      begin
        s:=fonts[i];
        if (Length(s)<=0) or (s[1]='@') then continue;
        if (curfont<>s) and (curfont<>'') then
          CommitFont;
        curfont:=s;

        ci:=integer(fonts.Objects[i]);
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

    finally
      FreeAndNil(fonts);
    end;

    if curfont<>'' then
      CommitFont;

    fSelectFont.teststring:=teststring;
    fSelectFont.deffont:=defaultfont;
    fSelectFont.StringGrid1.RowCount:=y;
    if selectfirst then
    begin
      if y>1 then result:=curfont else result:='!';
      exit;
    end;
    if fSelectFont.ShowModal<>idOK then result:=defaultfont else result:=fSelectFont.selfont;
    supportedsets:=fSelectFont.selcoding;
  finally
    FreeAndNil(fSelectFont);
  end;
end;

end.
