unit JWBConvert;

interface
uses JWBCore;

function Conv_DetectType(filename:string):byte;
procedure Conv_Open(filename:string; tp:byte);
function Conv_Read:FString; //reads one char as string
function Conv_ReadChar:FChar; //reads one char as char
procedure Conv_Create(filename:string; tp:byte);
procedure Conv_Write(s:FString);
procedure Conv_WriteChar(s:FChar);
procedure Conv_Close;
procedure Conv_Flush;
function Conv_ChooseType(chinese:boolean; def:byte):byte;
procedure Conv_Rewind;

implementation

uses JWBConvertTbl, SysUtils, JWBFileType, Controls;

const IS_EUC=1;
      IS_HALFKATA=2;
      IS_SJIS1=3;
      IS_SJIS2=4;
      IS_MARU=5;
      IS_NIGORI=6;
      IS_JIS=7;
      FILETYPE_UNKNOWN=0;
      FILETYPE_UNICODE=1;
      FILETYPE_UNICODER=2;
      FILETYPE_UTF8=3;
      FILETYPE_JIS=4;
      FILETYPE_OLD=5;
      FILETYPE_NEC=6;
      FILETYPE_SJS=7;
      FILETYPE_EUC=8;
      FILETYPE_GB=9;
      FILETYPE_BIG5=10;
      FILETYPE_EUCORSJIS=99;
      FILETYPE_ASCII=98;
      FILETYPE_WTT=255;
      JIS_NL=10;          // New Line char.
      JIS_CR=13;         // Carrage Return.
      JIS_ESC=27;          // Escape.
      JIS_SS2=142;         // Half-width katakana marker.

type TUTFArray=array[0..3] of byte;

var f:file;
    ftp:byte;
    buf:array[1..1024] of byte;
    bufpos:integer;
    buflen:integer;
    inp_intwobyte:boolean;

function SJIS2JIS(w:word):word;
var b1,b2,adjust, rowOffset, cellOffset:byte;
begin
  b1:=w div 256;
  b2:=w mod 256;
  if b2<159 then adjust:=1 else adjust:=0;
  if b1<160 then rowOffset:=112 else rowOffset:=176;
  if adjust=1 then begin if b2>127 then cellOffset:=32 else cellOffset:=31 end else cellOffset:=126;
  b1 := ((b1-rowOffset) shl 1) - adjust;
  b2 := b2-cellOffset;
  result:=b1*256+b2;
end;

function JIS2SJIS(w:word):word;
var b1,b2,b1n:byte;
begin
  b1:=w div 256;
  b2:=w mod 256;
  if b1 mod 2<>0 then begin if b2>95 then b2:=b2+32 else b2:=b2+31 end else b2:=b2+126;
  b1n:=((b1+1) shr 1);
  if b1<95 then b1n:=b1n+112 else b1n:=b1n+176;
  result:=b1n*256+b2;
end;

function JIS2Unicode(w:word):word;
begin
  case w of
    $0000..$007e:result:=w; // ascii
    $0080..$00ff:result:=Table_ExtASCII[w-128];
    $2330..$237a:result:=w-$2330+$ff10; // japanese ASCII
    $2421..$2473:result:=w-$2421+$3041; // hiragana
    $2521..$2576:result:=w-$2521+$30a1; // katakana
    $2621..$2658:if w<=$2631 then result:=w-$2621+$0391 else
                 if w<=$2638 then result:=w-$2621+$0392 else
                 if w< $2641 then result:=0 else
                 if w<=$2651 then result:=w-$2621+$0391 else
                 if w<=$2658 then result:=w-$2621+$0392; // greek
    $2721..$2771:if w<=$2726 then result:=w-$2721+$0410 else
                 if w= $2727 then result:=$0401 else
                 if w<=$2741 then result:=w-$2722+$0410 else
                 if w< $2751 then result:=0 else
                 if w<=$2756 then result:=w-$2751+$0430 else
                 if w= $2757 then result:=$0451 else
                 if w<=$2771 then result:=w-$2752+$0430; // cyrillic
    $3021..$7426:if ((w and $7f)<$21) or ((w and $7f)>$7e) then result:=0 else
                   result:=Table_Kanji[((w-$3021) div 256)*94+((w-$3021) mod 256)]; // kanji
    $2121..$217e:result:=Table_Misc[w-$2121];
    $2221..$227e:result:=Table_Misc[w-$2221+94];
    $2821..$2840:result:=Table_Misc[w-$2821+94+94];
    else result:=0;
  end;  
end;

function Unicode2UTF(ch:word):TUTFArray;
begin
  if (ch and UTF8_WRITE1)=0 then
  begin
    result[0]:=1;
    result[1]:=ch;
    exit;
  end;
  if (ch and UTF8_WRITE2)=0 then
  begin
    result[0]:=2;
    result[1]:=(UTF8_VALUE2 or (ch shr 6));
    result[2]:=(UTF8_VALUEC or (ch and $3f));
  end;
  result[0]:=3;
  result[1]:=UTF8_VALUE3 or (ch shr 12);
  result[2]:=UTF8_VALUEC or ((ch shr 6) and $3f);
  result[3]:=UTF8_VALUEC or (ch and $3f);
end;

function Unicode2JIS(w:word):word;
var i:integer;
begin
  result:=0;
  case w of
    $0000..$007e:result:=w; // Ascii
    $3041..$3093:result:=w-$3041+$2421; // Hiragana
    $30a1..$30f6:result:=w-$30a1+$2521; // Katakana
    $0391..$03c9:if w<=$03a1 then result:=w-$0391+$2621 else
                 if w= $03a2 then result:=0 else
                 if w<=$03a9 then result:=w-$0392+$2621 else
                 if w< $03b1 then result:=0 else
                 if w<=$03c1 then result:=w-$0391+$2621 else
                 if w= $03c2 then result:=0 else
                 if w<=$03c9 then result:=w-$0392+$2621; // greek
    $0401       :result:=$2727;
    $0451       :result:=$2757;
    $0410..$044f:if w<=$0415 then result:=w-$0410+$2721 else
                 if w<=$042f then result:=w-$0416+$2728 else
                 if w<=$0435 then result:=w-$0430+$2751 else
                 if w<=$044f then result:=w-$0436+$2758; // cyrillic
    $ff10..$ff5a:result:=w-$ff10+$2330;
    $feff       :result:=w;
    $fffe       :result:=w;
  end;
  if result<>0 then exit;
  for i:=0 to NUMBER_KANJIUNICODE-1 do if Table_Kanji[i]=w then
  begin
    result:=i div 94;
    result:=(((result+$30) shl 8) or (i-result*94)+$21);
    exit;
  end;
  for i:=0 to NUMBER_MISCUNICODE-1 do if Table_Misc[i]=w then
  begin
    case i div 94 of
      0:result:=$2121+i;
      1:result:=$2221+i-94;
      2:result:=$2821+i-94-94;
    end;
    exit;
  end;
  for i:=0 to NUMBER_EXTUNICODE-1 do if Table_ExtASCII[i]=w then
  begin
    result:=i+$80;
    exit;
  end;
  result:=0;
end;

function UTF2Unicode(b1,b2,b3,b4:byte;var inc:byte):word;
begin
  if (b1 and UTF8_MASK1)=UTF8_VALUE1 then
  begin
    result:=b1;
    inc:=1;
    exit;
  end else if (b1 and UTF8_MASK2)=UTF8_VALUE2 then
  begin
    result:=((b1 and $1f) shl 6) or (b2 and $3f);
    inc:=2;
    exit;
  end else if (b1 and UTF8_MASK3)=UTF8_VALUE3 then
  begin
    result:=((b1 and $0f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f);
    inc:=3;
    exit;
  end else if (b1 and UTF8_MASK4)=UTF8_VALUE4 then
  begin
    result:=$ffff;
    inc:=4;
  end;
end;

function _is(b:word;cl:byte):boolean;
begin
  case cl of
    IS_EUC:result:=(b>=161) and (b<=254);
    IS_HALFKATA:result:=(b>=161) and (b<=223);
    IS_SJIS1:result:=((b>=129) and (b<=159)) or ((b>=224) and (b<=239));
    IS_SJIS2:result:=(b>=64) and (b<=252);
    IS_MARU:result:=(b>=202) and (b<=206);
    IS_NIGORI:result:=((b>=182) and (b<=196)) or ((b>=202) and (b<=206)) or (b=179);
    IS_JIS:result:=(b and $7f00)>0;
  end;
end;

procedure _rewind;
begin
  bufpos:=1;
  buflen:=0;
  seek(f,0);
end;

function _fread:integer;
begin
  if bufpos>buflen then
  begin
    if eof(f) then begin
      result:=-1;
      exit;
    end;
    blockread(f,buf,1024,buflen);
    bufpos:=1;
  end;
  result:=buf[bufpos];
  inc(bufpos);
end;

function _freadw:integer;
var b1,b2:integer;
begin
  b1:=_fread; b2:=_fread;
  if (b1=-1) or (b2=-1) then result:=-1 else result:=b2*256+b1;
end;

function _detftype:byte;
var i,b,j:integer;
    eucsjis:boolean;
    asciionly:boolean;
begin
  _rewind;
  result:=0;
  i:=_freadw;
  if i=$feff then result:=FILETYPE_UNICODE;
  if i=$fffe then result:=FILETYPE_UNICODER;
  if i=$f1ff then result:=FILETYPE_WTT;
  if result=0 then
  begin
    result:=FILETYPE_UNICODE;
    eucsjis:=true;
    while (i<>-1) and (result=FILETYPE_UNICODE) do
    begin
      if Unicode2JIS(i)=0 then result:=0;
      if (i and $8080)<>$8080 then eucsjis:=false;
      i:=_freadw;
    end;
    if eucsjis then result:=0;
  end;
  if result=0 then
  begin
    _rewind;
    asciionly:=true;
    i:=_fread;
    result:=FILETYPE_UTF8;
    while (i<>-1) and (result=FILETYPE_UTF8) do
    begin
      b:=0;
      if (i and UTF8_MASK1)=UTF8_VALUE1 then b:=0 else
      if (i and UTF8_MASK2)=UTF8_VALUE2 then b:=1 else
      if (i and UTF8_MASK3)=UTF8_VALUE3 then b:=2 else
      if (i and UTF8_MASK4)=UTF8_VALUE4 then b:=3 else result:=0;
      if b>0 then asciionly:=false;
      for j:=0 to b-1 do
      begin
        i:=_fread;
        if not ((i and $c0)=$80) then result:=0;
      end;
      i:=_fread;
    end;
    if asciionly then result:=0;
  end;
  if result=0 then
  begin
    _rewind;
    result:=FILETYPE_ASCII;
    while (result=FILETYPE_ASCII) or (result=FILETYPE_EUCORSJIS) do
    begin
      i:=_fread;
      if i=-1 then break;
      if i=JIS_ESC then
      begin
        i:=_fread;
        if i=ord('$') then
        begin
          i:=_fread;
          if i=ord('B') then result:=FILETYPE_JIS;
          if i=ord('@') then result:=FILETYPE_OLD;
        end else if i=ord('K') then result:=FILETYPE_NEC;
      end else if i=JIS_SS2 then
      begin
        i:=_fread;
        if (i>=161) and (i<=223) then result:=FILETYPE_EUCORSJIS
        else if (i<>127) and (i>=64) and (i<=252) then result:=FILETYPE_SJS;
      end else if (i>=129) and (i<=159) then result:=FILETYPE_SJS
      else if (i>=161) and (i<=223) then
      begin
        i:=_fread;
        if (i>=240) and (i<=254) then result:=FILETYPE_EUC
        else if (i>=161) and (i<=223) then result:=FILETYPE_EUCORSJIS
        else if (i>=224) and (i<=239) then
        begin
          result:=FILETYPE_EUCORSJIS;
          while ((i>=64) and (result=FILETYPE_EUCORSJIS)) do
          begin
            if i>=129 then
            begin
              if (i<=141) or ((i>=143) and (i<=159)) then result:=FILETYPE_SJS else
              if (i>=253) and (i<=254) then result:=FILETYPE_EUC;
            end;
            i:=_fread;
            if i=-1 then break;
          end;
        end else if i<=159 then result:=FILETYPE_SJS;
      end else if (i>=240) and (i<=254) then result:=FILETYPE_EUC
      else if (i>=224) and (i<=239) then
      begin
        i:=_fread;
        if ((i>=64) and (i<=126)) or ((i>=128) and (i<=160)) then result:=FILETYPE_SJS
        else if (i>=253) and (i<=254) then result:=FILETYPE_EUC
        else if (i>=161) and (i<=252) then result:=FILETYPE_EUCORSJIS;
      end;
    end;
  end;
  if result=FILETYPE_EUCORSJIS then result:=FILETYPE_SJS;
end;

function _input(tp:byte):integer;
var i,i2:integer;
begin
  while true do
  begin
    i:=_fread;
    if i=-1 then begin result:=-1; exit; end;
    case tp of
      FILETYPE_UTF8:begin
                      if (i and UTF8_MASK2)=UTF8_VALUE2 then
                        i:=((i and $1f) shl 6) or (_fread and $3f)
                      else if (i and UTF8_MASK3)=UTF8_VALUE3 then
                        i:=((i and $0f) shl 12) or ((_fread and $3f) shl 6) or (_fread and $3f)
                      else if (i and UTF8_MASK4)=UTF8_VALUE4 then
                      begin
                        i:=0; _fread; _fread; _fread;
                      end;
                      result:=i; exit;
                    end;
      FILETYPE_UNICODER:begin i:=256*i+_fread; result:=i; exit; end;
      FILETYPE_UNICODE: begin i:=256*_fread+i; result:=i; exit; end;
      FILETYPE_ASCII: begin result:=i; exit; end;
      FILETYPE_EUC: begin if _is(i,IS_EUC) then result:=JIS2Unicode((i*256+_fread) and $7f7f) else result:=i; exit; end;
      FILETYPE_SJS: if _is(i,IS_SJIS1) then
                    begin
                      i2:=_fread;
                      if _is(i2,IS_SJIS2) then result:=JIS2Unicode(SJIS2JIS(i*256+i2)) else result:=JIS2Unicode(i*256+i2); exit;
                    end else begin result:=i; exit; end;
      FILETYPE_JIS, FILETYPE_OLD, FILETYPE_NEC: if i=JIS_ESC then
                                                begin
                                                  i2:=_fread;
                                                  if (i2=ord('$')) or (i2=ord('(')) then _fread;
                                                  if (i2=ord('K')) or (i2=ord('$')) then inp_intwobyte:=true else inp_intwobyte:=false;
                                                end else if (i=JIS_NL) or (i=JIS_CR) then
                                                begin
                                                  inp_intwobyte:=false;
                                                  result:=i; exit;
                                                end else
                                                begin
                                                  if inp_intwobyte then result:=JIS2Unicode(i*256+_fread) else result:=i;
                                                  exit;
                                                end;
      FILETYPE_GB: begin if (i>=$a1) and (i<=$fe) then
                   begin
                     i2:=_fread;
                     if (i2>=$a1) and (i2<=$fe) then result:=Table_GB[(i-$a0)*96+(i2-$a0)] else result:=i*256+i2;
                   end else result:=i; exit; end;
      FILETYPE_BIG5: begin if (i>=$a1) and (i<=$fe) then
                     begin
                       i2:=_fread;
                       if (i2>=$40) and (i2<=$7f) then result:=Table_Big5[(i-$a0)*160+(i2-$40)]
                       else if (i2>=$a1) and (i2<=$fe) then result:=Table_Big5[(i-$a0)*160+(i2-$a0)] else result:=i*256+i2;
                     end else result:=i; exit; end;
     end;
  end;
end;

procedure _fwrite(b:byte);
begin
  if bufpos>buflen then
  begin
    blockwrite(f,buf,bufpos-1);
    bufpos:=1;
  end;
  buf[bufpos]:=b;
  inc(bufpos);
end;

procedure _fputstart(tp:byte);
begin
  if inp_intwobyte then exit;
  inp_intwobyte:=true;
  _fwrite(JIS_ESC);
  if (tp=FILETYPE_NEC) then _fwrite(ord('K')) else
  begin
    _fwrite(ord('$'));
    if tp=FILETYPE_OLD then _fwrite(ord('@')) else _fwrite(ord('B'));
  end;
end;

procedure _fputend(tp:byte);
begin
  if not inp_intwobyte then exit;
  inp_intwobyte:=false;
  _fwrite(JIS_ESC);
  if (tp=FILETYPE_NEC) then _fwrite(ord('H')) else
  begin
    _fwrite(ord('('));
    _fwrite(ord('J'));
  end;
end;

procedure _output(tp:byte;w:word);
var i:integer;
begin
  case tp of
    FILETYPE_UTF8:if (w and UTF8_WRITE1)=0 then _fwrite(w mod 256) else
                  if (w and UTF8_WRITE2)=0 then begin _fwrite(UTF8_VALUE2 or (w shr 6)); _fwrite(UTF8_VALUEC or (w and $3f)); end else
                  begin _fwrite(UTF8_VALUE3 or (w shr 12)); _fwrite(UTF8_VALUEC or ((w shr 6) and $3f)); _fwrite(UTF8_VALUEC or (w and $3f)); end;
    FILETYPE_UNICODE:begin _fwrite(w mod 256); _fwrite(w div 256); end;
    FILETYPE_UNICODER:begin _fwrite(w div 256); _fwrite(w mod 256); end;
    FILETYPE_EUC:begin w:=Unicode2JIS(w);
                   if _is(w,IS_JIS) then
                   begin
                     _fwrite((w shr 8) or $80);
                     _fwrite((w mod 256) or $80);
                   end else if (w and $80)>0 then
                   begin
                     _fwrite(JIS_SS2);
                     _fwrite(w and $7f);
                   end else _fwrite(w);
                 end;
    FILETYPE_SJS:begin w:=Unicode2JIS(w);
                   if _is(w,IS_JIS) then
                   begin
                     w:=jis2sjis(w);
                     _fwrite(w div 256);
                     _fwrite(w mod 256);
                   end else _fwrite(w);
                 end;
    FILETYPE_JIS, FILETYPE_OLD, FILETYPE_NEC: begin
                                                if (w=13) or (w=10) then
                                                begin
                                                  _fputend(tp);
                                                  _fwrite(w);
                                                end else
                                                begin
                                                  w:=Unicode2JIS(w);
                                                  if _is(w,IS_JIS) then
                                                  begin
                                                    _fputstart(tp);
                                                    _fwrite(w div 256);
                                                    _fwrite(w mod 256);
                                                  end else begin _fputend(tp); _fwrite(w); end;
                                                end;
                                              end;
    FILETYPE_GB: if w<128 then _fwrite(w) else
                 begin
                   for i:=0 to 96*96-1 do if Table_GB[i]=w then begin _fwrite(i div 96+$a0); _fwrite(i mod 96+$a0); exit; end;
                   _fwrite(w div 256); _fwrite(w mod 256);
                 end;
    FILETYPE_BIG5: if w<128 then _fwrite(w) else
                   begin
                     for i:=0 to 96*160-1 do if Table_GB[i]=w then begin _fwrite(i div 96+$a0); _fwrite(i mod 96+$a0); exit; end;
                     _fwrite(w div 256); _fwrite(w mod 256);
                   end;
  end;
end;

function Conv_DetectType(filename:string):byte;
begin
  assignfile(f,filename);
  reset(f,1);
  _rewind;
  result:=_detftype;
  closefile(f);
end;

procedure Conv_Open(filename:string; tp:byte);
begin
  assignfile(f,filename);
  reset(f,1);
  _rewind;
  if (tp=FILETYPE_UNICODE) or (tp=FILETYPE_UNICODER) then begin _fread; _fread; end;
  ftp:=tp;
end;

//Reads one char as FString or returns empty string
function Conv_Read:FString;
var i:integer;
begin
  i:=_input(ftp);
 {$IFNDEF UNICODE}
  if i=-1 then result:='' else result:=Format('%4.4x',[i]);
 {$ELSE}
  if i=-1 then Result:='' else Result := WideChar(i);
 {$ENDIF}
end;

//Reads one char as char or returns:
//  Unicode: #$FFFF
//  Non-unicode: empty string
function Conv_ReadChar:FChar;
begin
 {$IFDEF UNICODE}
  Result := WideChar(_input(ftp));
 {$ELSE}
  Result := Conv_Read();
 {$ENDIF}
end;

procedure Conv_Create(filename:string; tp:byte);
begin
  assignfile(f,filename);
  rewrite(f,1);
  _rewind;
  if tp=FILETYPE_UNICODE then begin _fwrite(255); _fwrite(254); end;
  if tp=FILETYPE_UNICODER then begin _fwrite(254); _fwrite(255); end;
  ftp:=tp;
end;

procedure Conv_Write(s:FString);
{$IFNDEF UNICODE}
var s2:FString;
{$ENDIF}
begin
  while length(s)>0 do
  begin
   {$IFNDEF UNICODE}
    s2:=copy(s,1,4);
    delete(s,1,4);
    try
      _output(ftp,strtoint('0x'+s2));
    except
      _output(ftp,0);
    end;
   {$ELSE}
    _output(ftp,word(s[1]));
    delete(s,1,1);
   {$ENDIF}
  end;
end;

procedure Conv_WriteChar(s:FChar);
begin
 {$IFDEF UNICODE}
  _output(ftp, Word(s));
 {$ELSE}
  Conv_Write(s);
 {$ENDIF}
end;

procedure Conv_Rewind;
begin
  _rewind;
  if (ftp=FILETYPE_UNICODE) or (ftp=FILETYPE_UNICODER) then begin _fread; _fread; end;
end;

procedure Conv_Flush;
begin
  blockwrite(f,buf,bufpos-1);
end;

procedure Conv_Close;
begin
  closefile(f);
end;

function Conv_ChooseType(chinese:boolean; def:byte):byte;
begin
  result:=0;
  if def=FILETYPE_WTT then begin result:=def; exit; end;
  if chinese then
  begin
    fFileType.rgType.Items.Clear;
    fFileType.rgType.Items.Add('Unicode (UCS2)');
    fFileType.rgType.Items.Add('UTF-8');
    fFileType.rgType.Items.Add('Big5');
    fFileType.rgType.Items.Add('GB2312');
    fFileType.rgType.Items.Add('Unicode (UCS2) reversed bytes');
    case def of
      FILETYPE_UNICODE: fFileType.rgType.ItemIndex:=0;
      FILETYPE_UTF8: fFileType.rgType.ItemIndex:=1;
      FILETYPE_BIG5: fFileType.rgType.ItemIndex:=2;
      FILETYPE_GB: fFileType.rgType.ItemIndex:=3;
      FILETYPE_UNICODER: fFileType.rgType.ItemIndex:=4;
      else fFileType.rgType.ItemIndex:=0;
    end;
    if fFileType.ShowModal=mrOK then
    case fFileType.rgType.ItemIndex of
      0: result:=FILETYPE_UNICODE;
      1: result:=FILETYPE_UTF8;
      2: result:=FILETYPE_BIG5;
      3: result:=FILETYPE_GB;
      4: result:=FILETYPE_UNICODER;
    end else result:=0;
  end else
  begin
    fFileType.rgType.Items.Clear;
    fFileType.rgType.Items.Add('Unicode (UCS2)');
    fFileType.rgType.Items.Add('UTF-8');
    fFileType.rgType.Items.Add('Shift-JIS');
    fFileType.rgType.Items.Add('JIS');
    fFileType.rgType.Items.Add('Old JIS');
    fFileType.rgType.Items.Add('NEC JIS');
    fFileType.rgType.Items.Add('EUC');
    fFileType.rgType.Items.Add('Unicode (UCS2) reversed bytes');
    case def of
      FILETYPE_UNICODE: fFileType.rgType.ItemIndex:=0;
      FILETYPE_UTF8: fFileType.rgType.ItemIndex:=1;
      FILETYPE_SJS: fFileType.rgType.ItemIndex:=2;
      FILETYPE_JIS: fFileType.rgType.ItemIndex:=3;
      FILETYPE_OLD: fFileType.rgType.ItemIndex:=4;
      FILETYPE_NEC: fFileType.rgType.ItemIndex:=5;
      FILETYPE_EUC: fFileType.rgType.ItemIndex:=6;
      FILETYPE_UNICODER: fFileType.rgType.ItemIndex:=7;
      else fFileType.rgType.ItemIndex:=0;
    end;
    if fFileType.ShowModal=mrOK then
    case fFileType.rgType.ItemIndex of
      0: result:=FILETYPE_UNICODE;
      1: result:=FILETYPE_UTF8;
      2: result:=FILETYPE_SJS;
      3: result:=FILETYPE_JIS;
      4: result:=FILETYPE_OLD;
      5: result:=FILETYPE_NEC;
      6: result:=FILETYPE_EUC;
      7: result:=FILETYPE_UNICODER;
    end;
  end;
end;

end.
