unit JWBConvert;

interface
uses Classes, JWBStrings;

const
 //Conversion types
  FILETYPE_UNKNOWN=0;
  FILETYPE_UTF16LE=1;
  FILETYPE_UTF16BE=2;
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

type
  TJwbConvert = class
  protected
    FStream: TStream;
    FOwnsStream: boolean;
    FEOF: boolean;
    ftp:byte;
    buf:array[1..1024] of byte;
    bufpos:integer;
    buflen:integer;
    inp_intwobyte:boolean;
    function _fread:integer;
    function _freadw:integer;
    procedure _rewind;
    function _detftype(out tp:byte): boolean;
    function _input(tp:byte):integer;
    procedure _fwrite(b:byte);
    procedure _fputstart(tp:byte);
    procedure _fputend(tp:byte);
    procedure _output(tp:byte;w:word);
  public
    constructor Open(AStream: TStream; tp: byte); overload;
    constructor Open(const filename: string; tp: byte); overload;
    destructor Destroy; override;
    function DetectType: byte;
    function DetectTypeEx(out tp: byte): boolean;
    procedure RewindAsType(tp: byte);
    procedure RewriteAsType(tp: byte);
    function EOF: boolean;
    function Read: FString; //reads one char as FString
    function ReadChar(out ch:FChar): boolean; //reads one char as FChar
    function ReadLn: FString;
    procedure Rewind;
    constructor CreateNew(AStream: TStream; tp: byte); overload;
    constructor CreateNew(const filename: string; tp: byte); overload;
    procedure Write(s:FString);
    procedure WriteChar(s:FChar);
    procedure Flush;
    property Tp: byte read ftp;
    property Stream: TStream read FStream;
    property OwnsStream: boolean read FOwnsStream write FOwnsStream;
  end;

function Conv_DetectType(filename:string):byte;
function Conv_DetectTypeEx(filename:string; out tp:byte): boolean;
procedure Conv_Open(filename:string; tp:byte);
function Conv_EOF:boolean;
function Conv_Read:FString; //reads one char as FString
function Conv_ReadChar(out ch:FChar): boolean; //reads one char as FChar
function Conv_ReadLn:FString;
procedure Conv_Create(filename:string; tp:byte);
procedure Conv_Write(s:FString);
procedure Conv_WriteChar(s:FChar);
function Conv_IsOpen: boolean;
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
      JIS_NL=10;          // New Line char.
      JIS_CR=13;         // Carrage Return.
      JIS_ESC=27;          // Escape.
      JIS_SS2=142;         // Half-width katakana marker.

type TUTFArray=array[0..3] of byte;

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
  result:=0;//default case
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
  end else begin
    Result:=b1; //because we don't know what else to do
    inc:=1;
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
  else result:=false;
  end;
end;


{ TJwbConvert }

constructor TJwbConvert.Open(AStream: TStream; tp: byte);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := true;
  RewindAsType(tp);
end;

constructor TJwbConvert.Open(const filename: string; tp: byte);
begin
  Open(TFileStream.Create(filename, fmOpenRead), tp);
end;

constructor TJwbConvert.CreateNew(AStream: TStream; tp: byte);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := true;
  FEOF := true; //who cares?
  RewriteAsType(tp);
end;

constructor TJwbConvert.CreateNew(const filename: string; tp: byte);
begin
  CreateNew(TFileStream.Create(filename,fmCreate), tp);
end;

destructor TJwbConvert.Destroy;
begin
  if OwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

//Call after determining stream type to start reading
procedure TJwbConvert.RewindAsType(tp: byte);
begin
  _rewind;
  if (tp=FILETYPE_UTF16LE) or (tp=FILETYPE_UTF16BE) then begin _fread; _fread; end;
  ftp:=tp;
end;

//Call to restart the file in the specified encoding
procedure TJwbConvert.RewriteAsType(tp: byte);
begin
  _rewind;
  if tp=FILETYPE_UTF16LE then begin _fwrite(255); _fwrite(254); end;
  if tp=FILETYPE_UTF16BE then begin _fwrite(254); _fwrite(255); end;
  ftp:=tp;
end;

//Returns -1 if no char is available. It's fine because we return integer.
function TJwbConvert._fread:integer;
begin
  if bufpos>buflen then
  begin
    buflen := FStream.Read(buf,1024);
    if buflen<=0 then begin
      FEOF := true;
      Result := -1;
      exit;
    end;
    bufpos:=1;
  end;
  result:=buf[bufpos];
  inc(bufpos);
end;

function TJwbConvert._freadw:integer;
var b1,b2:integer;
begin
  b1:=_fread; b2:=_fread;
  if (b1=-1) or (b2=-1) then result:=-1 else result:=b2*256+b1;
end;

procedure TJwbConvert._rewind;
begin
  FEOF := false;
  bufpos:=1;
  buflen:=0;
  FStream.Seek(0,soFromBeginning);
end;

//Detects file encoding. Returns true if it's truly detected (i.e. through BOM),
//or false if guessed.
function TJwbConvert._detftype(out tp:byte): boolean;
var i,b,j:integer;
    eucsjis:boolean;
    asciionly:boolean;
begin
  _rewind;
  tp:=0;
  Result := false;
  i:=_freadw;

  if i=$feff then begin
    tp:=FILETYPE_UTF16LE;
    Result:=true;
    exit;
  end;
  if i=$fffe then begin
    tp:=FILETYPE_UTF16BE;
    Result:=true;
    exit;
  end;
  if i=$f1ff then begin
    tp:=FILETYPE_WTT;
    Result:=true;
    exit;
  end;

 //UTF-8 BOM: 0xEF, 0xBB, 0xBF
  if (i=$BBEF) and (_fread() = $BF) then begin
    tp:=FILETYPE_UTF8;
    Result:=true;
    exit;
  end else begin
    _rewind;
    i := _freadw;
  end;

  tp:=FILETYPE_UTF16LE;
  eucsjis:=true;
  while (i<>-1) and (tp=FILETYPE_UTF16LE) do
  begin
    if Unicode2JIS(i)=0 then tp:=0;
    if (i and $8080)<>$8080 then eucsjis:=false;
    i:=_freadw;
  end;
  if eucsjis then tp:=0;

  if tp=0 then
  begin
    _rewind;
    asciionly:=true;
    i:=_fread;
    tp:=FILETYPE_UTF8;
    while (i<>-1) and (tp=FILETYPE_UTF8) do
    begin
      b:=0;
      if (i and UTF8_MASK1)=UTF8_VALUE1 then b:=0 else
      if (i and UTF8_MASK2)=UTF8_VALUE2 then b:=1 else
      if (i and UTF8_MASK3)=UTF8_VALUE3 then b:=2 else
      if (i and UTF8_MASK4)=UTF8_VALUE4 then b:=3 else tp:=0;
      if b>0 then asciionly:=false;
      for j:=0 to b-1 do
      begin
        i:=_fread;
        if not ((i and $c0)=$80) then tp:=0;
      end;
      i:=_fread;
    end;
    if asciionly then tp:=0;
  end;
  if tp=0 then
  begin
    _rewind;
    tp:=FILETYPE_ASCII;
    while (tp=FILETYPE_ASCII) or (tp=FILETYPE_EUCORSJIS) do
    begin
      i:=_fread;
      if i=-1 then break;
      if i=JIS_ESC then
      begin
        i:=_fread;
        if i=ord('$') then
        begin
          i:=_fread;
          if i=ord('B') then tp:=FILETYPE_JIS;
          if i=ord('@') then tp:=FILETYPE_OLD;
        end else if i=ord('K') then tp:=FILETYPE_NEC;
      end else if i=JIS_SS2 then
      begin
        i:=_fread;
        if (i>=161) and (i<=223) then tp:=FILETYPE_EUCORSJIS
        else if (i<>127) and (i>=64) and (i<=252) then tp:=FILETYPE_SJS;
      end else if (i>=129) and (i<=159) then tp:=FILETYPE_SJS
      else if (i>=161) and (i<=223) then
      begin
        i:=_fread;
        if (i>=240) and (i<=254) then tp:=FILETYPE_EUC
        else if (i>=161) and (i<=223) then tp:=FILETYPE_EUCORSJIS
        else if (i>=224) and (i<=239) then
        begin
          tp:=FILETYPE_EUCORSJIS;
          while ((i>=64) and (tp=FILETYPE_EUCORSJIS)) do
          begin
            if i>=129 then
            begin
              if (i<=141) or ((i>=143) and (i<=159)) then tp:=FILETYPE_SJS else
              if (i>=253) and (i<=254) then tp:=FILETYPE_EUC;
            end;
            i:=_fread;
            if i=-1 then break;
          end;
        end else if i<=159 then tp:=FILETYPE_SJS;
      end else if (i>=240) and (i<=254) then tp:=FILETYPE_EUC
      else if (i>=224) and (i<=239) then
      begin
        i:=_fread;
        if ((i>=64) and (i<=126)) or ((i>=128) and (i<=160)) then tp:=FILETYPE_SJS
        else if (i>=253) and (i<=254) then tp:=FILETYPE_EUC
        else if (i>=161) and (i<=252) then tp:=FILETYPE_EUCORSJIS;
      end;
    end;
  end;
  if tp=FILETYPE_EUCORSJIS then tp:=FILETYPE_SJS;
end;

//Returns -1 if no char is available. It's fine because we return integer.
function TJwbConvert._input(tp:byte):integer;
var i,i2,i3:integer;
begin
  Result := -1;
  while true do
  begin
    i:=_fread;
    if i<0 then exit;

    case tp of
      FILETYPE_UTF8: begin
        if (i and UTF8_MASK2)=UTF8_VALUE2 then begin
          i2:=_fread; if i2<0 then exit;
          Result:=((i and $1f) shl 6) or (i2 and $3f);
        end else
        if (i and UTF8_MASK3)=UTF8_VALUE3 then begin
          i2:=_fread; i3:=_fread; if (i2<0) or (i3<0) then exit;
          Result:=((i and $0f) shl 12) or ((i2 and $3f) shl 6) or (i3 and $3f)
        end else
        if (i and UTF8_MASK4)=UTF8_VALUE4 then
        begin
          _fread; _fread; _fread;
          Result:=0;
        end else
          Result:=i;
        exit;
      end;
      FILETYPE_UTF16BE: begin i2:=_fread; if i2<0 then exit; result:=256*i+i2; exit; end;
      FILETYPE_UTF16LE: begin i2:=_fread; if i2<0 then exit; result:=256*i2+i; exit; end;
      FILETYPE_ASCII: begin result:=i; exit; end;
      FILETYPE_EUC: begin
        if _is(i,IS_EUC) then begin
          i2:=_fread; if i2<0 then exit;
          result:=JIS2Unicode((i*256+i2) and $7f7f);
        end else
          result:=i;
        exit;
      end;
      FILETYPE_SJS:
      begin
        if _is(i,IS_SJIS1) then
        begin
          i2:=_fread; if i2<0 then exit;
          if _is(i2,IS_SJIS2) then result:=JIS2Unicode(SJIS2JIS(i*256+i2)) else result:=JIS2Unicode(i*256+i2);
        end else
          Result:=i;
        exit;
      end;
      FILETYPE_JIS, FILETYPE_OLD, FILETYPE_NEC:
        if i=JIS_ESC then
        begin
          i2:=_fread; if i2<0 then exit;
          if (i2=ord('$')) or (i2=ord('(')) then _fread;
          if (i2=ord('K')) or (i2=ord('$')) then inp_intwobyte:=true else inp_intwobyte:=false;
         //Do not exit, continue to next char
        end else begin
          if (i=JIS_NL) or (i=JIS_CR) then
          begin
            inp_intwobyte:=false;
            result:=i;
          end else begin
            i2:=_fread; if i2<0 then exit;
            if inp_intwobyte then result:=JIS2Unicode(i*256+i2) else result:=i;
          end;
          exit;
        end;
      FILETYPE_GB:
      begin
        if (i>=$a1) and (i<=$fe) then
        begin
          i2:=_fread; if i2<0 then exit;
          if (i2>=$a1) and (i2<=$fe) then result:=Table_GB[(i-$a0)*96+(i2-$a0)] else result:=i*256+i2;
        end else
          result:=i;
        exit;
      end;
      FILETYPE_BIG5:
      begin
        if (i>=$a1) and (i<=$fe) then
        begin
          i2:=_fread; if i2<0 then exit;
          if (i2>=$40) and (i2<=$7f) then result:=Table_Big5[(i-$a0)*160+(i2-$40)]
          else if (i2>=$a1) and (i2<=$fe) then result:=Table_Big5[(i-$a0)*160+(i2-$a0)] else result:=i*256+i2;
        end else
          result:=i;
        exit;
      end;

    end; //of case(tp)
  end; //of while true
end;

procedure TJwbConvert._fwrite(b:byte);
begin
  if bufpos>buflen then
  begin
    FStream.Write(buf,bufpos-1);
    bufpos:=1;
  end;
  buf[bufpos]:=b;
  inc(bufpos);
end;

procedure TJwbConvert._fputstart(tp:byte);
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

procedure TJwbConvert._fputend(tp:byte);
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

procedure TJwbConvert._output(tp:byte;w:word);
var i:integer;
begin
  case tp of
    FILETYPE_UTF8:if (w and UTF8_WRITE1)=0 then _fwrite(w mod 256) else
                  if (w and UTF8_WRITE2)=0 then begin _fwrite(UTF8_VALUE2 or (w shr 6)); _fwrite(UTF8_VALUEC or (w and $3f)); end else
                  begin _fwrite(UTF8_VALUE3 or (w shr 12)); _fwrite(UTF8_VALUEC or ((w shr 6) and $3f)); _fwrite(UTF8_VALUEC or (w and $3f)); end;
    FILETYPE_UTF16LE:begin _fwrite(w mod 256); _fwrite(w div 256); end;
    FILETYPE_UTF16BE:begin _fwrite(w div 256); _fwrite(w mod 256); end;
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

function TJwbConvert.DetectType: byte;
begin
  _rewind;
  _detftype(result);
end;

//Makes a guess about file encoding, and returns true if it's a sure thing (i.e. there's BOM).
function TJwbConvert.DetectTypeEx(out tp: byte): boolean;
begin
  _rewind;
  Result:=_detftype(tp);
end;

//Since Conv_ReadChar might legitimately return FFFF, check this before deciding file's over.
function TJwbConvert.EOF: boolean;
begin
  Result := FEOF;
end;

//Reads one char as FString or returns empty string
function TJwbConvert.Read: FString;
var i:integer;
begin
  i:=_input(ftp);
 {$IFNDEF UNICODE}
  if i=-1 then result:='' else result:=Format('%4.4x',[i]);
 {$ELSE}
  if i=-1 then Result:='' else Result := WideChar(i);
 {$ENDIF}
end;

//Reads one char as char or returns false
function TJwbConvert.ReadChar(out ch:FChar): boolean;
var i: integer;
begin
 {$IFDEF UNICODE}
  i := _input(ftp);
  Result := (i>=0);
  if Result then ch := WideChar(i);
 {$ELSE}
  i := _input(ftp);
  Result := (i>=0);
  if Result then ch := Format('%4.4x',[i]);
 {$ENDIF}
end;

function TJwbConvert.ReadLn: FString;
var c: FChar;
begin
  Result := '';
  while Conv_ReadChar(c) and (c<>UH_LF) do begin
    if c<>UH_CR then
      Result := Result + c;
  end;
end;

procedure TJwbConvert.Rewind;
begin
  _rewind;
 //Skip BOM if present
  case ftp of
    FILETYPE_UTF16LE:
      if (_fread<>$ff) or (_fread<>$fe) then
        _rewind;
    FILETYPE_UTF16BE:
      if (_fread<>$fe) or (_fread<>$ff) then
        _rewind;
    FILETYPE_UTF8:
      if (_fread<>$ef) or (_fread<>$bb) or (_fread<>$bf) then
        _rewind;
  end;
end;

procedure TJwbConvert.Write(s:FString);
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

procedure TJwbConvert.WriteChar(s:FChar);
begin
 {$IFDEF UNICODE}
  _output(ftp, Word(s));
 {$ELSE}
  Conv_Write(s);
 {$ENDIF}
end;

procedure TJwbConvert.Flush;
begin
  FStream.Write(buf,bufpos-1)
end;


var
  DefaultConvert: TJwbConvert;

function Conv_DetectType(filename:string):byte;
begin
  DefaultConvert := TJwbConvert.Open(filename, FILETYPE_UNKNOWN);
  try
    Result := DefaultConvert.DetectType;
  finally
    FreeAndNil(DefaultConvert);
  end;
end;

function Conv_DetectTypeEx(filename:string; out tp:byte): boolean;
begin
  DefaultConvert := TJwbConvert.Open(filename, FILETYPE_UNKNOWN);
  try
    Result := DefaultConvert.DetectTypeEx(tp);
  finally
    FreeAndNil(DefaultConvert);
  end;
end;

procedure Conv_Open(filename:string; tp:byte);
begin
  DefaultConvert := TJwbConvert.Open(filename, tp);
end;

function Conv_EOF:boolean;
begin
  Result := DefaultConvert.EOF;
end;

function Conv_Read:FString;
begin
  Result := DefaultConvert.Read;
end;

function Conv_ReadChar(out ch:FChar): boolean;
begin
  Result := DefaultConvert.ReadChar(ch);
end;

function Conv_ReadLn:FString;
begin
  Result := DefaultConvert.ReadLn;
end;

procedure Conv_Create(filename:string; tp:byte);
begin
  DefaultConvert := TJwbConvert.CreateNew(filename, tp);
end;

procedure Conv_Write(s:FString);
begin
  DefaultConvert.Write(s);
end;

procedure Conv_WriteChar(s:FChar);
begin
  DefaultConvert.WriteChar(s);
end;

function Conv_IsOpen: boolean;
begin
  Result := DefaultConvert<>nil;
end;

procedure Conv_Close;
begin
  FreeAndNil(DefaultConvert);
end;

procedure Conv_Flush;
begin
  DefaultConvert.Flush;
end;

procedure Conv_Rewind;
begin
  DefaultConvert.Rewind;
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
      FILETYPE_UTF16LE: fFileType.rgType.ItemIndex:=0;
      FILETYPE_UTF8: fFileType.rgType.ItemIndex:=1;
      FILETYPE_BIG5: fFileType.rgType.ItemIndex:=2;
      FILETYPE_GB: fFileType.rgType.ItemIndex:=3;
      FILETYPE_UTF16BE: fFileType.rgType.ItemIndex:=4;
      else fFileType.rgType.ItemIndex:=0;
    end;
    if fFileType.ShowModal=mrOK then
    case fFileType.rgType.ItemIndex of
      0: result:=FILETYPE_UTF16LE;
      1: result:=FILETYPE_UTF8;
      2: result:=FILETYPE_BIG5;
      3: result:=FILETYPE_GB;
      4: result:=FILETYPE_UTF16BE;
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
      FILETYPE_UTF16LE: fFileType.rgType.ItemIndex:=0;
      FILETYPE_UTF8: fFileType.rgType.ItemIndex:=1;
      FILETYPE_SJS: fFileType.rgType.ItemIndex:=2;
      FILETYPE_JIS: fFileType.rgType.ItemIndex:=3;
      FILETYPE_OLD: fFileType.rgType.ItemIndex:=4;
      FILETYPE_NEC: fFileType.rgType.ItemIndex:=5;
      FILETYPE_EUC: fFileType.rgType.ItemIndex:=6;
      FILETYPE_UTF16BE: fFileType.rgType.ItemIndex:=7;
      else fFileType.rgType.ItemIndex:=0;
    end;
    if fFileType.ShowModal=mrOK then
    case fFileType.rgType.ItemIndex of
      0: result:=FILETYPE_UTF16LE;
      1: result:=FILETYPE_UTF8;
      2: result:=FILETYPE_SJS;
      3: result:=FILETYPE_JIS;
      4: result:=FILETYPE_OLD;
      5: result:=FILETYPE_NEC;
      6: result:=FILETYPE_EUC;
      7: result:=FILETYPE_UTF16BE;
    end;
  end;
end;

end.
