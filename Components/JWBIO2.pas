unit JWBIO2;

interface
uses SysUtils, Classes, JWBStrings;

{ TODO: Buffered _fread/_fwrite/_fseeks }
{ TODO: Active code page (through buffering/QueryNextBlock) }
{ TODO: RtlDecoder(TEncoding) }

type
 { Base classes }
  TStreamDecoder = class
  protected
    FStream: TStream;
    FOwnsStream: boolean;
    FEOF: boolean; //set when you cannot read one more entry from the stream
   { You can directly access Stream or you can read/write with low level functions.
    This gives you some preimplemented caching + peeking + handy functions }
    function _fread1: integer; inline; //[0..255], -1 if EOF
    function _fread2: integer; inline; //[0..65535], -1 if EOF
    function _fread(var buf; const sz: integer): integer; //returns size read
    procedure _fseek(const AOffset: integer; AOrigin: TSeekOrigin);
    function _fpeek(var buf; const sz: integer): integer;
  public
    constructor Open(AStream: TStream; AOwnsStream: boolean = false); overload; virtual;
    constructor Open(const AFilename: string); overload;
    destructor Destroy; override;
    procedure DetachStream; virtual; //clears whatever caches the instance may have for the Stream
   { Since this may require backward seek, try not to TrySkipBom for sources where
    you do not really expect it (i.e. console) }
    procedure TrySkipBom; virtual;
    function EOF: boolean; virtual;
    function ReadChar(out ch: WideChar): boolean; overload; virtual;
   { If reading next position produces a surrogate pair, store it somewhere and
    return in two calls. }
    function ReadLn(out ln: UnicodeString): boolean; overload; virtual;
    function ReadChar: WideChar; overload; //#0000 if no char
    function ReadLn: UnicodeString; overload; //empty string if no string
   { Or should these raise exceptions? }
    property Stream: TStream read FStream;
    property OwnsStream: boolean read FOwnsStream;
  end;
  CStreamDecoder = class of TStreamDecoder;

  TStreamEncoder = class
  protected
    FStream: TStream;
    FOwnsStream: boolean;
   //Shortcuts because you can't do _fwrite() for a constant expression (you need var)
    procedure _fwrite1(const b: byte); inline;
    procedure _fwrite2(const w: word); inline;
    procedure _fwrite3(const dw: integer); inline;
    procedure _fwrite4(const dw: integer); inline;
    function _fwrite(const buf; const sz: integer): integer; //returns size written
  public
    constructor Open(AStream: TStream; AOwnsStream: boolean = false); virtual;
    constructor CreateNew(const AFilename: string);
    constructor Append(const AFilename: string);
    destructor Destroy; override;
    procedure Flush; virtual; //clears whatever caches instance may have for the stream
    procedure WriteBom; virtual;
    procedure WriteChar(const ch: WideChar); virtual;
    procedure Write(const ln: UnicodeString); virtual;
    procedure WriteLn(const ln: UnicodeString);
    property Stream: TStream read FStream;
    property OwnsStream: boolean read FOwnsStream;
  end;
  CStreamEncoder = class of TStreamDecoder;

 { Simple encodings }
 { TODO: UTF16 surrogate pairs (LE and BE in different order) }

  TAnsiDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TAnsiEncoder = class(TStreamEncoder)
    procedure WriteChar(const ch: WideChar); override;
  end;

 { TODO: ACP (Active codepage, similar to ANSI but with local [128..255]) }

  TUTF8Decoder = class(TStreamDecoder)
    procedure TrySkipBom; override;
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TUTF8Encoder = class(TStreamEncoder)
    procedure WriteBom; override;
    procedure WriteChar(const ch: WideChar); override;
  end;

  TUTF16LEDecoder = class(TStreamDecoder)
    procedure TrySkipBom; override;
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TUTF16LEEncoder = class(TStreamEncoder)
    procedure WriteBom; override;
    procedure WriteChar(const ch: WideChar); override;
  end;

  TUTF16BEDecoder = class(TStreamDecoder)
    procedure TrySkipBom; override;
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TUTF16BEEncoder = class(TStreamEncoder)
    procedure WriteBom; override;
    procedure WriteChar(const ch: WideChar); override;
  end;

  TUTF16Decoder = TUTF16LEDecoder;
  TUTF16Encoder = TUTF16LEEncoder;
  TUnicodeDecoder = TUTF16LEDecoder;
  TUnicodeEncoder = TUTF16LEEncoder;

 { Compability names }
  TAnsiFileReader = TAnsiDecoder;
  TAnsiFileWriter = TAnsiEncoder;
  TUnicodeFileReader = TUnicodeDecoder;
  TUnicodeFileWriter = TUnicodeEncoder;
 {$IFDEF UNICODE}
  TFileReader = TUnicodeFileReader;
  TFileWriter = TUnicodeFileWriter;
 {$ELSE}
  TFileReader = TAnsiFileReader;
  TFileWriter = TAnsiFileWriter;
 {$ENDIF}

 { We do not use Delphi's RTL TEncodings because those do not support converting
  only a part of the buffer.
  We may write a wrapper using some tricks (convert too much + drop ending) maybe. }

{ Wakan decoders }

  TEUCDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TEUCEncoder = class(TStreamEncoder)
    procedure WriteChar(const ch: WideChar); override;
  end;

  TSJSDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TSJSEncoder = class(TStreamEncoder)
    procedure WriteChar(const ch: WideChar); override;
  end;

 { Supports New-JIS, Old-JIS and NEC-JIS. Decoder handles all of these, but encoder
  requires specifying one before writing. }
  TJISDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TJISType = (jtNew = 0, jtOld, jtNec);
  TJISEncoder = class(TStreamEncoder)
  protected
    FJISType: TJISType;
    intwobyte: boolean;
    procedure _fputstart();
    procedure _fputend();
  public
    procedure WriteChar(const ch: WideChar); override;
    property JISType: TJISType read FJISType write FJISType;
  end;

  TGBDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TGBEncoder = class(TStreamEncoder)
    procedure WriteChar(const ch: WideChar); override;
  end;

  TBIG5Decoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;
  TBIG5Encoder = class(TStreamEncoder)
    procedure WriteChar(const ch: WideChar); override;
  end;

function Conv_DetectType(AStream: TStream): TStreamDecoder;
function Conv_DetectTypeEx(AStream: TStream; out ADecoder: TStreamDecoder): boolean;
function Conv_ChooseType(AChinese:boolean; ADefault: CStreamDecoder): TStreamDecoder;

(*
function Conv_DetectType(filename:string):byte;
function Conv_DetectTypeEx(filename:string; out tp:byte): boolean;
function Conv_ChooseType(chinese:boolean; def:byte):byte;
*)

implementation
uses Controls, JWBConvertTbl, JWBFileType;

{ Stream decoder }

constructor TStreamDecoder.Open(AStream: TStream; AOwnsStream: boolean = false);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FEOF := false;
 { Extend this function in descendants }
end;

constructor TStreamDecoder.Open(const AFilename: string);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  Self.Open(fs, {OwnsStream=}true);
end;

destructor TStreamDecoder.Destroy;
begin
  if not FOwnsStream then
    DetachStream; //if the stream is ours, no point in restoring it's actual position
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

function TStreamDecoder._fread1: integer;
begin
  Result := 0; //higher bytes
  if _fread(Result, 1)<>1 then
    Result := -1;
end;

function TStreamDecoder._fread2: integer;
begin
  Result := 0; //higher bytes
  if _fread(Result, 1)<>2 then
    Result := -1;
end;

function TStreamDecoder._fread(var buf; const sz: integer): integer;
begin
  if FEOF then
    Result := 0
  else begin
    Result := Stream.Read(buf, sz);
    if Result=0 then
      FEOF := true;
  end;
end;

procedure TStreamDecoder._fseek(const AOffset: integer; AOrigin: TSeekOrigin);
begin
  Stream.Seek(AOffset, AOrigin); //TODO: change if using buffering
end;

function TStreamDecoder._fpeek(var buf; const sz: integer): integer;
begin
 //No-buffering version (requires seekable stream):
  Result := _fread(buf, sz);
  if Result>0 then
    _fseek(-Result, soCurrent);
  //TODO: Implement buffered version? Or just rely on _fseek which supports limited seek in buffer at all times?
  // In the second case we handle this transparently and seek as much as we can,
  // but in the first case we keep strange requests (peek > buffer size) in check
  // but it requires special coding.
end;

procedure TStreamDecoder.DetachStream;
begin
 { Implement in descendants if needed }
end;

procedure TStreamDecoder.TrySkipBom;
begin
 { Implement in descendants if BOM is supported }
end;

function TStreamDecoder.EOF: boolean;
begin
 { Reimplement in descendants if needed }
  Result := FEOF;
end;

function TStreamDecoder.ReadChar(out ch: WideChar): boolean;
begin
 { Implement in descendants. Note that FEOF is being set by _fread so no need to
  do it manually if you use it. }
  FEOF := true;
  Result := false;
end;

function TStreamDecoder.ReadLn(out ln: UnicodeString): boolean;
var ch: WideChar;
begin
 { Reimplement in descendants if you have a better way }
  ln := '';
  Result := false;
  while ReadChar(ch) do begin
   { Maybe a more thorough CRLF/CR/LF handling is needed }
    if ch=#$000D then begin
     //ignore
    end else
    if ch=#$000A then
      break
    else
      ln := ln + ch;
    Result := true;
  end;
end;

function TStreamDecoder.ReadChar: WideChar;
begin
  if not ReadChar(Result) then
    Result := #0000;
end;

function TStreamDecoder.ReadLn: UnicodeString;
begin
  if not ReadLn(Result) then
    Result := '';
end;


{ Stream encoder }

constructor TStreamEncoder.Open(AStream: TStream; AOwnsStream: boolean = false);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
 { Extend this function in descendants }
end;

constructor TStreamEncoder.CreateNew(const AFilename: string);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  Self.Open(fs,{OwnsStream=}true);
end;

constructor TStreamEncoder.Append(const AFilename: string);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  fs.Seek(0,soFromEnd);
  Self.Open(fs,{OwnsStream=}true);
end;

destructor TStreamEncoder.Destroy;
begin
  Flush();
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

{ Writes 1 byte }
procedure TStreamEncoder._fwrite1(const b: byte);
begin
  _fwrite(b, 1);
end;

{ Writes 2 bytes }
procedure TStreamEncoder._fwrite2(const w: word);
begin
  _fwrite(w, 2);
end;

{ Writes 3 lower bytes }
procedure TStreamEncoder._fwrite3(const dw: integer);
begin
  _fwrite(dw, 3);
end;

{ Writes 4 bytes }
procedure TStreamEncoder._fwrite4(const dw: integer);
begin
  _fwrite(dw, 4);
end;

function TStreamEncoder._fwrite(const buf; const sz: integer): integer;
begin
  Result := Stream.Write(buf, sz);
end;

procedure TStreamEncoder.Flush;
begin
 { Implement in descendants if needed }
end;

procedure TStreamEncoder.WriteBom;
begin
 { Implement in descendants if supported }
end;

procedure TStreamEncoder.WriteChar(const ch: WideChar);
begin
 { Implement in descendants }
end;

procedure TStreamEncoder.Write(const ln: UnicodeString);
var i: integer;
begin
 { Reimplement in descendants if you have a better way }
  for i := 1 to Length(ln) do
    WriteChar(ln[i]);
end;

procedure TStreamEncoder.WriteLn(const ln: UnicodeString);
begin
  Write(ln+#$000D+#$000A);
end;


{ Simple encodings }

function TAnsiDecoder.ReadChar(out ch: WideChar): boolean;
var ac: AnsiChar;
begin
  Result := _fread(ac,SizeOf(ac))=SizeOf(ac);
  if Result then
    ch := WideChar(ac);
end;

procedure TAnsiEncoder.WriteChar(const ch: WideChar);
var ac: AnsiChar;
begin
  ac := AnsiChar(ch); //Maybe more sophisticated conversion?
  _fwrite(ac,SizeOf(ac)); //Maybe check for errors?
end;

procedure TUTF8Decoder.TrySkipBom;
var bom: integer;
  read_sz: integer;
begin
  bom := 0; //zero high order byte
  read_sz := _fread(bom, 3);
  if (read_sz<>3) or (bom<>$BFBBEF) then
    _fseek(-read_sz,soCurrent);
end;

function TUTF8Decoder.ReadChar(out ch: WideChar): boolean;
var b1, b2, b3: byte;
  tmp: integer;
begin
  Result := _fread(b1, 1)=1;
  if not Result then exit;

  if (b1 and UTF8_MASK2)=UTF8_VALUE2 then begin
    Result := _fread(b2, 1)=1;
    if not Result then exit;
    ch := WideChar(((b1 and $1f) shl 6) or (b2 and $3f))
  end else
  if (b1 and UTF8_MASK3)=UTF8_VALUE3 then begin
    Result := (_fread(b2, 1)=1) and (_fread(b3, 1)=1);
    if not Result then exit;
    ch := WideChar(((b1 and $0f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f));
  end else
  if (b1 and UTF8_MASK4)=UTF8_VALUE4 then
  begin
   { TODO: Decode and return a surrogate pair }
    Result := (_fread(tmp,3)=3);
    ch := #$0000;
  end else begin
    Result := true;
    ch := WideChar(b1);
  end;
end;

procedure TUTF8Encoder.WriteBom;
var bom: integer;
begin
  bom := $BFBBEF;
  _fwrite(bom,3);
end;

procedure TUTF8Encoder.WriteChar(const ch: WideChar);
var w: word absolute ch;
begin
  if (w and UTF8_WRITE1)=0 then
    _fwrite1(w mod 256)
  else
  if (w and UTF8_WRITE2)=0 then
    _fwrite2(
        (UTF8_VALUE2 or (w shr 6))
      + (UTF8_VALUEC or (w and $3f)) shl 8
    )
  else
    _fwrite3(
        (UTF8_VALUE3 or (w shr 12))
      + (UTF8_VALUEC or ((w shr 6) and $3f)) shl 8
      + (UTF8_VALUEC or (w and $3f)) shl 16
    );
end;

procedure TUTF16LEDecoder.TrySkipBom;
var bom: word;
  read_sz: integer;
begin
  read_sz := _fread(bom, 2);
  if (read_sz<>2) or (bom<>$FEFF) then
    _fseek(-read_sz,soCurrent);
end;

function TUTF16LEDecoder.ReadChar(out ch: WideChar): boolean;
begin
  Result := _fread(ch,SizeOf(ch))=SizeOf(ch);
  if not Result then
    FEOF := true;
end;

procedure TUTF16LEEncoder.WriteBom;
begin
  _fwrite2($FEFF);
end;

procedure TUTF16LEEncoder.WriteChar(const ch: WideChar);
begin
  _fwrite(ch,SizeOf(ch)); //Maybe check for errors?
end;

procedure TUTF16BEDecoder.TrySkipBom;
var bom: word;
  read_sz: integer;
begin
  read_sz := _fread(bom, 2);
  if (read_sz<>2) or (bom<>$FEFF) then
    _fseek(-read_sz,soCurrent);
end;

function TUTF16BEDecoder.ReadChar(out ch: WideChar): boolean;
begin
  Result := _fread(ch,SizeOf(ch))=SizeOf(ch);
  if not Result then
    FEOF := true
  else
   //swap bytes
    ch := WideChar(
      (Word(ch) and $00FF) shl 8 +
      (Word(ch) and $FF00 ) shr 8
    );
end;

procedure TUTF16BEEncoder.WriteBom;
begin
  _fwrite2($FEFF);
end;

procedure TUTF16BEEncoder.WriteChar(const ch: WideChar);
begin
 //swap bytes
  _fwrite2(
    (Word(ch) and $00FF) shl 8 +
    (Word(ch) and $FF00 ) shr 8
  );
end;


{ Wakan encodings }

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

function TEUCDecoder.ReadChar(out ch: WideChar): boolean;
var b1, b2: byte;
begin
  Result := _fread(b1, 1)=1;
  if not Result then exit;

  if _is(b1,IS_EUC) then begin
    Result := _fread(b2, 1)=1;
    if not Result then exit;
    ch := WideChar(JIS2Unicode((b1*256+b2) and $7f7f));
  end else
    ch := WideChar(b1);
end;

procedure TEUCEncoder.WriteChar(const ch: WideChar);
var w: word;
begin
  w := Unicode2JIS(Word(ch));
  if _is(w,IS_JIS) then
    _fwrite2(
        ((w shr 8) or $80)
      + ((w mod 256) or $80) shl 8
    )
  else if (w and $80)>0 then
    _fwrite2(
         JIS_SS2
      + (w and $7f) shl 8
    )
  else
    _fwrite1(w);
end;

function TSJSDecoder.ReadChar(out ch: WideChar): boolean;
var b1, b2: byte;
begin
  Result := _fread(b1, 1)=1;
  if not Result then exit;

  if _is(b1,IS_SJIS1) then begin
    Result := _fread(b2, 1)=1;
    if not Result then exit;
    if _is(b2,IS_SJIS2) then
      ch := WideChar(JIS2Unicode(SJIS2JIS(b1*256+b2)))
    else
      ch := WideChar(JIS2Unicode(b1*256+b2));
  end else
    ch := WideChar(b1);
end;

procedure TSJSEncoder.WriteChar(const ch: WideChar);
var w: word;
begin
  w:=Unicode2JIS(Word(ch));
  if _is(w,IS_JIS) then
  begin
    w:=jis2sjis(w);
    _fwrite2(
        (w div 256)
      + (w mod 256) shl 8
    );
  end else
    _fwrite1(w);
end;

function TJISDecoder.ReadChar(out ch: WideChar): boolean;
var b1, b2: byte;
  inp_intwobyte: boolean;
begin
  Result := false;
  inp_intwobyte := false;
  while true do begin
    Result := _fread(b1, 1)=1;
    if not Result then exit;

    if b1=JIS_ESC then
    begin
      Result := _fread(b2, 1)=1;
      if not Result then exit;
      if (b2=ord('$')) or (b2=ord('(')) then _fread(b1, 1); //don't care about the result
      if (b2=ord('K')) or (b2=ord('$')) then inp_intwobyte:=true else inp_intwobyte:=false;
     //Do not exit, continue to the next char
    end else begin
      if (b1=JIS_NL) or (b1=JIS_CR) then
        ch:=WideChar(b1)
      else begin
        Result := _fread(b2, 1)=1;
        if not Result then exit;
        if inp_intwobyte then ch:=WideChar(JIS2Unicode(b1*256+b2)) else ch:=WideChar(b1);
      end;
      break;
    end;
  end;
end;

procedure TJISEncoder._fputstart();
begin
  if intwobyte then exit;
  intwobyte:=true;
  _fwrite1(JIS_ESC);
  case JISType of
    jtNew: _fwrite2(ord('$') + ord('B') shl 8);
    jtOld: _fwrite2(ord('$') + ord('@') shl 8);
    jtNec: _fwrite1(ord('K'));
  end;
end;

procedure TJISEncoder._fputend();
begin
  if not intwobyte then exit;
  intwobyte:=false;
  _fwrite1(JIS_ESC);
  if (JISType=jtNEC) then
    _fwrite1(ord('H'))
  else begin
    _fwrite1(ord('('));
    _fwrite1(ord('J'));
  end;
end;

procedure TJISEncoder.WriteChar(const ch: WideChar);
var w: Word;
begin
  w := Word(ch);

  if (w=13) or (w=10) then
  begin
    _fputend();
    _fwrite1(w);
  end else
  begin
    w:=Unicode2JIS(w);
    if _is(w,IS_JIS) then
    begin
      _fputstart();
      _fwrite2(
          (w mod 256) shl 8
        + (w div 256)
      );
    end else begin
      _fputend();
      _fwrite1(w);
    end;
  end;
end;

function TGBDecoder.ReadChar(out ch: WideChar): boolean;
var b1, b2: byte;
begin
  Result := _fread(b1, 1)=1;
  if not Result then exit;

  if (b1>=$a1) and (b1<=$fe) then
  begin
    Result := _fread(b2, 1)=1;
    if not Result then exit;
    if (b2>=$a1) and (b2<=$fe) then
      ch := WideChar(Table_GB[(b1-$a0)*96+(b2-$a0)])
    else
      ch := WideChar(b1*256+b2);
  end else
    ch := WideChar(b1);
end;

procedure TGBEncoder.WriteChar(const ch: WideChar);
var i: integer;
begin
  if Word(ch)<128 then
    _fwrite1(byte(ch))
  else
  begin
    for i:=0 to 96*96-1 do if Table_GB[i]=Word(ch) then begin
      _fwrite2(
          (i mod 96+$a0) shl 8
        + (i div 96+$a0)
      );
      exit;
    end;
    _fwrite2(
      (Word(ch) mod 256) shl 8
      + (Word(ch) div 256)
    );
  end;
end;

function TBIG5Decoder.ReadChar(out ch: WideChar): boolean;
var b1, b2: byte;
begin
  Result := _fread(b1, 1)=1;
  if not Result then exit;

  if (b1>=$a1) and (b1<=$fe) then
  begin
    Result := _fread(b2, 1)=1;
    if not Result then exit;
    if (b2>=$40) and (b2<=$7f) then
      ch := WideChar(Table_Big5[(b1-$a0)*160+(b2-$40)])
    else
    if (b2>=$a1) and (b2<=$fe) then
      ch := WideChar(Table_Big5[(b1-$a0)*160+(b2-$a0)])
    else
      ch := WideChar(b1*256+b2);
  end else
    ch := WideChar(b1);
end;

procedure TBIG5Encoder.WriteChar(const ch: WideChar);
var i: integer;
begin
  if Word(ch)<128 then
    _fwrite1(byte(ch))
  else
  begin
    for i:=0 to 96*160-1 do if Table_GB[i]=Word(ch) then begin
      _fwrite2(
          (i mod 96+$a0) shl 8
        + (i div 96+$a0)
      );
      exit;
    end;
    _fwrite2(
        (Word(ch) mod 256) shl 8
      + (Word(ch) div 256)
    );
  end;
end;

(*

constructor TRtlDecoder.Open(AStream: TStream; AOwnsStream: boolean;
  AEncoding: TEncoding; AOwnsEncoding: boolean);
begin
  inherited Open(AStream, AOwnsStream);
  FEncoding := TEncoding;
  FOwnsEncoding := AOwnsEncoding;
  bpos := 0;
  bsz := 0;
end;

constructor TRtlDecoder.Open(AStream: TStream; AOwnsStream: boolean;
  ACodePage: integer);
begin
  Open(AStream, AOwnsStream, TEncoding.GetEncoding(ACodePage), {OwnsEncoding=}true);
end;

constructor TRtlDecoder.Open(AStream: TStream; AOwnsStream: boolean;
  const AEncodingName: string);
begin
  Open(AStream, AOwnsStream, TEncoding.GetEncoding(AEncodingName), {OwnsEncoding=}true);
end;

constructor TRtlDecoder.Open(const AFilename: string; AEncoding: TEncoding;
  AOwnsEncoding: boolean = false);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  Self.Open(fs, {OwnsStream=}true, AEncoding, AOwnsEncoding);
end;

constructor TRtlDecoder.Open(const AFilename: string; ACodePage: integer);
begin
  Open(AFilename, TEncoding.GetEncoding(ACodePage), {OwnsEncoding=}true);
end;

constructor TRtlDecoder.Open(const AFilename: string; const AEncodingName: string);
begin
  Open(AFilename, TEncoding.GetEncoding(AEncodingName), {OwnsEncoding=}true);
end;

destructor TRtlDecoder.Destroy;
begin
  if FOwnsEncoding then
    FreeAndNil(FEncoding);
  inherited;
end;

function TRtlDecoder.ReadChar(out ch: WideChar): boolean;
begin
  Result := false;
  if bpos>=bsz then begin
    if feof then exit;
    NewBlock;
  end;
  if bpos>bsz-2 then //don't have two bytes
    exit;
  w := PWideChar(@buf[bpos])^;
  Inc(bpos, 2);
  Result := true;
end;
*)

function Conv_DetectType(AStream: TStream): TStreamDecoder;
begin
  if not Conv_DetectTypeEx(AStream, Result) then
    Result := nil;
end;

function Conv_DetectTypeEx(AStream: TStream; out ADecoder: TStreamDecoder): boolean;
begin

end;

function Conv_ChooseType(AChinese:boolean; ADefault: CStreamDecoder): TStreamDecoder;
var fFileType: TfFileType;
begin
  Result := nil;
  fFileType := TfFileType.Create(nil);
  try
    if AChinese then
    begin
      fFileType.rgType.Items.Clear;
      fFileType.rgType.Items.Add('Unicode (UCS2)');
      fFileType.rgType.Items.Add('UTF-8');
      fFileType.rgType.Items.Add('Big5');
      fFileType.rgType.Items.Add('GB2312');
      fFileType.rgType.Items.Add('Unicode (UCS2) reversed bytes');
      if ADefault=TUTF16LEDecoder then
        fFileType.rgType.ItemIndex:=0
      else
      if ADefault=TUTF8Decoder then
        fFileType.rgType.ItemIndex:=1
      else
      if ADefault=TBIG5Decoder then
        fFileType.rgType.ItemIndex:=2
      else
      if ADefault=TGBDecoder then
        fFileType.rgType.ItemIndex:=3
      else
      if ADefault=TUTF16BEDecoder then
        fFileType.rgType.ItemIndex:=4
      else
        fFileType.rgType.ItemIndex:=0;
      if fFileType.ShowModal=mrOK then
      case fFileType.rgType.ItemIndex of
        0: Result:=TUTF16LEDecoder.Create;
        1: Result:=TUTF8Decoder.Create;
        2: Result:=TBIG5Decoder.Create;
        3: Result:=TUTF16BEDecoder.Create;
        4: Result:=TUTF16BEDecoder.Create;
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
  finally
    FreeAndNil(fFileType);
  end;
end;


(*
{ Detects file encoding. Returns true if it's truly detected (i.e. through BOM),
 or false if it's a best guess. }
function TJwbDecoder.DetectType(out tp: byte): boolean;
var i,b,j:integer;
    eucsjis:boolean;
    asciionly:boolean;
begin
  Rewind();
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
    eucsjis:=false;
    while (tp=FILETYPE_ASCII) or ((tp=0) and eucsjis) do
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
        if (i>=161) and (i<=223) then begin
          tp:=0;
          eucsjis:=true;
        end
        else if (i<>127) and (i>=64) and (i<=252) then tp:=FILETYPE_SJS;
      end else if (i>=129) and (i<=159) then tp:=FILETYPE_SJS
      else if (i>=161) and (i<=223) then
      begin
        i:=_fread;
        if (i>=240) and (i<=254) then tp:=FILETYPE_EUC
        else if (i>=161) and (i<=223) then begin
          tp:=0;
          eucsjis:=true;
        end
        else if (i>=224) and (i<=239) then
        begin
          tp:=0;
          eucsjis:=true;
          while ((i>=64) and (tp=0) and eucsjis) do
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
        else if (i>=161) and (i<=252) then begin
          tp:=0;
          eucsjis:=true;
        end;
      end;
    end;
  end;
  if (tp=0) and eucsjis then tp:=FILETYPE_SJS;
end;

function TJwbDecoder.DetectType: byte;
begin
  if not DetectType(Result) then
    Result := FILETYPE_UNKNOWN;
end;

//Reads two bytes.
function TJwbDecoder._freadw:integer;
var b1,b2:integer;
begin
  b1:=_fread; b2:=_fread;
  if (b1=-1) or (b2=-1) then result:=-1 else result:=b2*256+b1;
end;


function Conv_DetectType(filename:string):byte;
var dec: TJwbDecoder;
begin
  dec := TJwbDecoder.Open(filename, FILETYPE_UNKNOWN);
  try
    Result := dec.DetectType;
  finally
    FreeAndNil(dec);
  end;
end;

function Conv_DetectTypeEx(filename:string; out tp:byte): boolean;
var dec: TJwbDecoder;
begin
  dec := TJwbDecoder.Open(filename, FILETYPE_UNKNOWN);
  try
    Result := dec.DetectType(tp);
  finally
    FreeAndNil(dec);
  end;
end;

function Conv_ChooseType(chinese:boolean; def:byte):byte;
var fFileType: TfFileType;
begin
  result:=0;
  fFileType := TfFileType.Create(nil);
  try
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
  finally
    FreeAndNil(fFileType);
  end;
end;
*)

end.
