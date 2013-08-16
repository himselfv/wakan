unit JWBIO2;

interface
uses SysUtils, Classes, JWBStrings;

type
 { Base classes }

  TStreamDecoder = class
  protected
    FStream: TStream;
    FOwnsStream: boolean;
    FEOF: boolean; //set when you cannot read one more entry from the stream
  public
    constructor Open(AStream: TStream; AOwnsStream: boolean = false); overload; virtual;
    constructor Open(const AFilename: string); overload;
    destructor Destroy; override;
    procedure Flush; virtual; //clears whatever caches the instance may have for the Stream
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

  TJwbCreateFlag = (
    cfBom    //write byte order mark if supported by this encoding
  );
  TJwbCreateFlags = set of TJwbCreateFlag;

  TStreamEncoder = class
  protected
    FStream: TStream;
    FOwnsStream: boolean;
  public
    constructor Open(AStream: TStream; AFlags: TJwbCreateFlags; AOwnsStream: boolean = false); virtual;
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


 { Simple encodings }
 { TODO: UTF16 surrogate pairs (LE and BE in different order) }

  TAnsiDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;

  TUTF16LEDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;

  TUTF16BEDecoder = class(TStreamDecoder)
    function ReadChar(out ch: WideChar): boolean; override;
  end;

  TUTF16Decoder = TUTF16LEDecoder;
  TUnicodeDecoder = TUTF16LEDecoder;

  TAnsiEncoder = class(TStreamEncoder)
    procedure WriteChar(const ch: WideChar); override;
  end;

  TUTF16LEEncoder = class(TStreamEncoder)
    procedure WriteBom; override;
    procedure WriteChar(const ch: WideChar); override;
  end;

  TUTF16BEEncoder = class(TStreamEncoder)
    procedure WriteBom; override;
    procedure WriteChar(const ch: WideChar); override;
  end;

  TUTF16Encoder = TUTF16LEEncoder;
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
  only a part of the buffer. }


{ Wakan decoders }

  TUTF8Decoder = class(TStreamDecoder)
  end;

  TUTF8Encoder = class(TStreamEncoder)
  end;

  TGBDecoder = class(TStreamDecoder)
  end;

  TGBEncoder = class(TStreamEncoder)
  end;

  TBIG5Decoder = class(TStreamDecoder)
  end;

  TBIG5Encoder = class(TStreamEncoder)
  end;


const
 //Conversion types
  FILETYPE_JIS=4;
  FILETYPE_OLD=5;
  FILETYPE_NEC=6;
  FILETYPE_SJS=7;
  FILETYPE_EUC=8;

type
  TJwbDecoder = class(TStreamDecoder)
  protected
    FFileType: byte;
    function _fread:integer;
    function _freadw:integer;
  public
    constructor Open(AStream: TStream; AOwnsStream: boolean; AFileType: byte); overload;
    constructor Open(const AFilename: string; AFileType: byte); overload;
    function DetectType(out tp: byte): boolean; overload; virtual;
    function DetectType: byte; overload;
    function ReadChar(out ch: WideChar): boolean; override;
    procedure Rewind;
    procedure RewindAsType(tp: byte);
    property FileType: byte read FFileType;
  end;

  TJwbEncoder = class(TStreamEncoder)
  protected
    FFileType: byte;
  public
    constructor Open(AStream: TStream; AFlags: TJwbCreateFlags;
      AOwnsStream: boolean; AFileType: byte); overload;
    constructor CreateNew(const AFilename: string; AFileType: byte); overload;
    constructor Append(const AFilename: string; AFileType: byte); overload;
    destructor Destroy; override;
    procedure WriteBom; override;
    procedure WriteChar(const ch: WideChar); override;
  end;


function Conv_DetectType(filename:string):byte;
function Conv_DetectTypeEx(filename:string; out tp:byte): boolean;
function Conv_ChooseType(chinese:boolean; def:byte):byte;

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
    Flush; //if the stream is ours, no point in restoring it's actual position
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

procedure TStreamDecoder.Flush;
begin
 { Implement in descendants if needed }
end;

function TStreamDecoder.EOF: boolean;
begin
 { Reimplement in descendants if needed }
  Result := FEOF;
end;

function TStreamDecoder.ReadChar(out ch: WideChar): boolean;
begin
 { Implement in descendants }
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

constructor TStreamEncoder.Open(AStream: TStream; AFlags: TJwbCreateFlags; AOwnsStream: boolean = false);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  if cfBom in AFlags then
    WriteBom();
 { Extend this function in descendants }
end;

constructor TStreamEncoder.CreateNew(const AFilename: string);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  Self.Open(fs,[cfBom],{OwnsStream=}true);
end;

constructor TStreamEncoder.Append(const AFilename: string);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  fs.Seek(0,soFromEnd);
  Self.Open(fs,[],{OwnsStream=}true);
end;

destructor TStreamEncoder.Destroy;
begin
  Flush();
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
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
  Result := Stream.Read(ac,SizeOf(ac))=SizeOf(ac);
  if not Result then
    FEOF := true
  else
    ch := WideChar(ac);
end;

function TUTF16LEDecoder.ReadChar(out ch: WideChar): boolean;
begin
  Result := Stream.Read(ch,SizeOf(ch))=SizeOf(ch);
  if not Result then
    FEOF := true;
end;

function TUTF16BEDecoder.ReadChar(out ch: WideChar): boolean;
begin
  Result := Stream.Read(ch,SizeOf(ch))=SizeOf(ch);
  if not Result then
    FEOF := true
  else
   //swap bytes
    ch := WideChar(
      (Word(ch) and $00FF) shl 8 +
      (Word(ch) and $FF00 ) shr 8
    );
end;

procedure TAnsiEncoder.WriteChar(const ch: WideChar);
var ac: AnsiChar;
begin
  ac := AnsiChar(ch); //Maybe more sophisticated conversion?
  Stream.Write(ac,SizeOf(ac)); //Maybe check for errors?
end;

procedure TUTF16LEEncoder.WriteBom;
begin
  Write(#$FEFF);
end;

procedure TUTF16LEEncoder.WriteChar(const ch: WideChar);
begin
  Stream.Write(ch,SizeOf(ch)); //Maybe check for errors?
end;

procedure TUTF16BEEncoder.WriteBom;
begin
  Write(#$FFFE);
end;

procedure TUTF16BEEncoder.WriteChar(const ch: WideChar);
var ch2: char;
begin
 //swap bytes
  ch2 := WideChar(
    (Word(ch) and $00FF) shl 8 +
    (Word(ch) and $FF00 ) shr 8
  );
  Stream.Write(ch2,SizeOf(ch2));
end;


{ Wakan encodings }

constructor TJwbDecoder.Open(AStream: TStream; AOwnsStream: boolean; AFileType: byte); overload;
begin
  inherited Open(AStream, AOwnsStream);
  FFileType := AFileType;
end;

constructor TJwbDecoder.Open(const AFilename: string; AFileType: byte);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  Self.Open(fs, {OwnsStream=}true, AFileType);
end;

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

//Reads one byte. Returns -1 if no byte is available. It's fine because we return integer.
function TJwbDecoder._fread:integer;
var ch: byte;
begin
  if Stream.Read(ch,1)=1 then
    Result := ch
  else
    Result := -1;
end;

//Reads two bytes.
function TJwbDecoder._freadw:integer;
var b1,b2:integer;
begin
  b1:=_fread; b2:=_fread;
  if (b1=-1) or (b2=-1) then result:=-1 else result:=b2*256+b1;
end;

function TJwbDecoder.ReadChar(out ch: WideChar): boolean;
begin

end;

procedure TJwbDecoder.Rewind;
begin
  Stream.Seek(0,soFromBeginning);
end;

procedure TJwbDecoder.RewindAsType(tp: byte);
begin
  FFileType := tp;
  Rewind;
end;


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

end.
