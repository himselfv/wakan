unit SevenZipUtils;
(*
  Various utilities to simplify working with 7z.dll
*)

interface
uses SysUtils, Classes, Windows, ActiveX, JclBase, SevenZip;

type
  ESevenZipException = class(Exception);

 (*
  Even if you don't set OwnsStream, direct interaction with a stream is
  forbidden while TIfStream handles it.
 *)
  TIfStream = class(TInterfacedObject)
  protected
    FStream: TStream;
    FOwnsStream: boolean;
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
  public
    constructor Create(AStream: TStream; AOwnsStream: boolean = false);
    destructor Destroy; override;
  end;

  TInStream = class(TIfStream, ISequentialInStream, IInStream)
  protected
    function Read(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
  end;

  TOutStream = class(TIfStream, ISequentialOutStream, IOutStream)
    function Write(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    function SetSize(NewSize: Int64): HRESULT; stdcall;
  end;

  TExtractFileCallback = class(TInterfacedObject, IArchiveExtractCallback)
  public
    OpResult: integer;
    destructor Destroy; override;
  protected
    FData: TMemoryStream;
    FOutStream: TOutStream;
    function GetStream(Index: Cardinal; out OutStream: ISequentialOutStream;
      askExtractMode: Cardinal): HRESULT; stdcall;
    function PrepareOperation(askExtractMode: Cardinal): HRESULT; stdcall;
    function SetOperationResult(resultEOperationResult: Integer): HRESULT; stdcall;
    function SetTotal(Total: Int64): HRESULT; stdcall;
    function SetCompleted(CompleteValue: PInt64): HRESULT; stdcall;
  end;

  TSevenZipArchive = class
  protected
    FArchive: IInArchive;
  public
    constructor Create(ClsID: TGUID; Filename: UnicodeString); overload;
    constructor Create(ClsID: TGUID; Stream: TStream); overload;
    destructor Destroy; override;
    function NumberOfItems: cardinal;
    function BoolProperty(Index, PropID: cardinal): boolean;
    function StrProperty(Index, PropID: cardinal): UnicodeString;
    function ExtractFile(Index: cardinal): TMemoryStream;

  end;

resourcestring
  eCannotLoadSevenZip = 'Cannot load 7z.dll which is required when working with '
    +'archives. This means two things. First, you have a dynamic link build '
    +'of engine, and second, you lack the DLL.';
  eWrongPropertyType = '%s property expected, type %d returned.';

//Variant utilities forgotten by Borland
procedure PropVariantInit(pvar: PPropVariant); stdcall;
function PropVariantClear(pvar: PPropVariant): HRESULT; stdcall;

//Generates exceptions
procedure ZipFail(msg: string); overload;
procedure ZipFail(msg: string; args: array of const); overload;
procedure ZipFail(hr: HRESULT; op: string); overload;

//Check that the library's in place
procedure SevenZipCheckLoaded;

implementation

//Simply speaking, nulls everything
procedure PropVariantInit(pvar: PPropVariant); stdcall;
begin
  ZeroMemory(pvar, SizeOf(pvar^)); //this will do.
end;

//Required to clear variant properties
function PropVariantClear(pvar: PPropVariant): HRESULT; stdcall; external 'ole32.dll';

procedure ZipFail(msg: string);
begin
  raise ESevenZipException.Create(msg);
end;

procedure ZipFail(msg: string; args: array of const);
begin
  ZipFail(Format(msg, args));
end;

procedure ZipFail(hr: HRESULT; op: string);
begin
  raise ESevenZipException.Create(op+': 0x'+IntToHex(hr, 8));
end;

procedure SevenZipCheckLoaded;
begin
  if not Is7ZipLoaded then
    if not Load7Zip then
      ZipFail(eCannotLoadSevenZip);
end;

constructor TIfStream.Create(AStream: TStream; AOwnsStream: boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TIfStream.Destroy;
begin
  if FOwnsStream = true then
    FreeAndNil(FStream);
  inherited;
end;

function TIfStream.Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT;
var pos: int64;
begin
  pos := FStream.Seek(Offset, SeekOrigin);
  if NewPosition <> nil then
    NewPosition^ := pos;
  Result := S_OK;
end;

function TInStream.Read(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT;
var sz: integer;
begin
  sz := FStream.Read(Data^, Size);
  if ProcessedSize <> nil then
    ProcessedSize^ := sz;
  Result := S_OK;
end;

function TOutStream.Write(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
var sz: integer;
begin
  sz := FStream.Write(Data^, Size);
  if ProcessedSize <> nil then
    ProcessedSize^ := sz;
  Result := S_OK;
end;

function TOutStream.SetSize(NewSize: Int64): HRESULT; stdcall;
begin
  FStream.Size := NewSize;
  Result := S_OK;
end;

destructor TExtractFileCallback.Destroy;
begin
  if FOutStream <> nil then begin
    (FOutStream as IInterface)._Release;
    FOutStream := nil;
  end;
  FreeAndNil(FData);
  inherited;
end;

function TExtractFileCallback.GetStream(Index: Cardinal;
  out OutStream: ISequentialOutStream;
  askExtractMode: Cardinal): HRESULT; stdcall;
begin
  if FData = nil then
    FData := TMemoryStream.Create;
  if FOutStream = nil then begin
    FOutStream := TOutStream.Create(FData, {OwnsStream=}false);
    (FOutStream as IInterface)._AddRef;
  end;

  OutStream := FOutStream;
  Result := S_OK;
end;

function TExtractFileCallback.PrepareOperation(askExtractMode: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TExtractFileCallback.SetOperationResult(resultEOperationResult: Integer): HRESULT; stdcall;
begin
  OpResult := resultEOperationResult;
  Result := S_OK;
end;

function TExtractFileCallback.SetTotal(Total: Int64): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TExtractFileCallback.SetCompleted(CompleteValue: PInt64): HRESULT; stdcall;
begin
  Result := S_OK;
end;



constructor TSevenZipArchive.Create(ClsID: TGUID; Filename: UnicodeString);
var hr: HRESULT;
  g_IID: TGUID;
  InStream: IInStream;
begin
  SevenZipCheckLoaded;
  inherited Create;

  g_IID := IInArchive;
  hr := CreateObject(@ClsID, @g_IID, FArchive);
  if FAILED(hr) then ZipFail(hr, 'CreateObject');

  InStream := TInStream.Create(
    TFileStream.Create(Filename, fmOpenRead),
    {OwnsObject=}true);

  hr := FArchive.Open(InStream, nil, nil);
  if FAILED(hr) then ZipFail(hr, 'Open');
end;

{ You're giving out the ownership of the stream }
constructor TSevenZipArchive.Create(ClsID: TGUID; Stream: TStream);
var hr: HRESULT;
  g_IID: TGUID;
  InStream: IInStream;
begin
  SevenZipCheckLoaded;
  inherited Create;

  g_IID := IInArchive;
  hr := CreateObject(@ClsID, @g_IID, FArchive);
  if FAILED(hr) then ZipFail(hr, 'CreateObject');

  InStream := TInStream.Create(
    Stream,
    {OwnsObject=}true);

  hr := FArchive.Open(InStream, nil, nil);
  if FAILED(hr) then ZipFail(hr, 'Open');
end;

destructor TSevenZipArchive.Destroy;
begin
  if FArchive <> nil then begin
    FArchive.Close;
    FArchive := nil;
  end;
  inherited;
end;

function TSevenZipArchive.NumberOfItems: cardinal;
var hr: HRESULT;
begin
  hr := FArchive.GetNumberOfItems(@Result);
  if FAILED(hr) then ZipFail(hr, 'GetNumberOfItems');
end;

function TSevenZipArchive.BoolProperty(Index, PropID: cardinal): boolean;
var hr: HRESULT;
  val: PROPVARIANT;
  tmp: integer;
begin
  PropVariantInit(@val);

  hr := FArchive.GetProperty(Index, PropID, val);
  if FAILED(hr) then ZipFail(hr, 'GetProperty('+IntToStr(Index)+', ' + IntToStr(PropId) + ')');

  case val.vt of
    VT_EMPTY: Result := false;
    VT_BOOL: Result := val.boolVal;
  else
    tmp := val.vt;
    PropVariantClear(@val);
    ZipFail(eWrongPropertyType, ['Bool', tmp]);
    Result := false; //Delphi!
  end;

  PropVariantClear(@val);
end;

function TSevenZipArchive.StrProperty(Index, PropID: cardinal): UnicodeString;
var hr: HRESULT;
  val: PROPVARIANT;
  tmp: integer;
begin
  PropVariantInit(@val);

  hr := FArchive.GetProperty(Index, PropID, val);
  if FAILED(hr) then ZipFail(hr, 'GetProperty('+IntToStr(Index)+', ' + IntToStr(PropId) + ')');

  case val.vt of
    VT_EMPTY: Result := '';
    VT_BSTR: Result := val.bstrVal;
    VT_LPSTR: Result := string(val.pszVal);
    VT_LPWSTR: Result := val.pwszVal;
  else
    tmp := val.vt;
    PropVariantClear(@val);
    ZipFail(eWrongPropertyType, ['String', tmp]);
  end;

  PropVariantClear(@val);
end;

function TSevenZipArchive.ExtractFile(Index: cardinal): TMemoryStream;
var hr: HRESULT;
  Callback: TExtractFileCallback;
begin
  Result := nil;
  Callback := TExtractFileCallback.Create;
  (Callback as IInterface)._AddRef;
  try
    hr := FArchive.Extract(@Index, 1, 0, Callback);
    if FAILED(hr) then ZipFail(hr, 'Extract');

    if Callback.OpResult <> 0 then
      ZipFail('Cannot extract file, error '+IntToStr(Callback.OpResult))
    else //without this ELSE Delphi will say Result:=nil earlier was in vain
      Result := Callback.FData;
    Callback.FData := nil; //чтоб не уничтожил
  finally
    (Callback as IInterface)._Release;
  end;
end;

end.
