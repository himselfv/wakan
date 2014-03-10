unit JWBDownloaderCore;

interface
uses SysUtils, Classes, StreamUtils, JWBJobs, Windows, WinInet, StdPrompt;

//TODO: Resume downloading

const
  BufferSize = 4096; //Download chunk size.

type
  EDownloadException = class(Exception);

  TDownloadResult = (
    drDone,
    drUpToDate,
    drError
  );

  TDownloadJob = class(TJob)
  protected
    FFromURL: string;
    FToFilename: string; //If only path is provided, will use Content-Disposition header, put name here
    FIfModifiedSince: TDatetime;
    hSession, hURL: HInternet;
    Buffer: array[1..BufferSize] of Byte;
    BufferLen: dword;
    FOutputStream: TStreamWriter;
    FResult: TDownloadResult;
    FErrorCode: integer;
    FHttpCode: integer;
    FLastModified: TDatetime; //Last-Modified header
    function StartDownload: boolean;
    procedure Cleanup;
    procedure SetError(const AErrorCode: integer);
  public
    constructor Create(const AFromURL: string; AToFilename: string);
    destructor Destroy; override;
    procedure ProcessChunk; override;
    property FromURL: string read FFromURL write FFromURL;
    property ToFilename: string read FToFilename write FToFilename;
    property IfModifiedSince: TDatetime read FIfModifiedSince write FIfModifiedSince;
    property Result: TDownloadResult read FResult;
    property ErrorCode: integer read FErrorCode;
    property HttpCode: integer read FHttpCode;
    property LastModified: TDatetime read FLastModified;
  end;

procedure DownloadFile(const fileURL, FileName: string; prog: TSMPromptForm=nil);
function DownloadFileIfModified(const fileURL, FileName: string;
  since: TDatetime; out LastModified: TDatetime): boolean;

implementation
uses Forms, JWBStrings, JWBLanguage;

var
 //English month names -- used for datetime formatting to servers and so on
  HttpFormatSettings: TFormatSettings;

function TryHttpQueryInt(h: HInternet; info: integer; out value: integer): boolean;
var len, tmp: cardinal;
begin
  value := 0; //in case less than 4 bytes are written
  len := sizeof(value);
  tmp := 0; //reserved
  Result := HttpQueryInfo(h, info or HTTP_QUERY_FLAG_NUMBER, @value, len, tmp);
end;

function TryHttpQueryInt64(h: HInternet; info: integer; out value: int64): boolean;
var buf: array[1..20] of char;
  len, tmp: cardinal;
begin
 //Documentation tells us QUERY_FLAG_NUMBER means 32 bit integer,
 //so there's no other way than query string
  len := sizeof(buf);
  tmp := 0; //reserved
  Result := HttpQueryInfo(h, info, @buf, len, tmp)
    and TryStrToInt64(string(buf), value);
end;

function TryHttpQueryDatetime(h: HInternet; info: integer; out value: TDatetime): boolean;
var len, tmp: cardinal;
  tm: SYSTEMTIME;
begin
  len := sizeof(tm);
  tmp := 0; //reserved
  Result := HttpQueryInfo(h, info or HTTP_QUERY_FLAG_SYSTEMTIME, @tm, len, tmp);
  if Result then
    value := SystemTimeToDateTime(tm);
end;

function TryHttpQueryStr(h: HInternet; info: integer; out value: string): boolean;
var len, tmp: cardinal;
begin
  len := 64;
  SetLength(value, (len div SizeOf(char))+SizeOf(char){div safety});
  tmp := 0; //reserved
  Result := HttpQueryInfo(h, info, @value[1], len, tmp);
  if (not Result) and (GetLastError=ERROR_INSUFFICIENT_BUFFER) and (len>0) and (len<65535) then begin
   //Try again with bigger buffer
    SetLength(value, (len div SizeOf(char))+SizeOf(char){div safety});
    Result := HttpQueryInfo(h, info, @value[1], len, tmp);
  end;
  if Result and (len mod SizeOf(char) <> 0) then
    SetLength(value, len div SizeOf(char)); //Trim! Although this is WTF.
end;

{
//Returns the expected download size.  Returns -1 if one not provided
function GetContentLength(URLHandle: HInternet): Int64;
var
  SBuffer: Array[1..20] of char;
  SBufferSize: cardinal;
  srv: cardinal;
begin
  srv := 0;
  SBufferSize := 20;
  if HttpQueryInfo(URLHandle, HTTP_QUERY_CONTENT_LENGTH, @SBuffer, SBufferSize, srv) then
    Result := StrToInt64(String(SBuffer))
  else
    Result := -1;
end;
}

function GetContentLength(h: HInternet): Int64;
begin
  if not TryHttpQueryInt64(h,HTTP_QUERY_CONTENT_LENGTH,Result) then
    Result := -1;
end;

function GetStatusCode(h: HInternet): integer;
begin
  if not TryHttpQueryInt(h,HTTP_QUERY_STATUS_CODE,Result) then
    Result := -1;
end;

constructor TDownloadJob.Create(const AFromURL: string; AToFilename: string);
begin
  inherited Create;
  FFromURL := AFromURL;
  FToFilename := AToFilename;
  FIfModifiedSince := 0;
  hSession := nil;
  hURL := nil;
end;

destructor TDownloadJob.Destroy;
begin
  Cleanup;
  inherited;
end;

procedure TDownloadJob.SetError(const AErrorCode: integer);
begin
  FErrorCode := AErrorCode;
  FResult := drError;
  FState := jsCompleted;
end;

//To extract the main value, pass ''
function ExtractHeaderPart(const AValue, APartName: string): string;
var ps,pc: PChar;
  inQuotes: boolean;
  lcasePartName: string;
  sPartName: string;
  sPartValue: string;
begin
  if AValue='' then begin
    Result := '';
    exit;
  end;

  lcasePartName := AnsiLowerCase(APartName);

  inQuotes := false;
  sPartName := '';
  sPartValue := '';
  Result := '';

  ps := PChar(AValue);
  pc := ps;
  while pc^<>#00 do begin
    if pc^='"' then
      inQuotes := not inQuotes
    else
    if (not inQuotes) and (pc^='=') then begin
      sPartName := Trim(SpanCopy(ps, pc));
      ps := pc + 1;
    end else
    if (not inQuotes) and (pc^=';') then begin
      if AnsiLowerCase(sPartName)=lcasePartName then begin
        Result := AnsiDequotedStr(Trim(SpanCopy(ps, pc)), '"');
        break;
      end;
      ps := pc + 1;
    end;

    Inc(pc);
  end;
end;

function TDownloadJob.StartDownload: boolean;
var ContentLength: int64;
  sAppName: string;
  sSince: string;
  sHeaders: string;
  sContentDisposition: string;
begin
  sAppName := ExtractFileName(Application.ExeName);
  hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hSession=nil then begin
    SetError(GetLastError);
    Result := false;
    exit;
  end;

  sHeaders := '';
  if FIfModifiedSince>1 then begin
   //Ex.: Sat, 29 Oct 1994 19:43:31 GMT
    sSince := FormatDatetime('ddd, d mmm yyyy hh:mm:ss', FIfModifiedSince, HttpFormatSettings);
    sHeaders := 'If-Modified-Since: '+sSince+' GMT';
  end;

  hURL := InternetOpenURL(hSession, PChar(FFromURL), PChar(sHeaders), 0, 0, 0);
  if hURL=nil then begin
    SetError(GetLastError);
    InternetCloseHandle(hSession);
    Result := false;
    exit;
  end;

  FHttpCode := GetStatusCode(hURL);
  if FHttpCode<0 then begin
    SetError(GetLastError);
    InternetCloseHandle(hURL);
    InternetCloseHandle(hSession);
    Result := false;
    exit;
  end;

 {
  "If-Modified-Since", "304 Not Modified" may be not supported, so we have
  to check for "last-modified" in the header anyway.
  It may be missing, then we have no choice but to import.
  Because of the above, we are to check for new versions in only about 4 days.
  Even if the server supports If-Modified-Since, no point in overloading it with
  requests.
 }

  if FHttpCode=304 then begin
    FResult := drUpToDate;
    FState := jsCompleted;
    Result := false; //do not continue
    exit;
  end;

  if (FHttpCode<>200) //OK
  and (FHttpCode<>205) //No content
  then begin
    SetError(-1);
    Result := false;
    exit;
  end;

  if TryHttpQueryDatetime(hURL, HTTP_QUERY_LAST_MODIFIED, FLastModified) then begin
    if (FIfModifiedSince>1) and (FLastModified<FIfModifiedSince) then begin
      FResult := drUpToDate;
      FState := jsCompleted;
      Result := false; //do not continue
      exit;
    end;
  end else
    FLastModified := now(); //best guess

  ContentLength := GetContentLength(hURL);
  if (ContentLength<=0) or (ContentLength>=MaxInt) then
    Self.FMaxProgress := 0 //just display progress animation
  else
    Self.FMaxProgress := integer(ContentLength); //really hope it's less than 2Gb!

 //If filename is provided, use that, otherwise take one from Content-Disposition
 //or URL
  if ExtractFilename(FToFilename)='' then begin
    if TryHttpQueryStr(hURL, HTTP_QUERY_CONTENT_DISPOSITION, sContentDisposition) then
      sContentDisposition := ExtractHeaderPart(sContentDisposition, 'filename')
    else
      sContentDisposition := '';
    if sContentDisposition<>'' then
      FToFilename := FToFilename + sContentDisposition
    else
      FToFilename := FToFilename + ExtractFilenameURL(FFromURL);
   //NOTE: If under any circumstances we cannot get the filename even from URL,
   //  generate some randome one and return it in FToFilename.
  end;


  FOutputStream := TStreamWriter.Create(
    TFileStream.Create(FToFilename, fmCreate),
    {OwnsStream=}true
  );
  Result := true;
end;

procedure TDownloadJob.Cleanup;
begin
  if hURL<>nil then
    InternetCloseHandle(hURL);
  if hSession<>nil then
    InternetCloseHandle(hSession);
  FreeAndNil(FOutputStream);
end;

procedure TDownloadJob.ProcessChunk;
begin
  if State=jsPending then begin
    if not StartDownload then exit;
    FState := jsWorking;
  end;

  if not InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen) then begin
    SetError(GetLastError);
    exit;
  end;
  FOutputStream.Write(Buffer, BufferLen);
  Inc(FProgress, BufferLen);
  if BufferLen=0 then begin
    Cleanup(); //or we'd be holding file hostage
    FState := jsCompleted;
    FResult := drDone;
  end;

end;

//Shortcut
procedure DownloadFile(const fileURL, FileName: string; prog: TSMPromptForm);
var job: TDownloadJob;
begin
  if prog<>nil then
    prog.SetMessage(_l('^eDownloading %s...', [ExtractFilename(Filename)]));

  job := TDownloadJob.Create(fileUrl, fileName);
  try
    job.ProcessChunk;
    if prog<>nil then begin
      prog.SetMaxProgress(job.MaxProgress);
      prog.ProcessMessages;
    end;
    while job.State<>jsCompleted do begin
      job.ProcessChunk;
      if prog<>nil then begin
        prog.SetProgress(integer(job.Progress));
        prog.ProcessMessages;
      end;
    end;
    if job.Result=drError then
      RaiseLastOsError(job.ErrorCode);
  finally
    FreeAndNil(job);
  end;
end;

//True if modified and downloaded, False if unmodified.
//Exception if could not download.
function DownloadFileIfModified(const fileURL, FileName: string; since: TDatetime;
  out LastModified: TDatetime): boolean;
var job: TDownloadJob;
  prog: TSMPromptForm;
begin
  job := TDownloadJob.Create(fileUrl, fileName);
  try
    job.IfModifiedSince := since;

    prog:=SMProgressDlgCreate(_l('^eDownload'),_l('^eDownloading %s...', [ExtractFilename(Filename)]),100,{CanCancel=}true);
    if not Application.MainForm.Visible then
      prog.Position := poScreenCenter;
    prog.AppearModal;

    job.ProcessChunk;
    if prog<>nil then begin
      prog.SetMaxProgress(job.MaxProgress);
      prog.ProcessMessages;
    end;
    while job.State<>jsCompleted do begin
      job.ProcessChunk;
      if prog<>nil then begin
        prog.SetProgress(integer(job.Progress));
        prog.ProcessMessages;
      end;
    end;
    if job.Result=drError then
      RaiseLastOsError(job.ErrorCode);

    Result := job.State=jsCompleted; //else jsUpToDate
  finally
    FreeAndNil(job);
  end;
end;

initialization
  HttpFormatSettings := TFormatSettings.Create('en-us'); //$0409 but it's platform-incompatible

end.
