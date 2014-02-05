unit JWBDownloader;

interface
uses SysUtils, StdPrompt;

//TODO: Use JWBIO
//TODO: Check resulting header and determine if the download is needed before proceeding
 //If-Modified-Since
 //304 Not Modified
 //May be not supported, so we have to check for "last-modified" in the header anyway
 //It may be missing, then we have no choice but to import.
 //Because of all of the above, we are to check for new versions in only about 4 days.
 //Even if the server supports If-Modified-Since, no point in overloading it with requests.
//TODO: Resume downloading

type
  EDownloadException = class(Exception);

procedure DownloadFile(const fileURL, FileName: string; prog: TSMPromptForm=nil);
function DownloadFileIfModified(const fileURL, FileName: string;
  since: TDatetime; out LastModified: TDatetime): boolean;

implementation
uses Forms, Windows, WinInet, JWBUnit;

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

procedure FetchContents(hURL: HInternet; const Filename: string; prog: TSMPromptForm=nil);
const BufferSize = 4096;
var f: File;
  ContentLength: int64;
  TotalLength: int64;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
begin
  ContentLength := GetContentLength(hURL);
  if prog<>nil then
    if (ContentLength<=0) or (ContentLength>=MaxInt) then
      prog.SetMaxProgress(0) //just display progress animation
    else
      prog.SetMaxProgress(integer(ContentLength));

  TotalLength := 0;
  AssignFile(f, FileName);
  Rewrite(f,1);
  repeat
    if not InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen) then
      RaiseLastOsError();
    BlockWrite(f, Buffer, BufferLen);
    Inc(TotalLength, BufferLen);
    if prog<>nil then begin
      prog.SetProgress(integer(TotalLength));
      prog.ProcessMessages;
    end;
  until BufferLen = 0;
  CloseFile(f);
end;

procedure DownloadFile(const fileURL, FileName: string; prog: TSMPromptForm);
var hSession, hURL: HInternet;
  sAppName: string;
begin
  hSession := nil;
  hURL := nil;
  try
    if prog<>nil then
      prog.SetMessage(_l('^eDownloading %s...', [ExtractFilename(Filename)]));
    sAppName := ExtractFileName(Application.ExeName);
    hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hSession=nil then RaiseLastOsError();
    hURL := InternetOpenURL(hSession, PChar(fileURL), nil, 0, 0, 0);
    if hURL=nil then RaiseLastOsError();

    FetchContents(hURL, Filename, prog);
  finally
    if hURL<>nil then
      InternetCloseHandle(hURL);
    if hSession<>nil then
      InternetCloseHandle(hSession);
  end;
end;

//True if modified and downloaded, False if unmodified.
//Exception if could not download.
function DownloadFileIfModified(const fileURL, FileName: string; since: TDatetime;
  out LastModified: TDatetime): boolean;
var hSession, hURL: HInternet;
  sAppName: string;
  prog: TSMPromptForm;
  s_since: string;
  ret: integer;
begin
  hSession := nil;
  hURL := nil;
  prog := nil; //show only if needed
  try
    sAppName := ExtractFileName(Application.ExeName);
    hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hSession=nil then RaiseLastOsError();

   //Ex.: Sat, 29 Oct 1994 19:43:31 GMT
    s_since := FormatDatetime('ddd, d mmm yyyy hh:mm:ss', since, HttpFormatSettings);

    hURL := InternetOpenURL(hSession, PChar(fileURL), PChar('If-Modified-Since: '+s_since+' GMT'), 0, 0, 0);
    if hURL=nil then RaiseLastOsError();

    ret := GetStatusCode(hURL);
    if ret<0 then RaiseLastOsError();
    if ret=304 then begin
      Result := false; //unchanged
      exit;
    end;

    if (ret<>200) //OK
    and (ret<>200) and (ret<>205) //No content
    then begin
      Result := false; //not found
      exit;
    end;

    prog:=SMProgressDlgCreate(_l('^eDownload'),_l('^eDownloading %s...', [ExtractFilename(Filename)]),100,{CanCancel=}true);
    if not Application.MainForm.Visible then
      prog.Position := poScreenCenter;
    prog.AppearModal;

    if not TryHttpQueryDatetime(hURL, HTTP_QUERY_LAST_MODIFIED, LastModified) then
      LastModified := now(); //best guess

    FetchContents(hURL, Filename, prog);
    Result := true;
  finally
    if hURL<>nil then
      InternetCloseHandle(hURL);
    if hSession<>nil then
      InternetCloseHandle(hSession);
    FreeAndNil(prog);
  end;
end;

initialization
  HttpFormatSettings := TFormatSettings.Create('en-us'); //$0409 but it's platform-incompatible

end.
