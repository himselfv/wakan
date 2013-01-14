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

function DownloadFile(const fileURL, FileName: string): boolean;

implementation
uses Forms, Windows, WinInet, JWBUnit;

// returns the expected download size.  Returns -1 if one not provided
function GetContentLength(URLHandle: HINTERNET): Int64;
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

function DownloadFile(const fileURL, FileName: string): boolean;
const BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  f: File;
  sAppName: string;
  prog: TSMPromptForm;
  ContentLength: int64;
  TotalLength: int64;
begin
  hSession := nil;
  hURL := nil;
  prog:=SMProgressDlgCreate(_l('^eDownload'),_l('^eDownloading %s...', [ExtractFilename(Filename)]),100,{CanCancel=}true);
  if not Application.MainForm.Visible then
    prog.Position := poScreenCenter;
  prog.AppearModal;
  try
    sAppName := ExtractFileName(Application.ExeName);
    hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hSession=nil then RaiseLastOsError();
    hURL := InternetOpenURL(hSession, PChar(fileURL), nil, 0, 0, 0);
    if hURL=nil then RaiseLastOsError();

    ContentLength := GetContentLength(hURL);
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
      prog.SetProgress(integer(TotalLength));
      prog.ProcessMessages;
    until BufferLen = 0;
    CloseFile(f);
    result := True;
  finally
    if hURL<>nil then
      InternetCloseHandle(hURL);
    if hSession<>nil then
      InternetCloseHandle(hSession);
    FreeAndNil(prog);
  end;
end;

end.
