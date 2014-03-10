unit JWBDictCoding;
{ Converts dictionaries to UTF16-LE with UNICONV.
This is being phased out in favor of internal decoding routines.
Reasons:
1. We have to have decoders for reading/writing from text files anyway.
2. No dependency on third-party tools.
3. In real life, dictionaries are rarely in anything other than EUC/Shift-JIS
 and a bunch of common chinese encodings.
4. This is not even our job to support all possible encodings.
5. If push comes to shove, recode manually. }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, JwbForms;

type
  TfDictCoding = class(TJwbForm)
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    Label3: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  public
    succeeded:boolean;
  end;

function ConvertToUTF16(const AFilename, ANewFilename: string;
  const AEncodingClass: char): string;
procedure RunUniConv(srcFile, outpFile: string; srcEnc: string);

implementation
uses JWBCore, JWBLanguage;

{$R *.DFM}

procedure TfDictCoding.BitBtn1Click(Sender: TObject);
begin
  if (RadioGroup1.ItemIndex>0) and (not FileExists('UniConv.exe')) then
  begin
    Application.MessageBox(
      pchar(_l('#00070^eUNICONV.EXE was not found. It is required for encoding '
        +'conversion.')),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
    exit;
  end;
  succeeded:=true;
  close;
end;

procedure TfDictCoding.BitBtn2Click(Sender: TObject);
begin
  succeeded:=false;
  close;
end;

{
SetOperation(_l('#00085^eConverting ')+ExtractFilename(FFiles[fi].Filename)+'...');
try
  ConvertedFilename := ConvertToUTF16(Filename, tempDir + '\DICT_'+inttostr(fi)+'.TMP',
    EncodingClass);
except
  on E: EDictImportException do begin
    E.Message := _l('While converting ')+FFiles[fi].Filename+': '+E.Message;
    raise;
  end;
end;

Import(ConvertedFilename, TUTF16LEEncoding);
}

{
Uses UNICONV to convert file contents to UTF16 and store under a new name.
Current encoding is auto-detected in a class of encodings.
EncodingClass:
  #00:   no conversion
  'j':   any japanese
  'c':   any chinese
  'k':   any korean
Returns:
  New file name (maybe the same as the old one).
}
function ConvertToUTF16(const AFilename, ANewFilename: string;
  const AEncodingClass: char): string;
var cd: string;
begin
  case AEncodingClass of
    'j': cd:='JapaneseAutoDetect';
    'c': cd:='ChineseAutoDetect';
    'k': cd:='KoreanAutoDetect';
  else cd := '';
  end;

  if cd='' then
    Result := AFilename
  else begin
    RunUniConv(AFilename, ANewFilename, cd);
    if not FileExists(ANewFilename) then
      raise Exception.CreateFmt(_l('File conversion failed (%s)'), [ANewFilename]);
  end;
end;

{
Executes UNICONV.exe to convert srcFile to outpFile.
srcFile is assumed to be in srcEnc encoding (see UCONV docs). outpFile is in UCS2.
Raise exceptions on any errors.
}
procedure RunUniConv(srcFile, outpFile: string; srcEnc: string);
var lpi:PROCESS_INFORMATION;
  si:STARTUPINFO;
  fail: boolean;
  err: integer;
begin
  if not FileExists(AppFolder+'\UNICONV.exe') then
    raise Exception.Create(_l('#00070^eUNICONV.EXE was not found. It is '
      +'required for encoding conversion.'));

  FillChar(lpi, sizeof(lpi), 0);
  FillChar(si, sizeof(si), 0);
  si.dwFlags:=STARTF_USESHOWWINDOW;
  si.wShowWindow:=SW_HIDE;
  if not CreateProcess(nil,pchar(AppFolder+'\UNICONV.EXE '+srcEnc+' "'+srcFile
    +'" UCS2 "'+outpFile+'"'),nil,nil,false,0,nil,nil,si,lpi) then
    RaiseLastOSError();

  fail := (WaitForSingleObject(lpi.hProcess,30000)<>WAIT_OBJECT_0);
  if fail then err := GetLastError() else err := 0; //shut up delphi

 { We don't try to terminate the process in case of failure for two reasons:
   1. It might be impossible on Windows 7 or XP with stricter security.
   2. It's better to leave it be, for user to see what's the problem. }

  CloseHandle(lpi.hProcess);
  CloseHandle(lpi.hThread);
  if fail then
    RaiseLastOsError(err);
end;

end.
