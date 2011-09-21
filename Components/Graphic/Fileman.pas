unit Fileman;

interface

uses Windows, SysUtils, Classes, Forms, Registry, StrMan;

//Get windows directory
function GetWinDir : String;
//Create directories with 1 or more non-exists directories.
function CreateDirs(const Dirs: String) : Boolean;
//check if directory exists or not
function DirExists(const Dir: String): Boolean;
//return application path
function AppPath: String;
//Return application exename
function AppExe: String;
//Return the last dir in the Dir string.
function LastDir(Dir: String): String;
//Get the Temp dir.
function TmpDir: String;
//Noticed this function will not return '\' at all.
//unlike delphi's function, it will return '\' when the file
//contain no dir only a drive. ('a:\test.txt'->'a:\')
function ExtractFilepath(const Filename: TFilename): String;
//this function ('a:\test.txt'->'a:')

//Just like windows explorer go to the parent directory.
function DirUp(Directory: String): String;
//Test write the directory if it is readonly.
function TestWrite(const Dir: String): Boolean;
//Form a full path filename avoid '\' error.
function formFilename(const Dir, Filename: String): String;
function FilenameOnly(const Filename: String): String;

//Search file with the exxtention in a directory
procedure SearchFileExt(const Dir, Ext: String; Files: TStrings);

function FileLastWrite(const Filename: String): Integer;

var
	StopSearch: Boolean;
implementation

function FileLastWrite(const Filename: String): Integer;
var
	hf: Integer;
  F1, F2, F3: TFileTime;
begin
	hF := FileOpen(Filename, fmOpenRead);
	GetFileTime(hF, @F1, @F2, @F3);
  Result := F3.dwHighDateTime;
  FileClose(hF);
end;

procedure SearchFileExt(const Dir, Ext: String; Files: TStrings);
var
	Found: TSearchRec;
  Sub: String;
  i : Integer;
  Dirs: TStrings; //Store sub-directories
  Finished : Integer; //Result of Finding
begin
	StopSearch := False;
	Dirs := TStringList.Create;
	Finished := FindFirst(Dir + '*.*', 63, Found);
  while (Finished = 0) and not (StopSearch) do
  begin
  	//Check if the name is valid.
  	if (Found.Name[1] <> '.') then
 		begin
    //Check if file is a directory
    	if (Found.Attr and faDirectory = faDirectory) then
      	Dirs.Add(Dir + Found.Name)  //Add to the directories list.
    	else
   			if Pos(UpperCase(Ext), UpperCase(Found.Name))>0 then
      		Files.Add(Dir + Found.Name);
    end;
		Finished := FindNext(Found);
  end;
  //end the search process.
  FindClose(Found);
  //Check if any sub-directories found
	if not StopSearch then
  	for i := 0 to Dirs.Count - 1 do
    	//If sub-dirs then search agian ~>~>~> on and on, until it is done.
			SearchFileExt(Dirs[i], Ext, Files);

  //Clear the memories.
  Dirs.Free;
end;

function FilenameOnly(const Filename: String): String;
begin
	Result := ExtractFilename(Filename);
  Result := ChangeFileExt(Filename, '');
end;

function DirUp(Directory: String): String;
begin
	Result := Directory;
	if Result[Length(Result)]='\' then
  	Delete(Result, Length(Result), 1);
	Delete(Result, Length(Result) - Length(LastDir(Result)), Length(Result));
end;

function AppPath: String;
begin
	Result := ExtractFilepath(Application.Exename);
end;

function TmpDir: String;
var
	Dir: array[0..255] of Char;
  Size: Integer;
begin
	Size := SizeOf(Dir) - 1;
  GetTempPath(Size, Dir);
  Result := Dir;
end;

function ExtractFilepath(const Filename: TFilename): String;
begin
	Result := SysUtils.ExtractFilepath(Filename);
  //Use the orignal function first, and ...
  if Result[Length(Result)] = '\' then   //Delete it.
  	Delete(Result, Length(Result), 1);
end;

function LastDir(Dir: String): String;
begin
	if Dir[Length(Dir)]='\' then
  	Delete(Dir, Length(Dir), 1);
  Result := RightStr(Dir, RPos('\', Dir)+1);
end;

function AppExe: String;
begin
	Result := Application.Exename;
end;

function GetWinDir : String;
var
	Dir: array[0..255] of Char;
  Size: Integer;
begin
	Size := SizeOf(Dir) - 1;
  GetWindowsDirectory(Dir, Size);
  Result := Dir;
end;

function CreateDirs(const Dirs: String) : Boolean;
var
	i : Integer;
  mD, CurrDir : String;
begin
	MD := Dirs;
	if RightStr(MD, 1) <> '\' then MD := MD + '\';
	Result := DirExists(Dirs);
	i := 0;
  i := Instr(i + 1, MD, '\');
  if i = 0 then Exit;
  i := Instr(i + 1, MD, '\');
  if i = 0 then Exit;
  CurrDir := Copy(MD, 1, i - 1);
  while not DirExists(Dirs) do
	begin
    if not FileExists(CurrDir) then CreateDir(CurrDir);
	  i := Instr(i + 1, MD, '\');
	  if i > 0 then
    	CurrDir := Copy(MD, 1, i - 1);
  end;
  Result := True;
end;

function formFilename(const Dir, Filename: String): String;
begin
	if Dir[Length(Dir)] = '\' then
		Result := Dir + Filename
  else
		Result := Dir + '\' + Filename;
end;

function TestWrite(const Dir: String): Boolean;
var
	F: File;
begin
  {$I-}
  AssignFile(F, formFilename(Dir, '~xxy45!j.tmp'));
  ReWrite(F);
  if IOResult <> 0 then
	begin
  	Result := False;
    Exit;
  end
  else
  	Result := True;
  CloseFile(F);
  DeleteFile(formFilename(Dir, '~xxy45!j.tmp'));
  {$I+}
end;

function DirExists(const Dir: String): Boolean;
begin
  {$I-}
  { Change the dir see if IO error}
  ChDir(Dir);
  if IOResult <> 0 then
		Result := False
  else
  	Result := True;
  {$I+}
end;

end.
