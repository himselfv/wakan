program UnSafe;

uses Forms, Classes,
  MemSource in 'MemSource.pas';

{$R *.RES}

var a:TPackageSource;
    mf:TMemoryFile;
    ms:TMemoryStream;

begin
  writeln('UN-SAFE SAFEBACK PACKAGE EXTRACTOR');
  writeln('(C) LABYRINTH 1999');
  writeln;
  if paramcount<2 then
  begin
    writeln('Usage: UNSAFE <package file-name> <file-name-to-extract>');
    writeln('Example: UNSAFE C990919.PKG 000003.PAS');
    halt;
  end;
  writeln('Opening package...');
  a:=TPackageSource.Create(paramstr(1),0);
  if a.name<>'SafeBackup' then
  begin
    writeln('This is not a SafeBack package!');
    halt;
  end;
  mf:=a[paramstr(2)];
  if mf=nil then
  begin
    writeln('File not found in an package!');
    halt;
  end;
  writeln('Retrieving file...');
  ms:=mf.Lock;
  writeln('Writing file...');
  ms.SaveToFile(paramstr(2));
  writeln('Closing package...');
  mf.Unlock;
  a.Free;
  writeln('done.');
end.
