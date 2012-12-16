unit PackageCommon;
{
Common header for Wakan packages.
}

interface

type
  PKGHeader=packed record
    PKGTag:array[0..7] of AnsiChar;
    MemoryLimit:longint;
    HeaderLength:longint;
    PKGName:string[25];
  end;
  PKGFile=packed record
    PKGTag:array[0..3] of AnsiChar;
    FileName:string[128];
    FileExt:string[16];
    Directory:longint;
    LoadMode:byte;
    CryptMode:byte;
    PackMode:byte;
    CRCMode:byte;
    FileLength:longint;
    PackedLength:longint;
    HeaderCRC:byte;
  end;
  PKGCryptHeader=packed record
    PKGTag:array[0..2] of AnsiChar;
    MemoryLimit:longint;
    HeaderLength:longint;
  end;
  PKGCryptStarter=packed record
    ActualStart:longint;
    PKGTag:word;
  end;
  PKGHuffRec=record
    parent,bit1,bit0:integer;
    count:longint;
    active:boolean;
  end;
  PKGHuffArray=array[0..511] of PKGHuffRec;

procedure CalculateHuffmanCode(var ha:PKGHuffArray);

implementation

procedure CalculateHuffmanCode(var ha:PKGHuffArray);
var actcnt:word;
    lasthuff:word;
    max1n,max2n:integer;
    max1,max2:longint;
    i:integer;
begin
  actcnt:=256;
  lasthuff:=255;
  for i:=0 to 511 do ha[i].active:=false;
  for i:=0 to 255 do
  begin
    ha[i].parent:=-1; ha[i].bit1:=-1; ha[i].bit0:=-1;
    ha[i].active:=true;
  end;
  while actcnt>1 do
  begin
    max1:=-1;
    max2:=-1;
    max1n:=0;
    max2n:=0;
    for i:=0 to lasthuff do
    begin
      if ha[i].active then
      begin
        if (max1=-1) or (ha[i].count<max1) then
        begin
          max2:=max1;
          max2n:=max1n;
          max1:=ha[i].count;
          max1n:=i;
        end else if (max2=-1) or (ha[i].count<max2) then
        begin
          max2:=ha[i].count;
          max2n:=i;
        end;
      end;
    end;
    inc(lasthuff);
    ha[max1n].parent:=lasthuff;
    ha[max1n].active:=false;
    ha[max2n].parent:=lasthuff;
    ha[max2n].active:=false;
    ha[lasthuff].count:=ha[max1n].count+ha[max2n].count;
    ha[lasthuff].active:=true;
    ha[lasthuff].bit0:=max1n;
    ha[lasthuff].bit1:=max2n;
    ha[lasthuff].parent:=-1;
    dec(actcnt);
  end;
end;

end.
