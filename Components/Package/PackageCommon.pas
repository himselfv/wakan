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

implementation

end.
