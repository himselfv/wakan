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

{ Encryption mask generation.
 Originally, package format used standard random() to encrypt data. I don't know
 how it managed to not be broken in 8 releases of Delphi.
 I copied the algorithm here so that even if random changes, this stays the same. }

var
  EncSeed: Longint = 0;    { Base for encryption byte generator }

function EncMask(const ARange: Integer): Integer;

{ File name obfuscation.
 Used for file name and extension fields }

procedure ObfuscateFileName(FileSysCode: LongInt; TotFSize: LongInt; pc: PAnsiChar; len: integer);
procedure DeobfuscateFileName(FileSysCode: LongInt; TotFSize: LongInt; pc: PAnsiChar; len: integer);

{ CRC algorithm used in files when enabled }

function CalcCRC(buf: PByte; len: integer): cardinal;

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


{ Encryption mask generation }

//Previously "random()" by Borland/CodeGear/Embarcaderro.
//Whatever you change, it must produce exactly the same values given the same seed.
function EncMask(const ARange: Integer): Integer;
{$IF DEFINED(CPU386) }
asm
{     ->EAX     Range   }
{     <-EAX     Result  }
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
        MOV     ECX,[EBX].OFFSET EncSeed
        IMUL    EDX,[ECX],08088405H
        INC     EDX
        MOV     [ECX],EDX
{$ELSE}
        XOR     EBX, EBX
        IMUL    EDX,[EBX].EncSeed,08088405H
        INC     EDX
        MOV     [EBX].EncSeed,EDX
{$ENDIF}
        MUL     EDX
        MOV     EAX,EDX
        POP     EBX
end;
{$ELSE}
  {$MESSAGE ERROR 'EncMask: Implemented only on x86'}
{$IFEND}



procedure ObfuscateFileName(FileSysCode: LongInt; TotFSize: LongInt; pc: PAnsiChar; len: integer);
begin
  encseed := FileSysCode + TotFSize;
  while len > 0 do begin
    case pc^ of
      'A'..'Z': pc^ := AnsiChar((26+ord(pc^)-ord('A')-encmask(26)) mod 26 + ord('A'));
      'a'..'z': pc^ := AnsiChar((26+ord(pc^)-ord('a')-encmask(26)) mod 26 + ord('a'));
    else //keep the symbol
    end;
    Dec(len);
    Inc(pc);
  end;
end;

//Basically the same, only subtracts encmask instead of adding
procedure DeobfuscateFileName(FileSysCode: LongInt; TotFSize: LongInt; pc: PAnsiChar; len: integer);
begin
  encseed := FileSysCode + TotFSize;
  while len > 0 do begin
    case pc^ of
      'A'..'Z': pc^ := AnsiChar((ord(pc^)-ord('A')+encmask(26)) mod 26 + ord('A'));
      'a'..'z': pc^ := AnsiChar((ord(pc^)-ord('a')+encmask(26)) mod 26 + ord('a'));
    else //keep the symbol
    end;
    Dec(len);
    Inc(pc);
  end;
end;


{ CRC }

function CalcCRC(buf: PByte; len: integer): cardinal;
var i: integer;
begin
  Result := 0;
  for i:=1 to len do
    Result := (Result + cardinal(buf[i-1] xor i)) mod 12345678;
end;


var buf: string;
initialization

  buf := 'asdbsdasdasds';
  CalcCRC(@buf[1], Length(buf)*2);

end.
