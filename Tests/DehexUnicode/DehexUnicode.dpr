program DehexUnicode;
{ Decodes files partially encoded in hex unicode.
 Usage: dehexunicode <input.txt >output.txt }

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, JWBIO;

function IsHexChar(c: char): boolean;
begin
  Result := (c in ['a'..'z']) or (c in ['A'..'Z']) or (c in ['1'..'9']) or (c='0');
end;

procedure Main;
var ln, ln_out: string;
  of_beg, of_pos, of_end, of_end2: integer;
  inp: TStreamDecoder;
  wri: TStreamEncoder;
begin
  inp := ConsoleReader();
  wri := ConsoleWriter();
  while inp.ReadLn(ln) do begin
    ln_out := '';
    of_beg := 1;
    of_pos := of_beg;
    while True do begin
      of_end := pos('30', ln, of_pos);
      of_end2 := pos('31', ln, of_pos);
      if (of_end2>0) and (of_end2<of_end) then of_end := of_end2;
      if of_end<of_beg then begin
        ln_out := ln_out + copy(ln,of_beg,MaxInt);
        break;
      end;
      if (of_end>Length(ln)-4+1) or not IsHexChar(ln[of_end+2])
      or not IsHexChar(ln[of_end+3]) then begin
        of_pos := of_end+1;
        continue;
      end;
      ln_out := ln_out + copy(ln,of_beg,of_end-of_beg);
      ln_out := ln_out + WideChar(Word(StrToInt('$'+copy(ln, of_end, 4))));
      of_beg := of_end+4;
      of_pos := of_beg;
    end;
    wri.writeln(ln_out);
  end;
  wri.Flush;

end;

begin
  try
    Main();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
