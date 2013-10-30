program DehexUnicode;
{ Decodes files partially encoded in hex unicode.
 Usage: dehexunicode <input.txt >output.txt }

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

procedure Main;
var ln, ln_out: string;
  of_beg, of_end: integer;
begin
  while not eof(Input) do begin
    readln(ln);
    ln_out := '';
    of_beg := 1;
    while True do begin
      of_end := pos('30', ln, of_beg);
      if of_end<of_beg then begin
        ln_out := ln_out + copy(ln,of_beg,MaxInt);
        break;
      end;
      ln_out := ln_out + copy(ln,of_beg,of_end-of_beg);
      ln_out := ln_out + WideChar(Word(StrToInt('$'+copy(ln, of_end, 4))));
      of_beg := of_end+4;
    end;
    writeln(ln_out);
  end;

end;

begin
  try
    Main();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
