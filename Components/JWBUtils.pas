unit JWBUtils;
{ Various utility functions which aren't particular to Wakan }

interface

procedure ShellOpen(const sCommand: string);

implementation
uses
{$IFDEF MSWINDOWS}
  ShellAPI, Winapi.Windows
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib
{$ENDIF POSIX}
;

procedure ShellOpen(const sCommand: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(sCommand), '', '', SW_SHOW);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(sCommand)));
{$ENDIF POSIX}
end;

end.
