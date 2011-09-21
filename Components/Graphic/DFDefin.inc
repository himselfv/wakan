{I just copied and modified it from RX.inc}
{$IFDEF VER93} {- Borland C++ Builder 1.0 }
  {$DEFINE CBUILDER}
{$ENDIF}

 {$IFNDEF VER90}    {-- Delphi 2.0     }
  {$IFNDEF VER93}   {-- C++Builder 1.0 }
    {$DEFINE DF_D3} { Delphi 3.0 or higher }
  {$ENDIF}
 {$ENDIF}

{$IFNDEF CBUILDER}
	{$DEFINE DELPHI}
{$ENDIF}
