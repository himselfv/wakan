unit JWBFCharConvert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JWBStrings, ExtCtrls;

type
  TFCharConvert = class(TForm)
    Panel1: TPanel;
    mmRaw: TMemo;
    Panel2: TPanel;
    cbCJKOnly: TCheckBox;
    btnToFChar: TButton;
    btnToRaw: TButton;
    mmFChar: TMemo;
    btnClose: TButton;
    Splitter1: TSplitter;
    procedure btnCloseClick(Sender: TObject);
    procedure btnToFCharClick(Sender: TObject);
    procedure btnToRawClick(Sender: TObject);
  protected
    function UnicodeToHexEx(pc:PWideChar; len: integer):string;
  end;

var
  FCharConvert: TFCharConvert;

implementation

{$R *.dfm}

procedure TFCharConvert.btnCloseClick(Sender: TObject);
begin
  Close;
end;

{ Same as JWBStrings.UnicodeToHex, but allows to convert only certain symbols }
function TFCharConvert.UnicodeToHexEx(pc:PWideChar; len: integer):string;
const HexChars: string = '0123456789ABCDEF';
var i:integer;
  ec: integer;
begin
  if pc=nil then begin
    Result := '';
    exit;
  end;

  Result := '';
  for i := 0 to len - 1 do begin
    ec := EvalChar(pc^);
    if cbCJKOnly.Checked then
      case ec of
        EC_IDG_CHAR,
        EC_HIRAGANA,
        EC_KATAKANA,
        EC_IDG_PUNCTUATION,
        EC_IDG_OTHER,
        EC_KATAKANA_HW:
        begin
        end;
      else
        Result := Result + pc^;
        Inc(pc);
        continue;
      end;

    Result := Result
      + HexChars[1 + PWord(pc)^ shr 12]
      + HexChars[1 + (PWord(pc)^ shr 8) and $0F]
      + HexChars[1 + (PWord(pc)^ shr 4) and $0F]
      + HexChars[1 + PWord(pc)^ and $0F];
    Inc(pc);
  end;
end;

//Forgiving conversion -- leaves stuff which does not look like FChars intact
function HexToUnicodeEx(ps:PWideChar; maxlen: integer): UnicodeString; overload;
var pn: PWideChar; //next symbol pointer
  cc: word; //character code
begin
  Result := '';
  if (ps=nil) or (ps^=#00) then exit;
 {$IFNDEF UNICODE}
  if ps^=WideChar(UH_UNKNOWN_KANJI) then Inc(ps);
 {$ENDIF}
  pn := ps;
  while (maxlen>=4) and EatOneFCharW(pn) do begin
    if not IsHexChar(ps^)
    or not IsHexChar((ps+1)^)
    or not IsHexChar((ps+2)^)
    or not IsHexChar((ps+3)^) then begin
      Result := Result + ps^;
      Inc(ps);
      pn := ps;
      Dec(maxlen);
      continue;
    end;

    cc := HexCharCodeW(ps^) shl 12;
    Inc(ps);
    Inc(cc, HexCharCodeW(ps^) shl 8);
    Inc(ps);
    Inc(cc, HexCharCodeW(ps^) shl 4);
    Inc(ps);
    Inc(cc, HexCharCodeW(ps^));
    Result := Result + WideChar(cc);
    ps := pn;
    Dec(maxlen,4);
  end;

  //Remainder
  while maxlen>=0 do begin
    Result := Result + ps^;
    Inc(ps);
    Dec(maxlen);
  end;
end;

procedure TFCharConvert.btnToFCharClick(Sender: TObject);
var s: string;
begin
  s := UnicodeToHexEx(PWideChar(mmRaw.Text), Length(mmRaw.Text));
  mmFChar.Text := s;
end;

procedure TFCharConvert.btnToRawClick(Sender: TObject);
var s: string;
begin
  s := HexToUnicodeEx(PWideChar(mmFChar.Text), Length(mmFChar.Text));
  mmRaw.Text := s;
end;

end.
