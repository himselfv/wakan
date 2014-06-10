unit JWBClipboard;
{ Common clipboard handling for Wakan }

interface
uses Classes, Messages, Windows, Controls, ExtCtrls, Generics.Collections,
  JWBStrings;

type
  TClipboardMgr = class(TWinControl)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Clear;
  protected
    CbNextViewer: HWND;
    UpdatingClipboard:boolean;
    FWatchers: TList<TNotifyEvent>;
    FClip: FString;
    FClipboardUpdateTimer: TTimer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WmChangeCbChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
    procedure WmDrawClipboard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
    procedure ClipboardUpdateTimerTimer(Sender: TObject);
    procedure Update; reintroduce; //update FClip with UNICODETEXT from Clipboard
    procedure ClipboardChanged;
    procedure SetClip(const AValue: FString);
  public
   {
    Manually populate clipboard formats.
    How to use:
      Reset
      Add(...)
      Add(...)
      Publish
    Text will be set appropriately.
   }
    procedure ResetClipboard;
    procedure AddToClipboard(uFormat: UINT; data: pointer; size: integer); overload;
    procedure AddToClipboard(uFormat: UINT; text: RawByteString); overload;
    procedure AddToClipboard(uFormat: UINT; text: UnicodeString); overload;
    procedure AddToClipboard(uFormat: UINT; data: TMemoryStream; AOwnsStream: boolean = false); overload;
    procedure PublishClipboard;
    function GetClipboard(uFormat: UINT; out ms: TMemoryStream): boolean;
    property Text: string read FClip write SetClip;
    property Watchers: TList<TNotifyEvent> read FWatchers;
  end;

var
  Clipboard: TClipboardMgr;

implementation
uses SysUtils, clipbrd;

constructor TClipboardMgr.Create;
begin
  inherited Create(nil);
  FWatchers := TList<TNotifyEvent>.Create;

 //Init clipboard viewer
  CbNextViewer := SetClipboardViewer(Self.Handle);

 { SetClipboardViewer is supported starting with Windows 2000,
  so if there's a need we can implement dynamic linking and fall back to polling -
  it's kept as a safety measure anyway since CB chains are prone to breaking }
  FClipboardUpdateTimer := TTimer.Create(nil);
  FClipboardUpdateTimer.Interval := 2000;
  FClipboardUpdateTimer.Enabled:=true;
  FClipboardUpdateTimer.OnTimer := ClipboardUpdateTimerTimer;
  ClipboardUpdateTimerTimer(FClipboardUpdateTimer);
end;

destructor TClipboardMgr.Destroy;
begin
  FreeAndNil(FClipboardUpdateTimer);

 //Disconnect from clipboard viewer
  ChangeClipboardChain(Self.Handle, CbNextViewer);
  FreeAndNil(FWatchers);
  inherited;
end;

procedure TClipboardMgr.CreateParams(var Params: TCreateParams);
begin
  inherited;
 //Quick and dirty message-only window for clipboard and timer notifications
  Params.WndParent := HWND_MESSAGE;
end;

procedure TClipboardMgr.WmChangeCbChain(var Msg: TMessage);
begin
  if HWND(Msg.wParam)=CbNextViewer then begin
   // Replace next window and return
    CbNextViewer := Msg.lParam;
    Msg.Result := 0;
  end else
   // Pass to the next window
    if CbNextViewer<>0 then
      Msg.Result := SendMessage(CbNextViewer, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TClipboardMgr.WmDrawClipboard(var Msg: TMessage);
begin
  Self.Update;
  if CbNextViewer<>0 then
    SendMessage(CbNextViewer, Msg.Msg, Msg.wParam, Msg.lParam); //call next viewer
end;

procedure TClipboardMgr.Update;
var i:integer;
  h:boolean;
  newclip:FString;
  MyHandle:THandle;
  textptr:PWideChar;
  s:widestring;
begin
  if UpdatingClipboard then exit;
  UpdatingClipboard:=true;
  try
    clipbrd.Clipboard.Open;
    h:=false;
    for i:=0 to clipbrd.Clipboard.FormatCount-1 do
      if clipbrd.Clipboard.Formats[i]=CF_UNICODETEXT then h:=true;
    if h then
    begin
      MyHandle:=clipbrd.Clipboard.GetAsHandle(CF_UNICODETEXT);
      TextPtr:=GlobalLock(MyHandle);
      s:=textptr;
//      if length(s)>64000 then s:=_l('#00342^eToo much data.');
      newclip := fstr(s);
      GlobalUnlock(MyHandle);
    end;
    clipbrd.Clipboard.Close;
  except
    newclip := {$IFDEF UNICODE}'ERROR'{$ELSE}UnicodeToHex('ERROR'){$ENDIF};
  end;
  if newclip<>FClip then
  begin
    FClip := newclip;
    ClipboardChanged;
  end;
  UpdatingClipboard:=false;
end;

procedure TClipboardMgr.ClipboardUpdateTimerTimer(Sender: TObject);
begin
  Self.Update;
end;

procedure TClipboardMgr.Clear;
begin
  ResetClipboard;
  PublishClipboard;
end;

procedure TClipboardMgr.SetClip(const AValue: FString);
begin
  ResetClipboard;
  try
    AddToClipboard(CF_UNICODETEXT, AValue);
    FClip := AValue;
  finally
    PublishClipboard;
  end;
end;

//Open the clipboard and clear it
procedure TClipboardMgr.ResetClipboard;
begin
  OpenClipboard(Handle);
  EmptyClipboard;
end;

//Does not add terminating null
procedure TClipboardMgr.AddToClipboard(uFormat: UINT; data: pointer; size: integer);
var
  DataHandle :  THandle;
  ToPointer  :  Pointer;
begin
  DataHandle := GlobalAlloc(GMEM_DDESHARE OR GMEM_MOVEABLE, size);
  ToPointer := GlobalLock(DataHandle);
  if size>0 then
    Move(data^, ToPointer^, size); //die if data is invalid
  GlobalUnlock(DataHandle);
  SetClipboardData(uFormat, DataHandle);
end;

procedure TClipboardMgr.AddToClipboard(uFormat: UINT; text: RawByteString);
begin
  if Length(text)>0 then
    AddToClipboard(uFormat, pointer(text), Length(text)+1); //string + term 0
end;

procedure TClipboardMgr.AddToClipboard(uFormat: UINT; text: UnicodeString);
begin
  if Length(text)>0 then
    AddToClipboard(uFormat, pointer(text), Length(text)*2 + 2); //string + term 0
end;

procedure TClipboardMgr.AddToClipboard(uFormat: UINT; data: TMemoryStream; AOwnsStream: boolean = false);
begin
  AddToClipboard(uFormat,data.Memory,data.Size);
  if AOwnsStream then FreeAndNil(data);
end;

procedure TClipboardMgr.PublishClipboard;
begin
  CloseClipboard;
  Self.Update; //=> perhaps ClipboardChanged
end;

procedure TClipboardMgr.ClipboardChanged;
var i: integer;
begin
  for i := 0 to FWatchers.Count-1 do
    FWatchers[i](Self);
end;

//Retrieves a data for an HGLOBAL-containing clipboard format (most of them are)
function TClipboardMgr.GetClipboard(uFormat: UINT; out ms: TMemoryStream): boolean;
var h: THandle;
  pb: PByte;
  sz: integer;
  format: cardinal;
begin
  OpenClipboard(Handle);
  try
    format := 0;
    repeat
      format := EnumClipboardFormats(format);
    until (format=0) or (format=uFormat);

    if format=0 then begin
      Result := false;
      exit;
    end;

    h := GetClipboardData(uFormat);
    if h=0 then RaiseLastOsError();

    pb := GlobalLock(h);
    if pb=nil then RaiseLastOsError();

    sz := GlobalSize(h);
    if sz>MaxWord then sz := MaxWord; //won't accept more

    ms := TMemoryStream.Create;
    ms.Write(pb^,sz);

    GlobalUnlock(h);
    Result := true;
  finally
    CloseClipboard();
  end;
end;

initialization
 { Let's say for now that Clipboard is auto-created.
  If this causes problems (e.g. it steals first HWND in app flag), main form
  can auto-create it manually }
  Clipboard := TClipboardMgr.Create;

finalization
  FreeAndNil(Clipboard);

end.
