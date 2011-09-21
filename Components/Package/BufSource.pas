unit BufSource;

interface

type TBufLoadMode=(blPermanentLoad,blAutoLoad,blDepentLoad,blTemporaryLoad);
     TBufFile=class
                private
                  vLoadMode:TBufLoadMode;
                  vLoaded:boolean;
                  vLockCount:longint;
                protected
                  Source:TBufSource;
                  Locator:variant;
                  Stream:TMemoryStream;
                  constructor Create; virtual; abstract;
                  procedure Load; virtual; abstract;
                  procedure Unload; virtual;
                  procedure Refresh; virtual;
                  procedure SetLoadMode(LM:TBufLoadMode);
                  property LockCount read vLockCount;
                  property Loaded read vLoaded;
                  property LoadMode read vLoadMode write SetLoadMode;
                  destructor Done; virtual;
                public
                  function Lock:TMemoryStream;
                  procedure Unlock;
              end;
     TBufSource=class
                  private
                    Files:TStringList;
                  protected
                    procedure Add(Name:string;BF:TBufFile);
                  public
                    constructor Create; virtual; abstract;
                    function GetFile(Name:string):TBufFile;
                    destructor Done; virtual;
                end;

implementation

end.
