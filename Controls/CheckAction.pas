unit CheckAction;

interface
uses Classes, ActnList;

{
Sometimes we want to have centralized handling of some feature (ON/OFF),
but we also want to do additional processing when the state is changed manually.

I.e.:
 - show/hide some forms and update some controls unconditionally when Checked is changed
 - but only do some additional processing when Execute is called

This TAction descendant gives us two handlers:
  OnExecute to handle Action.Execute or button click/check
  OnChecked to handle any Action.Checked change, no matter what triggered it
}

type
 { Fortunately, there's already a way to receive some notifications through TActionLink }
  TCheckAction = class;

  TCheckActionLink = class(TActionLink)
  protected
    FClient: TCheckAction;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
  public
    Checking: boolean;
  end;

  TCheckAction = class(TAction)
  protected
    FOnChecking: TNotifyEvent;
    FOnChecked: TNotifyEvent;
    SelfLink: TCheckActionLink;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnChecking: TNotifyEvent read FOnChecking write FOnChecking;
    property OnChecked: TNotifyEvent read FOnChecked write FOnChecked;
  end;

procedure Register;

implementation
uses Actions;

procedure TCheckActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TCheckAction;
end;

procedure TCheckActionLink.SetChecked(Value: Boolean);
begin
 { TActionLink.SetChecked is called while FChecked is not yet changed,
  so we cannot fire OnChecked() -- everybody will try TAction.Checked and fail }
  if Assigned(FClient.FOnChecking) then
    FClient.FOnChecking(FClient);
 { We'll have to find some other point at which to call OnChecked() }
  Checking := true;
 { We'll do it in Change(), if Checking was set -- Change can be called for
  other reasons }
end;

constructor TCheckAction.Create(AOwner: TComponent);
begin
  inherited;
  SelfLink := TCheckActionLink.Create(Self);
  Self.RegisterChanges(SelfLink);
end;

procedure TCheckAction.Change;
begin
  inherited;
 { Change() is called on many occasions but we only need the call made from SetChecked().
  Let's hope this suffices as a test: }
  if SelfLink.Checking then begin
    if Assigned(FOnChecked) then FOnChecked(Self);
    SelfLink.Checking := false;
  end;
end;

procedure Register;
begin
  RegisterActions('', [TCheckAction], nil);
end;

end.
