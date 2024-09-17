unit Classe.Panel.Collapse;

interface

uses
  Vcl.ExtCtrls, Vcl.Forms, Classe.SmartPointer, System.Generics.Collections, System.SysUtils;

type
  IPanelCollapse = Interface(IInterface)
    ['{8B82092A-2668-4D8A-87EA-DA25A0A730BA}']
    function Panel(const AValue: TPanel): IPanelCollapse; overload;
    function Panel: TPanel; overload;
    function HeightStop(const AValue: Integer): IPanelCollapse; overload;
    function HeightStop: Integer; overload;
    function Millisecond(const AValue: Integer): IPanelCollapse; overload;
    function Millisecond: Integer; overload;
    function ValueIncrement(const AValue: Integer): IPanelCollapse; overload;
    function ValueIncrement: Integer; overload;
    function Executing: Boolean;
    function Execute(const AValue: Boolean): IPanelCollapse;
  End;

  TPanelCollapse = class(TInterfacedObject, IPanelCollapse)
  strict private
  var
    FPanel: TPanel;
    FHeightStop: Integer;
    FMillisecond: Integer;
    FValueIncrement: Integer;
    FIncrement: ISmartPointer<TDictionary<Integer, TProc>>;
    FDoIncrement: ISmartPointer<TDictionary<Boolean, TProc>>;
    FDoDecrement: ISmartPointer<TDictionary<Boolean, TProc>>;
    FDoHide: ISmartPointer<TDictionary<Boolean, TProc>>;
    FDoShow: ISmartPointer<TDictionary<Boolean, TProc>>;
    FStop: ISmartPointer<TDictionary<Boolean, TProc>>;
    FHide: ISmartPointer<TDictionary<Boolean, TProc>>;
    FTimer: ISmartPointer<TTimer>;
  private
    procedure Decrement;
    procedure DoNothing;
    procedure Hide;
    procedure Increment;
    procedure InitiHide;
    procedure InitiIncrement;
    procedure InitiStop;
    procedure InitiDoIncrement;
    procedure InitiDoDecrement;
    procedure InitiDoHide;
    procedure InitiDoShow;
    procedure DoTimer(Sender: TObject);
    procedure Show;
    procedure StopTimer;
    procedure DoIncrement;
    procedure DoDecrement;
    procedure DoHide;
    procedure DoShow;
  public
    class function New: IPanelCollapse;
    constructor Create; overload;
    function Panel(const AValue: TPanel): IPanelCollapse; overload;
    function Panel: TPanel; overload;
    function HeightStop(const AValue: Integer): IPanelCollapse; overload;
    function HeightStop: Integer; overload;
    function Millisecond(const AValue: Integer): IPanelCollapse; overload;
    function Millisecond: Integer; overload;
    function ValueIncrement(const AValue: Integer): IPanelCollapse; overload;
    function ValueIncrement: Integer; overload;
    function Executing: Boolean;
    function Execute(const AValue: Boolean): IPanelCollapse;
  End;

implementation

{ TPanelCollapse }

function TPanelCollapse.Execute(const AValue: Boolean): IPanelCollapse;
var
  lHide: TProc;
begin
  FHide.TryGetValue(AValue, lHide);
  lHide;
end;

class function TPanelCollapse.New: IPanelCollapse;
begin
  Result := Self.Create;
end;

function TPanelCollapse.Panel(const AValue: TPanel): IPanelCollapse;
begin
  Result := Self;
  FPanel := AValue;
end;

function TPanelCollapse.Panel: TPanel;
begin
  Result := FPanel;
end;

procedure TPanelCollapse.Increment;
var
  lDoIncrement: TProc;
  lStop: TProc;
begin
  FPanel.Tag := 0;
  FDoIncrement.TryGetValue((FPanel.Height >= FHeightStop), lDoIncrement);
  lDoIncrement;
  FStop.TryGetValue((FPanel.Height >= FHeightStop), lStop);
  lStop;
end;

constructor TPanelCollapse.Create;
begin
  InitiHide;
  InitiStop;
  InitiIncrement;
  InitiDoIncrement;
  InitiDoDecrement;
  InitiDoHide;
  InitiDoShow;
end;

procedure TPanelCollapse.Decrement;
var
  lDoDecrement: TProc;
  lStop: TProc;
begin
  FPanel.Tag := 1;
  FDoDecrement.TryGetValue((FPanel.Height <= FHeightStop), lDoDecrement);
  lDoDecrement;
  FStop.TryGetValue((FPanel.Height <= FHeightStop), lStop);
  lStop;
end;

procedure TPanelCollapse.DoDecrement;
begin
  FPanel.Height := FPanel.Height - ValueIncrement;
end;

procedure TPanelCollapse.DoHide;
begin
  FTimer := TSmartPointer<TTimer>.Create(TTimer.Create(nil));
  FTimer.Enabled := False;
  FTimer.Interval := Millisecond;
  FTimer.OnTimer := DoTimer;
  FTimer.Tag := 1;
  FTimer.Enabled := True;
end;

procedure TPanelCollapse.DoIncrement;
begin
  FPanel.Height := FPanel.Height + ValueIncrement;
end;

procedure TPanelCollapse.DoNothing;
begin

end;

procedure TPanelCollapse.DoShow;
begin
  FTimer := TSmartPointer<TTimer>.Create(TTimer.Create(nil));
  FTimer.Enabled := False;
  FTimer.Interval := Millisecond;
  FTimer.OnTimer := DoTimer;
  FTimer.Tag := 2;
  FTimer.Enabled := True;
end;

procedure TPanelCollapse.InitiDoDecrement;
begin
  FDoDecrement := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDoDecrement.Add(True, DoNothing);
  FDoDecrement.Add(False, DoDecrement);
end;

procedure TPanelCollapse.InitiDoHide;
begin
  FDoHide := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDoHide.Add(True, DoNothing);
  FDoHide.Add(False, DoHide);
end;

procedure TPanelCollapse.InitiDoIncrement;
begin
  FDoIncrement := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDoIncrement.Add(True, DoNothing);
  FDoIncrement.Add(False, DoIncrement);
end;

procedure TPanelCollapse.InitiDoShow;
begin
  FDoShow := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDoShow.Add(True, DoNothing);
  FDoShow.Add(False, DoShow);
end;

procedure TPanelCollapse.InitiHide;
begin
  FHide := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FHide.Add(True, Show);
  FHide.Add(False, Hide);
end;

procedure TPanelCollapse.InitiIncrement;
begin
  FIncrement := TSmartPointer<TDictionary<Integer, TProc>>.Create(nil);
  FIncrement.Add(1, Decrement);
  FIncrement.Add(2, Increment);
end;

procedure TPanelCollapse.InitiStop;
begin
  FStop := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FStop.Add(True, StopTimer);
  FStop.Add(False, DoNothing);
end;

function TPanelCollapse.Millisecond(const AValue: Integer): IPanelCollapse;
begin
  Result := Self;
  FMillisecond := AValue;
end;

function TPanelCollapse.Millisecond: Integer;
begin
  Result := FMillisecond;
end;

function TPanelCollapse.HeightStop: Integer;
begin
  Result := FHeightStop;
end;

function TPanelCollapse.HeightStop(const AValue: Integer): IPanelCollapse;
begin
  Result := Self;
  FHeightStop := AValue;
end;

procedure TPanelCollapse.Hide;
var
  lDoHide: TProc;
begin
  FDoHide.TryGetValue(Assigned(FTimer) and FTimer.Enabled, lDoHide);
  lDoHide;
end;

procedure TPanelCollapse.DoTimer(Sender: TObject);
var
  lIncrement: TProc;
begin
  FIncrement.TryGetValue(TTimer(Sender).Tag, lIncrement);
  lIncrement;
end;

procedure TPanelCollapse.Show;
var
  lDoShow: TProc;
begin
  FDoShow.TryGetValue(Assigned(FTimer) and FTimer.Enabled, lDoShow);
  lDoShow;
end;

procedure TPanelCollapse.StopTimer;
begin
  FTimer.Enabled := False;
end;

function TPanelCollapse.ValueIncrement(const AValue: Integer): IPanelCollapse;
begin
  Result := Self;
  FValueIncrement := AValue;
end;

function TPanelCollapse.ValueIncrement: Integer;
begin
  Result := FValueIncrement;
end;

function TPanelCollapse.Executing: Boolean;
begin
  Result := Assigned(FTimer) and (FTimer.Enabled);
end;

end.
