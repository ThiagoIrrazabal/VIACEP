unit Classe.Toast.Message;

interface

uses
  Vcl.Forms, Vcl.ExtCtrls, Classe.SmartPointer, Vcl.Controls, Vcl.Graphics,
  System.Generics.Collections, Classe.Panel.Collapse, System.SysUtils,
  Vcl.Buttons;

type
  TProcWait = procedure(var ATimes: Integer; const AInterval: Integer) of Object;
  TProcExec = procedure(const AExecute: Boolean) of Object;

  IToastMessage = Interface(IInterface)
    ['{988D4F65-B026-48DD-AF9B-975B27C5F7EE}']
    function Form(const AValue: TForm): IToastMessage; overload;
    function Form: TForm; overload;
    function Millisecond(const AValue: Integer): IToastMessage; overload;
    function Millisecond: Integer; overload;
    function ValueIncrement(const AValue: Integer): IToastMessage; overload;
    function ValueIncrement: Integer; overload;
    function Msg(const AValue: string): IToastMessage; overload;
    function Msg: string; overload;
    function Execute: IToastMessage;
  End;

  TToastMessage = class(TInterfacedObject, IToastMessage)
  strict private
  var
    FExecute: ISmartPointer<TDictionary<Boolean, TProcExec>>;
    FWaitNoFreeze: ISmartPointer<TDictionary<Boolean, TProcWait>>;
    FMsg: string;
    FForm: TForm;
    FMillisecond: Integer;
    FValueIncrement: Integer;
    FPanel: ISmartPointer<TPanel>;
    FPanelButton: ISmartPointer<TPanel>;
    FButton: ISmartPointer<TSpeedButton>;
    FExecuting: Boolean;
    FClose: Boolean;
  private
    procedure DoExecute(const AExecute: Boolean);
    procedure DoNothing(const AExecute: Boolean); overload;
    procedure DoNothing(var ATimes: Integer; const AInterval: Integer); overload;
    procedure InitiExecute;
    procedure InitiWaitNoFreeze;
    procedure WaitNoFreeze(var ATimes: Integer; const AInterval: Integer);
    procedure AnimatePanel;
    procedure CloseOnClik(Sender: TObject);
  public
    class function New: IToastMessage;
    constructor Create; overload;
    function Form(const AValue: TForm): IToastMessage; overload;
    function Form: TForm; overload;
    function Millisecond(const AValue: Integer): IToastMessage; overload;
    function Millisecond: Integer; overload;
    function ValueIncrement(const AValue: Integer): IToastMessage; overload;
    function ValueIncrement: Integer; overload;
    function Msg(const AValue: string): IToastMessage; overload;
    function Msg: string; overload;
    function Execute: IToastMessage;
  End;

implementation

{ TToastMessage }

function TToastMessage.Form(const AValue: TForm): IToastMessage;
begin
  Result := Self;
  FForm := AValue;
end;

constructor TToastMessage.Create;
begin
  InitiExecute;
  InitiWaitNoFreeze;
  FClose := False;
end;

procedure TToastMessage.CloseOnClik(Sender: TObject);
begin
  FClose := True;
end;

procedure TToastMessage.AnimatePanel;
var
  lTimes: Integer;
begin
  lTimes := 15;
  WaitNoFreeze(lTimes, 250);
  TPanelCollapse.New
                  .Panel(FPanel)
                  .HeightStop(0)
                  .Millisecond(Millisecond)
                  .ValueIncrement(ValueIncrement)
                  .Execute(not FExecuting);
  lTimes := ValueIncrement;
  WaitNoFreeze(lTimes, Millisecond);
end;

procedure TToastMessage.DoExecute(const AExecute: Boolean);
begin
  FExecuting := AExecute;
  FPanel := TSmartPointer<TPanel>.Create(TPanel.Create(FForm));
  FPanel.Parent := FForm;
  FPanel.BevelOuter := Vcl.ExtCtrls.TPanelBevel.bvNone;
  FPanel.ParentBackground := False;
  FPanel.ParentColor := False;
  FPanel.Color := $0006C8F9;
  FPanel.Caption := Msg;
  FPanel.Font.Size := 10;
  FPanel.Font.Style := [fsBold];
  FPanel.Font.Color := clBlack;
  FPanel.Align := alBottom;
  FPanel.AlignWithMargins := True;
  FPanel.BringToFront;
  FPanelButton := TSmartPointer<TPanel>.Create(TPanel.Create(nil));
  FPanelButton.ParentBackground := False;
  FPanelButton.Width := 15;
  FPanelButton.Parent := FPanel;
  FPanelButton.AlignWithMargins := True;
  FPanelButton.Caption := 'X';
  FPanelButton.Color := $005364FD;
  FPanelButton.BevelOuter := Vcl.ExtCtrls.TPanelBevel.bvNone;
  FPanelButton.Align := alRight;
  FPanelButton.Margins.Bottom := 23;
  FButton := TSmartPointer<TSpeedButton>.Create(TSpeedButton.Create(nil));
  FButton.Parent := FPanelButton;
  FButton.Align := alClient;
  FButton.Cursor := crHandPoint;
  FButton.Flat := True;
  FButton.Caption := EmptyStr;
  FButton.OnClick := CloseOnClik;
  AnimatePanel;
  FExecuting := not AExecute;
end;

procedure TToastMessage.DoNothing(var ATimes: Integer; const AInterval: Integer);
begin

end;

procedure TToastMessage.DoNothing(const AExecute: Boolean);
begin

end;

procedure TToastMessage.WaitNoFreeze(var ATimes: Integer; const AInterval: Integer);
var
  lWaitNoFreeze: TProcWait;
begin
  ATimes := ATimes - 1;
  Sleep(AInterval);
  Application.ProcessMessages;
  FWaitNoFreeze.TryGetValue((ATimes = 0) or FClose, lWaitNoFreeze);
  lWaitNoFreeze(ATimes, AInterval);
end;

function TToastMessage.Execute: IToastMessage;
var
  lExecute: TProcExec;
begin
  FExecute.TryGetValue(FExecuting, lExecute);
  lExecute(not FExecuting);
end;

function TToastMessage.Form: TForm;
begin
  Result := FForm;
end;

procedure TToastMessage.InitiExecute;
begin
  FExecute := TSmartPointer<TDictionary<Boolean, TProcExec>>.Create(nil);
  FExecute.Add(True, DoNothing);
  FExecute.Add(False, DoExecute);
end;

procedure TToastMessage.InitiWaitNoFreeze;
begin
  FWaitNoFreeze := TSmartPointer<TDictionary<Boolean, TProcWait>>.Create(nil);
  FWaitNoFreeze.Add(True, DoNothing);
  FWaitNoFreeze.Add(False, WaitNoFreeze);
end;

function TToastMessage.Millisecond(const AValue: Integer): IToastMessage;
begin
  Result := Self;
  FMillisecond := AValue;
end;

function TToastMessage.Millisecond: Integer;
begin
  Result := FMillisecond;
end;

function TToastMessage.Msg: string;
begin
  Result := FMsg;
end;

function TToastMessage.Msg(const AValue: string): IToastMessage;
begin
  Result := Self;
  FMsg := AValue;
end;

class function TToastMessage.New: IToastMessage;
begin
  Result := Self.Create;
end;

function TToastMessage.ValueIncrement(const AValue: Integer): IToastMessage;
begin
  Result := Self;
  FValueIncrement := AValue;
end;

function TToastMessage.ValueIncrement: Integer;
begin
  Result := FValueIncrement;
end;

end.
