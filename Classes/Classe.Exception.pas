unit Classe.Exception;

interface

uses
  System.SysUtils, System.Generics.Collections, Vcl.Forms, Classe.Log, Classe.SmartPointer,
  System.Classes, Vcl.Dialogs, System.StrUtils, Classe.Toast.Message;

{$M+}

type
  TException = class
  strict private
  var
    FE: Exception;
    FSender: TObject;
    FFormException: ISmartPointer<TDictionary<Boolean, TProc>>;
    FToast: ISmartPointer<TDictionary<Boolean, TProc>>;
    FMSG: ISmartPointer<TDictionary<Boolean, TProc>>;
    FName: ISmartPointer<TDictionary<Boolean, TFunc<string>>>;
    FCaption: ISmartPointer<TDictionary<Boolean, TFunc<string>>>;
    FAfterToast: TProc;
  private
    procedure FormException;
    procedure OtherException;
    procedure InitiException;
    procedure InitiName;
    procedure InitiCaption;
    procedure InitiToast;
    procedure InitiMSG;
    procedure DoNothing;
    procedure DoShowMSG;
    procedure DoShowToast;
    procedure ShowMSG;
    procedure ShowToast;
    function FormName: string;
    function OwnerName: string;
    function FormCaption: string;
    function OwnerCaption: string;
  published
    property AfterToast: TProc read FAfterToast write FAfterToast;
  public
    constructor Create; overload;
    procedure RaiseException(Sender: TObject; E: Exception);
  End;

var
  ClasseException: TException;

implementation

{ TException }

constructor TException.Create;
begin
  Application.OnException := RaiseException;
  InitiException;
  InitiMSG;
  InitiToast;
  InitiName;
  InitiCaption;
end;

procedure TException.DoNothing;
begin

end;

procedure TException.ShowMSG;
var
  lMSG: TProc;
begin
  FMSG.TryGetValue(ContainsText(FE.Message, 'MSG:'), lMSG);
  lMSG;
end;

function TException.OwnerCaption: string;
begin

end;

function TException.OwnerName: string;
begin
  Result := TForm(TComponent(FSender).Owner).Name;
end;

function TException.FormName: string;
begin
  Result := TForm(FSender).Name;
end;

function TException.FormCaption: string;
begin

end;

procedure TException.FormException;
var
  lName: TFunc<string>;
  lCaption: TFunc<string>;
  lForm: Boolean;
begin
  lForm :=
    (FSender.ClassParent.ClassName = 'TForm') or
    (FSender.ClassName = 'TForm');
  FName.TryGetValue(lForm, lName);
  FCaption.TryGetValue(lForm, lCaption);
  TLog.New
        .FormName(lName)
        .FormTitle(lCaption)
        .Exception(FE.Message)
        .Execute;
  ShowMSG;
  ShowToast;
end;

procedure TException.InitiCaption;
begin
  FCaption := TSmartPointer<TDictionary<Boolean, TFunc<string>>>.Create(nil);
  FCaption.Add(True, FormCaption);
  FCaption.Add(False, OwnerCaption);
end;

procedure TException.InitiException;
begin
  FFormException := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FFormException.Add(True, FormException);
  FFormException.Add(False, OtherException);
end;

procedure TException.InitiMSG;
begin
  FMSG := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FMSG.Add(True, DoShowMSG);
  FMSG.Add(False, DoNothing);
end;

procedure TException.InitiName;
begin
  FName := TSmartPointer<TDictionary<Boolean, TFunc<string>>>.Create(nil);
  FName.Add(True, FormName);
  FName.Add(False, OwnerName);
end;

procedure TException.ShowToast;
var
  lToast: TProc;
begin
  FToast.TryGetValue(ContainsText(FE.Message, 'INF:'), lToast);
  lToast;
end;

procedure TException.InitiToast;
begin
  FToast := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FToast.Add(True, DoShowToast);
  FToast.Add(False, DoNothing);
end;

procedure TException.OtherException;
begin
  TLog.New
        .FormName(FSender.ClassName)
        .FormTitle(EmptyStr)
        .Exception(FE.Message)
        .Execute;
  ShowMSG;
end;

procedure TException.RaiseException(Sender: TObject; E: Exception);
var
  lException: TProc;
begin
  FE := E;
  FSender := Sender;
  FFormException.TryGetValue(
    (Sender.ClassParent.ClassName = 'TForm') or
    (Sender.ClassName = 'TForm') or
    (TComponent(Sender).Owner.ClassName = 'TForm') or
    (TComponent(Sender).Owner.ClassParent.ClassName = 'TForm'), lException);
  lException;
end;

procedure TException.DoShowMSG;
begin
  ShowMessage(FE.Message.Replace('MSG: ', '', [rfIgnoreCase]));
end;

procedure TException.DoShowToast;
begin
  TToastMessage.New
                 .Form(TForm(TComponent(FSender).Owner))
                 .Millisecond(1)
                 .ValueIncrement(5)
                 .Msg(FE.Message.Replace('INF: ', '', [rfIgnoreCase]))
                 .Execute;
  AfterToast;
end;

initialization
  ClasseException := TException.Create;

finalization
  ClasseException.Free;

end.
