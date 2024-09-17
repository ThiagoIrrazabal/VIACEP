unit Classe.FB.Connection;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait,
  FireDAC.Phys.FBDef, FireDAC.Phys.IBBase, FireDAC.Phys.FB, Data.DB,
  FireDAC.Comp.Client, Classe.SmartPointer, System.SysUtils, System.IniFiles,
  System.IOUtils;

type
  IFBConnection = Interface(IInterface)
    ['{EA2E5C27-8C00-4C6D-8142-C72F5A6CA726}']
    function HostName(const AValue: string): IFBConnection; overload;
    function HostName: string; overload;
    function DataBase(const AValue: string): IFBConnection; overload;
		function DataBase: string; overload;
    function Porta(const AValue: string): IFBConnection; overload;
		function Porta: string; overload;
    function Password(const AValue: string): IFBConnection; overload;
    function Password: string; overload;
    function UserName(const AValue: string): IFBConnection; overload;
    function UserName: string; overload;
    function NewConnection: IFBConnection;
    function Connection: TFDConnection;
  End;

  TFBConnection = class(TInterfacedObject, IFBConnection)
  strict private
  var
    FHostName: string;
    FDataBase: string;
    FPorta: string;
    FPassword: string;
    FUserName: string;
    FBConnection: ISmartPointer<TFDConnection>;
    FDPhysFBDriverLink: ISmartPointer<TFDPhysFBDriverLink>;
  public
    class function New: IFBConnection;
    constructor Create; overload;
    function HostName(const AValue: string): IFBConnection; overload;
    function HostName: string; overload;
    function DataBase(const AValue: string): IFBConnection; overload;
		function DataBase: string; overload;
    function Porta(const AValue: string): IFBConnection; overload;
		function Porta: string; overload;
    function Password(const AValue: string): IFBConnection; overload;
    function Password: string; overload;
    function UserName(const AValue: string): IFBConnection; overload;
    function UserName: string; overload;
    function NewConnection: IFBConnection;
    function Connection: TFDConnection;
  End;

implementation

{ TFBConnection }

function TFBConnection.DataBase(const AValue: string): IFBConnection;
begin
  Result := Self;
  FDataBase := AValue;
end;

function TFBConnection.Connection: TFDConnection;
begin
  Result := FBConnection;
end;

constructor TFBConnection.Create;
var
  lFBConnection: ISmartPointer<TIniFile>;
begin
  lFBConnection :=
    TSmartPointer<TIniFile>.Create(TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FBConnection.ini'));
  HostName(lFBConnection.ReadString('HOSTNAME', 'hostname', EmptyStr));
  DataBase(lFBConnection.ReadString('DATABASE', 'database', EmptyStr));
  Porta(lFBConnection.ReadString('PORTA', 'porta', EmptyStr));
  UserName(lFBConnection.ReadString('USERNAME', 'username', EmptyStr));
  Password(lFBConnection.ReadString('PASSWORD', 'password', EmptyStr));
  DataBase(Copy(ExtractFileDir(ParamStr(0)), 1, LastDelimiter(PathDelim, ExtractFileDir(ParamStr(0))) - 1) + PathDelim + 'Banco' + PathDelim + FDataBase);
end;

function TFBConnection.DataBase: string;
begin
  Result := FDataBase;
end;

function TFBConnection.HostName: string;
begin
  Result := FHostName;
end;

function TFBConnection.HostName(const AValue: string): IFBConnection;
begin
  Result := Self;
  FHostName := AValue;
end;

class function TFBConnection.New: IFBConnection;
begin
  Result := Self.Create;
end;

function TFBConnection.NewConnection: IFBConnection;
begin
  Result := Self;
  FBConnection := TSmartPointer<TFDConnection>.Create(TFDConnection.Create(nil));
  FDPhysFBDriverLink := TSmartPointer<TFDPhysFBDriverLink>.Create(TFDPhysFBDriverLink.Create(nil));
  FDPhysFBDriverLink.VendorLib := ExtractFilePath(ParamStr(0)) + 'fbclient.dll';
  FBConnection.Params.Values['DriverID'] := 'FB';
  FBConnection.Params.Values['Server'] := HostName;
  FBConnection.Params.Values['Port'] := Porta;
  FBConnection.Params.Values['Database'] := DataBase;
  FBConnection.Params.Values['User_name'] := UserName;
  FBConnection.Params.Values['Password'] := Password;
  FBConnection.Connected := True;
end;

function TFBConnection.Password: string;
begin
  Result := FPassword;
end;

function TFBConnection.Password(const AValue: string): IFBConnection;
begin
  Result := Self;
  FPassword := AValue;
end;

function TFBConnection.Porta: string;
begin
  Result := FPorta;
end;

function TFBConnection.Porta(const AValue: string): IFBConnection;
begin
  Result := Self;
  FPorta := AValue;
end;

function TFBConnection.UserName(const AValue: string): IFBConnection;
begin
  Result := Self;
  FUserName := AValue;
end;

function TFBConnection.UserName: string;
begin
  Result := FUserName;
end;

end.
