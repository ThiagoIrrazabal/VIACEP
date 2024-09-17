unit Dao.ZIPCode;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait,
  FireDAC.Phys.FBDef, FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.DApt,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, System.SysUtils,
  System.Classes, Classe.SmartPointer, Datasnap.DBClient,
  System.Generics.Collections, Classe, Classe.Strings.Helper;

type
  TProcEOF = procedure(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet) of Object;
  TProcFieldsEnd = procedure(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet; var ACount: Integer) of Object;
  TProcDataSet = procedure(const AClientDataSet: TClientDataSet) of Object;

  IDaoZIPCode = Interface(IInterface)
    ['{07EBA120-93C3-48D7-8E24-04F64E4AAF92}']
    function Codigos(const AValue: string): IDaoZIPCode; overload;
    function Codigos: string; overload;
    function Codigo(const AValue: Integer): IDaoZIPCode; overload;
    function Codigo: Integer; overload;
    function CEP(const AValue: string): IDaoZIPCode; overload;
    function CEP: string; overload;
    function Logradouro(const AValue: string): IDaoZIPCode; overload;
    function Logradouro: string; overload;
    function Complemento(const AValue: string): IDaoZIPCode; overload;
    function Complemento: string; overload;
    function Bairro(const AValue: string): IDaoZIPCode; overload;
    function Bairro: string; overload;
    function Localidade(const AValue: string): IDaoZIPCode; overload;
    function Localidade: string; overload;
    function UF(const AValue: string): IDaoZIPCode; overload;
    function UF: string; overload;
    function Insert: IDaoZIPCode;
    function Update: IDaoZIPCode;
    function Delete: IDaoZIPCode;
    function GetAll(const AClientDataSet: TClientDataSet): IDaoZIPCode;
    function GetByCodigo(const AClientDataSet: TClientDataSet): IDaoZIPCode;
    function GetByCodigos(const AClientDataSet: TClientDataSet): IDaoZIPCode;
    function ZIPCodeExists: Integer;
    function LogradouroExists: string;
    function ClearDataSet(const AClientDataSet: TClientDataSet): IDaoZIPCode;
  End;

  TDaoZIPCode = class(TInterfacedObject, IDaoZIPCode)
  strict private
  var
    FQueryEOF: ISmartPointer<TDictionary<Boolean, TProcEOF>>;
    FFieldsEnd: ISmartPointer<TDictionary<Boolean, TProcFieldsEnd>>;
    FCreateDataSet: ISmartPointer<TDictionary<Boolean, TProcDataSet>>;
    FCodigos: string;
    FCodigo: Integer;
    FCEP: string;
    FLogradouro: string;
    FComplemento: string;
    FBairro: string;
    FLocalidade: string;
    FUF: string;
  private
    procedure InitiQueryEOf;
    procedure InitiFieldsEnd;
    procedure InitiCreateDataSet;
    procedure DoNothing(const AClientDataSet: TClientDataSet); overload;
    procedure DoNothing(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet; var ACount: Integer); overload;
    procedure CreateField(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet; var ACount: Integer);
    procedure AppendData(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet);
    procedure DoCreateDataSet(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet); overload;
    procedure CreateDataSet(const AClientDataSet: TClientDataSet);
  public
    class function New: IDaoZIPCode;
    constructor Create; overload;
    function Codigos(const AValue: string): IDaoZIPCode; overload;
    function Codigos: string; overload;
    function Codigo(const AValue: Integer): IDaoZIPCode; overload;
    function Codigo: Integer; overload;
    function CEP(const AValue: string): IDaoZIPCode; overload;
    function CEP: string; overload;
    function Logradouro(const AValue: string): IDaoZIPCode; overload;
    function Logradouro: string; overload;
    function Complemento(const AValue: string): IDaoZIPCode; overload;
    function Complemento: string; overload;
    function Bairro(const AValue: string): IDaoZIPCode; overload;
    function Bairro: string; overload;
    function Localidade(const AValue: string): IDaoZIPCode; overload;
    function Localidade: string; overload;
    function UF(const AValue: string): IDaoZIPCode; overload;
    function UF: string; overload;
    function Insert: IDaoZIPCode;
    function Update: IDaoZIPCode;
    function Delete: IDaoZIPCode;
    function GetAll(const AClientDataSet: TClientDataSet): IDaoZIPCode;
    function GetByCodigo(const AClientDataSet: TClientDataSet): IDaoZIPCode;
    function GetByCodigos(const AClientDataSet: TClientDataSet): IDaoZIPCode;
    function ZIPCodeExists: Integer;
    function LogradouroExists: string;
    function ClearDataSet(const AClientDataSet: TClientDataSet): IDaoZIPCode;
  End;

implementation
{ TDaoZIPCode }

function TDaoZIPCode.Bairro(const AValue: string): IDaoZIPCode;
begin
  Result := Self;
  FBairro := AValue;
end;

procedure TDaoZIPCode.AppendData(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet);
var
  lAppendData: TProcEOF;
  lCreateDataSet: TProcDataSet;
begin
  FCreateDataSet.TryGetValue(AClientDataSet.IsEmpty, lCreateDataSet);
  lCreateDataSet(AClientDataSet);

  AClientDataSet.Append;
  AClientDataSet.FieldByName('CODIGO').AsInteger := AQuery.FieldByName('CODIGO').AsInteger;
  AClientDataSet.FieldByName('CEP').AsString := AQuery.FieldByName('CEP').AsString;
  AClientDataSet.FieldByName('LOGRADOURO').AsString := AQuery.FieldByName('LOGRADOURO').AsString;
  AClientDataSet.FieldByName('COMPLEMENTO').AsString := AQuery.FieldByName('COMPLEMENTO').AsString;
  AClientDataSet.FieldByName('BAIRRO').AsString := AQuery.FieldByName('BAIRRO').AsString;
  AClientDataSet.FieldByName('LOCALIDADE').AsString := AQuery.FieldByName('LOCALIDADE').AsString;
  AClientDataSet.FieldByName('UF').AsString := AQuery.FieldByName('UF').AsString;
  AClientDataSet.Post;
  AQuery.Next;
  FQueryEOF.TryGetValue(AQuery.Eof, lAppendData);
  lAppendData(AQuery, AClientDataSet);
end;

function TDaoZIPCode.Bairro: string;
begin
  Result := FBairro;
end;

function TDaoZIPCode.CEP(const AValue: string): IDaoZIPCode;
begin
  Result := Self;
  FCEP := AValue;
end;

function TDaoZIPCode.CEP: string;
begin
  Result := FCEP;
end;

function TDaoZIPCode.ZIPCodeExists: Integer;
var
  lQuery: ISmartPointer<TFDQuery>;
begin
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'SELECT COALESCE(CODIGO, 0) AS CODIGO FROM CEP WHERE CEP = :CEP';
  lQuery.ParamByName('CEP').AsString := CEP;
  lQuery.Open;
  Result := lQuery.FieldByName('CODIGO').AsInteger;
end;

function TDaoZIPCode.Codigo: Integer;
begin
  Result := FCodigo;
end;

function TDaoZIPCode.Codigos(const AValue: string): IDaoZIPCode;
begin
  Result := Self;
  FCodigos := AValue;
end;

function TDaoZIPCode.Codigos: string;
begin
  Result := FCodigos;
end;

function TDaoZIPCode.Codigo(const AValue: Integer): IDaoZIPCode;
begin
  Result := Self;
  FCodigo := AValue;
end;

function TDaoZIPCode.Complemento: string;
begin
  Result := FComplemento;
end;

constructor TDaoZIPCode.Create;
begin
  InitiQueryEOf;
  InitiFieldsEnd;
  InitiCreateDataSet;
end;

procedure TDaoZIPCode.CreateField(const AQuery: TFDQuery;
  const AClientDataSet: TClientDataSet; var ACount: Integer);
var
  lCreateField: TProcFieldsEnd;
begin
  AClientDataSet.FieldDefs.Add(AQuery.Fields[ACount].FieldName,
                               AQuery.Fields[ACount].DataType,
                               AQuery.Fields[ACount].Size,
                               False);
  FFieldsEnd.TryGetValue((ACount = (AQuery.FieldCount - 1)), lCreateField);
  ACount := ACount + 1;
  lCreateField(AQuery, AClientDataSet, ACount);
end;

function TDaoZIPCode.Delete: IDaoZIPCode;
var
  lQuery: ISmartPointer<TFDQuery>;
begin
  Result := Self;
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'DELETE FROM CEP WHERE CODIGO = :CODIGO';
  lQuery.ParamByName('CODIGO').AsInteger := Codigo;
  lQuery.ExecSQL;
end;

procedure TDaoZIPCode.DoNothing(const AQuery: TFDQuery;
  const AClientDataSet: TClientDataSet; var ACount: Integer);
begin

end;

procedure TDaoZIPCode.DoNothing(const AClientDataSet: TClientDataSet);
begin

end;

procedure TDaoZIPCode.DoCreateDataSet(const AQuery: TFDQuery; const AClientDataSet: TClientDataSet);
var
  lCreateDataSet: TProcDataSet;
begin
  FCreateDataSet.TryGetValue(AClientDataSet.IsEmpty, lCreateDataSet);
  lCreateDataSet(AClientDataSet);
end;

function TDaoZIPCode.ClearDataSet(const AClientDataSet: TClientDataSet): IDaoZIPCode;
begin
  Result := Self;
  AClientDataSet.Close;
  AClientDataSet.FieldDefs.Clear;
end;

function TDaoZIPCode.GetAll(const AClientDataSet: TClientDataSet): IDaoZIPCode;
var
  lQuery: ISmartPointer<TFDQuery>;
  lAppendData: TProcEOF;
  lCreateField: TProcFieldsEnd;
  lCount: Integer;
begin
  Result := Self;
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'SELECT * FROM CEP';
  lQuery.Open;

  lCount := 0;
  FFieldsEnd.TryGetValue(False, lCreateField);
  lCreateField(lQuery, AClientDataSet, lCount);

  FQueryEOF.TryGetValue(lQuery.Eof, lAppendData);
  lAppendData(lQuery, AClientDataSet);
end;

function TDaoZIPCode.GetByCodigo(const AClientDataSet: TClientDataSet): IDaoZIPCode;
var
  lQuery: ISmartPointer<TFDQuery>;
  lAppendData: TProcEOF;
  lCreateField: TProcFieldsEnd;
  lCount: Integer;
begin
  Result := Self;
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'SELECT * FROM CEP WHERE CODIGO = :CODIGO';
  lQuery.ParamByName('CODIGO').AsInteger := Codigo;
  lQuery.Open;

  lCount := 0;
  FFieldsEnd.TryGetValue((AClientDataSet.FieldDefs.Count <> 0), lCreateField);
  lCreateField(lQuery, AClientDataSet, lCount);

  FQueryEOF.TryGetValue(lQuery.Eof, lAppendData);
  lAppendData(lQuery, AClientDataSet);
end;

function TDaoZIPCode.GetByCodigos(
  const AClientDataSet: TClientDataSet): IDaoZIPCode;
var
  lQuery: ISmartPointer<TFDQuery>;
  lAppendData: TProcEOF;
  lCreateField: TProcFieldsEnd;
  lCount: Integer;
begin
  Result := Self;
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'SELECT * FROM CEP WHERE CODIGO IN (' + Codigos + ')';
  lQuery.Open;

  lCount := 0;
  FFieldsEnd.TryGetValue(False, lCreateField);
  lCreateField(lQuery, AClientDataSet, lCount);

  FQueryEOF.TryGetValue(lQuery.Eof, lAppendData);
  lAppendData(lQuery, AClientDataSet);
end;

procedure TDaoZIPCode.CreateDataSet(const AClientDataSet: TClientDataSet);
begin
  AClientDataSet.CreateDataSet;
end;

procedure TDaoZIPCode.InitiCreateDataSet;
begin
  FCreateDataSet := TSmartPointer<TDictionary<Boolean, TProcDataSet>>.Create(nil);
  FCreateDataSet.Add(True, CreateDataSet);
  FCreateDataSet.Add(False, DoNothing);
end;

procedure TDaoZIPCode.InitiFieldsEnd;
begin
  FFieldsEnd := TSmartPointer<TDictionary<Boolean, TProcFieldsEnd>>.Create(nil);
  FFieldsEnd.Add(True, DoNothing);
  FFieldsEnd.Add(False, CreateField);
end;

procedure TDaoZIPCode.InitiQueryEOf;
begin
  FQueryEOF := TSmartPointer<TDictionary<Boolean, TProcEOF>>.Create(nil);
  FQueryEOF.Add(True, DoCreateDataSet);
  FQueryEOF.Add(False, AppendData);
end;

function TDaoZIPCode.Insert: IDaoZIPCode;
var
  lQuery: ISmartPointer<TFDQuery>;
begin
  Result := Self;
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'INSERT INTO CEP (CEP, LOGRADOURO, COMPLEMENTO, BAIRRO, LOCALIDADE, UF) ' +
                     'VALUES (:CEP, :LOGRADOURO, :COMPLEMENTO, :BAIRRO, :LOCALIDADE, :UF)';
  lQuery.ParamByName('CEP').AsString := Cep;
  lQuery.ParamByName('LOGRADOURO').AsString := Logradouro.ReplaceAccentuation;
  lQuery.ParamByName('COMPLEMENTO').AsString := Complemento;
  lQuery.ParamByName('BAIRRO').AsString := Bairro;
  lQuery.ParamByName('LOCALIDADE').AsString := Localidade.ReplaceAccentuation;
  lQuery.ParamByName('UF').AsString := UF;
  lQuery.ExecSQL;
end;

function TDaoZIPCode.Complemento(const AValue: string): IDaoZIPCode;
begin
  Result := Self;
  FComplemento := AValue;
end;

function TDaoZIPCode.Localidade: string;
begin
  Result := FLocalidade;
end;

function TDaoZIPCode.Localidade(const AValue: string): IDaoZIPCode;
begin
  Result := Self;
  FLocalidade := AValue;
end;

function TDaoZIPCode.Logradouro: string;
begin
  Result := FLogradouro;
end;

function TDaoZIPCode.LogradouroExists: string;
var
  lQuery: ISmartPointer<TFDQuery>;
begin
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'SELECT COALESCE(CAST(LIST(CODIGO) AS varchar(5000)), 0) AS CODIGOS ' +
                     'FROM CEP WHERE UF = :UF ' +
                     'AND LOCALIDADE LIKE :LOCALIDADE ' +
                     'AND LOGRADOURO LIKE :LOGRADOURO ';
  lQuery.ParamByName('UF').AsString := UF;
  lQuery.ParamByName('LOCALIDADE').AsString := Localidade.ReplaceAccentuation;
  lQuery.ParamByName('LOGRADOURO').AsString := Logradouro.ReplaceAccentuation + '%';
  lQuery.Open;
  Result := lQuery.FieldByName('CODIGOS').AsString;
end;

function TDaoZIPCode.Logradouro(const AValue: string): IDaoZIPCode;
begin
  Result := Self;
  FLogradouro := AValue;
end;

class function TDaoZIPCode.New: IDaoZIPCode;
begin
  Result := Self.Create;
end;

function TDaoZIPCode.UF: string;
begin
  Result := FUF;
end;

function TDaoZIPCode.Update: IDaoZIPCode;
var
  lQuery: ISmartPointer<TFDQuery>;
begin
  Result := Self;
  lQuery := TSmartPointer<TFDQuery>.Create(TFDQuery.Create(nil));
  lQuery.Connection :=
    TFBConnection.New
                   .NewConnection
                   .Connection;
  lQuery.SQL.Text := 'UPDATE CEP SET CEP = :CEP, LOGRADOURO = :LOGRADOURO, ' +
                     'COMPLEMENTO = :COMPLEMENTO, BAIRRO = :BAIRRO, LOCALIDADE = :LOCALIDADE, ' +
                     'UF = :UF WHERE CODIGO = :CODIGO';
  lQuery.ParamByName('CEP').AsString := Cep;
  lQuery.ParamByName('LOGRADOURO').AsString := Logradouro.ReplaceAccentuation;
  lQuery.ParamByName('COMPLEMENTO').AsString := Complemento;
  lQuery.ParamByName('BAIRRO').AsString := Bairro;
  lQuery.ParamByName('LOCALIDADE').AsString := Localidade.ReplaceAccentuation;
  lQuery.ParamByName('UF').AsString := UF;
  lQuery.ParamByName('CODIGO').AsInteger := Codigo;
  lQuery.ExecSQL;
end;

function TDaoZIPCode.UF(const AValue: string): IDaoZIPCode;
begin
  Result := Self;
  FUF := AValue;
end;

end.
