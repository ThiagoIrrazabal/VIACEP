unit Controller.ZIPCode;

interface

uses
  Dao.ZIPCode, System.SysUtils, System.Generics.Collections, Classe.SmartPointer,
  Datasnap.DBClient, Classe.Enumerated, Classe.REST, Classe.APICEP.MakeURL,
  System.JSON, System.Classes, Data.DB, Xml.XMLIntf;

type
  TProcZip = procedure(const AMSG: string) of Object;
  TProcSetResult = procedure(const ARest: IRest) of Object;
  TProcJSONArrayEOF = procedure(const AJSONArray: ISmartPointer<TJSONArray>; var APosition: Integer) of Object;
  TProcLogradouroEOF = procedure(const AStringList: ISmartPointer<TStringList>; var APosition: Integer) of Object;
  TProcXMLArrayEOF = procedure(const AXMLArray: IXMLNode; var APosition: Integer) of Object;

  IControllerCEP = Interface(IInterface)
    ['{E12405C8-E737-4EE4-9C5A-61F84C1E7EBE}']
    function Codigo(const AValue: Integer): IControllerCEP; overload;
    function Codigo: Integer; overload;
    function CEP(const AValue: string): IControllerCEP; overload;
    function CEP: string; overload;
    function Logradouro(const AValue: string): IControllerCEP; overload;
    function Logradouro: string; overload;
    function Localidade(const AValue: string): IControllerCEP; overload;
    function Localidade: string; overload;
    function UF(const AValue: string): IControllerCEP; overload;
    function UF: string; overload;
    function FindMethod(const AValue: TFindMethod): IControllerCEP; overload;
    function FindMethod: TFindMethod; overload;
    function ZIPCodeExists: IControllerCEP;
    function FindZIPCode: IControllerCEP;
    function LogradouroExists: IControllerCEP;
    function FindLogradouro: IControllerCEP;
    function GetAll: IControllerCEP;
    function Delete: IControllerCEP;
    function ClientDataSet(const AValue: TClientDataSet): IControllerCEP; overload;
    function ClientDataSet: TClientDataSet; overload;
  End;

  TControllerCEP = class(TInterfacedObject, IControllerCEP)
  strict private
  var
    FCodigo: Integer;
    FCEP: string;
    FLogradouro: string;
    FLocalidade: string;
    FUF: string;
    FFindMethod: TFindMethod;
    FDaoZIPCode: IDaoZIPCode;
    FClientDataSet: TClientDataSet;
    FZIPCodeValidate: ISmartPointer<TDictionary<Boolean, TProcZip>>;
    FSetResult: ISmartPointer<TDictionary<Boolean, TProcSetResult>>;
    FSetJSON: ISmartPointer<TDictionary<Boolean, TProcSetResult>>;
    FSetXML: ISmartPointer<TDictionary<Boolean, TProcSetResult>>;
    FJSONArrayEOF: ISmartPointer<TDictionary<Boolean, TProcJSONArrayEOF>>;
    FDaoInsert: ISmartPointer<TDictionary<Boolean, TProc>>;
    FLocate: ISmartPointer<TDictionary<Boolean, TProc>>;
    FLogradouroEOF: ISmartPointer<TDictionary<Boolean, TProcLogradouroEOF>>;
    FDataSetEmpty: ISmartPointer<TDictionary<Boolean, TProc>>;
    FErroXML: ISmartPointer<TDictionary<Boolean, TProcSetResult>>;
    FErroJSON: ISmartPointer<TDictionary<Boolean, TProcSetResult>>;
    FXMLArrayEOF: ISmartPointer<TDictionary<Boolean, TProcXMLArrayEOF>>;
  private
    procedure InitiZIPCodeValidate;  
    procedure InitiLogradouroEOF;
    procedure InitiJSONArrayEOF;
    procedure InitiDataSetEmpty;
    procedure InitiXMLArrayEOF;
    procedure InitiSetResult;
    procedure InitiDaoInsert;
    procedure InitiErroJSON;
    procedure InitiSetJSON;
    procedure InitiErroXML;
    procedure InitiSetXML;
    procedure InitiLocate;
    procedure InitiDao;
    procedure DoNothing(const AMSG: string); overload;
    procedure DoNothing(const AJSONArray: ISmartPointer<TJSONArray>; var APosition: Integer); overload;
    procedure DoNothing(const AXMLArray: IXMLNode; var APosition: Integer); overload;
    procedure DoNothing(const AStringList: ISmartPointer<TStringList>; var APosition: Integer); overload;
    procedure DoZIPCodeNotValid(const AMSG: string);
    procedure ZIPCodeValid;
    procedure UFValid;
    procedure LogradouroValid;
    procedure LocalidadeValid;
    procedure GetByCodigo(const ACodigo: Integer); overload;
    procedure GetByCodigo(const AStringList: ISmartPointer<TStringList>;
      var APosition: Integer); overload;
    procedure SetJSON(const ARest: IRest);
    procedure SetXML(const ARest: IRest);
    procedure SetJSONArray(const ARest: IRest);
    procedure SetJSONObject(const ARest: IRest); overload;
    procedure SetJSONObject(const AJSONArray: ISmartPointer<TJSONArray>; var APosition: Integer); overload;
    procedure SetJSONObject(const AJSONObject: TJSONObject); overload;
    procedure Edit;
    procedure Insert;
    procedure SetDao;
    procedure DaoUpdate;
    procedure DaoInsert;
    procedure DoNotLocate;
    procedure DoDataSetLocate;
    procedure Erro(const ARest: IRest);
    procedure DoSetXML(const ARest: IRest);
    procedure DoSetJSON(const ARest: IRest);
    procedure SetXMLArray(const ARest: IRest);
    procedure SetXMLObject(const ARest: IRest); overload;
    procedure SetXMLObject(const AXMLArray: IXMLNode; var APosition: Integer); overload;
    procedure SetXMLObject(const AXMLObject: IXMLNode); overload;
  public
    constructor Create; overload;
    class function New: IControllerCEP;
    function Codigo(const AValue: Integer): IControllerCEP; overload;
    function Codigo: Integer; overload;
    function CEP(const AValue: string): IControllerCEP; overload;
    function CEP: string; overload;
    function Logradouro(const AValue: string): IControllerCEP; overload;
    function Logradouro: string; overload;
    function Localidade(const AValue: string): IControllerCEP; overload;
    function Localidade: string; overload;
    function UF(const AValue: string): IControllerCEP; overload;
    function UF: string; overload;
    function FindMethod(const AValue: TFindMethod): IControllerCEP; overload;
    function FindMethod: TFindMethod; overload;
    function ZIPCodeExists: IControllerCEP;
    function FindZIPCode: IControllerCEP;
    function LogradouroExists: IControllerCEP;
    function FindLogradouro: IControllerCEP;
    function GetAll: IControllerCEP;
    function Delete: IControllerCEP;
    function ClientDataSet(const AValue: TClientDataSet): IControllerCEP; overload;
    function ClientDataSet: TClientDataSet; overload;
  End;

implementation

{ TControllerCEP }

function TControllerCEP.ZIPCodeExists: IControllerCEP;
var
  lCodigo: Integer;
begin
  Result := Self;
  ClientDataSet.DisableControls;
  ZIPCodeValid;
  lCodigo :=
    FDaoZIPCode.CEP(Self.CEP)
                 .ClearDataSet(ClientDataSet)
                 .ZIPCodeExists;
  GetByCodigo(lCodigo);
  ClientDataSet.EnableControls;
end;

procedure TControllerCEP.ZIPCodeValid;
var
  lZIPCodeValidate: TProcZip;
begin
  FZIPCodeValidate.TryGetValue((Self.CEP.Length = 8), lZIPCodeValidate);
  lZIPCodeValidate('INF: O campo [CEP] não pode conter menos de 8 dígitos');
end;

function TControllerCEP.CEP: string;
begin
  Result := FCEP;
end;

procedure TControllerCEP.LocalidadeValid;
var
  lZIPCodeValidate: TProcZip;
begin
  FZIPCodeValidate.TryGetValue((Self.Localidade.Length >= 3), lZIPCodeValidate);
  lZIPCodeValidate('INF: O campo [LOCALIDADE] não pode conter menos de 3 dígitos');
end;

function TControllerCEP.CEP(const AValue: string): IControllerCEP;
begin
  Result := Self;
  FCEP := AValue;
end;

function TControllerCEP.ClientDataSet: TClientDataSet;
begin
  Result := FClientDataSet;
end;

function TControllerCEP.Codigo: Integer;
begin
  Result := FCodigo;
end;

function TControllerCEP.Codigo(const AValue: Integer): IControllerCEP;
begin
  Result := Self;
  FCodigo := AValue;
end;

function TControllerCEP.ClientDataSet(
  const AValue: TClientDataSet): IControllerCEP;
begin
  Result := Self;
  FClientDataSet := AValue;
end;

constructor TControllerCEP.Create;
begin
  InitiZIPCodeValidate;
  InitiLogradouroEOF;
  InitiJSONArrayEOF;
  InitiDataSetEmpty;
  InitiXMLArrayEOF;
  InitiSetResult;
  InitiDaoInsert;
  InitiErroJSON;
  InitiErroXML;
  InitiSetJSON;
  InitiSetXML;
  InitiLocate;
  InitiDao;
end;

procedure TControllerCEP.DoNothing(const AMSG: string);
begin

end;

procedure TControllerCEP.DoNothing(
  const AStringList: ISmartPointer<TStringList>; var APosition: Integer);
begin

end;

procedure TControllerCEP.DoNothing(const AXMLArray: IXMLNode;
  var APosition: Integer);
begin

end;

procedure TControllerCEP.DoZIPCodeNotValid(const AMSG: string);
begin
  raise Exception.Create(AMSG);
end;

function TControllerCEP.FindLogradouro: IControllerCEP;
var
  lRest: IRest;
  lSetResult: TProcSetResult;
begin
  Result := Self;
  ClientDataSet.DisableControls;
  UFValid;
  LocalidadeValid;
  LogradouroValid;
  lRest :=
    TRest.New
           .BaseURL(TAPICEPMakeURL.New
                                    .UF(Self.UF)
                                    .Localidade(Self.Localidade)
                                    .Logradouro(Self.Logradouro)
                                    .FindMethod(Self.FindMethod)
                                    .Execute
                                      .URL
                                      .ToString)
           .Execute;
  FSetResult.TryGetValue(Assigned(lRest.JSONValue), lSetResult);
  lSetResult(lRest);
  ClientDataSet.EnableControls;
end;

function TControllerCEP.FindMethod: TFindMethod;
begin
  Result := FFindMethod;
end;

function TControllerCEP.FindMethod(const AValue: TFindMethod): IControllerCEP;
begin
  Result := Self;
  FFindMethod := AValue;
end;

function TControllerCEP.FindZIPCode: IControllerCEP;
var
  lRest: IRest;
  lSetResult: TProcSetResult;
begin
  Result := Self;
  ClientDataSet.DisableControls;
  ZIPCodeValid;
  lRest :=
    TRest.New
           .BaseURL(TAPICEPMakeURL.New
                                    .CEP(Self.CEP)
                                    .FindMethod(Self.FindMethod)
                                    .Execute
                                      .URL
                                      .ToString)
           .Execute;
  FSetResult.TryGetValue(Assigned(lRest.JSONValue), lSetResult);
  lSetResult(lRest);
  ClientDataSet.EnableControls;
end;

procedure TControllerCEP.GetByCodigo(const ACodigo: Integer);
begin
  FDaoZIPCode.Codigo(ACodigo)
             .GetByCodigo(ClientDataSet);
end;

procedure TControllerCEP.InitiDao;
begin
  FDaoZIPCode :=
    TDaoZIPCode.New;
end;

procedure TControllerCEP.DaoInsert;
begin
  FDaoZIPCode.Insert;
end;

procedure TControllerCEP.DaoUpdate;
begin
  FDaoZIPCode.Update;
end;

function TControllerCEP.Delete: IControllerCEP;
begin
  Result := Self;
  FDaoZIPCode.Codigo(Self.Codigo)
             .Delete;
end;

procedure TControllerCEP.InitiDaoInsert;
begin
  FDaoInsert := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDaoInsert.Add(True, DaoUpdate);
  FDaoInsert.Add(False, DaoInsert);
end;

procedure TControllerCEP.DoDataSetLocate;
var
  lLocate: TProc;
begin
  FLocate.TryGetValue(ClientDataSet.Locate('CEP', FDaoZIPCode.CEP, []), lLocate);
  lLocate;
end;

procedure TControllerCEP.DoNotLocate;
var
  lLocate: TProc;
begin
  FLocate.TryGetValue(False, lLocate);
  lLocate;
end;

procedure TControllerCEP.InitiDataSetEmpty;
begin
  FDataSetEmpty := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDataSetEmpty.Add(True, DoNotLocate);
  FDataSetEmpty.Add(False, DoDataSetLocate);
end;

procedure TControllerCEP.Erro(const ARest: IRest);
begin
  raise Exception.Create('INF: Erro ao fazer a busca na API VIA CEP');
end;

procedure TControllerCEP.DoSetXML(const ARest: IRest);
var
  lSetXML: TProcSetResult;
begin
  FSetXML.TryGetValue(Assigned(ARest.XMLDocument.DocumentElement.ChildNodes.FindNode('enderecos')), lSetXML);
  lSetXML(ARest);
end;

procedure TControllerCEP.DoSetJSON(const ARest: IRest);
var
  lSetJSON: TProcSetResult;
begin
  FSetJSON.TryGetValue((ARest.JSONValue is TJSONArray), lSetJSON);
  lSetJSON(ARest);
end;

procedure TControllerCEP.InitiErroJSON;
begin
  FErroJSON := TSmartPointer<TDictionary<Boolean, TProcSetResult>>.Create(nil);
  FErroJSON.Add(True, Erro);
  FErroJSON.Add(False, DoSetJSON);
end;

procedure TControllerCEP.InitiErroXML;
begin
  FErroXML := TSmartPointer<TDictionary<Boolean, TProcSetResult>>.Create(nil);
  FErroXML.Add(True, Erro);
  FErroXML.Add(False, DoSetXML);
end;

procedure TControllerCEP.InitiJSONArrayEOF;
begin
  FJSONArrayEOF := TSmartPointer<TDictionary<Boolean, TProcJSONArrayEOF>>.Create(nil);
  FJSONArrayEOF.Add(True, DoNothing);
  FJSONArrayEOF.Add(False, SetJSONObject);
end;

procedure TControllerCEP.InitiLocate;
begin
  FLocate := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FLocate.Add(True, Edit);
  FLocate.Add(False, Insert);
end;

procedure TControllerCEP.InitiLogradouroEOF;
begin
  FLogradouroEOF := TSmartPointer<TDictionary<Boolean, TProcLogradouroEOF>>.Create(nil);
  FLogradouroEOF.Add(True, DoNothing);
  FLogradouroEOF.Add(False, GetByCodigo);
end;

procedure TControllerCEP.SetXMLArray(const ARest: IRest);
var
  lXMLArray: IXMLNode;
  lPosition: Integer;
begin
  lPosition := 0;
  lXMLArray := ARest.XMLDocument.DocumentElement.ChildNodes.FindNode('enderecos');
  SetXMLObject(lXMLArray, lPosition);
end;

procedure TControllerCEP.SetXMLObject(const AXMLArray: IXMLNode;
  var APosition: Integer);
var
  lXMLArrayEOF: TProcXMLArrayEOF;
  lXMLObject: IXMLNode;
begin
  lXMLObject := AXMLArray.ChildNodes[APosition];
  SetXMLObject(lXMLObject);
  APosition := APosition + 1;
  FXMLArrayEOF.TryGetValue(((AXMLArray.ChildNodes.Count - 1) <= APosition), lXMLArrayEOF);
  lXMLArrayEOF(AXMLArray, APosition);
end;

procedure TControllerCEP.SetXMLObject(const AXMLObject: IXMLNode);
var
  lDataSetEmpty: TProc;
begin
  FDaoZIPCode.CEP(AXMLObject.ChildNodes['cep'].Text.Replace('-', '', [rfReplaceAll]));
  FDataSetEmpty.TryGetValue(ClientDataSet.IsEmpty, lDataSetEmpty);
  lDataSetEmpty;

  ClientDataSet.FieldByName('CEP').AsString := FDaoZIPCode.CEP;
  ClientDataSet.FieldByName('LOGRADOURO').AsString := AXMLObject.ChildNodes['logradouro'].Text.ToUpper;
  ClientDataSet.FieldByName('COMPLEMENTO').AsString := AXMLObject.ChildNodes['complemento'].Text.ToUpper;
  ClientDataSet.FieldByName('BAIRRO').AsString := AXMLObject.ChildNodes['bairro'].Text.ToUpper;
  ClientDataSet.FieldByName('LOCALIDADE').AsString := AXMLObject.ChildNodes['localidade'].Text.ToUpper;
  ClientDataSet.FieldByName('UF').AsString := AXMLObject.ChildNodes['uf'].Text.ToUpper;
  ClientDataSet.Post;
  SetDao;
end;

procedure TControllerCEP.SetXMLObject(const ARest: IRest);
var
  lXMLObject: IXMLNode;
begin
  lXMLObject := ARest.XMLDocument.DocumentElement;
  SetXMLObject(lXMLObject);
end;

procedure TControllerCEP.SetJSONArray(const ARest: IRest);
var
  lJSONArray: ISmartPointer<TJSONArray>;
  lPosition: Integer;
begin
  lPosition := 0;
  lJSONArray := TSmartPointer<TJSONArray>.Create(TJSONObject.ParseJSONValue(ARest.JSONValue.ToJSON) as TJSONArray);
  SetJSONObject(lJSONArray, lPosition);
end;

procedure TControllerCEP.SetJSONObject(
  const AJSONArray: ISmartPointer<TJSONArray>; var APosition: Integer);
var
  lJSONArrayEOF: TProcJSONArrayEOF;
  lJSONObject: TJSONObject;
begin
  lJSONObject := AJSONArray.Items[APosition] as TJSONObject;
  SetJSONObject(lJSONObject);
  APosition := APosition + 1;
  FJSONArrayEOF.TryGetValue(((AJSONArray.Count - 1) <= APosition), lJSONArrayEOF);
  lJSONArrayEOF(AJSONArray, APosition);
end;

procedure TControllerCEP.SetJSONObject(const ARest: IRest);
var
  lJSONObject: TJSONObject;
begin
  lJSONObject := ARest.JSONValue as TJSONObject;
  SetJSONObject(lJSONObject);
end;

procedure TControllerCEP.SetJSON(const ARest: IRest);
var
  lErro: TProcSetResult;
begin
  FErroJSON.TryGetValue((ARest.JSONValue is TJSONObject) and Assigned((ARest.JSONValue as TJSONObject).GetValue('erro')), lErro);
  lErro(ARest);
end;

procedure TControllerCEP.SetXML(const ARest: IRest);
var
  lErro: TProcSetResult;
begin
  FErroXML.TryGetValue(Assigned(ARest.XMLDocument.DocumentElement.ChildNodes.FindNode('erro')), lErro);
  lErro(ARest);
end;

function TControllerCEP.UF(const AValue: string): IControllerCEP;
begin
  Result := Self;
  FUF := AValue;
end;

function TControllerCEP.UF: string;
begin
  Result := FUF;
end;

procedure TControllerCEP.UFValid;
var
  lZIPCodeValidate: TProcZip;
begin
  FZIPCodeValidate.TryGetValue((Self.UF.Length = 2), lZIPCodeValidate);
  lZIPCodeValidate('INF: O campo [UF] não pode conter menos de 2 dígitos');
end;

procedure TControllerCEP.InitiSetJSON;
begin
  FSetJSON := TSmartPointer<TDictionary<Boolean, TProcSetResult>>.Create(nil);
  FSetJSON.Add(True, SetJSONArray);
  FSetJSON.Add(False, SetJSONObject);
end;

procedure TControllerCEP.InitiSetResult;
begin
  FSetResult := TSmartPointer<TDictionary<Boolean, TProcSetResult>>.Create(nil);
  FSetResult.Add(True, SetJSON);
  FSetResult.Add(False, SetXML);
end;

procedure TControllerCEP.InitiSetXML;
begin
  FSetXML := TSmartPointer<TDictionary<Boolean, TProcSetResult>>.Create(nil);
  FSetXML.Add(True, SetXMLArray);
  FSetXML.Add(False, SetXMLObject);
end;

procedure TControllerCEP.InitiXMLArrayEOF;
begin
  FXMLArrayEOF := TSmartPointer<TDictionary<Boolean, TProcXMLArrayEOF>>.Create(nil);
  FXMLArrayEOF.Add(True, DoNothing);
  FXMLArrayEOF.Add(False, SetXMLObject);
end;

procedure TControllerCEP.InitiZIPCodeValidate;
begin
  FZIPCodeValidate := TSmartPointer<TDictionary<Boolean, TProcZip>>.Create(nil);
  FZIPCodeValidate.Add(True, DoNothing);
  FZIPCodeValidate.Add(False, DoZIPCodeNotValid);
end;

function TControllerCEP.Localidade: string;
begin
  Result := FLocalidade;
end;

function TControllerCEP.Localidade(const AValue: string): IControllerCEP;
begin
  Result := Self;
  FLocalidade := AValue;
end;

function TControllerCEP.Logradouro: string;
begin
  Result := FLogradouro;
end;

function TControllerCEP.GetAll: IControllerCEP;
begin
  Result := Self;
  FDaoZIPCode.ClearDataSet(Self.ClientDataSet)
             .GetAll(Self.ClientDataSet);
end;

procedure TControllerCEP.GetByCodigo(const AStringList: ISmartPointer<TStringList>; var APosition: Integer);
var
  lCodigo: Integer;
  lLogradouroEOF: TProcLogradouroEOF;
begin
  lCodigo := AStringList[APosition].ToInteger;
  GetByCodigo(lCodigo);
  APosition := APosition + 1;
  FLogradouroEOF.TryGetValue((APosition = AStringList.Count), lLogradouroEOF);
  lLogradouroEOF(AStringList, APosition);
end;

function TControllerCEP.LogradouroExists: IControllerCEP;
var
  lCodigos: string;
  lLogradouroEOF: TProcLogradouroEOF;
  lStringList: ISmartPointer<TStringList>;
  lPosition: Integer;
begin
  Result := Self;
  ClientDataSet.DisableControls;
  lPosition := 0;
  UFValid;
  LogradouroValid;
  LocalidadeValid;
  lCodigos :=
    FDaoZIPCode.UF(Self.UF)
               .Localidade(Self.Localidade)
               .Logradouro(Self.Logradouro) 
                 .ClearDataSet(ClientDataSet)
                 .LogradouroExists;
  lStringList := TSmartPointer<TStringList>.Create(TStringList.Create);
  lStringList.Delimiter := ',';
  lStringList.DelimitedText := lCodigos;
  FLogradouroEOF.TryGetValue((lCodigos = EmptyStr), lLogradouroEOF);
  lLogradouroEOF(lStringList, lPosition);
  ClientDataSet.EnableControls;
end;

procedure TControllerCEP.LogradouroValid;
var
  lZIPCodeValidate: TProcZip;
begin
  FZIPCodeValidate.TryGetValue((Self.Logradouro.Length >= 3), lZIPCodeValidate);
  lZIPCodeValidate('INF: O campo [LOGRADOURO] não pode conter menos de 3 dígitos');
end;

function TControllerCEP.Logradouro(const AValue: string): IControllerCEP;
begin
  Result := Self;
  FLogradouro := AValue;
end;

class function TControllerCEP.New: IControllerCEP;
begin
  Result := Self.Create;
end;

procedure TControllerCEP.DoNothing(const AJSONArray: ISmartPointer<TJSONArray>;
  var APosition: Integer);
begin

end;

procedure TControllerCEP.Insert;
begin
  ClientDataSet.Insert;
end;

procedure TControllerCEP.Edit;
begin
  ClientDataSet.Edit;
end;

procedure TControllerCEP.SetDao;
var
  lDaoInsert: TProc;
begin
  FDaoZIPCode.Codigo(ClientDataSet.FieldByName('CODIGO').AsInteger)
             .Logradouro(ClientDataSet.FieldByName('LOGRADOURO').AsString)
             .Complemento(ClientDataSet.FieldByName('COMPLEMENTO').AsString)
             .Bairro(ClientDataSet.FieldByName('BAIRRO').AsString)
             .Localidade(ClientDataSet.FieldByName('LOCALIDADE').AsString)
             .UF(ClientDataSet.FieldByName('UF').AsString);
  FDaoInsert.TryGetValue(ClientDataSet.FieldByName('CODIGO').AsInteger > 0, lDaoInsert);
  lDaoInsert;
end;

procedure TControllerCEP.SetJSONObject(const AJSONObject: TJSONObject);
var
  lDataSetEmpty: TProc;
begin
  FDaoZIPCode.CEP(AJSONObject.GetValue('cep').Value.Replace('-', '', [rfReplaceAll]));
  FDataSetEmpty.TryGetValue(ClientDataSet.IsEmpty, lDataSetEmpty);
  lDataSetEmpty;

  ClientDataSet.FieldByName('CEP').AsString := FDaoZIPCode.CEP;
  ClientDataSet.FieldByName('LOGRADOURO').AsString := AJSONObject.GetValue('logradouro').Value.ToUpper;
  ClientDataSet.FieldByName('COMPLEMENTO').AsString := AJSONObject.GetValue('complemento').Value.ToUpper;
  ClientDataSet.FieldByName('BAIRRO').AsString := AJSONObject.GetValue('bairro').Value.ToUpper;
  ClientDataSet.FieldByName('LOCALIDADE').AsString := AJSONObject.GetValue('localidade').Value.ToUpper;
  ClientDataSet.FieldByName('UF').AsString := AJSONObject.GetValue('uf').Value.ToUpper;
  ClientDataSet.Post;
  SetDao;
end;

end.
