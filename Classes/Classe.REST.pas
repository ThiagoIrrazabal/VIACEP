unit Classe.REST;

interface

uses
  REST.Client, REST.Types, System.JSON, Xml.XMLIntf, Xml.XMLDoc, Classe.SmartPointer,
  System.Generics.Collections, System.SysUtils, System.Classes;

type
  IRest = Interface(IInterface)
    ['{224F8073-50D5-4C54-AD2B-E0A98EECF384}']
    function BaseURL(const AValue: string): IRest; overload;
    function BaseURL: string; overload;
    function JSONValue: TJSONValue;
    function XMLDocument: IXMLDocument;
    function Execute: IRest; overload;
  End;

  TRest = class(TInterfacedObject, IRest)
  strict private
  var
    FBaseURL: string;
    FJSONValue: TJSONValue;
    FXMLDocument: IXMLDocument;
    FRestClient: ISmartPointer<TRESTClient>;
    FRestRequest: ISmartPointer<TRESTRequest>;
    FRestResponse: ISmartPointer<TRESTResponse>;
    FSetResponse: ISmartPointer<TDictionary<Boolean, TProc>>;
    FSetJSONResponse: ISmartPointer<TDictionary<Boolean, TProc>>;
  private
    procedure InitiSetJSONResponse;
    procedure InitiSetResponse;
    procedure InitiRest;
    procedure DoSetResponse;
    procedure DoErrorResponse;
    procedure DoSetJSONResponse;
    procedure DoSetXMLResponse;
  public
    constructor Create; overload;
    class function New: IRest;
    function BaseURL(const AValue: string): IRest; overload;
    function BaseURL: string; overload;
    function JSONValue: TJSONValue;
    function XMLDocument: IXMLDocument;
    function Execute: IRest; overload;
  End;

implementation

{ TRest }

function TRest.BaseURL(const AValue: string): IRest;
begin
  Result := Self;
  FBaseURL := AValue;
end;

function TRest.BaseURL: string;
begin
  Result := FBaseURL;
end;

constructor TRest.Create;
begin
  InitiRest;
  InitiSetResponse;
  InitiSetJSONResponse;
end;

function TRest.Execute: IRest;
var
  lSetResponse: TProc;
begin
  Result := Self;
  FRestClient.BaseURL := BaseURL;
  FRestRequest.Method := TRESTRequestMethod.rmGet;
  FRestRequest.Execute;
  FSetResponse.TryGetValue((FRestResponse.StatusCode in [200, 201]), lSetResponse);
  lSetResponse;
end;

procedure TRest.InitiRest;
begin
  FRestClient := TSmartPointer<TRESTClient>.Create(TRESTClient.Create(nil));
  FRestResponse := TSmartPointer<TRESTResponse>.Create(TRESTResponse.Create(nil));
  FRestRequest := TSmartPointer<TRESTRequest>.Create(TRESTRequest.Create(nil));
  FRestRequest.Client := FRestClient;
  FRestRequest.Response := FRestResponse;
end;

procedure TRest.DoSetResponse;
var
  lSetJSONResponse: TProc;
begin
  FSetJSONResponse.TryGetValue(Assigned(FRestResponse.JSONValue), lSetJSONResponse);
  lSetJSONResponse;
end;

procedure TRest.DoErrorResponse;
begin
  raise Exception.Create('INF: Problema ao acessar a API VIA CEP [' + FRestResponse.StatusCode.ToString + '] ' + FRestResponse.StatusText);
end;

procedure TRest.DoSetJSONResponse;
begin
  FJSONValue := FRestResponse.JSONValue;
end;

procedure TRest.DoSetXMLResponse;
var
  lContentStream: ISmartPointer<TStringStream>;
begin
  lContentStream := TSmartPointer<TStringStream>.Create(TStringStream.Create(FRestResponse.Content, TEncoding.UTF8));
  FXMLDocument := TXMLDocument.Create(nil);
  FXMLDocument.LoadFromStream(lContentStream);
  FXMLDocument.Active := True;
end;

procedure TRest.InitiSetJSONResponse;
begin
  FSetJSONResponse := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FSetJSONResponse.Add(True, DoSetJSONResponse);
  FSetJSONResponse.Add(False, DoSetXMLResponse);
end;

procedure TRest.InitiSetResponse;
begin
  FSetResponse := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FSetResponse.Add(True, DoSetResponse);
  FSetResponse.Add(False, DoErrorResponse);
end;

function TRest.JSONValue: TJSONValue;
begin
  Result := FJSONValue;
end;

class function TRest.New: IRest;
begin
  Result := Self.Create;
end;

function TRest.XMLDocument: IXMLDocument;
begin
  Result := FXMLDocument;
end;

end.
