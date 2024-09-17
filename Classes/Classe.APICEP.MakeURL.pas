unit Classe.APICEP.MakeURL;

interface

uses
  System.SysUtils, Classe.Enumerated, System.Generics.Collections, Classe.SmartPointer;

type
  IAPICEPMakeURL = Interface(IInterface)
    ['{3F2A1AB0-7E05-4B00-B8A0-2A347B267347}']
    function CEP(const AValue: string): IAPICEPMakeURL; overload;
    function CEP: string; overload;
    function UF(const AValue: string): IAPICEPMakeURL; overload;
    function UF: string; overload;
    function Localidade(const AValue: string): IAPICEPMakeURL; overload;
    function Localidade: string; overload;
    function Logradouro(const AValue: string): IAPICEPMakeURL; overload;
    function Logradouro: string; overload;
    function FindMethod(const AValue: TFindMethod): IAPICEPMakeURL; overload;
    function FindMethod: TFindMethod; overload;
    function URL(const AValue: ISmartPointer<TStringBuilder>): IAPICEPMakeURL; overload;
    function URL: ISmartPointer<TStringBuilder>; overload;
    function Execute: IAPICEPMakeURL;
  End;

  TAPICEPMakeURL = class(TInterfacedObject, IAPICEPMakeURL)
  strict private
  const
    URLBase: string = 'viacep.com.br/ws';
  var
    FCEP: string;
    FUF: string;
    FLocalidade: string;
    FLogradouro: string;
    FURL: ISmartPointer<TStringBuilder>;
    FFindMethod: TFindMethod;
    FDoCEP: ISmartPointer<TDictionary<Boolean, TProc>>;
    FDoFindMethod: ISmartPointer<TDictionary<Boolean, TProc>>;
  private
    procedure InitiDoFindMethod;
    procedure InitiDoCEP;
    procedure InitiURL;
    procedure DoCEP;
    procedure DoLogradouro;
    procedure DoJSON;
    procedure DoXML;
  public
    constructor Create; overload;
    class function New: IAPICEPMakeURL;
    function CEP(const AValue: string): IAPICEPMakeURL; overload;
    function CEP: string; overload;
    function UF(const AValue: string): IAPICEPMakeURL; overload;
    function UF: string; overload;
    function Localidade(const AValue: string): IAPICEPMakeURL; overload;
    function Localidade: string; overload;
    function Logradouro(const AValue: string): IAPICEPMakeURL; overload;
    function Logradouro: string; overload;
    function FindMethod(const AValue: TFindMethod): IAPICEPMakeURL; overload;
    function FindMethod: TFindMethod; overload;
    function URL(const AValue: ISmartPointer<TStringBuilder>): IAPICEPMakeURL; overload;
    function URL: ISmartPointer<TStringBuilder>; overload;
    function Execute: IAPICEPMakeURL;
  End;

implementation

{ TAPICEPMakeURL }

function TAPICEPMakeURL.CEP(const AValue: string): IAPICEPMakeURL;
begin
  Result := Self;
  FCEP := AValue;
end;

function TAPICEPMakeURL.CEP: string;
begin
  Result := FCEP;
end;

constructor TAPICEPMakeURL.Create;
begin
  InitiDoFindMethod;
  InitiDoCEP;
  InitiURL;
end;

function TAPICEPMakeURL.Logradouro: string;
begin
  Result := FLogradouro;
end;

function TAPICEPMakeURL.Logradouro(const AValue: string): IAPICEPMakeURL;
begin
  Result := Self;
  FLogradouro := AValue;
end;

function TAPICEPMakeURL.FindMethod: TFindMethod;
begin
  Result := FFindMethod;
end;

procedure TAPICEPMakeURL.DoCEP;
begin
  URL.Append('/')
     .Append(CEP);
end;

procedure TAPICEPMakeURL.DoLogradouro;
begin
  URL.Append('/')
     .Append(UF)
     .Append('/')
     .Append(Localidade)
     .Append('/')
     .Append(Logradouro);
end;

procedure TAPICEPMakeURL.DoJSON;
begin
  URL.Append('/')
     .Append('json')
     .Append('/');
end;

procedure TAPICEPMakeURL.DoXML;
begin
  URL.Append('/')
     .Append('xml')
     .Append('/');
end;

procedure TAPICEPMakeURL.InitiDoCEP;
begin
  FDoCEP := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDoCEP.Add(True, DoCEP);
  FDoCEP.Add(False, DoLogradouro);
end;

procedure TAPICEPMakeURL.InitiDoFindMethod;
begin
  FDoFindMethod := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FDoFindMethod.Add(True, DoJSON);
  FDoFindMethod.Add(False, DoXML);
end;

procedure TAPICEPMakeURL.InitiURL;
begin
  FURL := TSmartPointer<TStringBuilder>.Create(TStringBuilder.Create);
end;

function TAPICEPMakeURL.FindMethod(const AValue: TFindMethod): IAPICEPMakeURL;
begin
  Result := Self;
  FFindMethod := AValue;
end;

function TAPICEPMakeURL.Localidade(const AValue: string): IAPICEPMakeURL;
begin
  Result := Self;
  FLocalidade := AValue;
end;

function TAPICEPMakeURL.Localidade: string;
begin
  Result := FLocalidade;
end;

class function TAPICEPMakeURL.New: IAPICEPMakeURL;
begin
  Result := Self.Create;
end;

function TAPICEPMakeURL.Execute: IAPICEPMakeURL;
var
  lDoCEP: TProc;
  lDoFindMethod: TProc;
begin
  Result := Self;
  URL.Append(URLBase);
  FDoCEP.TryGetValue((CEP <> EmptyStr), lDoCEP);
  lDoCEP;

  FDoFindMethod.TryGetValue((FindMethod = fmJSON), lDoFindMethod);
  lDoFindMethod;
end;

function TAPICEPMakeURL.UF(const AValue: string): IAPICEPMakeURL;
begin
  Result := Self;
  FUF := AValue;
end;

function TAPICEPMakeURL.UF: string;
begin
  Result := FUF;
end;

function TAPICEPMakeURL.URL: ISmartPointer<TStringBuilder>;
begin
  Result := FURL;
end;

function TAPICEPMakeURL.URL(const AValue: ISmartPointer<TStringBuilder>): IAPICEPMakeURL;
begin
  Result := Self;
  FURL := AValue;
end;

end.
