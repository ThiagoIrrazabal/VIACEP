unit Classe.Open.Form;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, Vcl.Dialogs, Vcl.Forms, Classe.SmartPointer;

type
  IOpenForm = Interface(IInterface)
    ['{7CEE105A-38A8-4634-94BC-02E8EB9BE6C6}']
    function ClassForm(const AValue: string): IOpenForm; overload;
    function ClassForm: string; overload;
    function ShowModal: IOpenForm;
  End;

  TOpenForm = class(TInterfacedObject, IOpenForm)
  strict private
  var
    FClassForm: string;
    FPersistentClass: TPersistentClass;
    FForm: ISmartPointer<TForm>;
    procedure FormNotFound;
    procedure DoShowModal;
  public
    class function New: IOpenForm;
    function ClassForm(const AValue: string): IOpenForm; overload;
    function ClassForm: string; overload;
    function ShowModal: IOpenForm;
  End;

implementation

{ TAbrirFormulario }

function TOpenForm.ClassForm(
  const AValue: string): IOpenForm;
begin
  Result := Self;
  FClassForm := AValue;
end;

function TOpenForm.ClassForm: string;
begin
  Result := FClassForm;
end;

procedure TOpenForm.DoShowModal;
begin
  FForm := TSmartPointer<TForm>.Create(TForm(TComponentClass(FPersistentClass).Create(nil)));
  FForm.ShowModal;
end;

procedure TOpenForm.FormNotFound;
begin
  raise Exception.Create(
    'MSG: Desculpe, mas o formulário que você está tentando abrir não foi encontrado.' + sLineBreak +
    'Uma possível causa é a não ter registrado a classe do formulário na inicialização da UNIT.');
end;

class function TOpenForm.New: IOpenForm;
begin
  Result := Self.Create;
end;

function TOpenForm.ShowModal: IOpenForm;
var
  lShowModal: ISmartPointer<TDictionary<Pointer, TProc>>;
  lDoShowModal: TProc;
begin
  FPersistentClass := GetClass(FClassForm);
  lShowModal := TSmartPointer<TDictionary<Pointer, TProc>>.Create(nil);
  lShowModal.Add(Pointer(NativeInt(FPersistentClass)), DoShowModal);
  lShowModal.AddOrSetValue(nil, FormNotFound);
  lShowModal.TryGetValue(Pointer(NativeInt(FPersistentClass)), lDoShowModal);
  lDoShowModal;
end;

end.
