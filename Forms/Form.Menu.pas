unit Form.Menu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.Buttons, System.Generics.Collections, Classe.SmartPointer, Classe.Open.Form,
  Classe.Exception, Classe.FB.Connection;

type
  TFormMenu = class(TForm)
    pnlCenter: TPanel;
    pnlTelas: TGridPanel;
    pnlBuscarCEP: TPanel;
    imgBuscarCEP: TImage;
    lblBuscarCEP: TLabel;
    btnBuscarCEP: TSpeedButton;
    pnlCadastrosCEP: TPanel;
    imgCadastroCEP: TImage;
    lblCadastroCEP: TLabel;
    btnCadastrosCEP: TSpeedButton;
    pnlTop: TPanel;
    pnlConexao: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnBuscarCEPClick(Sender: TObject);
    procedure btnCadastrosCEPClick(Sender: TObject);
    procedure tmrCloseTimer(Sender: TObject);
  private
    { Private declarations }
    FTimerClose: ISmartPointer<TTimer>;
    FListaMenus: ISmartPointer<TDictionary<Integer, string>>;
    FConexao: ISmartPointer<TDictionary<Boolean, TProc>>;
    function GetListaMenus: ISmartPointer<TDictionary<Integer, string>>;
    procedure SetListaMenus(const Value: ISmartPointer<TDictionary<Integer, string>>);
    procedure IniciarListaMenus;
    procedure AbrirFormulario(const AKey: Integer);
    procedure IniciarConexao;
    procedure Conectado;
    procedure DoNothing;
    procedure ValidarConexao;
    procedure InitiTimerClose;
    procedure DesabilitarTimerClose;
    procedure HabilitarMenus;
  published
    { Published declarations }
    property ListaMenus: ISmartPointer<TDictionary<Integer, string>> read GetListaMenus write SetListaMenus;
  public
    { Public declarations }
  end;

var
  FormMenu: TFormMenu;

implementation

{$R *.dfm}

{ TFormularioMenu }

procedure TFormMenu.ValidarConexao;
var
  lConectado: TProc;
begin
  FConexao.TryGetValue(TFBConnection.New
                                      .NewConnection
                                      .Connection
                                      .Connected,
    lConectado);
  lConectado;
end;

procedure TFormMenu.AbrirFormulario(const AKey: Integer);
var
  lClasseFormulario: string;
begin
  ListaMenus.TryGetValue(AKey, lClasseFormulario);
  TOpenForm.New
             .ClassForm(lClasseFormulario)
             .ShowModal;
end;

procedure TFormMenu.btnBuscarCEPClick(Sender: TObject);
begin
  AbrirFormulario(TSpeedButton(Sender).Tag);
end;

procedure TFormMenu.btnCadastrosCEPClick(Sender: TObject);
begin
  AbrirFormulario(TSpeedButton(Sender).Tag);
end;

procedure TFormMenu.InitiTimerClose;
begin
  FTimerClose := TSmartPointer<TTimer>.Create(TTimer.Create(nil));
  FTimerClose.Interval := 5000;
  FTimerClose.Enabled := True;
  FTimerClose.OnTimer := tmrCloseTimer;
end;

procedure TFormMenu.DesabilitarTimerClose;
begin
  FTimerClose.Enabled := False;
end;

procedure TFormMenu.HabilitarMenus;
begin
  pnlCenter.Enabled := True;
end;

procedure TFormMenu.FormCreate(Sender: TObject);
begin
  InitiTimerClose;
  IniciarListaMenus;
  IniciarConexao;
  ValidarConexao;
  DesabilitarTimerClose;
  HabilitarMenus;
end;

function TFormMenu.GetListaMenus: ISmartPointer<TDictionary<Integer, string>>;
begin
  Result := FListaMenus;
end;

procedure TFormMenu.Conectado;
begin
  pnlConexao.ShowCaption := False;
  pnlConexao.Caption := 'Conexão estabelecida';
  pnlConexao.Color := $006FB12C;
  pnlConexao.ShowCaption := True;
end;

procedure TFormMenu.DoNothing;
begin

end;

procedure TFormMenu.IniciarConexao;
begin
  FConexao := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FConexao.Add(True, Conectado);
  FConexao.Add(False, DoNothing);
end;

procedure TFormMenu.IniciarListaMenus;
begin
  FListaMenus := TSmartPointer<TDictionary<Integer, string>>.Create(nil);
  FListaMenus.Add(btnBuscarCEP.Tag, 'TFormFindZIPCode');
  FListaMenus.Add(btnCadastrosCEP.Tag, 'TFormRegisteredZIPCode');
end;

procedure TFormMenu.SetListaMenus(
  const Value: ISmartPointer<TDictionary<Integer, string>>);
begin
  FListaMenus := Value;
end;

procedure TFormMenu.tmrCloseTimer(Sender: TObject);
begin
  Application.Terminate;
end;

end.
