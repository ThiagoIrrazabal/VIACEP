program ProjetoCEP;

uses
  Vcl.Forms,
  Form.Menu in 'Forms\Form.Menu.pas' {FormMenu},
  Form.FindZIPCode in 'Forms\Form.FindZIPCode.pas' {FormFindZIPCode},
  Form.RegisteredZIPCode in 'Forms\Form.RegisteredZIPCode.pas' {FormRegisteredZIPCode},
  Classe.SmartPointer in 'Classes\Classe.SmartPointer.pas',
  Classe.Open.Form in 'Classes\Classe.Open.Form.pas',
  Classe.Exception in 'Classes\Classe.Exception.pas',
  Classe.Log in 'Classes\Classe.Log.pas',
  Classe.Panel.Collapse in 'Classes\Classe.Panel.Collapse.pas',
  Classe.FB.Connection in 'Classes\Classe.FB.Connection.pas',
  Classe.Strings.Helper in 'Classes\Classe.Strings.Helper.pas',
  Classe.Toast.Message in 'Classes\Classe.Toast.Message.pas',
  Classe in 'Classes\Classe.pas',
  Classe.REST in 'Classes\Classe.REST.pas',
  Classe.APICEP.MakeURL in 'Classes\Classe.APICEP.MakeURL.pas',
  Classe.Enumerated in 'Classes\Classe.Enumerated.pas',
  Controller.ZIPCode in 'Controllers\Controller.ZIPCode.pas',
  Dao.ZIPCode in 'DAO\Dao.ZIPCode.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Projeto VIA CEP';
  Application.CreateForm(TFormMenu, FormMenu);
  Application.Run;
end.
