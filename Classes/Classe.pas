unit Classe;

interface

uses
  Classe.FB.Connection, Classe.Exception, Classe.Log,
  Classe.Open.Form, Classe.Panel.Collapse, Classe.Toast.Message,
  Classe.REST, Classe.Enumerated;

type
  TFindMethod = Classe.Enumerated.TFindMethod;
  TException = Classe.Exception.TException;
  IFBConnection = Classe.FB.Connection.IFBConnection;
  TFBConnection = Classe.FB.Connection.TFBConnection;
  ILog = Classe.Log.ILog;
  IOpenForm = Classe.Open.Form.IOpenForm;
  IPanelCollapse = Classe.Panel.Collapse.IPanelCollapse;
  TPanelCollapse = Classe.Panel.Collapse.TPanelCollapse;
  IToastMessage = Classe.Toast.Message.IToastMessage;
  IRest = Classe.REST.IRest;
  TRest = Classe.REST.TRest;

implementation

end.
