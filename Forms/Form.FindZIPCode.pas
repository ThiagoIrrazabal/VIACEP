unit Form.FindZIPCode;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Mask, System.Generics.Collections,
  Classe.SmartPointer, Classe.Panel.Collapse, Vcl.Buttons, Datasnap.DBClient,
  Controller.ZIPCode, Classe.Exception, Classe.Enumerated;

type
  TFormFindZIPCode = class(TForm)
    pnlTop: TPanel;
    dbgCEP: TDBGrid;
    pnlCEP: TPanel;
    pnlLogradouro: TPanel;
    pnlBotoes: TPanel;
    lblCEP: TLabel;
    pnlUF: TPanel;
    lblUF: TLabel;
    cbxUF: TComboBox;
    edtCEP: TEdit;
    pnlCidade: TPanel;
    lblCidade: TLabel;
    edtCidade: TEdit;
    pnlEndereco: TPanel;
    lblEndereco: TLabel;
    edtEndereco: TEdit;
    rdbMetodo: TRadioGroup;
    btnPesquisar: TSpeedButton;
    procedure edtCEPEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtCEPChange(Sender: TObject);
    procedure cbxUFCloseUp(Sender: TObject);
    procedure cbxUFKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtCidadeChange(Sender: TObject);
    procedure edtEnderecoChange(Sender: TObject);
    procedure btnPesquisarClick(Sender: TObject);
  strict private
  var
    FPanelLogradouroHeight: ISmartPointer<TDictionary<Integer, Integer>>;
    FPanelCEPHeight: ISmartPointer<TDictionary<Integer, Integer>>;
    FComboClear: ISmartPointer<TDictionary<Boolean, TProc>>;
    FResetAll: ISmartPointer<TDictionary<Boolean, TProc>>;
    FClear: ISmartPointer<TDictionary<Boolean, TProc>>;
    FAnimatePanelLogradouro: ISmartPointer<TDictionary<Boolean, TProc>>;
    FAnimatePanelCEP: ISmartPointer<TDictionary<Boolean, TProc>>;
    FFindCEP: ISmartPointer<TDictionary<Boolean, TProc>>;
    FPanelAnimateLogradouro: IPanelCollapse;
    FPanelAnimateCEP: IPanelCollapse;
    FClientDataSet: ISmartPointer<TClientDataSet>;
    FDataSource: ISmartPointer<TDataSource>;
  private
    { Private declarations }
    procedure InitiAnimatePanelLogradouro;
    procedure InitiPanelLogradouroHeight;
    procedure InitiAnimatePanelCEP;
    procedure InitiPanelCEPHeight;
    procedure InitiPanelAnimate;
    procedure InitiDataControls;
    procedure InitiComboClear;
    procedure InitiResetAll;
    procedure InitiFindCEP;
    procedure InitiClear;
    procedure LogradouroClear;
    procedure CEPClear;
    procedure DoAnimatePanelLogradouro;
    procedure DoNothing;
    procedure DoAnimatePanelCEP;
    procedure ComboClear;
    procedure AnimateCEP;
    procedure ClearAll;
    procedure DoClearAll;
    procedure DesabilitarPanels;
    procedure HabilitarPanels;
    procedure SetAfterToast;
    procedure FindCEP;
    procedure FindLogradouro;
    function EmptyLogradouro: Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormFindZIPCode.SetAfterToast;
begin
  ClasseException.AfterToast := HabilitarPanels;
end;

procedure TFormFindZIPCode.DesabilitarPanels;
begin
  pnlCEP.Enabled := False;
  pnlLogradouro.Enabled := False;
end;

procedure TFormFindZIPCode.HabilitarPanels;
begin
  pnlCEP.Enabled := True;
  pnlLogradouro.Enabled := True;
end;

procedure TFormFindZIPCode.btnPesquisarClick(Sender: TObject);
var
  lFindCEP: TProc;
begin
  DesabilitarPanels;
  FFindCEP.TryGetValue((edtCEP.Text <> EmptyStr), lFindCEP);
  lFindCEP;
  HabilitarPanels;
end;

procedure TFormFindZIPCode.cbxUFCloseUp(Sender: TObject);
begin
  AnimateCEP;
  edtCidade.SetFocus;
end;

procedure TFormFindZIPCode.cbxUFKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lComboClear: TProc;
begin
  FComboClear.TryGetValue((Key = VK_DELETE) or (Key = VK_BACK), lComboClear);
  lComboClear;
end;

procedure TFormFindZIPCode.CEPClear;
begin
  edtCEP.OnChange := nil;
  edtCEP.Text := EmptyStr;
  edtCEP.OnChange := edtCEPChange;
  pnlLogradouro.Enabled := True;
  pnlCEP.Enabled := False;
end;

procedure TFormFindZIPCode.DoAnimatePanelLogradouro;
var
  lPanelHeight: Integer;
  lExecute: Boolean;
  lClear: TProc;
begin
  lExecute := (string(edtCEP.Text).Length = 0);
  FPanelLogradouroHeight.TryGetValue(pnlTop.Tag, lPanelHeight);
  FClear.TryGetValue(lExecute, lClear);
  lClear;
  FPanelAnimateLogradouro.HeightStop(lPanelHeight)
                         .Execute(lExecute);
  ClearAll;
end;

function TFormFindZIPCode.EmptyLogradouro: Boolean;
begin
  Result :=
    (cbxUF.ItemIndex = -1) and
    (string(edtCidade.Text).Length = 0) and
    (string(edtEndereco.Text).Length = 0);
end;

procedure TFormFindZIPCode.DoAnimatePanelCEP;
var
  lPanelHeight: Integer;
  lExecute: Boolean;
  lClear: TProc;
begin
  lExecute := EmptyLogradouro;
  FPanelCEPHeight.TryGetValue(pnlCEP.Tag, lPanelHeight);
  FClear.TryGetValue(not lExecute, lClear);
  lClear;
  FPanelAnimateCEP.HeightStop(lPanelHeight)
                  .Execute(lExecute);
  ClearAll;
end;

procedure TFormFindZIPCode.edtCEPChange(Sender: TObject);
var
  lAnimate: TProc;
begin
  FAnimatePanelLogradouro.TryGetValue(FPanelAnimateLogradouro.Executing, lAnimate);
  lAnimate;
end;

procedure TFormFindZIPCode.edtCEPEnter(Sender: TObject);
begin
  edtCEP.SelStart := 0;
end;

procedure TFormFindZIPCode.AnimateCEP;
var
  lAnimate: TProc;
begin
  FAnimatePanelCEP.TryGetValue(FPanelAnimateCEP.Executing, lAnimate);
  lAnimate;
end;

procedure TFormFindZIPCode.edtCidadeChange(Sender: TObject);
begin
  AnimateCEP;
end;

procedure TFormFindZIPCode.edtEnderecoChange(Sender: TObject);
begin
  AnimateCEP;
end;

procedure TFormFindZIPCode.FormCreate(Sender: TObject);
begin
  InitiAnimatePanelLogradouro;
  InitiPanelLogradouroHeight;
  InitiAnimatePanelCEP;
  InitiPanelCEPHeight;
  InitiPanelAnimate;
  InitiDataControls;
  InitiComboClear;
  InitiResetAll;
  InitiFindCEP;
  InitiClear;
  SetAfterToast;
end;

procedure TFormFindZIPCode.DoNothing;
begin

end;

procedure TFormFindZIPCode.InitiAnimatePanelCEP;
begin
  FAnimatePanelCEP := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FAnimatePanelCEP.Add(True, DoNothing);
  FAnimatePanelCEP.Add(False, DoAnimatePanelCEP);
end;

procedure TFormFindZIPCode.InitiAnimatePanelLogradouro;
begin
  FAnimatePanelLogradouro := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FAnimatePanelLogradouro.Add(True, DoNothing);
  FAnimatePanelLogradouro.Add(False, DoAnimatePanelLogradouro);
end;

procedure TFormFindZIPCode.InitiClear;
begin
  FClear := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FClear.Add(True, CEPClear);
  FClear.Add(False, LogradouroClear);
end;

procedure TFormFindZIPCode.ComboClear;
begin
  cbxUF.ItemIndex := -1;
  LogradouroClear;
  AnimateCEP;
end;

procedure TFormFindZIPCode.InitiComboClear;
begin
  FComboClear := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FComboClear.Add(True, ComboClear);
  FComboClear.Add(False, DoNothing);
end;

procedure TFormFindZIPCode.InitiDataControls;
begin
  FClientDataSet := TSmartPointer<TClientDataSet>.Create(TClientDataSet.Create(nil));
  FDataSource := TSmartPointer<TDataSource>.Create(TDataSource.Create(nil));
  FDataSource.DataSet := FClientDataSet;
  dbgCEP.DataSource := FDataSource;
end;

procedure TFormFindZIPCode.FindCEP;
begin
  TControllerCEP.New
                  .ClientDataSet(FClientDataSet)
                  .CEP(edtCEP.Text)
                  .FindMethod(TFindMethod(rdbMetodo.ItemIndex))
                  .ZIPCodeExists
                  .FindZIPCode;
end;

procedure TFormFindZIPCode.FindLogradouro;
begin
  TControllerCEP.New
                  .ClientDataSet(FClientDataSet)
                  .UF(cbxUF.Text)
                  .Localidade(edtCidade.Text)
                  .Logradouro(edtEndereco.Text)
                  .FindMethod(TFindMethod(rdbMetodo.ItemIndex))
                  .LogradouroExists
                  .FindLogradouro;
end;

procedure TFormFindZIPCode.InitiFindCEP;
begin
  FFindCEP := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FFindCEP.Add(True, FindCEP);
  FFindCEP.Add(False, FindLogradouro);
end;

procedure TFormFindZIPCode.InitiPanelAnimate;
begin
  FPanelAnimateLogradouro :=
    TPanelCollapse.New
                    .Panel(pnlTop)
                    .Millisecond(1)
                    .ValueIncrement(5);

  FPanelAnimateCEP :=
    TPanelCollapse.New
                    .Panel(pnlCEP)
                    .Millisecond(1)
                    .ValueIncrement(5);
end;

procedure TFormFindZIPCode.InitiPanelLogradouroHeight;
begin
  FPanelLogradouroHeight := TSmartPointer<TDictionary<Integer, Integer>>.Create(nil);
  FPanelLogradouroHeight.Add(0, pnlTop.Height - pnlLogradouro.Height);
  FPanelLogradouroHeight.Add(1, pnlTop.Height);
end;

procedure TFormFindZIPCode.InitiResetAll;
begin
  FResetAll := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FResetAll.Add(True, DoClearAll);
  FResetAll.Add(False, DoNothing);
end;

procedure TFormFindZIPCode.ClearAll;
var
  lResetAll: TProc;
  lExecute: Boolean;
begin
  lExecute := EmptyLogradouro and
    (string(edtCEP.Text).Length = 0);
  FResetAll.TryGetValue(lExecute, lResetAll);
  lResetAll;
end;

procedure TFormFindZIPCode.DoClearAll;
begin
  edtCEP.OnChange := nil;
  edtCidade.OnChange := nil;
  edtEndereco.OnChange := nil;
  cbxUF.ItemIndex := -1;
  edtCidade.Text := EmptyStr;
  edtEndereco.Text := EmptyStr;
  edtCEP.Text := EmptyStr;
  pnlLogradouro.Enabled := True;
  pnlCEP.Enabled := True;
  edtCEP.OnChange := edtCEPChange;
  edtCidade.OnChange := edtCidadeChange;
  edtEndereco.OnChange := edtEnderecoChange;
end;

procedure TFormFindZIPCode.InitiPanelCEPHeight;
begin
  FPanelCEPHeight := TSmartPointer<TDictionary<Integer, Integer>>.Create(nil);
  FPanelCEPHeight.Add(0, 0);
  FPanelCEPHeight.Add(1, pnlCEP.Height);
end;

procedure TFormFindZIPCode.LogradouroClear;
begin
  edtCidade.OnChange := nil;
  edtEndereco.OnChange := nil;
  cbxUF.ItemIndex := -1;
  edtCidade.Text := EmptyStr;
  edtEndereco.Text := EmptyStr;
  edtCidade.OnChange := edtCidadeChange;
  edtEndereco.OnChange := edtEnderecoChange;
  pnlLogradouro.Enabled := False;
  pnlCEP.Enabled := True;
end;

initialization
  System.Classes.RegisterClass(TFormFindZIPCode);

finalization
  System.Classes.UnRegisterClass(TFormFindZIPCode);

end.
