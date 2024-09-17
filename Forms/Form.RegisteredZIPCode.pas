unit Form.RegisteredZIPCode;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids, Controller.ZIPCode,
  Classe.SmartPointer, Datasnap.DBClient, System.Generics.Collections;

type
  TProcDraw = procedure(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState) of Object;
  TProcMouseMove = procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of Object;

  TFormRegisteredZIPCode = class(TForm)
    dbgCEP: TDBGrid;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dbgCEPDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure dbgCEPMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure dbgCEPCellClick(Column: TColumn);
  strict private
  var
    FClientDataSet: ISmartPointer<TClientDataSet>;
    FDataSource: ISmartPointer<TDataSource>;
    FExcluirDraw: ISmartPointer<TDictionary<Boolean, TProcDraw>>;
    FCursor: ISmartPointer<TDictionary<Boolean, TProcMouseMove>>;
    FDoCursor: ISmartPointer<TDictionary<Boolean, TProcMouseMove>>;
    FExcluirColumnIndex: ISmartPointer<TDictionary<Integer, Integer>>;
    FIndicator: ISmartPointer<TDictionary<Boolean, TProc>>;
    FExcluirClick: ISmartPointer<TDictionary<Boolean, TProc>>;
  private
    procedure InitiExcluirDraw;
    procedure InitirExcluirClick;
    procedure InitiCursor;
    procedure InitiDoCursor;
    procedure InitiIndicator;
    procedure InitiDataControls;
    procedure InitiExcluirColumnIndex;
    procedure DrawExcluir(Sender: TObject; const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState);
    procedure DoNothing; overload;
    procedure DoNothing(Sender: TObject; const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); overload;
    procedure MouseCrHand(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseCrDefault(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseCrHand(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoAddSixSeven;
    procedure DoAddSix;
    procedure Delete;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormRegisteredZIPCode.dbgCEPCellClick(Column: TColumn);
var
  lExcluirClick: TProc;
begin
  FExcluirClick.TryGetValue((Column.Index = 6) and not FClientDataSet.IsEmpty, lExcluirClick);
  lExcluirClick;
end;

procedure TFormRegisteredZIPCode.dbgCEPDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  lExcluirDraw: TProcDraw;
begin
  FExcluirDraw.TryGetValue((Column.Index = 6), lExcluirDraw);
  lExcluirDraw(Sender, Rect, DataCol, Column, State);
end;

procedure TFormRegisteredZIPCode.FormCreate(Sender: TObject);
begin
  InitiDataControls;
  InitiExcluirDraw;
  InitirExcluirClick;
  InitiCursor;
  InitiDoCursor;
  InitiIndicator;
  InitiExcluirColumnIndex;
end;

procedure TFormRegisteredZIPCode.FormShow(Sender: TObject);
begin
  TControllerCEP.New
                  .ClientDataSet(FClientDataSet)
                  .GetAll;
end;

procedure TFormRegisteredZIPCode.MouseCrHand(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  lCol: TColumn;
  lMouseCursor: TProcMouseMove;
  lExcluirColumnIndex: Integer;
begin
  FExcluirColumnIndex.TryGetValue(dbgCEP.MouseCoord(X, Y).X, lExcluirColumnIndex);
  lCol := dbgCEP.Columns[lExcluirColumnIndex];
  FDoCursor.TryGetValue((lCol.Index = 6), lMouseCursor);
  lMouseCursor(Sender, Shift, X, Y);
end;

procedure TFormRegisteredZIPCode.MouseCrDefault(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  dbgCEP.Cursor := crDefault;
end;

procedure TFormRegisteredZIPCode.DoMouseCrHand(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  dbgCEP.Cursor := crHandPoint;
end;

procedure TFormRegisteredZIPCode.DoNothing;
begin

end;

procedure TFormRegisteredZIPCode.InitiCursor;
begin
  FCursor := TSmartPointer<TDictionary<Boolean, TProcMouseMove>>.Create(nil);
  FCursor.Add(True, MouseCrHand);
  FCursor.Add(False, MouseCrDefault);
end;

procedure TFormRegisteredZIPCode.InitiDataControls;
begin
  FClientDataSet := TSmartPointer<TClientDataSet>.Create(TClientDataSet.Create(nil));
  FDataSource := TSmartPointer<TDataSource>.Create(TDataSource.Create(nil));
  FDataSource.DataSet := FClientDataSet;
  dbgCEP.DataSource := FDataSource;
end;

procedure TFormRegisteredZIPCode.InitiDoCursor;
begin
  FDoCursor := TSmartPointer<TDictionary<Boolean, TProcMouseMove>>.Create(nil);
  FDoCursor.Add(True, DoMouseCrHand);
  FDoCursor.Add(False, MouseCrDefault);
end;

procedure TFormRegisteredZIPCode.DrawExcluir(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  dbgCEP.Canvas.TextRect(Rect, Trunc((Rect.Width / 2) - (dbgCEP.Canvas.TextWidth('Excluir') / 2) + Rect.Left), Rect.Top + 2, 'Excluir');
end;

procedure TFormRegisteredZIPCode.dbgCEPMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  lCell: TGridCoord;
  lMouseCursor: TProcMouseMove;
begin
  lCell := dbgCEP.MouseCoord(X, Y);
  FCursor.TryGetValue((lCell.X >= 0) and (lCell.Y >= 0), lMouseCursor);
  lMouseCursor(Sender, Shift, X, Y);
end;

procedure TFormRegisteredZIPCode.DoNothing(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin

end;

procedure TFormRegisteredZIPCode.InitiExcluirDraw;
begin
  FExcluirDraw := TSmartPointer<TDictionary<Boolean, TProcDraw>>.Create(nil);
  FExcluirDraw.Add(True, DrawExcluir);
  FExcluirDraw.Add(False, DoNothing);
end;

procedure TFormRegisteredZIPCode.InitiExcluirColumnIndex;
var
  lIndicator: TProc;
begin
  FExcluirColumnIndex := TSmartPointer<TDictionary<Integer, Integer>>.Create(nil);
  FExcluirColumnIndex.Add(0, 0);
  FExcluirColumnIndex.Add(1, 1);
  FExcluirColumnIndex.Add(2, 2);
  FExcluirColumnIndex.Add(3, 3);
  FExcluirColumnIndex.Add(4, 4);
  FExcluirColumnIndex.Add(5, 5);
  FIndicator.TryGetValue((dgIndicator in dbgCEP.Options), lIndicator);
  lIndicator;
end;

procedure TFormRegisteredZIPCode.DoAddSixSeven;
begin
  FExcluirColumnIndex.Add(6, 5);
  FExcluirColumnIndex.Add(7, 6);
end;

procedure TFormRegisteredZIPCode.DoAddSix;
begin
  FExcluirColumnIndex.Add(6, 6);
end;

procedure TFormRegisteredZIPCode.InitiIndicator;
begin
  FIndicator := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FIndicator.Add(True, DoAddSixSeven);
  FIndicator.Add(False, DoAddSix);
end;

procedure TFormRegisteredZIPCode.Delete;
begin
  TControllerCEP.New
                  .Codigo(FClientDataSet.FieldByName('CODIGO').AsInteger)
                  .Delete
                  .ClientDataSet(FClientDataSet)
                  .GetAll;
end;

procedure TFormRegisteredZIPCode.InitirExcluirClick;
begin
  FExcluirClick := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  FExcluirClick.Add(True, Delete);
  FExcluirClick.Add(False, DoNothing);
end;

initialization
  System.Classes.RegisterClass(TFormRegisteredZIPCode);

finalization
  System.Classes.UnRegisterClass(TFormRegisteredZIPCode);

end.
