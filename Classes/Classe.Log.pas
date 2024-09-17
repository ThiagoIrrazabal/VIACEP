unit Classe.Log;

interface

uses
  System.SysUtils, Vcl.Forms, System.Generics.Collections, Classe.SmartPointer;

type
  ILog = interface(IInterface)
    ['{4BEC02B1-6864-4C62-BE79-2F857079E365}']
    function FormName(const AValue: string): ILog; overload;
    function FormName: string; overload;
    function FormTitle(const AValue: string): ILog; overload;
    function FormTitle: string; overload;
    function Exception(const AValue: string): ILog; overload;
    function Exception: string; overload;
    procedure Execute;
  end;

  TLog = class(TInterfacedObject, ILog)
  strict private
  const
    FileName: string = 'Exceptions.txt';
    Path: string = 'Log\';
  var
    FLog: TextFile;
    FFormName: string;
    FFormTitle: string;
    FException: string;
  private
    procedure OpenFile;
    procedure MakeOpen;
    procedure MakeDirectory;
    procedure DoNothing;
  public
    class function New: ILog;
    constructor Create; overload;
    function FormName(const AValue: string): ILog; overload;
    function FormName: string; overload;
    function FormTitle(const AValue: string): ILog; overload;
    function FormTitle: string; overload;
    function Exception(const AValue: string): ILog; overload;
    function Exception: string; overload;
    procedure Execute;
  end;

implementation

{ TLog }

constructor TLog.Create;
var
  lExistsDirectory: ISmartPointer<TDictionary<Boolean, TProc>>;
  lMakeDirectory: TProc;
begin
  lExistsDirectory := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  lExistsDirectory.Add(True, DoNothing);
  lExistsDirectory.Add(False, MakeDirectory);
  lExistsDirectory.TryGetValue(DirectoryExists(ExtractFilePath(ParamStr(0)) + Path), lMakeDirectory);
  lMakeDirectory;
  OpenFile;
end;

class function TLog.New: ILog;
begin
  Result := Self.Create;
end;

procedure TLog.OpenFile;
var
  lExistsFile: ISmartPointer<TDictionary<Boolean, TProc>>;
  lMakeOpen: TProc;
  lFilePath: string;
begin
  lFilePath := ExtractFilePath(ParamStr(0)) + Path + FileName;
  AssignFile(FLog, lFilePath);
  lExistsFile := TSmartPointer<TDictionary<Boolean, TProc>>.Create(nil);
  lExistsFile.Add(True, DoNothing);
  lExistsFile.Add(False, MakeOpen);
  lExistsFile.TryGetValue(FileExists(lFilePath), lMakeOpen);
  lMakeOpen;
end;

procedure TLog.DoNothing;
begin

end;

function TLog.Exception(const AValue: string): ILog;
begin
  Result := Self;
  FException := AValue;
end;

function TLog.Exception: string;
begin
  Result := FException;
end;

procedure TLog.Execute;
begin
  Append(FLog);
  WriteLn(FLog, 'Date time........: ' + FormatDateTime('[dd/mm/yyyy hh:nn:ss]', Now));
  WriteLn(FLog, 'FormName.........: ' + FormName);
  WriteLn(FLog, 'FormTitle........: ' + FormTitle);
  WriteLn(FLog, 'Exception........: ' + Exception);
  WriteLn(FLog, EmptyStr);
  CloseFile(FLog);
end;

function TLog.FormName: string;
begin
  Result := FFormName;
end;

function TLog.FormName(const AValue: string): ILog;
begin
  Result := Self;
  FFormName := AValue;
end;

function TLog.FormTitle(const AValue: string): ILog;
begin
  Result := Self;
  FFormTitle := AValue;
end;

function TLog.FormTitle: string;
begin
  Result := FFormTitle;
end;

procedure TLog.MakeDirectory;
begin
  ForceDirectories(ExtractFilePath(ParamStr(0)) + Path);
end;

procedure TLog.MakeOpen;
begin
  Rewrite(FLog);
  CloseFile(FLog);
end;

end.
