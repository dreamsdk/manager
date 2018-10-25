unit ShellThd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PortMgr;

type
  TShellThreadOperation = (stoPortInstall, stoPortUpdate, stoPortUninstall);

  TShellThreadCommandTerminateEvent = procedure(
    Operation: TShellThreadOperation;
    Success: Boolean;
    ResultOutput: string;
    KallistiPortUpdateState: TKallistiPortUpdateState
  ) of object;

  { TShellThread }
  TShellThread = class(TThread)
  private
    fOperationSuccessKallistiPortUpdate: TKallistiPortUpdateState;
    fOperationSuccess: Boolean;
    fOperationResultOutput: string;
    fOperation: TShellThreadOperation;
    fCommandTerminate: TShellThreadCommandTerminateEvent;
    fSelectedKallistiPort: TKallistiPortItem;
    procedure CloseProgressWindow;
    procedure TriggerCommandTerminate;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    property SelectedKallistiPort: TKallistiPortItem
      read fSelectedKallistiPort write fSelectedKallistiPort;
    property Operation: TShellThreadOperation read fOperation write fOperation;
    property OnCommandTerminate: TShellThreadCommandTerminateEvent
      read fCommandTerminate write fCommandTerminate;
  end;

procedure ExecuteThreadOperation(const AOperation: TShellThreadOperation);

implementation

uses
  Forms, Main, Progress;

procedure ExecuteThreadOperation(const AOperation: TShellThreadOperation);
var
  ShellThread: TShellThread;
  ValidContext: Boolean;
  OperationTitle, KallistiPortText: string;

begin
  ValidContext := Assigned(frmMain.SelectedKallistiPort)
    and (
         (AOperation = stoPortInstall)
      or (AOperation = stoPortUninstall)
      or (AOperation = stoPortUpdate)
    );

  if Assigned(frmMain.SelectedKallistiPort) then
  begin
    KallistiPortText := Format('kos-port %s %s...', [frmMain.SelectedKallistiPort.Name,
      frmMain.SelectedKallistiPort.Version]);
  end;

  if ValidContext then
  begin
    ShellThread := TShellThread.Create(True);
    with ShellThread do
    begin
      SelectedKallistiPort := frmMain.SelectedKallistiPort;
      Operation := AOperation;
      OnCommandTerminate := @frmMain.OnCommandTerminateThread;
      Start;
    end;

    with frmProgress do
    begin
      case AOperation of
        stoPortInstall:
          OperationTitle := Format('Installing %s', [KallistiPortText]);
        stoPortUpdate:
           OperationTitle := Format('Updating %s', [KallistiPortText]);
        stoPortUninstall:
          OperationTitle := Format('Uninstalling %s', [KallistiPortText]);
      end;
      Caption := OperationTitle;
      ShowModal;
    end;
  end;
end;

{ TShellThread }

constructor TShellThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TShellThread.CloseProgressWindow;
begin
  frmProgress.Close;
end;

procedure TShellThread.TriggerCommandTerminate;
begin
  if Assigned(fCommandTerminate) then
    fCommandTerminate(fOperation, fOperationSuccess, fOperationResultOutput,
      fOperationSuccessKallistiPortUpdate);
end;

procedure TShellThread.Execute;
var
  Buffer: string;

begin
  fOperationSuccess := False;
  Buffer := '';

  if Assigned(SelectedKallistiPort) then
    case Operation of
      stoPortInstall:
         fOperationSuccess := SelectedKallistiPort.Install(Buffer);
      stoPortUninstall:
         fOperationSuccess := SelectedKallistiPort.Uninstall(Buffer);
      stoPortUpdate:
         fOperationSuccessKallistiPortUpdate := SelectedKallistiPort.Update(Buffer);
    end;

  Synchronize(@CloseProgressWindow);
  Synchronize(@Application.ProcessMessages);

  fOperationResultOutput := Buffer;
  Synchronize(@TriggerCommandTerminate);
end;

end.

