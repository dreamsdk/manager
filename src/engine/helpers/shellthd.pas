unit ShellThd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCSDKMgr, PortMgr, Environ;

type
  TShellThreadOperation = (
    stoKallistiInstall,
    stoKallistiUpdate,
    stoPortInstall,
    stoPortUpdate,
    stoPortUninstall
  );

  TShellThreadContext = (
    stcUndefined,
    stcKallistiInstall,
    stcKallistiUpdate,
    stcSingleKallistiPort
  );

  TShellThreadCommandTerminateEvent = procedure(
    Operation: TShellThreadOperation;
    Success: Boolean;
    ResultOutput: string;
    KallistiPortUpdateState: TUpdateOperationState
  ) of object;

  { TShellThread }
  TShellThread = class(TThread)
  private
    fManager: TDreamcastSoftwareDevelopmentKitManager;
    fProgressText: string;
    fContext: TShellThreadContext;
    fOperationSuccessKallistiPortUpdate: TUpdateOperationState;
    fOperationSuccess: Boolean;
    fOperationResultOutput: string;
    fOperation: TShellThreadOperation;
    fCommandTerminate: TShellThreadCommandTerminateEvent;
    fSelectedKallistiPort: TKallistiPortItem;
    procedure SyncCloseProgressWindow;
    procedure SyncUpdateProgressText;
    procedure SyncTriggerCommandTerminate;
  protected
    function CanContinue: Boolean;
    procedure Execute; override;
    procedure UpdateProgressText(const Text: string);
    procedure TriggerCommandTerminate(OutputBuffer: string);
    procedure ProcessKallistiOS(var OutputBuffer: string);
  public
    constructor Create(CreateSuspended: Boolean);

    property Context: TShellThreadContext read fContext write fContext;
    property Operation: TShellThreadOperation read fOperation write fOperation;
    property SelectedKallistiPort: TKallistiPortItem
      read fSelectedKallistiPort write fSelectedKallistiPort;
    property Manager: TDreamcastSoftwareDevelopmentKitManager
      read fManager write fManager;

    property OnCommandTerminate: TShellThreadCommandTerminateEvent
      read fCommandTerminate write fCommandTerminate;
  end;

procedure AbortThreadOperation;
procedure ExecuteThreadOperation(const AOperation: TShellThreadOperation);

implementation

uses
  Forms, Main, Progress;

type
  { TShellThreadHelper }
  TShellThreadHelper = class(TObject)
  public
    procedure HandleTerminate(Sender: TObject);
  end;

var
  ShellThread: TShellThread;
  ShellThreadHelper: TShellThreadHelper;

function GetThreadContext(const AOperation: TShellThreadOperation): TShellThreadContext;
var
  ValidKallistiPortContext,
  IsSingleKallistiPortOperation: Boolean;

begin
  Result := stcUndefined;

  IsSingleKallistiPortOperation := (AOperation = stoPortInstall)
    or (AOperation = stoPortUninstall) or (AOperation = stoPortUpdate);

  ValidKallistiPortContext := Assigned(frmMain.SelectedKallistiPort)
    and IsSingleKallistiPortOperation;

  if ValidKallistiPortContext then
    Result := stcSingleKallistiPort
  else if (AOperation = stoKallistiInstall) then
    Result := stcKallistiInstall
  else if (AOperation = stoKallistiUpdate) then
    Result := stcKallistiUpdate;
end;

procedure ExecuteThreadOperation(const AOperation: TShellThreadOperation);
var
  ShellThreadContext: TShellThreadContext;
  OperationTitle, KallistiPortText: string;

begin
  ShellThreadContext := GetThreadContext(AOperation);

  if ShellThreadContext <> stcUndefined then
  begin
    ShellThread := TShellThread.Create(True);
    with ShellThread do
    begin
      Manager := DreamcastSoftwareDevelopmentKitManager;
      SelectedKallistiPort := frmMain.SelectedKallistiPort;
      Context := ShellThreadContext;
      Operation := AOperation;
      OnCommandTerminate := @frmMain.OnCommandTerminateThread;
      OnTerminate := @ShellThreadHelper.HandleTerminate;
      Start;
    end;

    case ShellThreadContext of

      stcSingleKallistiPort:
        begin
          KallistiPortText := Format('kos-port %s %s...', [
            frmMain.SelectedKallistiPort.Name, frmMain.SelectedKallistiPort.Version]);
          case AOperation of
            stoPortInstall:
              OperationTitle := Format('Installing %s', [KallistiPortText]);
            stoPortUpdate:
              OperationTitle := Format('Updating %s', [KallistiPortText]);
            stoPortUninstall:
              OperationTitle := Format('Uninstalling %s', [KallistiPortText]);
          end;
        end;

      stcKallistiInstall:
        OperationTitle := 'Installing KallistiOS...';
    end;

    with frmProgress do
    begin
      Caption := OperationTitle;
      Finished := False;
      ShowModal;
    end;
  end;
end;

procedure AbortThreadOperation;
begin
  ShellThread.Manager.Environment.AbortShellCommand;
  Application.ProcessMessages;
  ShellThread.Terminate;
end;

{ TShellThreadHelper }

procedure TShellThreadHelper.HandleTerminate(Sender: TObject);
begin
  ShellThread := nil;
end;

{ TShellThread }

constructor TShellThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TShellThread.SyncCloseProgressWindow;
begin
  frmProgress.Finished := True;
  frmProgress.Close;
  Application.ProcessMessages;
end;

procedure TShellThread.SyncUpdateProgressText;
begin
  frmProgress.Caption := fProgressText;
end;

procedure TShellThread.SyncTriggerCommandTerminate;
begin
  if Assigned(fCommandTerminate) then
    fCommandTerminate(fOperation, fOperationSuccess, fOperationResultOutput,
      fOperationSuccessKallistiPortUpdate);
end;

function TShellThread.CanContinue: Boolean;
begin
  Result := fOperationSuccess and not Terminated;
end;

procedure TShellThread.Execute;
var
  Buffer: string;

begin
  fOperationSuccess := False;
  Buffer := '';

  case Operation of
    stoKallistiInstall:
      ProcessKallistiOS(Buffer);
  end;

  // Single KallistiPort Management
  if Assigned(SelectedKallistiPort) then
    case Operation of
      stoPortInstall:
         fOperationSuccess := SelectedKallistiPort.Install(Buffer);
      stoPortUninstall:
         fOperationSuccess := SelectedKallistiPort.Uninstall(Buffer);
      stoPortUpdate:
         fOperationSuccessKallistiPortUpdate := SelectedKallistiPort.Update(Buffer);
    end;

  TriggerCommandTerminate(Buffer);
end;

procedure TShellThread.UpdateProgressText(const Text: string);
begin
  fProgressText := Text;
  Synchronize(@SyncUpdateProgressText);
end;

procedure TShellThread.TriggerCommandTerminate(OutputBuffer: string);
begin
  Synchronize(@SyncCloseProgressWindow);

  fOperationResultOutput := OutputBuffer;
  Synchronize(@SyncTriggerCommandTerminate);
end;

procedure TShellThread.ProcessKallistiOS(var OutputBuffer: string);
begin
  fOperationSuccess := True;

  // Handle KallistiOS Repository
  if CanContinue and (not Manager.KallistiOS.Installed) then
  begin
    UpdateProgressText('Cloning KallistiOS Repository...');
    fOperationSuccess := fOperationSuccess
      and Manager.KallistiOS.CloneRepository(OutputBuffer);
  end
  else
  begin
    UpdateProgressText('Updating KallistiOS Repository...');
    fOperationSuccess := fOperationSuccess
      and (Manager.KallistiOS.UpdateRepository(OutputBuffer) <> uosUpdateFailed);
  end;

  // Handle KallistiPorts Repository
  if CanContinue and (not Manager.KallistiPorts.Installed) then
  begin
    UpdateProgressText('Cloning KallistiOS Ports Repository...');
    fOperationSuccess := fOperationSuccess
      and Manager.KallistiPorts.CloneRepository(OutputBuffer);
  end
  else
  begin
    UpdateProgressText('Updating KallistiOS Ports Repository...');
    fOperationSuccess := fOperationSuccess
      and (Manager.KallistiOS.UpdateRepository(OutputBuffer) <> uosUpdateFailed);
  end;

  // Generate environ.sh file
  if CanContinue then
  begin
    UpdateProgressText('Generating Environment Shell Script...');
    fOperationSuccess := fOperationSuccess
      and Manager.KallistiOS.InitializeEnvironShellScript;
  end;

  // Making KallistiOS library
  if CanContinue then
  begin
    UpdateProgressText('Building KallistiOS Library...');
    fOperationSuccess := fOperationSuccess
      and Manager.KallistiOS.BuildKallistiOS(OutputBuffer);
  end;

  // Fixing-up SH-4 Newlib...

end;

initialization
  ShellThreadHelper := TShellThreadHelper.Create;

finalization
  ShellThreadHelper.Free;

end.

