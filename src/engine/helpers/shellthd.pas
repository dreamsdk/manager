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
    fAborted: Boolean;
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
    procedure SyncSetErrorProgressWindow;
    procedure SyncUpdateProgressText;
    procedure SyncTriggerCommandTerminate;
  protected
    function CanContinue: Boolean;
    procedure CloseProgressWindow;
    procedure SetErrorProgressWindow;
    procedure Execute; override;
    procedure UpdateProgressText(const Text: string);
    procedure TriggerCommandTerminate(OutputBuffer: string);
    procedure ProcessKallistiOS(var OutputBuffer: string);
    procedure SetOperationSuccess(const Success: Boolean);
  public
    constructor Create(CreateSuspended: Boolean);

    property Aborted: Boolean read fAborted write fAborted;
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
procedure PauseThreadOperation;
procedure ResumeThreadOperation;
procedure ExecuteThreadOperation(const AOperation: TShellThreadOperation);

implementation

uses
  Forms, Main, Progress;

type
  { TShellThreadHelper }
  TShellThreadHelper = class(TObject)
  public
    procedure HandleTerminate(Sender: TObject);
    procedure HandleNewLine(Sender: TObject; NewLine: string);
  end;

var
  ShellThread: TShellThread;
  ShellThreadHelper: TShellThreadHelper;

procedure PauseThreadOperation;
begin
  if Assigned(ShellThread) then
  begin
    ShellThread.Manager.Environment.PauseShellCommand;
    Application.ProcessMessages;
  end;
end;

procedure ResumeThreadOperation;
begin
  if Assigned(ShellThread) then
  begin
    ShellThread.Manager.Environment.ResumeShellCommand;
    Application.ProcessMessages;
  end;
end;

procedure InitializeNewLineHandler;
begin
  if not Assigned(DreamcastSoftwareDevelopmentKitManager.Environment.OnShellCommandNewLine) then
    DreamcastSoftwareDevelopmentKitManager.Environment.OnShellCommandNewLine :=
      @ShellThreadHelper.HandleNewLine;
end;

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
    InitializeNewLineHandler;

    ShellThread := TShellThread.Create(True);
    with ShellThread do
    begin
      Aborted := False;
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
          KallistiPortText := Format('kos-port %s %s', [
            frmMain.SelectedKallistiPort.Name, frmMain.SelectedKallistiPort.Version]);
          case AOperation of
            stoPortInstall:
              OperationTitle := Format('Installation of %s', [KallistiPortText]);
            stoPortUpdate:
              OperationTitle := Format('Update of %s', [KallistiPortText]);
            stoPortUninstall:
              OperationTitle := Format('Uninstallation of %s', [KallistiPortText]);
          end;
        end;

      stcKallistiInstall:
        OperationTitle := 'Installation of KallistiOS';
    end;

    with frmProgress do
    begin
      Caption := OperationTitle;
      ShowModal;
    end;
  end;
end;

procedure AbortThreadOperation;
begin
  ResumeThreadOperation;
  if Assigned(ShellThread) then
  begin
    ShellThread.Aborted := True;
    ShellThread.Manager.Environment.AbortShellCommand;
    Application.ProcessMessages;
    ShellThread.Terminate;
  end;
end;

{ TShellThreadHelper }

procedure TShellThreadHelper.HandleTerminate(Sender: TObject);
begin
  ShellThread := nil;
end;

procedure TShellThreadHelper.HandleNewLine(Sender: TObject; NewLine: string);
begin
  frmProgress.AddNewLine(NewLine);
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

procedure TShellThread.SyncSetErrorProgressWindow;
begin
  frmProgress.Finished := True;
  frmProgress.SetTerminateErrorState(Aborted);
  Application.ProcessMessages;
end;

procedure TShellThread.SyncUpdateProgressText;
begin
  frmProgress.SetProgressText(fProgressText);
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

procedure TShellThread.CloseProgressWindow;
begin
  Synchronize(@SyncCloseProgressWindow);
end;

procedure TShellThread.SetErrorProgressWindow;
begin
  Synchronize(@SyncSetErrorProgressWindow);
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
        begin
          UpdateProgressText('Installing the KallistiOS Port...');
          fOperationSuccess := SelectedKallistiPort.Install(Buffer);
        end;
      stoPortUninstall:
        begin
          UpdateProgressText('Uninstalling the KallistiOS Port...');
          fOperationSuccess := SelectedKallistiPort.Uninstall(Buffer);
        end;
      stoPortUpdate:
        begin
          UpdateProgressText('Updating the KallistiOS Port...');
          fOperationSuccessKallistiPortUpdate := SelectedKallistiPort.Update(Buffer);
          fOperationSuccess := fOperationSuccessKallistiPortUpdate <> uosUpdateFailed;
        end;
    end;

  // Handle the closing of the window.
  if fOperationSuccess then
    CloseProgressWindow
  else
    SetErrorProgressWindow;

  // Finish
  TriggerCommandTerminate(Buffer);
end;

procedure TShellThread.UpdateProgressText(const Text: string);
begin
  fProgressText := Text;
  Synchronize(@SyncUpdateProgressText);
end;

procedure TShellThread.TriggerCommandTerminate(OutputBuffer: string);
begin
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
    SetOperationSuccess(Manager.KallistiOS.CloneRepository(OutputBuffer));
  end
  else
  begin
    UpdateProgressText('Updating KallistiOS Repository...');
    SetOperationSuccess(Manager.KallistiOS.UpdateRepository(OutputBuffer) <> uosUpdateFailed);
  end;

  // Handle KallistiPorts Repository
  if CanContinue and (not Manager.KallistiPorts.Installed) then
  begin
    UpdateProgressText('Cloning KallistiOS Ports Repository...');
    SetOperationSuccess(Manager.KallistiPorts.CloneRepository(OutputBuffer));
  end
  else
  begin
    UpdateProgressText('Updating KallistiOS Ports Repository...');
    SetOperationSuccess(Manager.KallistiOS.UpdateRepository(OutputBuffer) <> uosUpdateFailed);
  end;

  // Generate environ.sh file
  if CanContinue then
  begin
    UpdateProgressText('Generating Environment Shell Script...');
    SetOperationSuccess(Manager.KallistiOS.InitializeEnvironShellScript);
  end;

  // Making KallistiOS library
  if CanContinue then
  begin
    UpdateProgressText('Building KallistiOS Library...');
    SetOperationSuccess(Manager.KallistiOS.BuildKallistiOS(OutputBuffer));
  end;

  // Fixing-up SH-4 Newlib
  if CanContinue then
  begin
    UpdateProgressText('Fixing Hitachi SH-4 Newlib...');
    SetOperationSuccess(Manager.KallistiOS.FixupHitachiNewlib(OutputBuffer));
  end;
end;

procedure TShellThread.SetOperationSuccess(const Success: Boolean);
begin
  fOperationSuccess := CanContinue and Success;
end;

initialization
  ShellThreadHelper := TShellThreadHelper.Create;

finalization
  ShellThreadHelper.Free;

end.

