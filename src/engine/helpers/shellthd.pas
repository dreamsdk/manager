unit ShellThd;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DCSDKMgr,
  PortMgr,
  Environ,
  PkgMgr,
  KOSInst;

type
  TShellThreadInputRequest = (
    stiKallistiManage,
    stiKallistiPortsInstall,
    stiKallistiPortsUninstall,
    stiKallistiSinglePortInstall,
    stiKallistiSinglePortUpdate,
    stiKallistiSinglePortUninstall
  );

  TShellThreadOutputResponse = (
    stoNothing,
    stoKallistiInstall,
    stoKallistiUpdate,
    stoKallistiPortsInstall,
    stoKallistiPortsUninstall,
    stoKallistiSinglePortInstall,
    stoKallistiSinglePortUpdate,
    stoKallistiSinglePortUninstall
  );

  TShellThreadContext = (
    stcUndefined,
    stcKallisti,
    stcKallistiSinglePort,
    stcKallistiPorts
  );

  TShellThreadCommandTerminateEvent = procedure(
    Sender: TObject;
    Request: TShellThreadInputRequest;
    Response: TShellThreadOutputResponse;
    Success: Boolean;
    UpdateState: TUpdateOperationState
  ) of object;

  { TShellThread }
  TShellThread = class(TThread)
  private
    fAborted: Boolean;
    fManager: TDreamcastSoftwareDevelopmentKitManager;
    fPaused: Boolean;
    fProgressText: string;
    fContext: TShellThreadContext;
    fRequest: TShellThreadInputRequest;
    fResponse: TShellThreadOutputResponse;
    fOperationUpdateState: TUpdateOperationState;
    fOperationSuccess: Boolean;
    fOperationResultOutput: string;
    fCommandTerminate: TShellThreadCommandTerminateEvent;
    fSelectedKallistiPort: TKallistiPortItem;
    fInstaller: TKallistiInstaller;
    function GetRunning: Boolean;
    procedure HandleInstallerProgressText(Sender: TObject; const Text: string);
    procedure SetAborted(const Value: Boolean);
    procedure SetInstaller(const Value: TKallistiInstaller);
    procedure SyncSetProgressTerminateState;
    procedure SyncUpdateProgressText;
    procedure SyncTriggerCommandTerminate;
  protected
    function TaskSuccess: Boolean;
    function CanContinue: Boolean;
    procedure SetProgressTerminateState;
    procedure Execute; override;
    procedure UpdateProgressText(const Text: string);
    procedure TriggerCommandTerminate(OutputBuffer: string);
    function ProcessKallistiOS: string;
    function ProcessKallistiPortSingle: string;
    function ProcessKallistiPorts: string;
    procedure SetOperationSuccess(const Success: Boolean);
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property Aborted: Boolean read fAborted write SetAborted;
    property Context: TShellThreadContext read fContext write fContext;
    property Installer: TKallistiInstaller read fInstaller write SetInstaller;
    property Operation: TShellThreadInputRequest read fRequest write fRequest;
    property SelectedKallistiPort: TKallistiPortItem
      read fSelectedKallistiPort write fSelectedKallistiPort;
    property Manager: TDreamcastSoftwareDevelopmentKitManager
      read fManager write fManager;
    property Paused: Boolean read fPaused;
    property Running: Boolean read GetRunning;

    property OnCommandTerminate: TShellThreadCommandTerminateEvent
      read fCommandTerminate write fCommandTerminate;
  end;

procedure AbortThreadOperation;
procedure PauseThreadOperation;
procedure ResumeThreadOperation;
procedure ExecuteThreadOperation(const AOperation: TShellThreadInputRequest);

(* Wait for the threads that are still finishing (e.g., the application is
   closed right after an operation), while executing their pending Synchronize
   calls; if not, they couldn't free themselves. Call this before freeing the
   global objects. *)
procedure WaitForThreadOperations;

implementation

uses
  Forms,
  Progress,
  StrRes,
  InstMode,
  SysTools,
  Version,
  Global,
  Main;

const
  ABORT_SHELL_THREAD_INSTANCES_COUNT_MAX = 3;
  ABORT_SHELL_THREAD_MAX_ABORT_CALLS = 5;
  WAIT_THREAD_OPERATIONS_TIMEOUT = 10000;
  WAIT_THREAD_OPERATIONS_STEP = 100;

type
  { TShellThreadHelper }
  TShellThreadHelper = class(TObject)
  private
    fAbortOrderReceived: Boolean;
    procedure AddNewLine(const NewLine: string);
  public
    procedure HandleTerminate(Sender: TObject);
    procedure HandleNewLine(Sender: TObject; NewLine: string);
    procedure HandleInstallerNewLine(Sender: TObject; const Text: string);
    procedure HandleInstallerUnpackTerminate(Sender: TObject;
      const Success: Boolean; const Aborted: Boolean);
  end;

  { TAbortShellThread }
  TAbortShellThread = class(TThread)
  private
    procedure SyncAbort;
  protected
    procedure Abort;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
  end;

var
  AbortShellThreadInstancesCount: Integer = 0;
  AliveThreadsCount: LongInt = 0; // TShellThread and TAbortShellThread instances
  ShellThread: TShellThread;
  ShellThreadHelper: TShellThreadHelper;

procedure WaitForThreadOperations;
var
  LogContext: TLogMessageContext;
  Elapsed: Integer;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    Elapsed := 0;
    while (AliveThreadsCount > 0) and (Elapsed < WAIT_THREAD_OPERATIONS_TIMEOUT) do
    begin
      CheckSynchronize(WAIT_THREAD_OPERATIONS_STEP);
      Inc(Elapsed, WAIT_THREAD_OPERATIONS_STEP);
    end;

    LogMessage(LogContext, Format('AliveThreadsCount: %d, Elapsed: %d', [
      AliveThreadsCount,
      Elapsed
    ]));
  finally
    LogMessageExit(LogContext);
  end;
end;

function GetShellThreadPackageManager: TPackageManager;
begin
  Result := nil;
  if Assigned(ShellThread) and Assigned(ShellThread.Installer) then
    Result := ShellThread.Installer.PackageManager;
end;

function IsThreadOperationAborted: Boolean;
begin
  // Test if toolchains thread has been aborted
  Result := Assigned(ShellThreadHelper)
    and ShellThreadHelper.fAbortOrderReceived;

  // Test if ShellThread (regular) thread has been aborted
  Result := Result
    or (Assigned(ShellThread) and (ShellThread.Running)
    and (ShellThread.Aborted));
end;

procedure AbortThreadOperation;
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

      if AbortShellThreadInstancesCount < ABORT_SHELL_THREAD_INSTANCES_COUNT_MAX then
      begin
        LogMessage(LogContext, Format('Running new TAbortShellThread (%d of %d)', [
          AbortShellThreadInstancesCount + 1,
          ABORT_SHELL_THREAD_INSTANCES_COUNT_MAX
        ]));
        TAbortShellThread.Create(False);
        Inc(AbortShellThreadInstancesCount);
      end
      else
        LogMessage(LogContext, 'Max concurrent instances reached.');

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure DoAbortThreadOperation;
var
  LogContext: TLogMessageContext;
  ShellThreadPackageManager: TPackageManager;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    if not IsThreadOperationAborted then
    begin
      ResumeThreadOperation;
      Sleep(500);

      // Toolchains Thread
      ShellThreadPackageManager := GetShellThreadPackageManager;
      if Assigned(ShellThreadPackageManager) and ShellThreadPackageManager.Running then
      begin
        LogMessage(LogContext, 'ShellThreadPackageManager::Abort');
        ShellThreadPackageManager.Abort;
      end;

      // KOS, KOS Ports and Dreamcast Tool thread
      if Assigned(ShellThread) and ShellThread.Running then
      begin
        LogMessage(LogContext, 'ShellThread.Manager.Environment.AbortShellCommand');
        ShellThread.Manager.Environment.AbortShellCommand;
        if (not ShellThread.Aborted) then
        begin
          LogMessage(LogContext, 'ShellThread.Aborted');
          ShellThread.Aborted := True;
          Application.ProcessMessages;
          if Assigned(ShellThread) then
          begin
            LogMessage(LogContext, 'ShellThread.Terminate');
            ShellThread.Terminate;
          end;
        end;
      end;
    end
    else
      LogMessage(LogContext, 'DoAbortThreadOperation::ShellThreadHelper.fAbortOrderReceived is TRUE');

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure PauseThreadOperation;
var
  LogContext: TLogMessageContext;
  ShellThreadPackageManager: TPackageManager;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    if Assigned(ShellThread) and (not ShellThread.Paused) then
    begin
      ShellThread.fPaused := True;
      ShellThread.Manager.Environment.PauseShellCommand;
      ShellThreadPackageManager := GetShellThreadPackageManager;
      if Assigned(ShellThreadPackageManager) and ShellThreadPackageManager.Running then
        ShellThreadPackageManager.Pause;
      Application.ProcessMessages;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure ResumeThreadOperation;
var
  LogContext: TLogMessageContext;
  ShellThreadPackageManager: TPackageManager;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    if Assigned(ShellThread) and (ShellThread.Paused) then
    begin
      LogMessage(LogContext, 'ResumeThreadOperation::ShellThread.Paused');
      ShellThread.fPaused := False;
      ShellThread.Manager.Environment.ResumeShellCommand;
      ShellThreadPackageManager := GetShellThreadPackageManager;
      if Assigned(ShellThreadPackageManager) and ShellThreadPackageManager.Running then
      begin
        LogMessage(LogContext, 'ResumeThreadOperation::ShellThread::ShellThreadPackageManager.Resume');
        ShellThreadPackageManager.Resume;
      end;
      Application.ProcessMessages;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure InitializeNewLineHandler;
begin
  if not Assigned(DreamcastSoftwareDevelopmentKitManager.Environment.OnShellCommandNewLine) then
    DreamcastSoftwareDevelopmentKitManager.Environment.OnShellCommandNewLine :=
      @ShellThreadHelper.HandleNewLine;
end;

function GetThreadContext(const AOperation: TShellThreadInputRequest): TShellThreadContext;
var
  ValidKallistiPortContext,
  IsSingleKallistiPortOperation,
  IsKallistiPortsOperation: Boolean;

begin
  Result := stcUndefined;

  IsSingleKallistiPortOperation := (AOperation = stiKallistiSinglePortInstall)
    or (AOperation = stiKallistiSinglePortUninstall) or (AOperation = stiKallistiSinglePortUpdate);

  ValidKallistiPortContext := Assigned(frmMain.SelectedKallistiPort)
    and IsSingleKallistiPortOperation;

  IsKallistiPortsOperation := (AOperation = stiKallistiPortsUninstall)
    or (AOperation = stiKallistiPortsInstall);

  Result := stcKallisti;
  if ValidKallistiPortContext then
    Result := stcKallistiSinglePort
  else if IsKallistiPortsOperation then
    Result := stcKallistiPorts;
end;

procedure ExecuteThreadOperation(const AOperation: TShellThreadInputRequest);
var
  LogContext: TLogMessageContext;
  ShellThreadContext: TShellThreadContext;

  procedure StartupThread;
  begin
    // Restart new Abort cycle
    AbortShellThreadInstancesCount := 0;
    ShellThreadHelper.fAbortOrderReceived := False;

    // Normal thread dealing with KOS, KOS Ports and DC Tool
    ShellThread := TShellThread.Create(True);
    with ShellThread do
    begin
{$IFDEF DEBUG}
      SetThreadName('TShellThread', ShellThread.Handle);
{$ENDIF}
      Aborted := False;
      Manager := DreamcastSoftwareDevelopmentKitManager;
      // This will be used for installing toolchains/debugger after installing
      // DreamSDK (mainly) and for handling KallistiOS, KallistiOS Ports and DC Tool
      Installer := TKallistiInstaller.Create(DreamcastSoftwareDevelopmentKitManager);
      SelectedKallistiPort := frmMain.SelectedKallistiPort;
      Context := ShellThreadContext;
      Operation := AOperation;
      OnCommandTerminate := @frmMain.OnCommandTerminateThread;
      OnTerminate := @ShellThreadHelper.HandleTerminate;
      Start;
    end;
  end;

  procedure ShowProgressWindow;
  var
    OperationTitle,
    KallistiPortText: string;

  begin
    case ShellThreadContext of
      stcKallistiSinglePort:
        begin
          KallistiPortText := Format(KallistiPortOperationText, [
            frmMain.SelectedKallistiPort.Name, frmMain.SelectedKallistiPort.Version]);
          case AOperation of
            stiKallistiSinglePortInstall:
              OperationTitle := Format(KallistiPortOperationInstallText, [KallistiPortText]);
            stiKallistiSinglePortUpdate:
              OperationTitle := Format(KallistiPortOperationUpdateText, [KallistiPortText]);
            stiKallistiSinglePortUninstall:
              OperationTitle := Format(KallistiPortOperationUninstallText, [KallistiPortText]);
          end;
        end;

      stcKallisti:
        begin
          OperationTitle := KallistiOperationText;
          if IsPostInstallMode then
            OperationTitle := Format(PostInstallOperationText, [GetProductName])
        end;

      stcKallistiPorts:
        OperationTitle := KallistiPortsOperationText;
    end;

    frmProgress := TfrmProgress.Create(Application);
    with frmProgress do
      try
        Caption := OperationTitle;
        ShowModal;
      finally
        Free;
        frmProgress := nil;
      end;
  end;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    DoAbortThreadOperation;

    ShellThreadContext := GetThreadContext(AOperation);
    if ShellThreadContext <> stcUndefined then
    begin
      LogMessage(LogContext, 'ShellThread initialization');
      InitializeNewLineHandler;
      StartupThread;
      ShowProgressWindow;
      LogMessage(LogContext, 'ShellThread started');
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

{ TAbortShellThread }

procedure TAbortShellThread.SyncAbort;
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    DoAbortThreadOperation;
    Sleep(500);

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TAbortShellThread.Abort;
begin
  Synchronize(@SyncAbort);
end;

procedure TAbortShellThread.Execute;
var
  LogContext: TLogMessageContext;
  i: Integer;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

      LogMessage(LogContext, Format('ThreadId: %d', [
        ThreadID
      ]));

      i := 0;
      while (i < ABORT_SHELL_THREAD_MAX_ABORT_CALLS)
        and Assigned(ShellThread)
        and (not ShellThread.CheckTerminated)
        and (not ShellThreadHelper.fAbortOrderReceived) do
      begin
        LogMessage(LogContext, Format('Abort called (%d of %d)', [
          i + 1,
          ABORT_SHELL_THREAD_MAX_ABORT_CALLS
        ]));

        Abort;

        Inc(i);
        Sleep(1000);
      end;

  finally
    LogMessageExit(LogContext);
  end;
end;

constructor TAbortShellThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  InterLockedIncrement(AliveThreadsCount);
end;

destructor TAbortShellThread.Destroy;
begin
  inherited Destroy;
  InterLockedDecrement(AliveThreadsCount);
end;

{ TShellThreadHelper }

procedure TShellThreadHelper.AddNewLine(const NewLine: string);
begin
  frmProgress.AddNewLine(NewLine);
end;

procedure TShellThreadHelper.HandleTerminate(Sender: TObject);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    ShellThread := nil; // FreeOnTerminate (Installer is freed by the ShellThread)

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TShellThreadHelper.HandleNewLine(Sender: TObject; NewLine: string);
begin
  AddNewLine(NewLine);
end;

procedure TShellThreadHelper.HandleInstallerNewLine(Sender: TObject;
  const Text: string);
begin
  AddNewLine(Text);
end;

procedure TShellThreadHelper.HandleInstallerUnpackTerminate(Sender: TObject;
  const Success: Boolean; const Aborted: Boolean);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    LogMessage(LogContext, Format('Success=%s, Aborted=%s', [
      BoolToStr(Success, True),
      BoolToStr(Aborted, True)
    ]));

    // Success status is merged by the Installer itself (see TShellThread.Execute)

    // Process cancelled while unpacking toolchains...
    if Aborted then
    begin
      LogMessage(LogContext, 'AbortThreadOperation called');
      ShellThreadHelper.fAbortOrderReceived := True; // Indicates that we are in Abort cycle
      AbortThreadOperation;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

{ TShellThread }

constructor TShellThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  fInstaller := nil;
  InterLockedIncrement(AliveThreadsCount);
end;

destructor TShellThread.Destroy;
begin
  FreeAndNil(fInstaller);
  inherited Destroy;
  InterLockedDecrement(AliveThreadsCount);
end;

procedure TShellThread.HandleInstallerProgressText(Sender: TObject;
  const Text: string);
begin
  UpdateProgressText(Text);
end;

procedure TShellThread.SetAborted(const Value: Boolean);
begin
  fAborted := Value;
  if Assigned(fInstaller) then
    fInstaller.Aborted := Value;
end;

procedure TShellThread.SetInstaller(const Value: TKallistiInstaller);
begin
  fInstaller := Value;
  if Assigned(fInstaller) then
  begin
    fInstaller.Aborted := fAborted;
    fInstaller.OnProgressText := @HandleInstallerProgressText;
    fInstaller.OnNewLine := @ShellThreadHelper.HandleInstallerNewLine;
    fInstaller.OnUnpackTerminate := @ShellThreadHelper.HandleInstallerUnpackTerminate;
  end;
end;

procedure TShellThread.SyncSetProgressTerminateState;
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    frmProgress.Finished := True;
    frmProgress.SetTerminateState(fOperationSuccess, Aborted);
    Application.ProcessMessages;
  finally
    LogMessageExit(LogContext);
  end;
end;

function TShellThread.GetRunning: Boolean;
begin
  Result := (not Finished);
end;

procedure TShellThread.SyncUpdateProgressText;
begin
  frmProgress.SetProgressText(fProgressText);
  Application.ProcessMessages;
end;

procedure TShellThread.SyncTriggerCommandTerminate;
begin
  SetProgressTerminateState;
  Application.ProcessMessages;
  if Assigned(fCommandTerminate) then
    fCommandTerminate(Self, fRequest, fResponse, fOperationSuccess, fOperationUpdateState);
end;

function TShellThread.TaskSuccess: Boolean;
begin
  Result := fOperationSuccess and (not Terminated) and (not Aborted);
end;

function TShellThread.CanContinue: Boolean;
begin
  Result := (not IsPostInstallMode and TaskSuccess) or
    (IsPostInstallMode and (not Terminated) and (not Aborted));
end;

procedure TShellThread.SetProgressTerminateState;
begin
  Synchronize(@SyncSetProgressTerminateState);
end;

procedure TShellThread.Execute;
var
  LogContext: TLogMessageContext;
  Buffer: string;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    LogMessage(LogContext, Format('ThreadId: %d', [
      ThreadID
    ]));

    fOperationSuccess := True;
    fResponse := stoNothing;
    fOperationUpdateState := uosUndefined;

    // Execute the shell once if post-install and unpack toolchains (if required)
    Installer.Prepare;
    fOperationSuccess := fOperationSuccess and Installer.Success;

    // Process KOS, KOS Ports setup or KOS single port
    if Installer.ShellAvailable then
      case fContext of
        stcKallisti:
          Buffer := ProcessKallistiOS;
        stcKallistiSinglePort:
          Buffer := ProcessKallistiPortSingle;
        stcKallistiPorts:
          Buffer := ProcessKallistiPorts;
      end
    else
    begin
      // This should never happens...
      fOperationSuccess := False;
      fOperationResultOutput := Format(InstallationProblem, [
        Manager.Environment.Settings.InstallPath,
        Manager.Environment.Settings.FileName
      ]);
      UpdateProgressText(fOperationResultOutput);
    end;

    // Finish
    TriggerCommandTerminate(Buffer);

  finally
    LogMessageExit(LogContext);
  end;
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

function TShellThread.ProcessKallistiOS: string;
begin
  // Install/Update KallistiOS, KallistiOS Ports and Dreamcast Tool
  Installer.ProcessKallisti;
  SetOperationSuccess(Installer.Success);

  case Installer.Response of
    kirInstall:
      fResponse := stoKallistiInstall;
    kirUpdate:
      fResponse := stoKallistiUpdate;
  end;
  fOperationUpdateState := Installer.UpdateState;

  Result := Installer.ResultOutput;
end;

function TShellThread.ProcessKallistiPortSingle: string;

  function RequestToResponse(Request: TShellThreadInputRequest): TShellThreadOutputResponse;
  begin
    Result := stoNothing;
    case Request of
      stiKallistiSinglePortInstall:
        Result := stoKallistiSinglePortInstall;
      stiKallistiSinglePortUpdate:
        Result := stoKallistiSinglePortUpdate;
      stiKallistiSinglePortUninstall:
        Result := stoKallistiSinglePortUninstall;
    end;
  end;

begin
  Result := '';
  fResponse := RequestToResponse(fRequest);

  case Operation of
    stiKallistiSinglePortInstall:
      begin
        UpdateProgressText(KallistiPortInstallText);
        SetOperationSuccess(SelectedKallistiPort.Install(Result));
      end;

    stiKallistiSinglePortUninstall:
      begin
        UpdateProgressText(KallistiPortUninstallText);
        SetOperationSuccess(SelectedKallistiPort.Uninstall(Result));
      end;

    stiKallistiSinglePortUpdate:
      begin
        UpdateProgressText(KallistiPortUpdateText);
        fOperationUpdateState := SelectedKallistiPort.Update(Result);
        SetOperationSuccess(fOperationUpdateState <> uosUpdateFailed);
      end;
  end;

end;

function TShellThread.ProcessKallistiPorts: string;
begin
  Result := '';

  case Operation of

    stiKallistiPortsInstall:
      begin
        UpdateProgressText(KallistiPortsInstallText);
        fResponse := stoKallistiPortsInstall;
        SetOperationSuccess(Manager.KallistiPorts.Install(Result));
      end;

    stiKallistiPortsUninstall:
      begin
        UpdateProgressText(KallistiPortsUninstallText);
        fResponse := stoKallistiPortsUninstall;
        SetOperationSuccess(Manager.KallistiPorts.Uninstall(Result));
      end;

  end;
end;

procedure TShellThread.SetOperationSuccess(const Success: Boolean);
begin
  fOperationSuccess := TaskSuccess and Success;
end;

initialization
  ShellThreadHelper := TShellThreadHelper.Create;

finalization
  ShellThreadHelper.Free;

end.

