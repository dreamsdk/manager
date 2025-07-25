unit ShellThd;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DCSDKMgr,
  PortMgr,
  Environ,
  PkgMgr;

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
    function GetRunning: Boolean;
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
    procedure ProcessUnpackToolchains;
    procedure SetOperationSuccess(const Success: Boolean);
  public
    constructor Create(CreateSuspended: Boolean);

    property Aborted: Boolean read fAborted write fAborted;
    property Context: TShellThreadContext read fContext write fContext;
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

implementation

uses
  TypInfo,
  Forms,
  FSTools,
  Progress,
  StrRes,
  PostInst,
  SysTools,
  Version,
  Global,
  Main;

const
  ABORT_SHELL_THREAD_INSTANCES_COUNT_MAX = 3;
  ABORT_SHELL_THREAD_MAX_ABORT_CALLS = 5;

type
  { TShellThreadHelper }
  TShellThreadHelper = class(TObject)
  private
    fAbortOrderReceived: Boolean;
    procedure AddNewLine(const NewLine: string);
  public
    procedure HandleTerminate(Sender: TObject);
    procedure HandleNewLine(Sender: TObject; NewLine: string);
    procedure HandlePackageManagerStart(Sender: TObject);
    procedure HandlePackagePickup(Sender: TObject;
      const SourceFileName, OutputDirectory: TFileName);
    procedure HandlePackageManagerProgress(Sender: TObject;
      const CurrentValue: Integer; const TotalValue: Integer);
    procedure HandlePackageManagerTerminate(Sender: TObject;
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
  end;

var
  AbortShellThreadInstancesCount: Integer = 0;
  ShellThread: TShellThread;
  ShellThreadHelper: TShellThreadHelper;
  ShellThreadPackageManager: TPackageManager;

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

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    if not IsThreadOperationAborted then
    begin
      ResumeThreadOperation;
      Sleep(500);

      // Toolchains Thread
      if Assigned(ShellThreadPackageManager) and ShellThreadPackageManager.Running then
      begin
        LogMessage(LogContext, 'ShellThreadPackageManager::Abort');
        ShellThreadPackageManager.Abort;
      end;

      // KOS, KOS Ports, Dreamcast Tool, and Ruby Thread
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

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    if Assigned(ShellThread) and (not ShellThread.Paused) then
    begin
      ShellThread.fPaused := True;
      ShellThread.Manager.Environment.PauseShellCommand;
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

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    if Assigned(ShellThread) and (ShellThread.Paused) then
    begin
      LogMessage(LogContext, 'ResumeThreadOperation::ShellThread.Paused');
      ShellThread.fPaused := False;
      ShellThread.Manager.Environment.ResumeShellCommand;
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
    // This will be used for installing toolchains/debugger after installing DreamSDK (mainly)
    ShellThreadPackageManager := TPackageManager.Create(
      DreamcastSoftwareDevelopmentKitManager, False);
    with ShellThreadPackageManager do
    begin
      EnableBusyWaitingOnExecution := True; // Active wait, as we are already on ShellThread
      Operation := pmrAutoDetectDebuggerToolchain; // Detect if DebuggerProfileKey/ToolchainProfileKey need to be replaced
      OnStart := @ShellThreadHelper.HandlePackageManagerStart;
      OnPackagePickup := @ShellThreadHelper.HandlePackagePickup;
      OnProgress := @ShellThreadHelper.HandlePackageManagerProgress;
      OnTerminate := @ShellThreadHelper.HandlePackageManagerTerminate;
    end;

    // Restart new Abort cycle
    AbortShellThreadInstancesCount := 0;
    ShellThreadHelper.fAbortOrderReceived := False;

    // Normal thread dealing with KOS, KOS Ports, DC Tool and Ruby
    ShellThread := TShellThread.Create(True);
    with ShellThread do
    begin
{$IFDEF DEBUG}
      SetThreadName('TShellThread', ShellThread.Handle);
{$ENDIF}
      Aborted := False;
      Manager := DreamcastSoftwareDevelopmentKitManager;
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

    ShellThread := nil; // FreeOnTerminate
    if Assigned(ShellThreadPackageManager) then
      FreeAndNil(ShellThreadPackageManager);

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TShellThreadHelper.HandleNewLine(Sender: TObject; NewLine: string);
begin
  AddNewLine(NewLine);
end;

procedure TShellThreadHelper.HandlePackageManagerStart(Sender: TObject);
begin
  ShellThread.UpdateProgressText(ToolchainsUnpackingText);
end;

procedure TShellThreadHelper.HandlePackagePickup(Sender: TObject;
  const SourceFileName, OutputDirectory: TFileName);
begin
  AddNewLine(Format(ToolchainsUnpackingPickupText, [
    ExtractFileName(SourceFileName)
  ]));
end;

procedure TShellThreadHelper.HandlePackageManagerProgress(Sender: TObject;
  const CurrentValue: Integer; const TotalValue: Integer);
begin
{$IFDEF DEBUG}
  DebugLog(Format('HandlePackageManagerProgress: %d%% (%d%%)', [
    CurrentValue,
    TotalValue
  ]));
{$ENDIF}

  AddNewLine(Format(ToolchainsUnpackingProgressText, [CurrentValue]));
end;

procedure TShellThreadHelper.HandlePackageManagerTerminate(Sender: TObject;
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

    // Updating success status to the main ShellThread
    ShellThread.fOperationSuccess := ShellThread.fOperationSuccess
      and Success;

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
end;

procedure TShellThread.SyncSetProgressTerminateState;
begin
  frmProgress.Finished := True;
  frmProgress.SetTerminateState(fOperationSuccess, Aborted);
  Application.ProcessMessages;
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

    // If we installed DreamSDK, execute the shell once, as on MinGW64/MSYS2
    // there is some additional setup done the first time the shell is run
    if IsPostInstallMode then
    begin
      UpdateProgressText(FinalizingPostInstall);
      Manager.Environment.ExecuteShellCommand('exit', GetCurrentDir);
    end;

    // Process Unpacking Toolchains (if required)
    // This is usually used while installing DreamSDK only.
    ProcessUnpackToolchains;

    // Process KOS, KOS Ports setup or KOS single port
    if FileExists(Manager.Environment.FileSystem.Shell.ShellExecutable) then
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
type
  TRepositoryOperation = (roNothing, roClone, roUpdate);

var
  LogContext: TLogMessageContext;
  OutputBuffer: string;
  IsModifiedKallisti,
  IsModifiedKallistiPorts,
  IsModifiedDreamcastTool,
  IsModifiedRuby,
  IsEnvironShellScriptUpdated: Boolean;

  procedure CombineOutputBuffer(const InputBuffer: string);
  var
    Separator: string;

  begin
    Separator := '';
    if OutputBuffer <> '' then
      Separator := sLineBreak;
    OutputBuffer := OutputBuffer + Separator + InputBuffer;
  end;

  function RepositoryKindToString(RepositoryKind: TRepositoryKind): string;
  begin
    Result := KallistiText;
    case RepositoryKind of
      rkKallistiPorts:
        Result := KallistiPortsText;
      rkDreamcastTool:
        Result := DreamcastToolText;
      rkRuby:
        Result := RubyText;
    end;
  end;

  function HandleRepository(Installed: Boolean; RepositoryKind: TRepositoryKind;
    var UpdateState: TUpdateOperationState): TRepositoryOperation;
  var
    LogSubContext: TLogMessageContext;
    RepositoryName,
    TempBuffer: string;
    IsSuccess: Boolean;
    WorkDirectory: TFileName;

  begin
    IsSuccess := False;
    Result := roNothing;
    TempBuffer := EmptyStr;

    LogSubContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
    try
      RepositoryName := RepositoryKindToString(RepositoryKind);

      (* Specific process: we will force the cloning in that case, so remove old
         directories in that case *)
      if IsPostInstallMode then
      begin
        case RepositoryKind of
          rkKallisti:
            WorkDirectory := Manager.Environment.FileSystem.Kallisti.KallistiDirectory;
          rkKallistiPorts:
            WorkDirectory := Manager.Environment.FileSystem.Kallisti.KallistiPortsDirectory;
          rkDreamcastTool:
            WorkDirectory := Manager.Environment.FileSystem.DreamcastTool.BaseDirectory;
          rkRuby:
            WorkDirectory := Manager.Environment.FileSystem.Ruby.BaseDirectory;
        end;
        IsSuccess := RenameFileOrDirectoryAsBackup(WorkDirectory);
        LogMessage(LogSubContext, Format('PostInstall Mode, renaming directory [%s]: "%s"', [
          BoolToStr(IsSuccess, True),
          WorkDirectory
        ]));
        IsSuccess := False;
      end;

      if CanContinue then
      begin
        if not Installed then
        begin
          // Install (Clone)
          Result := roClone;
          UpdateProgressText(Format(CloningOperation, [RepositoryName]));
          case RepositoryKind of
            rkKallisti:
              IsSuccess := Manager.KallistiOS.CloneRepository(TempBuffer);
            rkKallistiPorts:
              IsSuccess := Manager.KallistiPorts.CloneRepository(TempBuffer);
            rkDreamcastTool:
              IsSuccess := Manager.DreamcastTool.CloneRepository(TempBuffer);
            rkRuby:
              IsSuccess := Manager.Ruby.CloneRepository(TempBuffer);
          end;
          SetOperationSuccess(IsSuccess);
        end
        else
        begin
          // Update
          Result := roUpdate;
          UpdateProgressText(Format(UpdatingOperation, [RepositoryName]));
          case RepositoryKind of
            rkKallisti:
              UpdateState := Manager.KallistiOS.UpdateRepository(TempBuffer);
            rkKallistiPorts:
              UpdateState := Manager.KallistiPorts.UpdateRepository(TempBuffer);
            rkDreamcastTool:
              UpdateState := Manager.DreamcastTool.UpdateRepository(TempBuffer);
            rkRuby:
              UpdateState := Manager.Ruby.UpdateRepository(TempBuffer);
          end;
          SetOperationSuccess(UpdateState <> uosUpdateFailed);
        end;
        CombineOutputBuffer(TempBuffer);
      end;

      LogMessage(LogSubContext, Format('RepositoryKind: "%s", ' +
        'RepositoryName: "%s", Result: "%s", UpdateState: "%s", ' +
        'Installed: "%s", IsSuccess: "%s", CanContinue: "%s"', [
        GetEnumName(TypeInfo(TRepositoryKind), Ord(RepositoryKind)),
        RepositoryName,
        GetEnumName(TypeInfo(TRepositoryOperation), Ord(Result)),
        GetEnumName(TypeInfo(TUpdateOperationState), Ord(UpdateState)),
        BoolToStr(Installed, True),
        BoolToStr(IsSuccess, True),
        BoolToStr(CanContinue, True)
      ]));

      LogMessage(LogSubContext, Format('TempBuffer: "%s"', [
        TempBuffer
      ]));
    finally
      LogMessageExit(LogSubContext);
    end;
  end;

  function HandleResponse(RepositoryKind: TRepositoryKind;
    RepositoryOperation: TRepositoryOperation; UpdateState: TUpdateOperationState;
    ComponentBuilt: Boolean): Boolean;
  var
    RepositoryInstalled,
    RepositoryUpdated,
    BuildNecessary: Boolean;

  begin
    RepositoryInstalled := (RepositoryOperation = roClone);
    RepositoryUpdated := ((RepositoryOperation = roUpdate)
      and (UpdateState = uosUpdateSuccess));
    BuildNecessary := (not ComponentBuilt);

    if (RepositoryKind = rkKallisti) then
    begin
      // Kallisti has priority
      if RepositoryInstalled then
        fResponse := stoKallistiInstall
      else if RepositoryUpdated then
      begin
        fResponse := stoKallistiUpdate;
        fOperationUpdateState := UpdateState;
      end
      else if BuildNecessary then
      begin
        fResponse := stoKallistiUpdate;
        fOperationUpdateState := uosUpdateSuccess;
      end;
    end
    else if (fResponse = stoNothing) then
    begin
      // Kallisti Ports and Dreamcast Tool in second place...
      if (RepositoryInstalled or RepositoryUpdated or BuildNecessary) then
      begin
        fResponse := stoKallistiUpdate;
        fOperationUpdateState := uosUpdateSuccess;
      end;
    end;

    Result := RepositoryInstalled or BuildNecessary;
  end;

  function HandleKallisti: Boolean;
  var
    TempBuffer: string;
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;

  begin
    // Handle KallistiOS Repository
    TempBuffer := EmptyStr;
    UpdateState := uosUndefined;

    RepositoryOperation := HandleRepository(Manager.KallistiOS.Installed,
      rkKallisti, UpdateState);

    // Handle all the cases where Kallisti need to be compiled
    Result := HandleResponse(rkKallisti, RepositoryOperation, UpdateState,
      Manager.KallistiOS.Built);

    // Determine if we need to do something
    if Result then
    begin
      // Generate environ.sh file, unpacking genromfs and patching config.mk in kos-ports
      if CanContinue then
      begin
        UpdateProgressText(KallistiInitializeText);
        SetOperationSuccess(Manager.KallistiOS.InitializeEnvironment);
        IsEnvironShellScriptUpdated := True;
      end;

      // Making KallistiOS library
      if CanContinue then
      begin
        UpdateProgressText(KallistiBuildText);
        SetOperationSuccess(Manager.KallistiOS.Build(TempBuffer));
      end;

      // Fixing-up SH-4 Newlib
      if CanContinue then
      begin
        UpdateProgressText(KallistiFixNewlibText);
        SetOperationSuccess(Manager.KallistiOS.FixupHitachiNewlib(TempBuffer));
      end;
    end;
    CombineOutputBuffer(TempBuffer);
  end;

  function HandleKallistiPorts: Boolean;
  var
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;

  begin
    // Handle Kallisti Ports Repository
    UpdateState := uosUndefined;
    RepositoryOperation := HandleRepository(Manager.KallistiPorts.Installed,
      rkKallistiPorts, UpdateState);
    Result := HandleResponse(rkKallistiPorts, RepositoryOperation, UpdateState, True);

    // Determine if we need to do something
    if Result then
      Manager.KallistiPorts.InitializeEnvironment;
  end;

  function HandleDreamcastTool: Boolean;
  var
    TempBuffer: string;
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;

  begin
    // Handle Dreamcast Tool(dc-tool) repository
    TempBuffer := EmptyStr;
    UpdateState := uosUndefined;

    RepositoryOperation := HandleRepository(Manager.DreamcastTool.Installed,
      rkDreamcastTool, UpdateState);

    // Determine if we need to do something
    Result := HandleResponse(rkDreamcastTool, RepositoryOperation, UpdateState,
      Manager.DreamcastTool.Built) or IsEnvironShellScriptUpdated;

    if Result then
    begin
      // Preparing Makefile.cfg
      if CanContinue then
      begin
        UpdateProgressText(DreamcastToolInitializeText);
        SetOperationSuccess(Manager.DreamcastTool.InitializeEnvironment);
      end;

      // Making Dreamcast Tool binaries
      if CanContinue then
      begin
        UpdateProgressText(DreamcastToolBuildText);
        SetOperationSuccess(Manager.DreamcastTool.Build(TempBuffer));
      end;

      // Copying the right Dreamcast Tool binary in place
      if CanContinue then
      begin
        UpdateProgressText(DreamcastToolInstallText);
        SetOperationSuccess(Manager.DreamcastTool.Install);
      end;
    end;
    CombineOutputBuffer(TempBuffer);
  end;

  function HandleRuby: Boolean;
  var
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;
    TempBuffer: string;

  begin
    // Handle Ruby (mruby) Repository
    TempBuffer := EmptyStr;
    UpdateState := uosUndefined;
    RepositoryOperation := HandleRepository(Manager.Ruby.Installed,
      rkRuby, UpdateState);
    Result := HandleResponse(rkRuby, RepositoryOperation, UpdateState,
      Manager.Ruby.Built);

    // Determine if we need to do something
    if Result then
    begin
      // Copying build_config.rb file
      if CanContinue then
      begin
        UpdateProgressText(RubyInitializeText);
        SetOperationSuccess(Manager.Ruby.InitializeEnvironment);
      end;

      // Making Ruby library
      if CanContinue then
      begin
        UpdateProgressText(RubyBuildText);
        SetOperationSuccess(Manager.Ruby.Build(TempBuffer));
      end;
    end;
    CombineOutputBuffer(TempBuffer);
  end;

begin
  Result := EmptyStr;
  OutputBuffer := EmptyStr;
  IsEnvironShellScriptUpdated := False;

  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    IsModifiedKallisti := HandleKallisti;
    LogMessage(LogContext, Format('IsModifiedKallisti: "%s"', [
      BoolToStr(IsModifiedKallisti, True)
    ]));

    IsModifiedKallistiPorts := HandleKallistiPorts;
    LogMessage(LogContext, Format('IsModifiedKallistiPorts: "%s"', [
      BoolToStr(IsModifiedKallistiPorts, True)
    ]));

    IsModifiedDreamcastTool := HandleDreamcastTool;
    LogMessage(LogContext, Format('IsModifiedDreamcastTool: "%s"', [
      BoolToStr(IsModifiedDreamcastTool, True)
    ]));

    IsModifiedRuby := (not Manager.Environment.Settings.Ruby.Enabled) or
      (Manager.Environment.Settings.Ruby.Enabled and HandleRuby);
    LogMessage(LogContext, Format('IsModifiedRuby: "%s"', [
      BoolToStr(IsModifiedRuby, True)
    ]));

    if (CanContinue) and (not IsModifiedKallisti) and (not IsModifiedKallistiPorts)
      and (not IsModifiedDreamcastTool) and (not IsModifiedRuby) then
        UpdateProgressText(KallistiOperationNothingNeededText);
    LogMessage(LogContext, Format('CanContinue: "%s"', [
      BoolToStr(CanContinue, True)
    ]));

    Result := OutputBuffer;
    LogMessage(LogContext, Format('Result: "%s"', [
      Result
    ]));
  finally
    LogMessageExit(LogContext);
  end;
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

procedure TShellThread.ProcessUnpackToolchains;
begin
  ShellThreadPackageManager.Execute;
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

