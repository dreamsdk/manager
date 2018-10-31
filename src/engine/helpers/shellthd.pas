unit ShellThd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCSDKMgr, PortMgr, Environ;

type
  TShellThreadOperation = (
    stoKallistiManage,
    stoKallistiInstall,
    stoKallistiUpdate,
    stoPortInstall,
    stoPortUpdate,
    stoPortUninstall
  );

  TShellThreadContext = (
    stcUndefined,
    stcKallisti,
    stcKallistiPort
  );

  TShellThreadCommandTerminateEvent = procedure(
    Operation: TShellThreadOperation;
    Success: Boolean;
    UpdateState: TUpdateOperationState
  ) of object;

  { TShellThread }
  TShellThread = class(TThread)
  private
    fAborted: Boolean;
    fManager: TDreamcastSoftwareDevelopmentKitManager;
    fProgressText: string;
    fContext: TShellThreadContext;
    fOperationUpdateState: TUpdateOperationState;
    fOperationSuccess: Boolean;
    fOperationResultOutput: string;
    fOperation: TShellThreadOperation;
    fCommandTerminate: TShellThreadCommandTerminateEvent;
    fSelectedKallistiPort: TKallistiPortItem;
    procedure SyncSetProgressTerminateState;
    procedure SyncUpdateProgressText;
    procedure SyncTriggerCommandTerminate;
  protected
    function CanContinue: Boolean;
    procedure SetProgressTerminateState;
    procedure Execute; override;
    procedure UpdateProgressText(const Text: string);
    procedure TriggerCommandTerminate(OutputBuffer: string);
    function ProcessKallistiOS: string;
    function ProcessKallistiPort: string;
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
  Forms, Main, Progress, PostInst, StrRes;

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

  Result := stcKallisti;
  if ValidKallistiPortContext then
    Result := stcKallistiPort;
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

      stcKallistiPort:
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

      stcKallisti:
        OperationTitle := 'KallistiOS Installation/Update';
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

procedure TShellThread.SyncSetProgressTerminateState;
begin
  frmProgress.Finished := True;
  frmProgress.SetTerminateState(fOperationSuccess, Aborted);
  Application.ProcessMessages;
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
    fCommandTerminate(fOperation, fOperationSuccess, fOperationUpdateState);
end;

function TShellThread.CanContinue: Boolean;
begin
  Result := fOperationSuccess and not Terminated;
end;

procedure TShellThread.SetProgressTerminateState;
begin
  Synchronize(@SyncSetProgressTerminateState);
end;

procedure TShellThread.Execute;
var
  Buffer: string;

begin
  fOperationSuccess := False;
  Buffer := '';

  if FileExists(Manager.Environment.FileSystem.ShellExecutable) then
    case fContext of
      stcKallisti:
        Buffer := ProcessKallistiOS;
      stcKallistiPort:
        Buffer := ProcessKallistiPort;
    end
  else
  begin
    // This should never happens...
    fOperationResultOutput := Format(InstallationProblem, [Manager.Environment
      .Settings.InstallPath]);
    UpdateProgressText(fOperationResultOutput);
  end;

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

function TShellThread.ProcessKallistiOS: string;
type
  TRepositoryOperation = (roInstall, roUpdate);
  TRepositoryKind = (rkKallisti, rkKallistiPorts, rkDreamcastTool);

var
  IsEverythingUpToDate,
  IsKallistiFreshlyInstalled,
  IsKallistiNeedsToBeUpdated: Boolean;

  function RepositoryKindToString(RepositoryKind: TRepositoryKind): string;
  begin
    Result := 'KallistiOS';
    case RepositoryKind of
      rkKallistiPorts:
        Result := 'KallistiOS Ports';
      rkDreamcastTool:
        Result := 'Dreamcast Tool';
    end;
  end;

  function HandleRepository(Installed: Boolean; RepositoryKind: TRepositoryKind; var BufferOutput: string): TRepositoryOperation;
  var
    RepositoryName: string;
    IsSuccess: Boolean;

  begin
    Result := roInstall;
    RepositoryName := RepositoryKindToString(RepositoryKind);
    if CanContinue and (not Installed) then
    begin
      UpdateProgressText(Format('Cloning %s repository...', [RepositoryName]));
      case RepositoryKind of
        rkKallisti:
          IsSuccess := Manager.KallistiOS.CloneRepository(BufferOutput);
        rkKallistiPorts:
          IsSuccess := Manager.KallistiPorts.CloneRepository(BufferOutput);
        rkDreamcastTool:
          IsSuccess := Manager.DreamcastTool.CloneRepository(BufferOutput);
      end;
      SetOperationSuccess(IsSuccess);
    end
    else
    begin
      Result := roUpdate;
      UpdateProgressText(Format('Updating %s repository...', [RepositoryName]));
      case RepositoryKind of
        rkKallisti:
          fOperationUpdateState := Manager.KallistiOS.UpdateRepository(BufferOutput);
        rkKallistiPorts:
          fOperationUpdateState := Manager.KallistiPorts.UpdateRepository(BufferOutput);
        rkDreamcastTool:
          fOperationUpdateState := Manager.DreamcastTool.UpdateRepository(BufferOutput);
      end;
      SetOperationSuccess(fOperationUpdateState <> uosUpdateFailed);
    end;
  end;

begin
  Result := '';
  fOperationSuccess := True;
  fOperation := stoKallistiInstall;
  fOperationUpdateState := uosUndefined;

  // Handle KallistiOS Repository
  if HandleRepository(Manager.KallistiOS.Installed, rkKallisti, Result) = roUpdate then
    fOperation := stoKallistiUpdate;

  IsKallistiNeedsToBeUpdated := (fOperationUpdateState = uosUpdateSuccess);
  IsKallistiFreshlyInstalled := (fOperationUpdateState = uosUndefined) or (not Manager.KallistiOS.Built);

  // Handle KallistiPorts Repository
  HandleRepository(Manager.KallistiPorts.Installed, rkKallistiPorts, Result);

  if IsKallistiFreshlyInstalled then
  begin
    fOperation := stoKallistiInstall;
    fOperationUpdateState := uosUndefined;
  end;

  IsEverythingUpToDate := (not IsKallistiFreshlyInstalled)
    and (not IsKallistiNeedsToBeUpdated)
    and (Manager.DreamcastTool.Built);

  if IsKallistiFreshlyInstalled or IsKallistiNeedsToBeUpdated then
  begin
    // Generate environ.sh file, unpacking genromfs and patching config.mk in kos-ports
    if CanContinue then
    begin
      UpdateProgressText('Initialize KallistiOS and KallistiOS Ports environment...');
      SetOperationSuccess(Manager.KallistiOS.InitializeEnvironment);
    end;

    // Making KallistiOS library
    if CanContinue then
    begin
      UpdateProgressText('Building KallistiOS library...');
      SetOperationSuccess(Manager.KallistiOS.Build(Result));
    end;

    // Fixing-up SH-4 Newlib
    if CanContinue then
    begin
      UpdateProgressText('Fixing SH-4 Newlib...');
      SetOperationSuccess(Manager.KallistiOS.FixupHitachiNewlib(Result));
    end;
  end;

  // Handle Dreamcast Tool
  if not Manager.DreamcastTool.Built then
  begin
    // Handle Dreamcast Tool(dc-tool) repository
    HandleRepository(Manager.DreamcastTool.Installed, rkDreamcastTool, Result);

   // Generate environ.sh file, unpacking genromfs and patching config.mk in kos-ports
    if CanContinue then
    begin
      UpdateProgressText('Initialize Dreamcast Tool environment...');
      SetOperationSuccess(Manager.DreamcastTool.InitializeEnvironment);
    end;

    // Making KallistiOS library
    if CanContinue then
    begin
      UpdateProgressText('Building Dreamcast Tool binaries...');
      SetOperationSuccess(Manager.DreamcastTool.Build(Result));
    end;
  end;

  if IsEverythingUpToDate then
    UpdateProgressText('KallistiOS is already installed and up-to-date.');
end;

function TShellThread.ProcessKallistiPort: string;
begin
  Result := '';

  case Operation of
    stoPortInstall:
      begin
        UpdateProgressText('Installing the KallistiOS Port...');
        fOperationSuccess := SelectedKallistiPort.Install(Result);
      end;
    stoPortUninstall:
      begin
        UpdateProgressText('Uninstalling the KallistiOS Port...');
        fOperationSuccess := SelectedKallistiPort.Uninstall(Result);
      end;
    stoPortUpdate:
      begin
        UpdateProgressText('Updating the KallistiOS Port...');
        fOperationUpdateState := SelectedKallistiPort.Update(Result);
        fOperationSuccess := fOperationUpdateState <> uosUpdateFailed;
      end;
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

