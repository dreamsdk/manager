unit ShellThd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCSDKMgr, PortMgr, Environ;

type
  TShellThreadInputRequest = (
    stiKallistiManage,
    stiKallistiPortInstall,
    stiKallistiPortUpdate,
    stiKallistiPortUninstall
  );

  TShellThreadOutputResponse = (
    stoNothing,
    stoKallistiInstall,
    stoKallistiUpdate,
    stoKallistiPortInstall,
    stoKallistiPortUpdate,
    stoKallistiPortUninstall
  );

  TShellThreadContext = (
    stcUndefined,
    stcKallisti,
    stcKallistiPort
  );

  TShellThreadCommandTerminateEvent = procedure(
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
    fProgressText: string;
    fContext: TShellThreadContext;
    fRequest: TShellThreadInputRequest;
    fResponse: TShellThreadOutputResponse;
    fOperationUpdateState: TUpdateOperationState;
    fOperationSuccess: Boolean;
    fOperationResultOutput: string;
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
    property Operation: TShellThreadInputRequest read fRequest write fRequest;
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
procedure ExecuteThreadOperation(const AOperation: TShellThreadInputRequest);

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

function GetThreadContext(const AOperation: TShellThreadInputRequest): TShellThreadContext;
var
  ValidKallistiPortContext,
  IsSingleKallistiPortOperation: Boolean;

begin
  Result := stcUndefined;

  IsSingleKallistiPortOperation := (AOperation = stiKallistiPortInstall)
    or (AOperation = stiKallistiPortUninstall) or (AOperation = stiKallistiPortUpdate);

  ValidKallistiPortContext := Assigned(frmMain.SelectedKallistiPort)
    and IsSingleKallistiPortOperation;

  Result := stcKallisti;
  if ValidKallistiPortContext then
    Result := stcKallistiPort;
end;

procedure ExecuteThreadOperation(const AOperation: TShellThreadInputRequest);
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
          KallistiPortText := Format('KallistiOS Port %s %s', [
            frmMain.SelectedKallistiPort.Name, frmMain.SelectedKallistiPort.Version]);
          case AOperation of
            stiKallistiPortInstall:
              OperationTitle := Format('Installation of %s', [KallistiPortText]);
            stiKallistiPortUpdate:
              OperationTitle := Format('Update of %s', [KallistiPortText]);
            stiKallistiPortUninstall:
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
    fCommandTerminate(fRequest, fResponse, fOperationSuccess, fOperationUpdateState);
end;

function TShellThread.CanContinue: Boolean;
begin
  Result := fOperationSuccess and (not Terminated) and (not Aborted);
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
  TRepositoryOperation = (roNothing, roClone, roUpdate);
  TRepositoryKind = (rkKallisti, rkKallistiPorts, rkDreamcastTool);

var
  OutputBuffer: string;
  IsModifiedKallisti, IsModifiedKallistiPorts, IsModifiedDreamcastTool: Boolean;

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
    Result := 'KallistiOS';
    case RepositoryKind of
      rkKallistiPorts:
        Result := 'KallistiOS Ports';
      rkDreamcastTool:
        Result := 'Dreamcast Tool';
    end;
  end;

  function HandleRepository(Installed: Boolean; RepositoryKind: TRepositoryKind;
    var UpdateState: TUpdateOperationState): TRepositoryOperation;
  var
    RepositoryName,
    TempBuffer: string;
    IsSuccess: Boolean;

  begin
    Result := roNothing;
    RepositoryName := RepositoryKindToString(RepositoryKind);
    if CanContinue then
    begin
      if not Installed then
      begin
        // Install (Clone)
        Result := roClone;
        UpdateProgressText(Format('Cloning %s repository...', [RepositoryName]));
        case RepositoryKind of
          rkKallisti:
            IsSuccess := Manager.KallistiOS.CloneRepository(TempBuffer);
          rkKallistiPorts:
            IsSuccess := Manager.KallistiPorts.CloneRepository(TempBuffer);
          rkDreamcastTool:
            IsSuccess := Manager.DreamcastTool.CloneRepository(TempBuffer);
        end;
        SetOperationSuccess(IsSuccess);
      end
      else
      begin
        // Update
        Result := roUpdate;
        UpdateProgressText(Format('Updating %s repository...', [RepositoryName]));
        case RepositoryKind of
          rkKallisti:
            UpdateState := Manager.KallistiOS.UpdateRepository(TempBuffer);
          rkKallistiPorts:
            UpdateState := Manager.KallistiPorts.UpdateRepository(TempBuffer);
          rkDreamcastTool:
            UpdateState := Manager.DreamcastTool.UpdateRepository(TempBuffer);
        end;
        SetOperationSuccess(UpdateState <> uosUpdateFailed);
      end;
      CombineOutputBuffer(TempBuffer);
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

    Result := RepositoryInstalled or RepositoryUpdated or BuildNecessary;
  end;

  function HandleKallisti: Boolean;
  var
    TempBuffer: string;
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;

  begin
    // Handle KallistiOS Repository
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
        UpdateProgressText('Initialize KallistiOS environment...');
        SetOperationSuccess(Manager.KallistiOS.InitializeEnvironment);
      end;

      // Making KallistiOS library
      if CanContinue then
      begin
        UpdateProgressText('Building KallistiOS library...');
        SetOperationSuccess(Manager.KallistiOS.Build(TempBuffer));
      end;

      // Fixing-up SH-4 Newlib
      if CanContinue then
      begin
        UpdateProgressText('Fixing SH-4 Newlib...');
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
    UpdateState := uosUndefined;
    RepositoryOperation := HandleRepository(Manager.DreamcastTool.Installed,
      rkDreamcastTool, UpdateState);

    // Determine if we need to do something
    Result := HandleResponse(rkDreamcastTool, RepositoryOperation, UpdateState,
      Manager.DreamcastTool.Built);

    if Result then
    begin
      // Preparing Makefile.cfg
      if CanContinue then
      begin
        UpdateProgressText('Initialize Dreamcast Tool environment...');
        SetOperationSuccess(Manager.DreamcastTool.InitializeEnvironment);
      end;

      // Making Dreamcast Tool binaries
      if CanContinue then
      begin
        UpdateProgressText('Building Dreamcast Tool binaries...');
        SetOperationSuccess(Manager.DreamcastTool.Build(TempBuffer));
      end;
    end;
    CombineOutputBuffer(TempBuffer);
  end;

begin
  Result := '';

  fOperationSuccess := True;
  fResponse := stoNothing;
  fOperationUpdateState := uosUndefined;

  IsModifiedKallisti := HandleKallisti;

  IsModifiedKallistiPorts := HandleKallistiPorts;

  IsModifiedDreamcastTool := HandleDreamcastTool;

  if (not Aborted) and (not IsModifiedKallisti) and (not IsModifiedKallistiPorts)
    and (not IsModifiedDreamcastTool) then
      UpdateProgressText('KallistiOS is already installed and up-to-date.');

  Result := OutputBuffer;
end;

function TShellThread.ProcessKallistiPort: string;

  function RequestToResponse(Request: TShellThreadInputRequest): TShellThreadOutputResponse;
  begin
    Result := stoNothing;
    case Request of
      stiKallistiPortInstall:
        Result := stoKallistiPortInstall;
      stiKallistiPortUpdate:
        Result := stoKallistiPortUpdate;
      stiKallistiPortUninstall:
        Result := stoKallistiPortUninstall;
    end;
  end;

begin
  Result := '';
  fResponse := RequestToResponse(fRequest);

  case Operation of
    stiKallistiPortInstall:
      begin
        UpdateProgressText('Installing the KallistiOS Port...');
        fOperationSuccess := SelectedKallistiPort.Install(Result);
      end;

    stiKallistiPortUninstall:
      begin
        UpdateProgressText('Uninstalling the KallistiOS Port...');
        fOperationSuccess := SelectedKallistiPort.Uninstall(Result);
      end;

    stiKallistiPortUpdate:
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

