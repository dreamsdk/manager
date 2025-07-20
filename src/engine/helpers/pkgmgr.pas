unit PkgMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  SysTools,
  FSTools,
  SevenZip,
  RefBase,
  DCSDKMgr,
  Environ,
  Unpack;

type
  TPackageManagerPackagePickupEvent = procedure(Sender: TObject;
    const SourceFileName, OutputDirectory: TFileName) of object;
  TPackageManagerProgressValueEvent = procedure(Sender: TObject; const CurrentValue: Integer;
    const TotalValue: Integer) of object;
  TPackageManagerProgressRecordEvent = procedure(Sender: TObject; const RecordNode: string) of object;
  TPackageManagerTerminateEvent = procedure(Sender: TObject;
    const Success: Boolean; const Aborted: Boolean) of object;

  TPackageManagerRequest = (
    pmrUndefined,
    pmrToolchain,
    pmrDebugger,
    pmrOffline,
    pmrAutoDetectDebuggerToolchain
  );

  TPackageManagerRequestOffline = (
    pmroKallisti,
    pmroKallistiPorts,
    pmroDreamcastToolSerial,
    pmroDreamcastToolInternetProtocol,
    pmroRuby
  );

  { TPackageManager }
  TPackageManager = class(TObject)
  private
    fAborted: Boolean;
    fEnableBusyWaitingOnExecution: Boolean;
    fEnableUnpackWindow: Boolean;
    fOffline: TPackageManagerRequestOffline;
    fPackagePickup: TPackageManagerPackagePickupEvent;
    fProgress: TPackageManagerProgressValueEvent;
    fProgressRecord: TPackageManagerProgressRecordEvent;
    fStart: TNotifyEvent;
    fSuccessOperation: Boolean;
    fAutoDetectedDebuggerProfileKey: string;
    fDebuggerProfileKey: string;
    fManager: TDreamcastSoftwareDevelopmentKitManager;
    fOperation: TPackageManagerRequest;
    fSevenZipCommander: TSevenZipCommander;
    fTerminate: TPackageManagerTerminateEvent;
    fAutoDetectedToolchainProfileKey: string;
    fToolchainProfileKey: string;

    function GetFileSystem: TDreamcastSoftwareDevelopmentFileSystem;
    function GetRunning: Boolean;
    procedure HandleOperationPickup(Sender: TObject;
      const SourceFileName, OutputDirectory: TFileName);
    procedure HandleProgress(Sender: TObject; const CurrentValue: Integer;
      const TotalValue: Integer);
    procedure HandleProgressRecord(Sender: TObject; const RecordNode: string);
    procedure HandleTerminate(Sender: TObject; const Success: Boolean);
    procedure ShowUnpackWindow;
  protected
    procedure Add(const APackageFileName: TFileName;
      const AOutputDirectory: TFileName);
    function GetStampFromPackageFileName(
      const PackageFileName: TFileName): TFileName;

    function AutoDetectRequiredDebugger: Boolean;
    function AutoDetectRequiredToolchain: Boolean;
    procedure InitializeOperations;

    property FileSystem: TDreamcastSoftwareDevelopmentFileSystem
      read GetFileSystem;
  public
    constructor Create(AManager: TDreamcastSoftwareDevelopmentKitManager;
      const AEnableUnpackWindow: Boolean);
    constructor Create(AManager: TDreamcastSoftwareDevelopmentKitManager); overload;
    destructor Destroy; override;

    procedure Abort;
    procedure Execute;
    procedure Pause;
    procedure Resume;

    property DebuggerProfileKey: string
      read fDebuggerProfileKey write fDebuggerProfileKey;
    property EnableBusyWaitingOnExecution: Boolean
      read fEnableBusyWaitingOnExecution write fEnableBusyWaitingOnExecution;
    property EnableUnpackWindow: Boolean
      read fEnableUnpackWindow write fEnableUnpackWindow;
    property OfflinePackage: TPackageManagerRequestOffline
      read fOffline write fOffline;
    property Operation: TPackageManagerRequest read fOperation write fOperation;
    property ToolchainProfileKey: string
      read fToolchainProfileKey write fToolchainProfileKey;
    property Running: Boolean read GetRunning;

    property OnStart: TNotifyEvent
      read fStart write fStart;
    property OnPackagePickup: TPackageManagerPackagePickupEvent
      read fPackagePickup write fPackagePickup;
    property OnProgress: TPackageManagerProgressValueEvent
      read fProgress write fProgress;
    property OnProgressRecord: TPackageManagerProgressRecordEvent
      read fProgressRecord write fProgressRecord;
    property OnTerminate: TPackageManagerTerminateEvent read fTerminate
      write fTerminate;
  end;

function IsDebuggerPythonVersionInstalled(const DebuggerProfileKey: string;
  var VersionWithDot: string): Boolean;

implementation

uses
  TypInfo,
  Variants,
  PEUtils,
  StrTools,
  Global;

function IsDebuggerPythonVersionInstalled(const DebuggerProfileKey: string;
  var VersionWithDot: string): Boolean;
var
  GdbProfileInfo: TGdbProfileInfo;
  i: Integer;
  PythonFilePaths: TStringList;
  PythonFileName,
  PythonFilePath: TFileName;
  PythonBitness: TPortableExecutableBitness;
  VersionWithoutDot: string;

begin
  Result := False;

  GdbProfileInfo := DreamcastSoftwareDevelopmentKitManager.Environment
    .FileSystem.Packages.GetGdbProfileByKey(DebuggerProfileKey);

  if Assigned(GdbProfileInfo) then
  begin
    VersionWithDot := GdbProfileInfo.PythonVersion;
    Result := IsEmpty(VersionWithDot); // if no Python version available, then return TRUE (OK to continue)
    if not Result then
    begin
      // If there is a version to test, then do it

      VersionWithoutDot := StringReplace(VersionWithDot, '.', EmptyStr, []);
      PythonFileName := Format('python%s.dll', [VersionWithoutDot]);

{$IFDEF DEBUG}
      WriteLn('Checking Python ', VersionWithDot, ' ...');
{$ENDIF}
      PythonFilePaths := TStringList.Create;
      try
        if GetFileLocationsInSystemPath(PythonFileName, PythonFilePaths) then
          for i := 0 to PythonFilePaths.Count - 1 do
          begin
            PythonFilePath := PythonFilePaths[i];
{$IFDEF DEBUG}
            WriteLn('  Python ', VersionWithDot, ' is installed: ', PythonFilePath);
{$ENDIF}
            PythonBitness := GetPortableExecutableBitness(PythonFilePath);
            Result := Result or (PythonBitness = GdbProfileInfo.Bitness);
{$IFDEF DEBUG}
            WriteLn('  Python ', VersionWithDot, ' bitness: ', PythonBitness);
{$ENDIF}
          end
{$IFDEF DEBUG}
          else
            WriteLn(Format('  Python %s is not installed', [VersionWithDot]))
{$ENDIF}
          ;
      finally
        PythonFilePaths.Free;
      end;
    end;
  end;
end;

{ TPackageManager }

procedure TPackageManager.HandleProgress(Sender: TObject;
  const CurrentValue: Integer; const TotalValue: Integer);
begin
  if fEnableUnpackWindow then
    frmUnpack.ProgressValue := TotalValue;

  if Assigned(fProgress) then
    fProgress(Self, CurrentValue, TotalValue);
end;

function TPackageManager.GetRunning: Boolean;
begin
  Result := fSevenZipCommander.Active;
end;

procedure TPackageManager.HandleOperationPickup(Sender: TObject;
  const SourceFileName, OutputDirectory: TFileName);
begin
  if Assigned(fPackagePickup) then
    fPackagePickup(Self, SourceFileName, OutputDirectory);
end;

function TPackageManager.GetFileSystem: TDreamcastSoftwareDevelopmentFileSystem;
begin
  Result := fManager.Environment.FileSystem;
end;

procedure TPackageManager.HandleProgressRecord(Sender: TObject;
  const RecordNode: string);
begin
  if fEnableUnpackWindow then
    frmUnpack.ProgressText := RecordNode;

  if Assigned(fProgressRecord) then
    fProgressRecord(Self, RecordNode);
end;

procedure TPackageManager.HandleTerminate(Sender: TObject;
  const Success: Boolean);
begin
  fSevenZipCommander.Operations.Clear;
  fSuccessOperation := Success;

  if fEnableUnpackWindow then
  begin
    frmUnpack.Finished := True;
    if Success then
      Delay(1000);
    frmUnpack.Close;
    Application.ProcessMessages;
  end;

  if Assigned(fTerminate) then
    fTerminate(Self, Success, fAborted);
end;

procedure TPackageManager.ShowUnpackWindow;
begin
  if fEnableUnpackWindow then
  begin
    frmUnpack := TfrmUnpack.Create(Application);
    try
      frmUnpack.ShowModal;
    finally
      FreeAndNil(frmUnpack);
    end;
  end;
end;

constructor TPackageManager.Create(AManager: TDreamcastSoftwareDevelopmentKitManager;
  const AEnableUnpackWindow: Boolean);
begin
  fAutoDetectedDebuggerProfileKey := EmptyStr;
  fAutoDetectedToolchainProfileKey := EmptyStr;
  fEnableBusyWaitingOnExecution := False;
  fEnableUnpackWindow := AEnableUnpackWindow;
  fManager := AManager;
  fSevenZipCommander := TSevenZipCommander.Create;
  with fSevenZipCommander do
  begin
    OnOperationPickup := @HandleOperationPickup;
    OnProgress := @HandleProgress;
    OnProgressRecord := @HandleProgressRecord;
    OnTerminate := @HandleTerminate;
  end;
end;

constructor TPackageManager.Create(AManager: TDreamcastSoftwareDevelopmentKitManager);
begin
  Create(AManager, True);
end;

destructor TPackageManager.Destroy;
begin
  fSevenZipCommander.Free;
  inherited Destroy;
end;

procedure TPackageManager.Abort;
begin
  fAborted := True;
  if Assigned(fSevenZipCommander) then
    fSevenZipCommander.Abort;
end;

procedure TPackageManager.Execute;
var
  LogContext: TLogMessageContext;

  // Check if a valid action need to be performed
  function IsValidAction: Boolean;
  begin
    (*
      First check if operation is defined and not pmrAutoDetectDebuggerToolchain
      Indeed, pmrAutoDetectDebuggerToolchain is a fake action used for detecting
      if we need to update the Toolchain or GDB package(s).
    *)
    Result := (fOperation <> pmrUndefined)
      and (fOperation <> pmrAutoDetectDebuggerToolchain);

    // Handle pmrToolchain; this one requires other parameters to be usable.
    Result := Result or (
          (fOperation = pmrToolchain)
      and ((fDebuggerProfileKey <> EmptyStr) or (fToolchainProfileKey <> EmptyStr))
    );
  end;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    // Auto Detect DebuggerProfileKey/ToolchainProfileKey if possible
    if (fOperation = pmrAutoDetectDebuggerToolchain) then
    begin
      // Auto detect DebuggerProfileKey (if possible)

      if (fDebuggerProfileKey = EmptyStr) and AutoDetectRequiredDebugger then
      begin
        fDebuggerProfileKey := fAutoDetectedDebuggerProfileKey;
        Operation := pmrDebugger;
      end;

      // Auto detect ToolchainProfileKey (if possible)
      if (fToolchainProfileKey = EmptyStr) and AutoDetectRequiredToolchain then
      begin
        fToolchainProfileKey := fAutoDetectedToolchainProfileKey;
        Operation := pmrToolchain; // This includes DebuggerProfileKey
      end;
    end;

    // Process only if there is an action to make
    if IsValidAction then
    begin
      // Trigger OnStart event (if required)
      if Assigned(fStart) then
        fStart(Self);

      // Execute the process itself
      InitializeOperations;
      fSevenZipCommander.Execute;

      // Display the standard Unpack window (NOT in a thread)
      if fEnableUnpackWindow then
        ShowUnpackWindow;

      // Active Wait. Used only if this is used in a real Thread (e.g., ShellThread).
      if EnableBusyWaitingOnExecution then
        while Running do
        begin
          Sleep(100); // Not effective indeed, but it works from threaded code.
          LogMessage(LogContext, 'EnableBusyWaitingOnExecution::Running');
        end;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TPackageManager.Pause;
begin
  fSevenZipCommander.Pause;
end;

procedure TPackageManager.Resume;
begin
  fSevenZipCommander.Resume;
end;

procedure TPackageManager.Add(const APackageFileName: TFileName;
  const AOutputDirectory: TFileName);
begin
{$IFDEF DEBUG}
  WriteLn('APackageFileName: ', APackageFileName);
  WriteLn('AOutputDirectory: ', AOutputDirectory);
{$ENDIF}
  with fSevenZipCommander.Operations.Add do
  begin
    SourceFileName := APackageFileName;
    OutputDirectory := AOutputDirectory;
  end;
end;

function TPackageManager.GetStampFromPackageFileName(
  const PackageFileName: TFileName): TFileName;
begin
  Result := EmptyStr;
  if not IsEmpty(PackageFileName) then
    Result := ExtractFileName(ChangeFileExt(PackageFileName, '.stamp'));
end;

function TPackageManager.AutoDetectRequiredToolchain: Boolean;
(*
  This feature is designed to unpack the toolchains after installing
  DreamSDK for saving space on the installer. Indeed this avoid us to have
  the toolchains packaged in the 'packages' directory AND in the installer.
  Now the installation is done by DreamSDK itself.

  We check for all toolchains releases to install. First try with the worst
  (Legacy) and ends with the best (Stable), so it means that if there is
  different packages copied to the correct location, we take the best option.
  This should not happend by the way but we never know.

  Same as if we have different packages requests between Super-H and Arm.
  It should be the same flavour (e.g., 'Legacy' for both Super-H and Arm) but
  again, we never know. We take the best package if several different were
  copied to the destination directory, so if we have 'Super-H Legacy' and
  'Arm Stable', result will be 'Stable'.
*)
var
  LogContext: TLogMessageContext;
  i: Integer;
  StampFileNameSuperH,
  StampFileNameArm,
  ToolchainBaseDirectorySuperH,
  ToolchainBaseDirectoryArm: TFileName;
  AutoDetectedToolchainProfileKeySelected: TStringList;

begin
  Result := False;
  fAutoDetectedToolchainProfileKey := EmptyStr;

  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    AutoDetectedToolchainProfileKeySelected := TStringList.Create;
    try
      with fManager.Environment.FileSystem do
      begin
        // Extracting base directories for both Super-H and Arm
        ToolchainBaseDirectorySuperH := ToolchainSuperH.BaseDirectory;
        ToolchainBaseDirectoryArm := ToolchainARM.BaseDirectory;

        for i := 0 to Packages.GetToolchainProfileCount - 1 do
        begin
          StampFileNameSuperH := ToolchainBaseDirectorySuperH
            + GetStampFromPackageFileName(Packages.ToolchainProfiles[i].ShElfPackage);
          StampFileNameArm := ToolchainBaseDirectoryArm
            + GetStampFromPackageFileName(Packages.ToolchainProfiles[i].ArmEabiPackage);
          if FileExists(StampFileNameSuperH) or FileExists(StampFileNameArm) then
          begin
            // We detected a request to install a new toolchain, it could be from SH or ARM
            AutoDetectedToolchainProfileKeySelected.Add(
              Format('%02d'#9'%s', [
                i,
                Packages.ToolchainProfiles[i].ProfileKey
              ])
            );

            LogMessage(LogContext, Format('Detected profile: "%s"', [
              Packages.ToolchainProfiles[i].ProfileKey
            ]));

            // Cleanup
            KillFile(StampFileNameSuperH);
            KillFile(StampFileNameArm);
          end;
        end;
      end;

      // Choose the best toolchain to use
      if AutoDetectedToolchainProfileKeySelected.Count > 0 then
      begin
        AutoDetectedToolchainProfileKeySelected.Sort;
        fAutoDetectedToolchainProfileKey :=
          Right(#9, AutoDetectedToolchainProfileKeySelected[0]);
      end;
    finally
      AutoDetectedToolchainProfileKeySelected.Free;
    end;

    // Final result
    Result := not IsEmpty(fAutoDetectedToolchainProfileKey);

    LogMessage(LogContext, Format('Result: %s, AutoDetectedToolchainProfileKey: "%s"', [
      BoolToStr(Result),
      fAutoDetectedToolchainProfileKey
    ]));
  finally
    LogMessageExit(LogContext);
  end;
end;

function TPackageManager.AutoDetectRequiredDebugger: Boolean;
var
  i: Integer;
  StampFileName,
  ToolchainBaseDirectorySuperH: TFileName;

begin
  Result := False;
  fAutoDetectedToolchainProfileKey := EmptyStr;

  with fManager.Environment.FileSystem do
  begin
    // Extracting base directories for both Super-H and Arm
    ToolchainBaseDirectorySuperH := ToolchainSuperH.BaseDirectory;

    for i := 0 to Packages.GetGdbProfileCount - 1 do
    begin
      StampFileName := ToolchainBaseDirectorySuperH +
        GetStampFromPackageFileName(Packages.GdbProfiles[i].GdbPackage);
      if FileExists(StampFileName) then
      begin
        fAutoDetectedDebuggerProfileKey := Packages.GdbProfiles[i].ProfileKey;
        KillFile(StampFileName);
        // TODO: We retain only the newer version... ? not really needed probably
      end;
    end;
  end;

  Result := not IsEmpty(fAutoDetectedDebuggerProfileKey);
end;

procedure TPackageManager.InitializeOperations;
var
  DebuggerPackage,
  ToolchainArmPackage,
  ToolchainArmWrappersPackage,
  ToolchainSuperHPackage,
  ToolchainSuperHWrappersPackage: TFileName;

begin
  fAborted := False;

  // Nothing to do!
  if fOperation = pmrUndefined then
    Exit;

  // Toolchain
  if fOperation = pmrToolchain then
  begin
    FileSystem.ToolchainARM.Reset;
    FileSystem.ToolchainSuperH.Reset;

    with DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Packages do
    begin
      // ARM
      ToolchainArmPackage := GetToolchainPackage(ToolchainProfileKey, tkARM);
      ToolchainArmWrappersPackage := GetToolchainWrappersPackage(ToolchainProfileKey, tkARM);
      Add(ToolchainArmPackage, FileSystem.ToolchainBase);
      Add(ToolchainArmWrappersPackage, FileSystem.ToolchainBase); // DreamSDK Runner wrappers for IDE

      // Super-H
      ToolchainSuperHPackage := GetToolchainPackage(ToolchainProfileKey, tkSuperH);
      ToolchainSuperHWrappersPackage := GetToolchainWrappersPackage(ToolchainProfileKey, tkSuperH);
      Add(ToolchainSuperHPackage, FileSystem.ToolchainBase);
      Add(ToolchainSuperHWrappersPackage, FileSystem.ToolchainBase); // DreamSDK Runner wrappers for IDE
    end;
  end;

  // Toolchain and/or Debugger (GDB)
  if (fOperation = pmrToolchain) or (fOperation = pmrDebugger) then
  begin
    DebuggerPackage := EmptyStr;

    with DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Packages do
      DebuggerPackage := GetGdbPackage(DebuggerProfileKey);

    if not IsEmpty(DebuggerPackage) then
      Add(DebuggerPackage, FileSystem.ToolchainBase);
  end;

  // Offline only
  if fOperation = pmrOffline then
  begin
    case OfflinePackage of
      pmroKallisti:
        begin
          FileSystem.Kallisti.ResetRepositoryKallisti;
          Add(FileSystem.Kallisti.Packages.Kallisti, FileSystem.Kallisti.KallistiDirectory);
        end;
      pmroKallistiPorts:
        begin
          FileSystem.Kallisti.ResetRepositoryKallistiPorts;
          Add(FileSystem.Kallisti.Packages.KallistiPorts, FileSystem.Kallisti.KallistiPortsDirectory);
        end;
      pmroDreamcastToolSerial:
        begin
          KillFile(FileSystem.DreamcastTool.SerialExecutable);
          FileSystem.DreamcastTool.ResetRepositorySerial;
          Add(FileSystem.DreamcastTool.Packages.Serial, FileSystem.DreamcastTool.SerialDirectory);
        end;
      pmroDreamcastToolInternetProtocol:
        begin
          KillFile(FileSystem.DreamcastTool.InternetProtocolExecutable);
          FileSystem.DreamcastTool.ResetRepositoryInternetProtocol;
          Add(FileSystem.DreamcastTool.Packages.InternetProtocol, FileSystem.DreamcastTool.InternetProtocolDirectory);
        end;
      pmroRuby:
        begin
          FileSystem.Ruby.ResetRepository;
          Add(FileSystem.Ruby.Packages.RubyLibrary, FileSystem.Ruby.BaseDirectory);
          Add(FileSystem.Ruby.Packages.Samples, FileSystem.Ruby.SamplesDirectory);
        end;
    end;
  end;
end;

end.

