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

// TODO: Implement me
(*
const
  PACKAGE_MANAGER_REQUEST_TOOLCHAIN_NAMES: array[0..2] of string = (
    'Legacy',       // pmrtLegacy
    'Old Stable',   // pmrtOldStable
    'Stable'        // pmrtStable
  );
*)

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

  // Linked to TToolchainVersionKind from "Environ"
  // Please keep the same order please!
  TPackageManagerRequestToolchain = (
    pmrtUndefined,
    pmrtLegacy,       //  4
    pmrtOldStable,    //  9
    pmrtStable        // 13
  );

  TPackageManagerRequestDebugger = (
    pmrdUndefined,
    pmrdPythonDisabled,
    pmrdPython27,
    pmrdPython33,
    pmrdPython34,
    pmrdPython35,
    pmrdPython36,
    pmrdPython37,
    pmrdPython38,
    pmrdPython39,
    pmrdPython310,
    pmrdPython311,
    pmrdPython312
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
    fAutoDetectedDebugger: TPackageManagerRequestDebugger;
    fDebugger: TPackageManagerRequestDebugger;
    fManager: TDreamcastSoftwareDevelopmentKitManager;
    fOperation: TPackageManagerRequest;
    fSevenZipCommander: TSevenZipCommander;
    fTerminate: TPackageManagerTerminateEvent;
    fAutoDetectedToolchain: TPackageManagerRequestToolchain;
    fToolchain: TPackageManagerRequestToolchain;

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

    property Debugger: TPackageManagerRequestDebugger
      read fDebugger write fDebugger;
    property EnableBusyWaitingOnExecution: Boolean
      read fEnableBusyWaitingOnExecution write fEnableBusyWaitingOnExecution;
    property EnableUnpackWindow: Boolean
      read fEnableUnpackWindow write fEnableUnpackWindow;
    property OfflinePackage: TPackageManagerRequestOffline
      read fOffline write fOffline;
    property Operation: TPackageManagerRequest read fOperation write fOperation;
    property Toolchain: TPackageManagerRequestToolchain
      read fToolchain write fToolchain;
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

function IsDebuggerPythonVersionInstalled(const Version: TPackageManagerRequestDebugger;
  var VersionWithDot: string): Boolean;
function StringToPackageManagerRequestToolchain(
  const S: string): TPackageManagerRequestToolchain;

implementation

uses
  TypInfo,
  Variants,
  PEUtils;

// TODO: Refactor Me, as this suc*s
function StringToPackageManagerRequestToolchain(
  const S: string): TPackageManagerRequestToolchain;
begin
  Result := Default(TPackageManagerRequestToolchain);
  case UpperCase(S) of
    'LEGACY':
      Result := pmrtLegacy;
    'OLD STABLE':
      Result := pmrtOldStable;
    'STABLE':
      Result := pmrtStable;
  end;
end;

function IsDebuggerPythonVersionInstalled(const Version: TPackageManagerRequestDebugger;
  var VersionWithDot: string): Boolean;
var
  i: Integer;
  PythonFilePaths: TStringList;
  PythonFileName,
  PythonFilePath: TFileName;
  PythonBitness: TPortableExecutableBitness;
  VersionWithoutDot: string;

begin
  Result := True;

  VersionWithoutDot := EmptyStr;
  i := Integer(Version) - 2; // we ignore pmrdUndefined + pmrdPythonDisabled
  if (i <> -1) then
  begin
    Result := False;

    VersionWithDot := SUPPORTED_PYTHON_VERSIONS[i];
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
          Result := Result or (PythonBitness = peb32); // 32-bits only
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
  fAutoDetectedDebugger := pmrdUndefined;
  fAutoDetectedToolchain := pmrtUndefined;
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
  fSevenZipCommander.Abort;
end;

procedure TPackageManager.Execute;

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
      and ((fDebugger <> pmrdUndefined) or (fToolchain <> pmrtUndefined))
    );
  end;

begin
  // Auto Detect Debugger/Toolchain if possible
  if (fOperation = pmrAutoDetectDebuggerToolchain) then
  begin
    // Auto detect Debugger (if possible)
    if (fDebugger = pmrdUndefined) and AutoDetectRequiredDebugger then
    begin
      fDebugger := fAutoDetectedDebugger;
      Operation := pmrDebugger;
    end;

    // Auto detect Toolchain (if possible)
    if (fToolchain = pmrtUndefined) and AutoDetectRequiredToolchain then
    begin
      fToolchain := fAutoDetectedToolchain;
      Operation := pmrToolchain; // This includes Debugger
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
        Sleep(100); // Not effective indeed, but it works from threaded code.
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
var
  ToolchainBaseDirectorySuperH,
  ToolchainBaseDirectoryArm,
  LegacyPackageFileNameSuperH,
  LegacyPackageFileNameArm,
  OldStablePackageFileNameSuperH,
  OldStablePackageFileNameArm,
  StablePackageFileNameSuperH,
  StablePackageFileNameArm: TFileName;

begin
  Result := False;
  fAutoDetectedToolchain := pmrtUndefined;

  with fManager.Environment.FileSystem do
  begin
    // Extracting base directories for both Super-H and Arm
    ToolchainBaseDirectorySuperH := ToolchainSuperH.BaseDirectory;
    ToolchainBaseDirectoryArm := ToolchainARM.BaseDirectory;

    // Extracting package names for Super-H
    LegacyPackageFileNameSuperH := ToolchainBaseDirectorySuperH
      + GetStampFromPackageFileName(ToolchainSuperH.Packages.Legacy);
    OldStablePackageFileNameSuperH := ToolchainBaseDirectorySuperH
      + GetStampFromPackageFileName(ToolchainSuperH.Packages.OldStable);
    StablePackageFileNameSuperH := ToolchainBaseDirectorySuperH
      + GetStampFromPackageFileName(ToolchainSuperH.Packages.Stable);

    // Extracting package names for Arm
    LegacyPackageFileNameArm := ToolchainBaseDirectoryArm
      + GetStampFromPackageFileName(ToolchainArm.Packages.Legacy);
    OldStablePackageFileNameArm := ToolchainBaseDirectoryArm
      + GetStampFromPackageFileName(ToolchainArm.Packages.OldStable);
    StablePackageFileNameArm := ToolchainBaseDirectoryArm
      + GetStampFromPackageFileName(ToolchainArm.Packages.Stable);
  end;

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

  // Legacy (the worst)
  if FileExists(LegacyPackageFileNameSuperH) or FileExists(LegacyPackageFileNameArm) then
    fAutoDetectedToolchain := pmrtLegacy;

  // Old Stable (intermediate)
  if FileExists(OldStablePackageFileNameSuperH) or FileExists(OldStablePackageFileNameArm) then
    fAutoDetectedToolchain := pmrtOldStable;

  // Stable (the best)
  if FileExists(StablePackageFileNameSuperH) or FileExists(StablePackageFileNameArm) then
    fAutoDetectedToolchain := pmrtStable;

  // Final result
  Result := (fAutoDetectedToolchain <> pmrtUndefined);
end;

function TPackageManager.AutoDetectRequiredDebugger: Boolean;
var
  ToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain;
  ToolchainBaseDirectorySuperH,
  StampFileName: TFileName;
  PropertyInfo: PPropInfo;
  PropertiesList: PPropList;
  PropertiesCount,
  i: Integer;
  PropertyName,
  PropertyType,
  PropertyValue: string;

begin
  Result := False;
  fAutoDetectedDebugger := pmrdUndefined;

{$IFDEF DEBUG}
  DebugLog('IsPackageManagerInstallationRequestRequiredDebugger');
{$ENDIF}

  // We will use only SuperH toolchain
  ToolchainSuperH := fManager.Environment.FileSystem.ToolchainSuperH;

  // Extracting base directories for Super-H
  ToolchainBaseDirectorySuperH := ToolchainSuperH.BaseDirectory;

  (*
    This will use all published properties from the object:
    TDreamcastSoftwareDevelopmentFileSystemToolchainPackagesDebugger
  *)

  try

    // Checking for all ".stamp" files for each Python build
    PropertiesCount := GetPropList(ToolchainSuperH.Packages.Debugger.ClassInfo, PropertiesList);
    for i := 0 to PropertiesCount - 1 do
    begin
      PropertyInfo := PropertiesList^[i];
      if Assigned(PropertyInfo) then
      begin
        PropertyName := PropertyInfo^.Name;
        PropertyType := PropertyInfo^.PropType^.Name;

        // We take into account only properties of TFileName type
        if (PropertyInfo^.PropType^.Kind in tkProperties) and (PropertyType = 'TFileName') then
        begin
          PropertyValue := VarToStr(GetPropValue(ToolchainSuperH.Packages.Debugger, PropertyName));
          StampFileName := ToolchainBaseDirectorySuperH
            + GetStampFromPackageFileName(PropertyValue);

{$IFDEF DEBUG}
          DebugLog(
            Format('* Name="%s", Value="%s"' + sLineBreak
              + '  Stamp = "%s"', [
              PropertyName,
              PropertyValue,
              StampFileName
            ])
          );
{$ENDIF}

          // If the ".stamp" file is present, then we assign RequestedDebugger
          if FileExists(StampFileName) then
          begin
            fAutoDetectedDebugger := TPackageManagerRequestDebugger(
              GetEnumValue(TypeInfo(TPackageManagerRequestDebugger), 'pmrd' + PropertyName)
            );
            KillFile(StampFileName);
          end;

        end; // PropertyInfo + TFileName
      end; // Assigned(PropertyInfo)
    end; // for

  finally
    FreeMem(PropertiesList);
  end;

  Result := (fAutoDetectedDebugger <> pmrdUndefined);
end;

procedure TPackageManager.InitializeOperations;
var
  DebuggerPackage: TFileName;

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
    case Toolchain of
      pmrtLegacy:
        begin
          Add(FileSystem.ToolchainARM.Packages.Legacy, FileSystem.ToolchainBase);
          Add(FileSystem.ToolchainSuperH.Packages.Legacy, FileSystem.ToolchainBase);
        end;	
      pmrtOldStable:
        begin
          Add(FileSystem.ToolchainARM.Packages.OldStable, FileSystem.ToolchainBase);
          Add(FileSystem.ToolchainSuperH.Packages.OldStable, FileSystem.ToolchainBase);
        end;	
      pmrtStable:
        begin
          Add(FileSystem.ToolchainARM.Packages.Stable, FileSystem.ToolchainBase);
          Add(FileSystem.ToolchainSuperH.Packages.Stable, FileSystem.ToolchainBase);
        end;
    end;
  end;

  // Toolchain and/or Debugger
  if fOperation <> pmrOffline then
  begin
    DebuggerPackage := EmptyStr;
    case Debugger of
      pmrdPythonDisabled:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.PythonDisabled;
      pmrdPython27:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python27;
      pmrdPython33:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python33;
      pmrdPython34:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python34;
      pmrdPython35:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python35;
      pmrdPython36:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python36;
      pmrdPython37:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python37;
      pmrdPython38:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python38;
      pmrdPython39:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python39;
      pmrdPython310:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python310;
      pmrdPython311:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python311;
      pmrdPython312:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python312;	
    end;
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

