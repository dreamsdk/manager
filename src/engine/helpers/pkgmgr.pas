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
  TPackageManagerTerminateEvent = procedure(Sender: TObject;
    const Success: Boolean; const Aborted: Boolean) of object;

  TPackageManagerRequest = (
    pmrUndefined,
    pmrToolchain,
    pmrDebugger,
    pmrOffline
  );

  TPackageManagerRequestOffline = (
    pmroKallisti,
    pmroKallistiPorts,
    pmroDreamcastToolSerial,
    pmroDreamcastToolInternetProtocol,
    pmroRuby
  );

  TPackageManagerRequestToolchain = (
    pmrtStable,
    pmrtExperimental
  );

  TPackageManagerRequestDebugger = (
    pmrdPythonDisabled,
    pmrdPython27,
    pmrdPython34,
    pmrdPython35,
    pmrdPython36,
    pmrdPython37,
    pmrdPython38,
    pmrdPython39
  );

  { TPackageManager }
  TPackageManager = class(TObject)
  private
    fAborted: Boolean;
    fOffline: TPackageManagerRequestOffline;
    fSuccessOperation: Boolean;
    fDebugger: TPackageManagerRequestDebugger;
    fManager: TDreamcastSoftwareDevelopmentKitManager;
    fOperation: TPackageManagerRequest;
    fSevenZipCommander: TSevenZipCommander;
    fTerminate: TPackageManagerTerminateEvent;
    fToolchain: TPackageManagerRequestToolchain;
    function GetFileSystem: TDreamcastSoftwareDevelopmentFileSystem;
    function GetRunning: Boolean;
    procedure HandleProgress(Sender: TObject; const CurrentValue: Integer;
      const TotalValue: Integer);
    procedure HandleProgressRecord(Sender: TObject; const RecordNode: string);
    procedure HandleTerminate(Sender: TObject; const Success: Boolean);
    procedure ShowUnpackWindow;
  protected
    procedure Add(const APackageFileName: TFileName;
      const AOutputDirectory: TFileName);
    procedure InitializeOperations;
    property FileSystem: TDreamcastSoftwareDevelopmentFileSystem
      read GetFileSystem;
  public
    constructor Create(AManager: TDreamcastSoftwareDevelopmentKitManager);
    destructor Destroy; override;
    procedure Abort;
    procedure Execute;
    procedure Pause;
    procedure Resume;
    property Debugger: TPackageManagerRequestDebugger
      read fDebugger write fDebugger;
    property OfflinePackage: TPackageManagerRequestOffline
      read fOffline write fOffline;
    property Operation: TPackageManagerRequest read fOperation write fOperation;
    property Toolchain: TPackageManagerRequestToolchain
      read fToolchain write fToolchain;
    property Running: Boolean read GetRunning;
    property OnTerminate: TPackageManagerTerminateEvent read fTerminate
      write fTerminate;
  end;

function IsDebuggerPythonVersionInstalled(const Version: TPackageManagerRequestDebugger;
  var VersionWithDot: string): Boolean;

implementation

function IsDebuggerPythonVersionInstalled(const Version: TPackageManagerRequestDebugger;
  var VersionWithDot: string): Boolean;
var
  i: Integer;
  PythonFileName,
  PythonFilePath: TFileName;
  PythonBitness: TPortableExecutableBitness;
  VersionWithoutDot: string;

begin
  Result := True;

  VersionWithoutDot := EmptyStr;
  i := Integer(Version) - 1;
  if (i <> -1) then
  begin
    Result := False;

    VersionWithDot := SUPPORTED_PYTHON_VERSIONS[i];
    VersionWithoutDot := StringReplace(VersionWithDot, '.', EmptyStr, []);
    PythonFileName := Format('python%s.dll', [VersionWithoutDot]);
    PythonFilePath := GetFileLocationInSystemPath(PythonFileName);

{$IFDEF DEBUG}
    WriteLn('PythonFilePath: ', PythonFilePath);
{$ENDIF}

    if FileExists(PythonFilePath) then
    begin
{$IFDEF DEBUG}
      WriteLn('Python ', VersionWithDot, ' is installed: ', PythonFilePath);
{$ENDIF}
      PythonBitness := GetPortableExecutableBitness(PythonFilePath);
      Result := (PythonBitness = peb32); // 32-bits only
{$IFDEF DEBUG}
      WriteLn('Python ', VersionWithDot, ' bitness: ', PythonBitness);
{$ENDIF}
    end
{$IFDEF DEBUG}
    else
      WriteLn(Format('Python %s is not installed', [VersionWithDot]))
{$ENDIF};
  end;
end;

{ TPackageManager }

procedure TPackageManager.HandleProgress(Sender: TObject;
  const CurrentValue: Integer; const TotalValue: Integer);
begin
  frmUnpack.ProgressValue := TotalValue;
end;

function TPackageManager.GetRunning: Boolean;
begin
  Result := fSevenZipCommander.Active;
end;

function TPackageManager.GetFileSystem: TDreamcastSoftwareDevelopmentFileSystem;
begin
  Result := fManager.Environment.FileSystem;
end;

procedure TPackageManager.HandleProgressRecord(Sender: TObject;
  const RecordNode: string);
begin
  frmUnpack.ProgressText := RecordNode;
end;

procedure TPackageManager.HandleTerminate(Sender: TObject;
  const Success: Boolean);
begin
  fSevenZipCommander.Operations.Clear;
  frmUnpack.Finished := True;
  fSuccessOperation := Success;

  if Success then
    Delay(1000);

  frmUnpack.Close;
  Application.ProcessMessages;

  if Assigned(fTerminate) then
    fTerminate(Self, Success, fAborted);
end;

procedure TPackageManager.ShowUnpackWindow;
begin
  frmUnpack := TfrmUnpack.Create(Application);
  try
    frmUnpack.ShowModal;
//    ATerminateProc(fSuccessOperation);
  finally
    FreeAndNil(frmUnpack);
  end;
end;

constructor TPackageManager.Create(AManager: TDreamcastSoftwareDevelopmentKitManager);
begin
  fManager := AManager;
  fSevenZipCommander := TSevenZipCommander.Create;
  with fSevenZipCommander do
  begin
    OnProgress := @HandleProgress;
    OnProgressRecord := @HandleProgressRecord;
    OnTerminate := @HandleTerminate;
  end;
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
begin
  InitializeOperations;
  fSevenZipCommander.Execute;
  ShowUnpackWindow;
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
      pmrtStable:
        begin
          Add(FileSystem.ToolchainARM.Packages.Stable, FileSystem.ToolchainARM.BaseDirectory);
          Add(FileSystem.ToolchainSuperH.Packages.Stable, FileSystem.ToolchainSuperH.BaseDirectory);
        end;
      pmrtExperimental:
        begin
          Add(FileSystem.ToolchainARM.Packages.Experimental, FileSystem.ToolchainARM.BaseDirectory);
          Add(FileSystem.ToolchainSuperH.Packages.Experimental, FileSystem.ToolchainSuperH.BaseDirectory);
        end;
    end;
  end;

  // Toolchain and/or Debugger
  if fOperation <> pmrOffline then
  begin
    case Debugger of
      pmrdPythonDisabled:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.PythonDisabled;
      pmrdPython27:
        DebuggerPackage := FileSystem.ToolchainSuperH.Packages.Debugger.Python27;
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
    end;
    Add(DebuggerPackage, FileSystem.ToolchainSuperH.BaseDirectory);
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
          FileSystem.DreamcastTool.ResetRepositorySerial;
          Add(FileSystem.DreamcastTool.Packages.Serial, FileSystem.DreamcastTool.SerialDirectory);
        end;
      pmroDreamcastToolInternetProtocol:
        begin
          FileSystem.DreamcastTool.ResetRepositoryInternetProtocol;
          Add(FileSystem.DreamcastTool.Packages.InternetProtocol, FileSystem.DreamcastTool.InternetProtocolDirectory);
        end;
      pmroRuby:
        begin
          FileSystem.Ruby.ResetRepository;
          Add(FileSystem.Ruby.Packages.RubyLibrary, FileSystem.Ruby.BaseDirectory);
        end;
    end;
  end;
end;

end.
