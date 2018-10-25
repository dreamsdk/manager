unit Environ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDreamcastSoftwareDevelopmentFileSystemObject }

  TDreamcastSoftwareDevelopmentFileSystemObject = class(TObject)
  private
    fDCToolIPExecutable: TFileName;
    fDCToolSerialExecutable: TFileName;
    fGCCExecutable: TFileName;
    fGDBExecutable: TFileName;
    fKallistiOS: TFileName;
    fKallistiOSVersionFile: TFileName;
    fKallistiPorts: TFileName;
    fNewlibBinary: TFileName;
    fMSYSExecutable: TFileName;
    fMinGWGetExecutable: TFileName;
    fBinutilsExecutable: TFileName;
    fShellLauncherExecutable: TFileName;
    fToolchainInstalledARM: Boolean;
    fToolchainInstalledSH4: Boolean;
  protected
    procedure ComputeFileSystemObjectValues(InstallPath: TFileName);
  public
    property ShellLauncherExecutable: TFileName read fShellLauncherExecutable;
    property MSYSExecutable: TFileName read fMSYSExecutable;
    property MinGWGetExecutable: TFileName read fMinGWGetExecutable;
    property BinutilsExecutable: TFileName read fBinutilsExecutable;
    property GCCExecutable: TFileName read fGCCExecutable;
    property GDBExecutable: TFileName read fGDBExecutable;
    property NewlibBinary: TFileName read fNewlibBinary;
    property DreamcastToolSerialExecutable: TFileName read fDCToolSerialExecutable;
    property DreamcastToolIPExecutable: TFileName read fDCToolIPExecutable;
    property KallistiPorts: TFileName read fKallistiPorts;
    property KallistiOS: TFileName read fKallistiOS;
    property KallistiOSVersionFile: TFileName read fKallistiOSVersionFile;
    property ToolchainInstalledARM: Boolean read fToolchainInstalledARM;
    property ToolchainInstalledSH4: Boolean read fToolchainInstalledSH4;
  end;

  { TDreamcastSoftwareDevelopmentEnvironment }
  TDreamcastSoftwareDevelopmentEnvironment = class(TObject)
  private
    fApplicationPath: TFileName;
    fFileSystem: TDreamcastSoftwareDevelopmentFileSystemObject;
    fInstallPath: TFileName;
    fUseMintty: Boolean;
    function GetApplicationPath: TFileName;
    function GetConfigurationFileName: TFileName;
    procedure LoadConfig;
    procedure SaveConfig;
  public
    constructor Create;
    destructor Destroy; override;
    function ExecuteShellCommand(CommandLine: string;
      WorkingDirectory: TFileName): string;
    property FileSystem: TDreamcastSoftwareDevelopmentFileSystemObject read fFileSystem;
    property InstallPath: TFileName read fInstallPath;
    property UseMintty: Boolean read fUseMintty write fUseMintty;
  end;

implementation

uses
  IniFiles, SysTools;

{ TDreamcastSoftwareDevelopmentFileSystemObject }

procedure TDreamcastSoftwareDevelopmentFileSystemObject.ComputeFileSystemObjectValues(
  InstallPath: TFileName);
var
  MSYSBase, ToolchainBase: TFileName;
begin
  MSYSBase := InstallPath + 'msys\1.0\';
  ToolchainBase := MSYSBase + 'opt\toolchains\dc\';

  // DreamSDK
  fShellLauncherExecutable := MSYSBase + 'opt\dcsdk\dcsdk.exe';

  // MinGW/MSYS
  fMSYSExecutable := MSYSBase + 'msys.bat';
  fMinGWGetExecutable := InstallPath + 'bin\mingw-get.exe';

  // Toolchain
  fToolchainInstalledARM := DirectoryExists(ToolchainBase + 'arm-eabi');
  fToolchainInstalledSH4 := DirectoryExists(ToolchainBase + 'sh-elf');
  fBinutilsExecutable := ToolchainBase + 'sh-elf\bin\sh-elf-ld.exe';
  fGCCExecutable := ToolchainBase + 'sh-elf\bin\sh-elf-gcc.exe';
  fGDBExecutable := ToolchainBase + 'sh-elf\bin\sh-elf-gdb.exe';
  fNewlibBinary := ToolchainBase + 'sh-elf\sh-elf\lib\libnosys.a';

  // dcload
  fDCToolSerialExecutable := ToolchainBase + 'bin\dc-tool.exe';
  fDCToolIPExecutable := ToolchainBase + 'bin\dc-tool-ip.exe';

  // KallistiOS
  fKallistiPorts := ToolchainBase + 'kos-ports\';
  fKallistiOS := ToolchainBase + 'kos\';
  fKallistiOSVersionFile := KallistiOS + 'doc\CHANGELOG';
end;

{ TDreamcastSoftwareDevelopmentEnvironment }

function TDreamcastSoftwareDevelopmentEnvironment.GetApplicationPath: TFileName;
var
  Path: TFileName;
{$IFDEF Darwin}
  i: Integer;
{$ENDIF}

begin
  if (fApplicationPath = '') then
  begin
    Path := ExtractFilePath(ParamStr(0));
{$IFDEF Darwin}
    i := Pos('.app', Path);
    if i > 0 then
    begin
      i := LastDelimiter('/', Copy(Path, 1, i));
      Path := Copy(Path, 1, i);
    end;
{$ENDIF}
    fApplicationPath := IncludeTrailingPathDelimiter(Path);
  end;
  Result := fApplicationPath;
end;

function TDreamcastSoftwareDevelopmentEnvironment.GetConfigurationFileName: TFileName;
begin
  Result := GetApplicationPath + 'dcsdk.conf';
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.LoadConfig;
var
  IniFile: TIniFile;
  DefaultInstallationPath: TFileName;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    DefaultInstallationPath := ExpandFileName(GetApplicationPath + '..\..\..\..\');
    fInstallPath := IncludeTrailingPathDelimiter(IniFile.ReadString('General',
      'InstallPath', DefaultInstallationPath));
    FileSystem.ComputeFileSystemObjectValues(fInstallPath);
    fUseMintty := IniFile.ReadBool('General', 'UseMinTTY', False);
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.SaveConfig;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    IniFile.WriteString('General', 'InstallPath', fInstallPath);
    IniFile.WriteBool('General', 'UseMinTTY', fUseMintty);
  finally
    IniFile.Free;
  end;
end;

constructor TDreamcastSoftwareDevelopmentEnvironment.Create;
begin
  fFileSystem := TDreamcastSoftwareDevelopmentFileSystemObject.Create;
  LoadConfig;
end;

destructor TDreamcastSoftwareDevelopmentEnvironment.Destroy;
begin
  fFileSystem.Free;
  SaveConfig;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentEnvironment.ExecuteShellCommand(
  CommandLine: string; WorkingDirectory: TFileName): string;
var
  CurrentDir, ShellLauncher: TFileName;

begin
  CurrentDir := GetCurrentDir;
  SetCurrentDir(WorkingDirectory);

  ShellLauncher := FileSystem.ShellLauncherExecutable;
  CommandLine := Format('%s --dcsdk-automated-call', [CommandLine]);
  Result := RunShadow(ShellLauncher, CommandLine);

  SetCurrentDir(CurrentDir);
end;

end.

