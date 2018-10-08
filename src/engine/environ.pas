unit Environ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TDreamcastSoftwareDevelopmentEnvironment }
  TDreamcastSoftwareDevelopmentEnvironment = class(TObject)
  private
    fApplicationPath: TFileName;
    fDCToolIPExecutable: TFileName;
    fDCToolSerialExecutable: TFileName;
    fGCCExecutable: TFileName;
    fGDBExecutable: TFileName;
    fKallistiOS: TFileName;
    fKallistiOSVersionFile: TFileName;
    fKallistiPorts: TFileName;
    fNewlibBinary: TFileName;
    fInstallPath: string;

    fMSYSExecutable: TFileName;
    fMinGWGetExecutable: TFileName;
    fBinutilsExecutable: TFileName;

    fUseMintty: Boolean;
    function GetApplicationPath: TFileName;
    function GetConfigurationFileName: TFileName;
    procedure LoadConfig;
    procedure SaveConfig;
  public
    constructor Create;
    destructor Destroy; override;

    property InstallPath: string
      read fInstallPath;

    property MSYSExecutable: TFileName
      read fMSYSExecutable write fMSYSExecutable;

    property MinGWGetExecutable: TFileName
      read fMinGWGetExecutable write fMinGWGetExecutable;

    property BinutilsExecutable: TFileName
      read fBinutilsExecutable write fBinutilsExecutable;
    property GCCExecutable: TFileName
      read fGCCExecutable write fGCCExecutable;
    property GDBExecutable: TFileName
      read fGDBExecutable write fGDBExecutable;

    property NewlibBinary: TFileName
      read fNewlibBinary write fNewlibBinary;

    property DreamcastToolSerialExecutable: TFileName
      read fDCToolSerialExecutable write fDCToolSerialExecutable;
    property DreamcastToolIPExecutable: TFileName
      read fDCToolIPExecutable write fDCToolIPExecutable;

    property KallistiPorts: TFileName
      read fKallistiPorts write fKallistiPorts;

    property KallistiOS: TFileName
      read fKallistiOS write fKallistiOS;

    property KallistiOSVersionFile: TFileName
      read fKallistiOSVersionFile write fKallistiOSVersionFile;

    property UseMintty: Boolean
      read fUseMintty write fUseMintty;
  end;

implementation

uses
  IniFiles;

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
  MSYSBase,
  ToolchainBase: TFileName;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    fInstallPath := IncludeTrailingPathDelimiter(
      IniFile.ReadString('General', 'InstallPath',
      ExpandFileName(GetApplicationPath + '..\..\..\..\')));

    MSYSBase := fInstallPath + 'msys\1.0\';

    ToolchainBase := MSYSBase + 'opt\toolchains\dc\';

    fMSYSExecutable := IniFile.ReadString('General', 'MSYSExecutable', MSYSBase + 'msys.bat');
    fMinGWGetExecutable := IniFile.ReadString('General', 'MinGWGetExecutable', fInstallPath + 'bin\mingw-get.exe');
    fBinutilsExecutable := IniFile.ReadString('General', 'BinutilsExecutable', ToolchainBase + 'sh-elf\bin\sh-elf-ld.exe');
    fGCCExecutable := IniFile.ReadString('General', 'GCCExecutable', ToolchainBase + 'sh-elf\bin\sh-elf-gcc.exe');
    fGDBExecutable := IniFile.ReadString('General', 'GDBExecutable', ToolchainBase + 'sh-elf\bin\sh-elf-gdb.exe');

    fNewlibBinary := IniFile.ReadString('General', 'NewlibBinary', ToolchainBase + 'sh-elf\sh-elf\lib\libnosys.a');

    fDCToolSerialExecutable := ToolchainBase + 'bin\dc-tool.exe';
    fDCToolIPExecutable := ToolchainBase + 'bin\dc-tool-ip.exe';

    fKallistiPorts := IniFile.ReadString('General', 'KallistiPorts', ToolchainBase + 'kos-ports\');
    fKallistiOS := IniFile.ReadString('General', 'KallistiOS', ToolchainBase + 'kos\');
    fKallistiOSVersionFile := IniFile.ReadString('General', 'KallistiOSVersionFile', KallistiOS + 'doc\CHANGELOG');

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

    IniFile.WriteString('General', 'MSYSExecutable', fMSYSExecutable);
    IniFile.WriteString('General', 'MinGWGetExecutable', fMinGWGetExecutable);
    IniFile.WriteString('General', 'BinutilsExecutable', fBinutilsExecutable);
    IniFile.WriteString('General', 'GCCExecutable', fGCCExecutable);
    IniFile.WriteString('General', 'GDBExecutable', fGDBExecutable);

    IniFile.WriteString('General', 'NewlibBinary', fNewlibBinary);
    IniFile.WriteString('General', 'DCToolSerialExecutable', fDCToolSerialExecutable);
    IniFile.WriteString('General', 'DCToolIPExecutable', fDCToolIPExecutable);

    IniFile.WriteString('General', 'KallistiPorts', fKallistiPorts);
    IniFile.WriteString('General', 'KallistiOS', fKallistiOS);
    IniFile.WriteString('General', 'KallistiOSVersionFile', fKallistiOSVersionFile);

    IniFile.WriteBool('General', 'UseMinTTY', fUseMintty);
  finally
    IniFile.Free;
  end;
end;

constructor TDreamcastSoftwareDevelopmentEnvironment.Create;
begin
  LoadConfig;
end;

destructor TDreamcastSoftwareDevelopmentEnvironment.Destroy;
begin
  SaveConfig;
  inherited Destroy;
end;

end.

