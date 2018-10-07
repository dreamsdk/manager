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
    fInstallPath: string;
    fMSYSExecutable: string;
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
    property MSYSExecutable: string
      read fMSYSExecutable write fMSYSExecutable;
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

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    fInstallPath := IniFile.ReadString('General', 'InstallPath', ExpandFileName(GetApplicationPath + '..\..\..\..\'));
    fMSYSExecutable := IniFile.ReadString('General', 'MSYSExecutable', ExpandFileName(GetApplicationPath + '..\..\msys.bat'));
    fUseMintty := IniFile.ReadBool('General', 'UseMintty', False);
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
    IniFile.WriteBool('General', 'UseMintty', fUseMintty);
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

