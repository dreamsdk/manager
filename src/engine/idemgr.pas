unit IDEMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  TIntegratedDevelopmentEnvironmentKind = (ideNone, ideCodeBlocks);

  { TIntegratedDevelopmentEnvironment }
  TIntegratedDevelopmentEnvironment = class(TObject)
  private
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fConfigurationFileName: TFileName;
    fExportLibraryInformation: Boolean;
    fExportLibraryInformationPath: TFileName;
    fKind: TIntegratedDevelopmentEnvironmentKind;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    procedure ProcessConfigurationFile;
    property Kind: TIntegratedDevelopmentEnvironmentKind read fKind;
    property ExportLibraryInformation: Boolean read fExportLibraryInformation;
    property ExportLibraryInformationPath: TFileName read fExportLibraryInformationPath;
  end;

implementation

uses
  IniFiles, SysTools;

{ TIntegratedDevelopmentEnvironment }

constructor TIntegratedDevelopmentEnvironment.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
  fConfigurationFileName := fEnvironment.FileSystem.Shell.IntegratedDevelopmentEnvironmentConfigurationFile;
  ProcessConfigurationFile;
end;

procedure TIntegratedDevelopmentEnvironment.ProcessConfigurationFile;
var
  IniFile: TIniFile;

begin
  if FileExists(fConfigurationFileName) then
  begin
    IniFile := TIniFile.Create(fConfigurationFileName);
    try
      fKind := TIntegratedDevelopmentEnvironmentKind(IniFile.ReadInteger('IDE', 'Kind', 0));
      fExportLibraryInformation := IniFile.ReadBool('IDE', 'ExportLibraryInformation', False);
      fExportLibraryInformationPath := IniFile.ReadString('IDE', 'ExportLibraryInformationPath', EmptyStr);
      if not SameText(fExportLibraryInformationPath, EmptyStr) then
      begin
        fExportLibraryInformationPath := IncludeTrailingPathDelimiter(fExportLibraryInformationPath);
        ForceDirectories(fExportLibraryInformationPath);
        SetDirectoryRights(fExportLibraryInformationPath, EVERYONE_GROUP_SID, ACL_RIGHT_FULL);
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

end.

