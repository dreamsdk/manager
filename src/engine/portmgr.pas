unit PortMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  TKallistiPortManager = class;

  { TKallistiPortItem }
  TKallistiPortItem = class(TObject)
  private
    fOwner: TKallistiPortManager;
    fDirectory: TFileName;
    fDescription: string;
    fLicense: string;
    fMaintainer: string;
    fName: string;
    fShortDescription: string;
    fURL: string;
    fVersion: string;
    function GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function IsPortInstalled: Boolean;
  protected
    function ExecuteShellCommand(const CommandLine: string): string;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read GetEnvironment;
  public
    constructor Create(AOwner: TKallistiPortManager);
    function Install(var BufferOutput: string): Boolean;
    function Uninstall(var BufferOutput: string): Boolean;
    function Update(var BufferOutput: string): TUpdateOperationState;
    property Name: string read fName;
    property Description: string read fDescription;
    property Directory: TFileName read fDirectory;
    property Installed: Boolean read IsPortInstalled;
    property Maintainer: string read fMaintainer;
    property License: string read fLicense;
    property ShortDescription: string read fShortDescription;
    property URL: string read fURL;
    property Version: string read fVersion;
  end;

  { TKallistiPortManager }
  TKallistiPortManager = class(TObject)
  private
    fList: TList;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetCount: Integer;
    function GetInstalled: Boolean;
    function GetItem(Index: Integer): TKallistiPortItem;
    function Add: TKallistiPortItem;
    procedure Clear;
    procedure ProcessPort(const PackagingDescriptionFilename: TFileName);
  public
    constructor Create(Environment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;
    function CloneRepository(var BufferOutput: string): Boolean;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    procedure RetrieveAvailablePorts;
    property Count: Integer read GetCount;
    property Installed: Boolean read GetInstalled;
    property Items[Index: Integer]: TKallistiPortItem read GetItem; default;
  end;

implementation

uses
  FileUtil, SysTools;

{ TKallistiPortItem }

function TKallistiPortItem.IsPortInstalled: Boolean;
begin
  Result := FileExists(fDirectory + '..\lib\.kos-ports\' + Name);
end;

function TKallistiPortItem.ExecuteShellCommand(const CommandLine: string): string;
begin
  Result := Environment.ExecuteShellCommand(CommandLine, Directory);
end;

function TKallistiPortItem.GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
begin
  Result := fOwner.fEnvironment;
end;

constructor TKallistiPortItem.Create(AOwner: TKallistiPortManager);
begin
  fOwner := AOwner;
end;

function TKallistiPortItem.Install(var BufferOutput: string): Boolean;
const
  SUCCESS_TAG = 'Marking %s %s as installed.';

var
  SuccessTag: string;

begin
  SuccessTag := Format(SUCCESS_TAG, [Name, Version]);
  BufferOutput := ExecuteShellCommand('make install');
  ExecuteShellCommand('make clean');
  Result := IsInString(SuccessTag, BufferOutput);
end;

function TKallistiPortItem.Uninstall(var BufferOutput: string): Boolean;
const
  SUCCESS_TAG = 'Uninstalled %s.';

var
  SuccessTag: string;

begin
  SuccessTag := Format(SUCCESS_TAG, [Name]);
  BufferOutput := ExecuteShellCommand('make uninstall');
  Result := IsInString(SuccessTag, BufferOutput);
end;

function TKallistiPortItem.Update(var BufferOutput: string): TUpdateOperationState;
const
  SUCCESS_TAG = '%s is already installed';

var
  SuccessTag: string;

begin
  Result := uosUndefined;

  SuccessTag := Format(SUCCESS_TAG, [Name]);

  if Install(BufferOutput) then
    Result := uosUpdateSuccess
  else
    if IsInString(SuccessTag, BufferOutput) then
      Result := uosUpdateUseless
    else
      Result := uosUpdateFailed;
end;

{ TKallistiPortManager }

procedure TKallistiPortManager.RetrieveAvailablePorts;
const
  KALLISTI_PORTS_PACKAGE_DESCRIPTION = 'pkg-descr';

var
  PortsAvailable: TStringList;
  i: Integer;

begin
  Clear;
  PortsAvailable := TStringList.Create;
  try
    FindAllFiles(PortsAvailable, fEnvironment.FileSystem.Kallisti.KallistiPortsDirectory,
      KALLISTI_PORTS_PACKAGE_DESCRIPTION, True);
    for i := 0 to PortsAvailable.Count - 1 do
      ProcessPort(PortsAvailable[i]);
  finally
    PortsAvailable.Free;
  end;
end;

function TKallistiPortManager.GetItem(Index: Integer): TKallistiPortItem;
begin
  Result := TKallistiPortItem(fList[Index]);
end;

function TKallistiPortManager.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TKallistiPortManager.GetInstalled: Boolean;
begin
  Result := DirectoryExists(fEnvironment.FileSystem.Kallisti.KallistiPortsDirectory);
end;

function TKallistiPortManager.Add: TKallistiPortItem;
begin
  Result := TKallistiPortItem.Create(Self);
  fList.Add(Result);
end;

procedure TKallistiPortManager.Clear;
var
  i: Integer;

begin
  for i := 0 to fList.Count - 1 do
    TKallistiPortItem(fList[i]).Free;
  fList.Clear;
end;

procedure TKallistiPortManager.ProcessPort(const PackagingDescriptionFilename: TFileName);
const
  MAKEFILE_FILE_NAME = 'Makefile';

var
  MakefileContent: TStringList;
  MakefileContentText, ExtractedURL: string;
  PortDirectory: TFileName;

  function GetPackageString(const Key: string): string;
  var
    S: string;

  begin
    Result := '';
    S := Right(Key, MakefileContentText);
    if S <> '' then
      Result := Trim(ExtractStr('=', #10, S));
  end;

  function SanitizeText(Text: string): string;
  begin
    Text := StringReplace(Text, #13#10, #10, [rfReplaceAll]);
    Text := StringReplace(Text, #13, #10, [rfReplaceAll]);
    Text := StringReplace(Text, #9, '', [rfReplaceAll]);
    Result := Text;
  end;

  function GetPackageDescription(var URL: string): string;
  const
    URL_TAG = ' URL: ';

  var
    DescriptionContent: TStringList;

  begin
    Result := '';
    DescriptionContent := TStringList.Create;
    try
      if FileExists(PackagingDescriptionFilename) then
      begin
        DescriptionContent.LoadFromFile(PackagingDescriptionFilename);
        Result := SanitizeText(DescriptionContent.Text);
        Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
        Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
        if IsInString(URL_TAG, Result) then
        begin
          URL := Trim(Right(URL_TAG, Result));
          Result := Left(URL_TAG, Result);
        end;
        Result := Trim(Result);
      end;
    finally
      DescriptionContent.Free;
    end;
  end;

begin
  PortDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(PackagingDescriptionFilename));
  MakefileContent := TStringList.Create;
  try
    MakefileContent.LoadFromFile(PortDirectory + MAKEFILE_FILE_NAME);
    MakefileContentText := SanitizeText(MakefileContent.Text);

    with Add do
    begin
      fDirectory := PortDirectory;
      fName := GetPackageString('PORTNAME');
      fVersion := GetPackageString('PORTVERSION');
      fMaintainer := GetPackageString('MAINTAINER');
      fLicense := GetPackageString('LICENSE');
      fShortDescription := GetPackageString('SHORT_DESC');
      fDescription := GetPackageDescription(ExtractedURL);
      fURL := ExtractedURL;
    end;
  finally
    MakefileContent.Free;
  end;
end;

constructor TKallistiPortManager.Create(
  Environment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := Environment;
  fList := TList.Create;
  RetrieveAvailablePorts;
end;

destructor TKallistiPortManager.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

function TKallistiPortManager.CloneRepository(var BufferOutput: string): Boolean;
const
  KALLISTI_PORTS_INSTALLATION_DIRECTORY = 'kos-ports';

begin
  Result := fEnvironment.CloneRepository(fEnvironment.Repositories.KallistiPortsURL,
    KALLISTI_PORTS_INSTALLATION_DIRECTORY,
    fEnvironment.FileSystem.Kallisti.KallistiPortsDirectory + '..\', BufferOutput);
end;

function TKallistiPortManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := fEnvironment.UpdateRepository(fEnvironment.FileSystem.Kallisti.KallistiPortsDirectory, BufferOutput);
end;

function TKallistiPortManager.InitializeEnvironment: Boolean;
const
  CONFIG_MAKEFILE = 'config.mk';

var
  MakefileFileName: TFileName;

begin
  MakefileFileName := fEnvironment.FileSystem.Kallisti.KallistiPortsDirectory
    + CONFIG_MAKEFILE;
  Result := FileExists(MakefileFileName);

  if Result then
  begin
    fEnvironment.PatchMakefile(
      MakefileFileName,
      '#FETCH_CMD = wget',
      'FETCH_CMD = wget --no-check-certificate'
    );

    fEnvironment.PatchMakefile(
      MakefileFileName,
      'FETCH_CMD = curl',
      '#FETCH_CMD = curl'
    );
  end;
end;

end.

