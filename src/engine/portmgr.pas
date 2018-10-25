unit PortMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  TKallistiPortUpdateState = (usUndefined, usUpdated, usUpdateNotNeeded, usUpdateFailed);

  TKallistiPortsManager = class;

  { TKallistiPortItem }
  TKallistiPortItem = class(TObject)
  private
    fOwner: TKallistiPortsManager;
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
    constructor Create(AOwner: TKallistiPortsManager);
    function Install(var BufferOutput: string): Boolean;
    function Uninstall(var BufferOutput: string): Boolean;
    function Update(var BufferOutput: string): TKallistiPortUpdateState;
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

  { TKallistiPortsManager }
  TKallistiPortsManager = class(TObject)
  private
    fList: TList;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetCount: Integer;
    function GetItem(Index: Integer): TKallistiPortItem;
    function Add: TKallistiPortItem;
    procedure Clear;
    procedure ProcessPort(const PackagingDescriptionFilename: TFileName);
    procedure RetrieveAvailablePorts;
  public
    constructor Create(Environment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;
    property Count: Integer read GetCount;
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

function TKallistiPortItem.ExecuteShellCommand(
  const CommandLine: string): string;
begin
  Result := Environment.ExecuteShellCommand(CommandLine, Directory);
end;

function TKallistiPortItem.GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
begin
  Result := fOwner.fEnvironment;
end;

constructor TKallistiPortItem.Create(AOwner: TKallistiPortsManager);
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

function TKallistiPortItem.Update(var BufferOutput: string): TKallistiPortUpdateState;
const
  SUCCESS_TAG = '%s is already installed';

var
  SuccessTag: string;

begin
  Result := usUndefined;

  SuccessTag := Format(SUCCESS_TAG, [Name]);

  if Install(BufferOutput) then
    Result := usUpdated
  else
    if IsInString(SuccessTag, BufferOutput) then
      Result := usUpdateNotNeeded
    else
      Result := usUpdateFailed;
end;

{ TKallistiPortsManager }

procedure TKallistiPortsManager.RetrieveAvailablePorts;
var
  PortsAvailable: TStringList;
  i: Integer;

begin
  PortsAvailable := TStringList.Create;
  try
    FindAllFiles(PortsAvailable, fEnvironment.FileSystem.KallistiPorts, 'pkg-descr', True);
    for i := 0 to PortsAvailable.Count - 1 do
      ProcessPort(PortsAvailable[i]);
  finally
    PortsAvailable.Free;
  end;
end;

function TKallistiPortsManager.GetItem(Index: Integer): TKallistiPortItem;
begin
  Result := TKallistiPortItem(fList[Index]);
end;

function TKallistiPortsManager.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TKallistiPortsManager.Add: TKallistiPortItem;
begin
  Result := TKallistiPortItem.Create(Self);
  fList.Add(Result);
end;

procedure TKallistiPortsManager.Clear;
var
  i: Integer;

begin
  for i := 0 to fList.Count - 1 do
    TKallistiPortItem(fList[i]).Free;
  fList.Clear;
end;

procedure TKallistiPortsManager.ProcessPort(const PackagingDescriptionFilename: TFileName);
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
    MakefileContent.LoadFromFile(PortDirectory + 'Makefile');
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

constructor TKallistiPortsManager.Create(
  Environment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := Environment;
  fList := TList.Create;
  RetrieveAvailablePorts;
end;

destructor TKallistiPortsManager.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

end.

