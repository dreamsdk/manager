unit GetVer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  { TComponentName }
  TComponentName = (
    cnGit,
    cnSVN,
    cnPython,
    cnMinGW,
    cnBinutils,
    cnGCC,
    cnGDB,
    cnNewlib,
    cnToolSerial,
    cnToolIP,
    cnKallistiOS,
    cnBinutilsARM,
    cnGCCARM
  );

  { TToolchainVersion }
  TToolchainVersion = class(TObject)
  private
    fKind: TToolchainKind;
    fVersionBinutils: string;
    fVersionGCC: string;
    fVersionGDB: string;
    fVersionNewlib: string;
  public
    constructor Create(ToolchainKind: TToolchainKind);
    property Binutils: string read fVersionBinutils;
    property GCC: string read fVersionGCC;
    property GDB: string read fVersionGDB;
    property Newlib: string read fVersionNewlib;
    property Kind: TToolchainKind read fKind;
  end;

  { TComponentVersion }
  TComponentVersion = class(TObject)
  private
    fBuildDateKallistiOS: TDateTime;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fToolchainARM: TToolchainVersion;
    fToolchainSuperH: TToolchainVersion;
    fVersionGit: string;
    fVersionKallistiOS: string;
    fChangeLogKallistiOS: string;
    fVersionMinGW: string;
    fVersionPython: string;
    fVersionSVN: string;
    fVersionToolIP: string;
    fVersionToolSerial: string;
    function RetrieveVersion(Executable, CommandLine,
      StartTag, EndTag: string): string;
    function RetrieveVersionWithFind(FindTargetFileName: TFileName;
      StartTag, EndTag: string): string;
  protected
    function RetrieveKallistiVersion: string;
    function RetrieveKallistiBuildDate: TDateTime;
    procedure RetrieveKallistiInformation;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;

    function GetComponentVersion(const ComponentName: TComponentName): string;
    procedure RetrieveVersions;

    property Git: string read fVersionGit;
    property MinGW: string read fVersionMinGW;
    property SVN: string read fVersionSVN;
    property Python: string read fVersionPython;
    property ToolSerial: string read fVersionToolSerial;
    property ToolIP: string read fVersionToolIP;
    property KallistiOS: string read fVersionKallistiOS;
    property KallistiBuildDate: TDateTime read fBuildDateKallistiOS;
    property KallistiChangeLog: string read fChangeLogKallistiOS;
    property ToolchainSuperH: TToolchainVersion read fToolchainSuperH;
    property ToolchainARM: TToolchainVersion read fToolchainARM;
  end;

function ComponentNameToString(const ComponentName: TComponentName): string;
function IsVersionValid(const Version: string): Boolean;

implementation

uses
  Forms,
  SysTools;

const
  INVALID_VERSION = '(##INVALID##)';
  DEVELOPMENT_VERSION_SUFFIX = '-dev';

function ComponentNameToString(const ComponentName: TComponentName): string;
const
  COMPONENTS_NAME: array[0..12] of string = (
    'Git',
    'SVN',
    'Python',
    'MinGW',
    'Binutils',
    'GCC',
    'GDB',
    'Newlib',
    'ToolSerial',
    'ToolIP',
    'KallistiOS',
    'BinutilsARM',
    'GCCARM'
  );

begin
  Result := COMPONENTS_NAME[Integer(ComponentName)];
end;

function IsVersionValid(const Version: string): Boolean;
begin
  Result := not IsInString(INVALID_VERSION, Version);
end;

{ TToolchainVersion }

constructor TToolchainVersion.Create(ToolchainKind: TToolchainKind);
begin
  fKind := ToolchainKind;
end;

{ TComponentVersion }

function TComponentVersion.RetrieveVersion(Executable, CommandLine,
  StartTag, EndTag: string): string;
var
  Buffer: string;

begin
  try
    Buffer := Run(Executable, CommandLine);
    Result := Trim(ExtractStr(StartTag, EndTag, Buffer));
  except
    Result := INVALID_VERSION;
  end;
end;

function TComponentVersion.RetrieveVersionWithFind(
  FindTargetFileName: TFileName; StartTag, EndTag: string): string;
var
  CommandLine: string;

begin
  CommandLine := Format('"%s" %s', [StartTag, FindTargetFileName]);
  Result := RetrieveVersion('find', CommandLine, StartTag, EndTag);
  if (Result = '') then
    Result := INVALID_VERSION;
end;

function TComponentVersion.RetrieveKallistiVersion: string;
const
  START_TAG = 'KallistiOS ';
  END_TAG = ':';

begin
  Result := RetrieveVersionWithFind(Environment.FileSystem.Kallisti.KallistiLibrary,
    START_TAG, END_TAG);
  if IsInString(START_TAG, Result) then
    Result := Right(START_TAG, Result)
  else
    Result := INVALID_VERSION;
end;

function TComponentVersion.RetrieveKallistiBuildDate: TDateTime;
const
  c_UnassignedDate = -693594;

var
  BuildDate: Integer;

begin
  Result := c_UnassignedDate;
  BuildDate := FileAge(Environment.FileSystem.Kallisti.KallistiLibrary);
  if BuildDate <> -1 then
    Result := FileDateToDateTime(BuildDate);
end;

procedure TComponentVersion.RetrieveKallistiInformation;
begin
  with Environment.FileSystem.Kallisti do
  begin
    fVersionKallistiOS := RetrieveKallistiVersion;
    fBuildDateKallistiOS := RetrieveKallistiBuildDate;
    fChangeLogKallistiOS := INVALID_VERSION;
    if FileExists(KallistiLibrary) then
    begin
      fChangeLogKallistiOS := RetrieveVersionWithFind(KallistiChangeLogFile,
        'KallistiOS version ', ' -----');
      if IsVersionValid(fChangeLogKallistiOS) then
        fChangeLogKallistiOS := fChangeLogKallistiOS + DEVELOPMENT_VERSION_SUFFIX;
    end;
  end;
end;

procedure TComponentVersion.RetrieveVersions;

  procedure RetrieveVersionToolchain(Version: TToolchainVersion;
    Environment: TDreamcastSoftwareDevelopmentFileSystemToolchain);
  begin
    with Environment do
    begin
      Version.fVersionBinutils := RetrieveVersion(BinutilsExecutable,
        '--version', ' (GNU Binutils)', sLineBreak);
      Version.fVersionGCC := RetrieveVersion(GCCExecutable,
        '--version', ' (GCC)', sLineBreak);
      if Environment.Kind = tkSuperH then
      begin
        Version.fVersionGDB := RetrieveVersion(GDBExecutable,
          '--version', ' (GDB)', sLineBreak);
        Version.fVersionNewlib := RetrieveVersionWithFind(NewlibBinary,
          '/dc-chain/newlib-', '/newlib/libc/');
      end;
    end;
  end;

begin
  with Environment.FileSystem do
  begin
    fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak);
    fVersionSVN := RetrieveVersion('svn', '--version', 'svn, version', sLineBreak);
    fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak);
    fVersionMinGW := RetrieveVersion(Shell.MinGWGetExecutable,
      '--version', 'mingw-get version', sLineBreak);

    RetrieveVersionToolchain(fToolchainSuperH, ToolchainSuperH);
    RetrieveVersionToolchain(fToolchainARM, ToolchainARM);

    fVersionToolSerial := RetrieveVersion(DreamcastTool.SerialExecutable,
      '-h', 'dc-tool', 'by <');
    fVersionToolIP := RetrieveVersion(DreamcastTool.InternetProtocolExecutable,
      '-h', 'dc-tool-ip', 'by <');

    RetrieveKallistiInformation;
  end;
end;

constructor TComponentVersion.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
  fToolchainSuperH := TToolchainVersion.Create(tkSuperH);
  fToolchainARM := TToolchainVersion.Create(tkARM);
  RetrieveVersions;
end;

destructor TComponentVersion.Destroy;
begin
  fToolchainSuperH.Free;
  fToolchainARM.Free;
  inherited Destroy;
end;

function TComponentVersion.GetComponentVersion(
  const ComponentName: TComponentName): string;
begin
  Result := '';
  case ComponentName of
    cnGit:
      Result := fVersionGit;
    cnSVN:
      Result := fVersionSVN;
    cnPython:
      Result := fVersionPython;
    cnMinGW:
      Result := fVersionMinGW;
    cnBinutils:
      Result := fToolchainSuperH.fVersionBinutils;
    cnGCC:
      Result := fToolchainSuperH.fVersionGCC;
    cnGDB:
      Result := fToolchainSuperH.fVersionGDB;
    cnNewlib:
      Result := fToolchainSuperH.fVersionNewlib;
    cnToolSerial:
      Result := fVersionToolSerial;
    cnToolIP:
      Result := fVersionToolIP;
    cnKallistiOS:
      Result := fVersionKallistiOS;
    cnBinutilsARM:
      Result := fToolchainARM.fVersionBinutils;
    cnGCCARM:
      Result := fToolchainARM.fVersionGCC;
  end;
end;

end.

