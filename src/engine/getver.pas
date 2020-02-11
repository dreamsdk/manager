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
    cnPythonGDB,
    cnNewlib,
    cnToolSerial,
    cnToolIP,
    cnKallistiOS,
    cnBinutilsARM,
    cnGCCARM,
    cnBinutilsWin32,
    cnGCCWin32,
    cnGDBWin32
  );

  { TToolchainVersion }
  TToolchainVersion = class(TObject)
  private
    fKind: TToolchainKind;
    fVersionBinutils: string;
    fVersionGCC: string;
    fVersionGDB: string;
    fVersionNewlib: string;
    fVersionPythonGDB: string;
  public
    constructor Create(ToolchainKind: TToolchainKind);
    property Binutils: string read fVersionBinutils;
    property GCC: string read fVersionGCC;
    property GDB: string read fVersionGDB;
    property PythonGDB: string read fVersionPythonGDB;
    property Newlib: string read fVersionNewlib;
    property Kind: TToolchainKind read fKind;
  end;

  { TComponentVersion }
  TComponentVersion = class(TObject)
  private
    fBuildDateKallistiOS: TDateTime;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fPythonInstalled: Boolean;
    fSubversionInstalled: Boolean;
    fToolchainVersionARM: TToolchainVersion;
    fToolchainVersionSuperH: TToolchainVersion;
    fToolchainVersionWin32: TToolchainVersion;
    fVersionGit: string;
    fVersionKallistiOS: string;
    fChangeLogKallistiOS: string;
    fVersionMinGW: string;
    fVersionPython: string;
    fVersionSVN: string;
    fVersionToolIP: string;
    fVersionToolSerial: string;
  protected
    function IsValidVersion(const Version: string): Boolean;
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
    property Subversion: string read fVersionSVN;
    property SubversionInstalled: Boolean read fSubversionInstalled;
    property Python: string read fVersionPython;
    property PythonInstalled: Boolean read fPythonInstalled;
    property ToolSerial: string read fVersionToolSerial;
    property ToolIP: string read fVersionToolIP;
    property KallistiOS: string read fVersionKallistiOS;
    property KallistiBuildDate: TDateTime read fBuildDateKallistiOS;
    property KallistiChangeLog: string read fChangeLogKallistiOS;
    property ToolchainSuperH: TToolchainVersion read fToolchainVersionSuperH;
    property ToolchainARM: TToolchainVersion read fToolchainVersionARM;
    property ToolchainWin32: TToolchainVersion read fToolchainVersionWin32;
  end;

function ComponentNameToString(const ComponentName: TComponentName): string;

implementation

uses
  Forms,
  StrUtils,
  SysTools,
  VerIntf;

const
  DEVELOPMENT_VERSION_SUFFIX = '-dev';

function ComponentNameToString(const ComponentName: TComponentName): string;
const
  COMPONENTS_NAME: array[0..16] of string = (
    'Git',
    'SVN',
    'Python',
    'MinGW',
    'Binutils',
    'GCC',
    'GDB',
    'PythonGDB',
    'Newlib',
    'ToolSerial',
    'ToolIP',
    'KallistiOS',
    'BinutilsARM',
    'GCCARM',
    'BinutilsWin32',
    'GCCWin32',
    'GDBWin32'
  );

begin
  Result := COMPONENTS_NAME[Integer(ComponentName)];
end;

{ TToolchainVersion }

constructor TToolchainVersion.Create(ToolchainKind: TToolchainKind);
begin
  fKind := ToolchainKind;
end;

{ TComponentVersion }

function TComponentVersion.IsValidVersion(const Version: string): Boolean;
begin
  Result := not SameText(Version, INVALID_VERSION);
end;

function TComponentVersion.RetrieveKallistiVersion: string;
const
  START_TAG = 'KallistiOS ';
  END_TAG = ':';

var
  TargetFileName:  TFileName;

begin
  TargetFileName := Environment.FileSystem.Kallisti.KallistiLibrary;
  Result := GetRegisteredVersion(TargetFileName);

  if IsEmpty(Result) then
  begin
    Result := RetrieveVersionWithFind(TargetFileName, START_TAG, END_TAG, False);
    if IsInString(START_TAG, Result) then
    begin
      Result := Right(START_TAG, Result);
      SetRegisteredVersion(TargetFileName, Result);
    end;
  end;

  if IsEmpty(Result) then
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

  function RetrievePythonGdb(const GdbExecutable: TFileName): string;
  begin
    Result := RetrieveVersion(GdbExecutable, '--configuration',
      '--with-python=c:/python/', '/x86', False); // Major.Minor.Build
    Result := Right('.', ReverseString(Result)); // Minor.Major
    Result := ReverseString(Result); // Major.Minor
    if SameText(Result, EmptyStr) then
      Result := INVALID_VERSION;
  end;

  procedure RetrieveVersionToolchain(var AVersion: TToolchainVersion;
    AEnvironment: TDreamcastSoftwareDevelopmentFileSystemToolchain);
  begin
    if Assigned(AVersion) then
    begin
      AVersion.fVersionBinutils := RetrieveVersion(AEnvironment.BinutilsExecutable,
        '--version', ' (GNU Binutils)', sLineBreak);
      AVersion.fVersionGCC := RetrieveVersion(AEnvironment.GCCExecutable,
        '--version', ') ', sLineBreak);

      if AEnvironment.Kind <> tkARM then
      begin
        // Super-H and Win32
        AVersion.fVersionGDB := RetrieveVersion(AEnvironment.GDBExecutable,
          '--version', ' (GDB)', sLineBreak);
      end;

      if AEnvironment.Kind = tkSuperH then
      begin
        // Super-H only
        AVersion.fVersionPythonGDB := RetrievePythonGdb(AEnvironment.GDBExecutable);
        AVersion.fVersionNewlib := RetrieveVersionWithFind(AEnvironment.NewlibBinary,
          '/dc-chain/newlib-', '/newlib/libc/');
      end;
    end;
  end;

begin
  with Environment.FileSystem do
  begin
    fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak);
    fVersionSVN := RetrieveVersion('svn', '--version', 'svn, version', sLineBreak);
    fSubversionInstalled := IsValidVersion(fVersionSVN);
    fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak);
    fPythonInstalled := IsValidVersion(fVersionPython);
    fVersionMinGW := RetrieveVersion(Shell.MinGWGetExecutable,
      '--version', 'mingw-get version', sLineBreak);

    RetrieveVersionToolchain(fToolchainVersionSuperH, ToolchainSuperH);
    RetrieveVersionToolchain(fToolchainVersionARM, ToolchainARM);
    RetrieveVersionToolchain(fToolchainVersionWin32, ToolchainWin32);

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
  fToolchainVersionSuperH := TToolchainVersion.Create(tkSuperH);
  fToolchainVersionARM := TToolchainVersion.Create(tkARM);
  fToolchainVersionWin32 := TToolchainVersion.Create(tkWin32);
  RetrieveVersions;
end;

destructor TComponentVersion.Destroy;
begin
  fToolchainVersionWin32.Free;
  fToolchainVersionSuperH.Free;
  fToolchainVersionARM.Free;
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
      Result := fToolchainVersionSuperH.fVersionBinutils;
    cnGCC:
      Result := fToolchainVersionSuperH.fVersionGCC;
    cnGDB:
      Result := fToolchainVersionSuperH.fVersionGDB;
    cnPythonGDB:
      Result := fToolchainVersionSuperH.fVersionPythonGDB;
    cnNewlib:
      Result := fToolchainVersionSuperH.fVersionNewlib;
    cnToolSerial:
      Result := fVersionToolSerial;
    cnToolIP:
      Result := fVersionToolIP;
    cnKallistiOS:
      Result := fVersionKallistiOS;
    cnBinutilsARM:
      Result := fToolchainVersionARM.fVersionBinutils;
    cnGCCARM:
      Result := fToolchainVersionARM.fVersionGCC;
    cnBinutilsWin32:
      Result := fToolchainVersionWin32.fVersionBinutils;
    cnGCCWin32:
      Result := fToolchainVersionWin32.fVersionGCC;
    cnGDBWin32:
      Result := fToolchainVersionWin32.fVersionGDB;
  end;
end;

end.

