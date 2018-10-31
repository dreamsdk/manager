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
    cnKallistiOS
  );

  { TVersionRetriever }
  TVersionRetriever = class(TObject)
  private
    fBuildDateKallistiOS: TDateTime;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fVersionKallistiOS: string;
    fVersionGDB: string;
    fVersionBinutils: string;
    fVersionGCC: string;
    fVersionGit: string;
    fChangeLogKallistiOS: string;
    fVersionMinGW: string;
    fVersionNewlib: string;
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
  public
    constructor Create(Environment: TDreamcastSoftwareDevelopmentEnvironment);

    function GetComponentVersion(const ComponentName: TComponentName): string;
    procedure RetrieveVersions;

    property Git: string read fVersionGit;
    property MinGW: string read fVersionMinGW;
    property SVN: string read fVersionSVN;
    property Python: string read fVersionPython;
    property Binutils: string read fVersionBinutils;
    property Newlib: string read fVersionNewlib;
    property GCC: string read fVersionGCC;
    property GDB: string read fVersionGDB;
    property ToolSerial: string read fVersionToolSerial;
    property ToolIP: string read fVersionToolIP;
    property KallistiOS: string read fVersionKallistiOS;
    property KallistiBuildDate: TDateTime read fBuildDateKallistiOS;
    property KallistiChangeLog: string read fChangeLogKallistiOS;
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
  COMPONENTS_NAME: array[0..10] of string = (
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
    'KallistiOS'
  );

begin
  Result := COMPONENTS_NAME[Integer(ComponentName)];
end;

function IsVersionValid(const Version: string): Boolean;
begin
  Result := not IsInString(INVALID_VERSION, Version);
end;

{ TVersionRetriever }

function TVersionRetriever.RetrieveVersion(Executable, CommandLine,
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

function TVersionRetriever.RetrieveVersionWithFind(
  FindTargetFileName: TFileName; StartTag, EndTag: string): string;
var
  CommandLine: string;

begin
  CommandLine := Format('"%s" %s', [StartTag, FindTargetFileName]);
  Result := RetrieveVersion('find', CommandLine, StartTag, EndTag);
  if (Result = '') then
    Result := INVALID_VERSION;
end;

function TVersionRetriever.RetrieveKallistiVersion: string;
const
  START_TAG = 'KallistiOS ';
  END_TAG = ':';


begin
  Result := RetrieveVersionWithFind(fEnvironment.FileSystem.KallistiLibrary,
    START_TAG, END_TAG);
  if IsInString(START_TAG, Result) then
    Result := Right(START_TAG, Result)
  else
    Result := INVALID_VERSION;
end;

function TVersionRetriever.RetrieveKallistiBuildDate: TDateTime;
const
  c_UnassignedDate = -693594;

var
  BuildDate: Integer;

begin
  Result := c_UnassignedDate;
  BuildDate := FileAge(fEnvironment.FileSystem.KallistiLibrary);
  if BuildDate <> -1 then
    Result := FileDateToDateTime(BuildDate);
end;

procedure TVersionRetriever.RetrieveKallistiInformation;
begin
  with fEnvironment.FileSystem do
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

procedure TVersionRetriever.RetrieveVersions;
begin
  with fEnvironment.FileSystem do
  begin
    fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak);
    fVersionSVN := RetrieveVersion('svn', '--version', 'svn, version', sLineBreak);
    fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak);
    fVersionMinGW := RetrieveVersion(MinGWGetExecutable,
      '--version', 'mingw-get version', sLineBreak);

    fVersionBinutils := RetrieveVersion(BinutilsExecutable,
      '--version', ' (GNU Binutils)', sLineBreak);
    fVersionGCC := RetrieveVersion(GCCExecutable,
      '--version', ' (GCC)', sLineBreak);
    fVersionGDB := RetrieveVersion(GDBExecutable,
      '--version', ' (GDB)', sLineBreak);

    fVersionNewlib := RetrieveVersionWithFind(NewlibBinary,
      '/dc-chain/newlib-', '/newlib/libc/');

    fVersionToolSerial := RetrieveVersion(DreamcastTool.SerialExecutable,
      '-h', 'dc-tool', 'by <');
    fVersionToolIP := RetrieveVersion(DreamcastTool.InternetProtocolExecutable,
      '-h', 'dc-tool-ip', 'by <');

    RetrieveKallistiInformation;
  end;
end;

constructor TVersionRetriever.Create(
  Environment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := Environment;
  RetrieveVersions;
end;

function TVersionRetriever.GetComponentVersion(
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
      Result := fVersionBinutils;
    cnGCC:
      Result := fVersionGCC;
    cnGDB:
      Result := fVersionGDB;
    cnNewlib:
      Result := fVersionNewlib;
    cnToolSerial:
      Result := fVersionToolSerial;
    cnToolIP:
      Result := fVersionToolIP;
    cnKallistiOS:
      Result := fVersionKallistiOS;
  end;
end;

end.

