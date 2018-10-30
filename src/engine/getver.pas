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
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fVersionGDB: string;
    fVersionBinutils: string;
    fVersionGCC: string;
    fVersionGit: string;
    fVersionKallistiOS: string;
    fVersionMinGW: string;
    fVersionNewlib: string;
    fVersionPython: string;
    fVersionSVN: string;
    fVersionToolIP: string;
    fVersionToolSerial: string;
    function RetrieveVersion(Executable, CommandLine,
      StartTag, EndTag: string): string;
    function RetrieveVersionWithFind(ComponentName: string;
      FindTargetFileName: TFileName; StartTag, EndTag: string): string;
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
  end;

function ComponentNameToString(const ComponentName: TComponentName): string;

implementation

uses
  Forms,
  SysTools;

resourcestring
  ComponentNotFound = '(Component not found: %s)';

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
    Result := Format(ComponentNotFound,
      [ExtractFileName(ChangeFileExt(Executable, ''))]);
  end;
end;

function TVersionRetriever.RetrieveVersionWithFind(ComponentName: string;
  FindTargetFileName: TFileName; StartTag, EndTag: string): string;
var
  CommandLine: string;

begin
  CommandLine := Format('"%s" %s', [StartTag, FindTargetFileName]);
  Result := RetrieveVersion('find', CommandLine, StartTag, EndTag);
  if (Result = '') then
    Result := Format(ComponentNotFound, [ComponentName]);
end;

procedure TVersionRetriever.RetrieveVersions;
begin
  fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak);
  fVersionSVN := RetrieveVersion('svn', '--version', 'svn, version', sLineBreak);
  fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak);
  fVersionMinGW := RetrieveVersion(fEnvironment.FileSystem.MinGWGetExecutable,
    '--version', 'mingw-get version', sLineBreak);

  fVersionBinutils := RetrieveVersion(fEnvironment.FileSystem.BinutilsExecutable,
    '--version', ' (GNU Binutils)', sLineBreak);
  fVersionGCC := RetrieveVersion(fEnvironment.FileSystem.GCCExecutable,
    '--version', ' (GCC)', sLineBreak);
  fVersionGDB := RetrieveVersion(fEnvironment.FileSystem.GDBExecutable,
    '--version', ' (GDB)', sLineBreak);

  fVersionNewlib := RetrieveVersionWithFind('newlib',
    fEnvironment.FileSystem.NewlibBinary, '/dc-chain/newlib-', '/newlib/libc/');

  fVersionKallistiOS := RetrieveVersionWithFind('kos',
    fEnvironment.FileSystem.KallistiVersionFile, 'KallistiOS version ', ' -----');

  fVersionToolSerial := RetrieveVersion(fEnvironment.FileSystem.DreamcastToolSerialExecutable,
    '-h', 'dc-tool', 'by <andrewk');
  fVersionToolIP := RetrieveVersion(fEnvironment.FileSystem.DreamcastToolIPExecutable,
    '-h', 'dc-tool-ip', 'by <andrewk');
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

