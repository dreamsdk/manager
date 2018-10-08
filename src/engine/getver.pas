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
    function Execute(Executable, CommandLine: string): string;
    function RetrieveVersion(Executable, CommandLine,
      StartTag, EndTag: string): string;
    function RetrieveVersionWithFind(ComponentName: string;
      FindTargetFileName: TFileName; StartTag, EndTag: string): string;
    procedure RetrieveVersions;
  public
    constructor Create(Environment: TDreamcastSoftwareDevelopmentEnvironment);

    function GetComponentVersion(const ComponentName: TComponentName): string;

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
{$IFDEF DEBUG}
  Dialogs,
{$ENDIF}
  Forms,
  SysTools,
  Process
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process
{$ENDIF};

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

{ Thanks to Marc Weustink and contributors
  http://wiki.freepascal.org/Executing_External_Programs }
function TVersionRetriever.Execute(Executable, CommandLine: string): string;
const
  READ_BYTES = 2048;

var
  OutputLines: TStringList;
  MemStream: TMemoryStream;
{$IFDEF Windows}
  OurProcess: TProcess;
{$ELSE}
  OurProcess: TProcessUTF8;
{$ENDIF}
  NumBytes: LongInt;
  BytesRead: LongInt;

begin
  // A temp Memorystream is used to buffer the output
  MemStream := TMemoryStream.Create;
  try
    BytesRead := 0;

{$IFDEF Windows}
    OurProcess := TProcess.Create(nil);
{$ELSE}
    OurProcess := TProcessUTF8.Create(nil);
{$ENDIF}
    try
      OurProcess.Executable := Executable;

      if Length(CommandLine) > 0 then
        OurProcess.Parameters.Add(CommandLine);

      { We cannot use poWaitOnExit here since we don't know the size of the output.
        On Linux the size of the output pipe is 2 kB; if the output data is more, we
        need to read the data. This isn't possible since we are waiting.
        So we get a deadlock here if we use poWaitOnExit. }
      OurProcess.Options := [poUsePipes, poStderrToOutput];
      OurProcess.ShowWindow := swoHide;
      OurProcess.Execute;

      while True do
      begin
        // Refresh the GUI
        Application.ProcessMessages;

        // make sure we have room
        MemStream.SetSize(BytesRead + READ_BYTES);

        // try reading it
        NumBytes := OurProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
        begin
          Inc(BytesRead, NumBytes);
        end
        else
          Break;
      end;

      MemStream.SetSize(BytesRead);

      OutputLines := TStringList.Create;
      try
         OutputLines.LoadFromStream(MemStream);
         Result := OutputLines.Text;
      finally
        OutputLines.Free;
      end;

    finally
      OurProcess.Free;
    end;

  finally
    MemStream.Free;
  end;
end;

function TVersionRetriever.RetrieveVersion(Executable, CommandLine,
  StartTag, EndTag: string): string;
var
  Buffer: string;

begin
  try
    Buffer := Execute(Executable, CommandLine);
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
  fVersionMinGW := RetrieveVersion(fEnvironment.MinGWGetExecutable,
    '--version', 'mingw-get version', sLineBreak);

  fVersionBinutils := RetrieveVersion(fEnvironment.BinutilsExecutable,
    '--version', ' (GNU Binutils)', sLineBreak);
  fVersionGCC := RetrieveVersion(fEnvironment.GCCExecutable,
    '--version', ' (GCC)', sLineBreak);
  fVersionGDB := RetrieveVersion(fEnvironment.GDBExecutable,
    '--version', ' (GDB)', sLineBreak);

  fVersionNewlib := RetrieveVersionWithFind('newlib',
    fEnvironment.NewlibBinary, '/dc-chain/newlib-', '/newlib/libc/');

  fVersionKallistiOS := RetrieveVersionWithFind('kos',
    fEnvironment.KallistiOSVersionFile, 'KallistiOS version ', ' -----');

  fVersionToolSerial := RetrieveVersion(fEnvironment.DreamcastToolSerialExecutable,
    '-h', 'dc-tool', 'by <andrewk');
  fVersionToolIP := RetrieveVersion(fEnvironment.DreamcastToolIPExecutable,
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

