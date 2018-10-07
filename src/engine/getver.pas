unit GetVer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TVersionRetriever }

  TVersionRetriever = class(TObject)
  private
    fInstallPath: TFileName;
    fVersionGit: string;
    fVersionMinGW: string;
    fVersionPython: string;
    fVersionSVN: string;
    function Execute(Executable, CommandLine: string): string;
    function RetrieveVersion(Executable, CommandLine,
      StartTag, EndTag: string): string;
    procedure RetrieveVersions;
  public
    constructor Create(const AInstallPath: TFileName);
    property Git: string read fVersionGit;
    property MinGW: string read fVersionMinGW;
    property SVN: string read fVersionSVN;
    property Python: string read fVersionPython;
  end;

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
    Result := Format(ComponentNotFound, [Executable]);
  end;
end;

procedure TVersionRetriever.RetrieveVersions;
begin
  fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak);
  fVersionSVN := RetrieveVersion('svn', '--version', 'svn, version', sLineBreak);
  fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak);
  fVersionMinGW := RetrieveVersion(fInstallPath + 'bin\mingw-get', '--version', 'mingw-get version', sLineBreak);
end;

constructor TVersionRetriever.Create(const AInstallPath: TFileName);
begin
  fInstallPath := IncludeTrailingPathDelimiter(AInstallPath);
  RetrieveVersions;
end;

end.

