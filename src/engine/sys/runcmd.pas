unit RunCmd;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF Windows}Process{$ELSE}ProcessUTF8{$ENDIF};

type
  TNewLineEvent = procedure(Sender: TObject; NewLine: string) of object;

  { TRunCommand }

  TRunCommand = class(TThread)
  private
    fBufferOutput: TStringList;
    fProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};
    fEnvironment: TStringList;
    fNewLineBuffer: string;
    fExecutable: TFileName;
    fNewLine: TNewLineEvent;
    fParameters: TStringList;
    procedure InitializeProcess;
    procedure SyncSendNewLineEvent;
    procedure SendNewLine(const NewLine: string);
  protected
    procedure Execute; override;
    procedure KillRunningProcess;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Abort;
    procedure Pause;
    procedure Resume;
    property BufferOutput: TStringList read fBufferOutput;
    property Environment: TStringList read fEnvironment;
    property Executable: TFileName read fExecutable write fExecutable;
    property Parameters: TStringList read fParameters;
    property OnNewLine: TNewLineEvent read fNewLine write fNewLine;
  end;

implementation

{ TRunCommand }

procedure TRunCommand.InitializeProcess;
var
  i: Integer;

begin
  fProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  for i := 1 to GetEnvironmentVariableCount do
    fProcess.Environment.Add(GetEnvironmentString(i));
end;

procedure TRunCommand.SyncSendNewLineEvent;
begin
  if Assigned(fNewLine) then
    fNewLine(Self, fNewLineBuffer);
end;

procedure TRunCommand.SendNewLine(const NewLine: string);
begin
  fNewLineBuffer := NewLine;
  fBufferOutput.Add(AdjustLineBreaks(fNewLineBuffer));
  Synchronize(@SyncSendNewLineEvent);
end;

procedure TRunCommand.Execute;
const
  BUF_SIZE = 2048;

var
  Buffer: array[0..BUF_SIZE - 1] of Char;
  BytesRead: Integer;
  NewLine: string;

begin
  Buffer[0] := #0;

  fProcess.Executable := Executable;
  fProcess.Parameters.AddStrings(Parameters);
  fProcess.Environment.AddStrings(fEnvironment);
  fProcess.Options := [poUsePipes, poStderrToOutPut];
  fProcess.ShowWindow := swoHide;
  fProcess.Execute;

  repeat
    BytesRead := fProcess.Output.Read(Buffer, BUF_SIZE);
    SetString(NewLine, PChar(@Buffer[0]), BytesRead);
    if NewLine <> '' then
      SendNewLine(NewLine);
  until (BytesRead = 0) or (Terminated);
end;

procedure TRunCommand.KillRunningProcess;
var
  AExitCode: Integer;

begin
  AExitCode := -1;
  if Assigned(fProcess) then
  begin
    fProcess.Terminate(AExitCode);
  end;
end;

constructor TRunCommand.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fBufferOutput := TStringList.Create;
  fParameters := TStringList.Create;
  fEnvironment := TStringList.Create;
  InitializeProcess;
  FreeOnTerminate := True;
end;

destructor TRunCommand.Destroy;
begin
  fBufferOutput.Free;
  fProcess.Free;
  fEnvironment.Free;
  fParameters.Free;
  inherited Destroy;
end;

procedure TRunCommand.Abort;
begin
  KillRunningProcess;
  Terminate;
end;

procedure TRunCommand.Pause;
begin
  if Assigned(fProcess) then
    fProcess.Suspend;
end;

procedure TRunCommand.Resume;
begin
  if Assigned(fProcess) then
    fProcess.Resume;
end;

end.

