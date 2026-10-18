unit PostInst;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

function IsInstallOrUpdateRequired: Boolean;
procedure ExecutePostInstall;

implementation

uses
  Forms,
  SysTools,
  ShellThd,
  Global,
  InstMode;

procedure InitializePostInstallMode;
const
  POST_INSTALL_SWITCH = '--post-install';

var
  i: Integer;
  ParamValue: string;

begin
  SetPostInstallMode(False);
  for i := 1 to ParamCount do
  begin
    ParamValue := LowerCase(ParamStr(i));
    if (ParamValue = POST_INSTALL_SWITCH) then
      SetPostInstallMode(True);
  end;
end;

function IsInstallOrUpdateRequired: Boolean;
begin
  Result := (not DreamcastSoftwareDevelopmentKitManager.KallistiOS.Built)
    or (not DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Installed)
    or (not DreamcastSoftwareDevelopmentKitManager.DreamcastTool.Built);
end;

procedure ExecutePostInstall;
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    LogMessage(LogContext, Format('Main ThreadId: %d', [ThreadID]));
    if IsPostInstallMode then
    begin
      if IsInstallOrUpdateRequired then
      begin
        // Repositories URL and branch/tag are read from the configuration
        // file, as passed by DreamSDK Setup; empty URL means default URL
        ExecuteThreadOperation(stiKallistiManage);
      end
      else
      begin
        LogMessage(LogContext, 'Calling Application.Terminate');
        Application.Terminate;
      end;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

initialization
  InitializePostInstallMode;

end.

