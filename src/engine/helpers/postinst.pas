unit PostInst;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

function IsPostInstallMode: Boolean;
function IsInstallOrUpdateRequired: Boolean;
procedure ExecutePostInstall;

implementation

uses
  Forms,
  SysTools,
  ShellThd,
  Settings,
  Global;

var
  PostInstallMode: Boolean;

procedure SetPostInstallMode;
const
  POST_INSTALL_SWITCH = '--post-install';

var
  i: Integer;
  ParamValue: string;

begin
  PostInstallMode := False;
  for i := 1 to ParamCount do
  begin
    ParamValue := LowerCase(ParamStr(i));
    if (ParamValue = POST_INSTALL_SWITCH) then
      PostInstallMode := True;
  end;
end;

function IsPostInstallMode: Boolean;
begin
  Result := PostInstallMode;
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
    if PostInstallMode then
    begin
      if IsInstallOrUpdateRequired then
      begin
        with DreamcastSoftwareDevelopmentKitManager.Environment.Settings
          .Repositories do
        begin
          KallistiURL := GetDefaultUrlKallisti;
          KallistiPortsURL := GetDefaultUrlKallistiPorts;
          DreamcastToolSerialURL := GetDefaultUrlDreamcastToolSerial;
          DreamcastToolInternetProtocolURL := GetDefaultUrlDreamcastToolInternetProtocol;
          RubyURL := GetDefaultUrlRuby;
        end;
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
  SetPostInstallMode;

end.

