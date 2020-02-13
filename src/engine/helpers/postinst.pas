unit PostInst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IsPostInstallMode: Boolean;
function IsInstallOrUpdateRequired: Boolean;
procedure ExecutePostInstall;

implementation

uses
  Main, ShellThd, Forms, Settings;

var
  PostInstallMode: Boolean;

procedure SetPostInstallMode;
const
  POST_INSTALL_SWITCH = '--post-install';

var
  i: Integer;

begin
  for i := 1 to ParamCount do
  begin
    PostInstallMode := LowerCase(ParamStr(i)) = POST_INSTALL_SWITCH;
    if PostInstallMode then
      Break;
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
begin
  if PostInstallMode then
  begin
    if IsInstallOrUpdateRequired then
    begin
      with DreamcastSoftwareDevelopmentKitManager.Environment.Settings
        .Repositories do
      begin
        KallistiURL := DEFAULT_KALLISTI_URL;
        KallistiPortsURL := DEFAULT_KALLISTI_PORTS_URL;
        DreamcastToolSerialURL := DEFAULT_DREAMCAST_TOOL_SERIAL_URL;
        DreamcastToolInternetProtocolURL := DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL;
      end;
      ExecuteThreadOperation(stiKallistiManage);
    end
    else
      Application.Terminate;
  end;
end;

initialization
  SetPostInstallMode;

end.

