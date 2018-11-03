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
  Main, ShellThd, Forms;

var
  PostInstall: Boolean;

procedure SetPostInstallMode;
const
  POST_INSTALL_SWITCH = '--post-install';

var
  i: Integer;

begin
  for i := 1 to ParamCount do
  begin
    PostInstall := LowerCase(ParamStr(i)) = POST_INSTALL_SWITCH;
    if PostInstall then
      break;
  end;
end;

function IsPostInstallMode: Boolean;
begin
  Result := PostInstall;
end;

function IsInstallOrUpdateRequired: Boolean;
begin
  Result := (not DreamcastSoftwareDevelopmentKitManager.KallistiOS.Built)
    or (not DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Installed)
    or (not DreamcastSoftwareDevelopmentKitManager.DreamcastTool.Built);
end;

procedure ExecutePostInstall;
begin
  if PostInstall then
  begin
    if IsInstallOrUpdateRequired then
      ExecuteThreadOperation(stiKallistiManage)
    else
      Application.Terminate;
  end;
end;

initialization
  SetPostInstallMode;

end.

