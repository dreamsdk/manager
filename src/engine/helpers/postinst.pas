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
      ExecuteThreadOperation(stiKallistiManage)
    else
      Application.Terminate;
  end;
end;

initialization
  SetPostInstallMode;

end.

