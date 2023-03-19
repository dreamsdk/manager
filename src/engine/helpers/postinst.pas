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
  PostInstallMode,
  RubyEnabled: Boolean;

procedure SetPostInstallMode;
const
  POST_INSTALL_SWITCH = '--post-install';
  RUBY_ENABLED = '--enable-ruby';

var
  i: Integer;
  ParamValue: string;

begin
  PostInstallMode := False;
  RubyEnabled := False;
  for i := 1 to ParamCount do
  begin
    ParamValue := LowerCase(ParamStr(i));
    if (ParamValue = POST_INSTALL_SWITCH) then
      PostInstallMode := True;
    if (ParamValue = RUBY_ENABLED) then
      RubyEnabled := True;
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
        KallistiURL := GetDefaultUrlKallisti;
        KallistiPortsURL := GetDefaultUrlKallistiPorts;
        DreamcastToolSerialURL := GetDefaultUrlDreamcastToolSerial;
        DreamcastToolInternetProtocolURL := GetDefaultUrlDreamcastToolInternetProtocol;
        RubyURL := GetDefaultUrlRuby;
      end;
      DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Ruby.Enabled := RubyEnabled;
      ExecuteThreadOperation(stiKallistiManage);
    end
    else
      Application.Terminate;
  end;
end;

initialization
  SetPostInstallMode;

end.

