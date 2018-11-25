unit FirstRun;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IsFirstRunMode: Boolean;
function GetFirstRunInstallationDirectory: TFileName;

implementation

uses
  SysTools;

var
  FirstRunMode: Boolean;
  FirstRunModeInstallationDirectory: TFileName;

procedure SetFirstRunMode;
const
  FIRST_RUN_SWITCH = '--first-run';
  FIRST_RUN_DIR_SWITCH = '--directory';

var
  i: Integer;
  Param: string;

begin
  FirstRunMode := False;
  for i := 1 to ParamCount do
  begin
    Param := LowerCase(ParamStr(i));
    FirstRunMode := FirstRunMode or IsInString(FIRST_RUN_SWITCH, Param);
    if IsInString(FIRST_RUN_DIR_SWITCH, Param) then
      FirstRunModeInstallationDirectory := ParamStr(i + 1);
  end;
end;

function IsFirstRunMode: Boolean;
begin
  Result := FirstRunMode;
end;

function GetFirstRunInstallationDirectory: TFileName;
begin
  Result := FirstRunModeInstallationDirectory;
end;

initialization
  SetFirstRunMode;

end.

