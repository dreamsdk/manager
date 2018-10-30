program Manager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, getver, Environ, portmgr, dcsdkmgr, Progress,
  kosmgr, ShellThd, SysTools, RunCmd, postinst;

{$R *.res}



begin
  Application.Title:='DreamSDK Manager';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.ShowMainForm := not IsPostInstallMode;
  ExecutePostInstall;
  Application.Run;
end.

