program Manager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, GetVer, Environ, PortMgr, DCSDKMgr, Progress, KOSMgr, ShellThd,
  SysTools, RunCmd, Version, PostInst, FirstRun, StrRes, ToolMgr, Settings,
  VerIntf, About, UITools, MsgDlg, modver;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='DreamSDK Manager';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.ShowMainForm := not IsPostInstallMode;
  ExecutePostInstall;
  Application.Run;
end.
