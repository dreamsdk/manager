program Manager;

{$mode objfpc}{$H+}

{$IFDEF DEBUG}
{$IFDEF CONSOLE}
{$APPTYPE Console}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, GetVer, Environ, PortMgr, DCSDKMgr, Progress, KOSMgr, ShellThd,
  SysTools, RunCmd, Version, PostInst, StrRes, ToolMgr, Settings,
  VerIntf, About, UITools, MsgDlg, ModVer, IDEMgr, Splash, refbase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='DreamSDK Manager';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  SplashInitialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.ShowMainForm := not IsPostInstallMode;
  ExecutePostInstall;
  SplashFinalize;
  Application.Run;
end.

