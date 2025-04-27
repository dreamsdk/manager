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
  Forms,
{$IFDEF DEBUG}
  Windows,
{$ENDIF}
  Main,
  GetVer,
  Environ,
  PortMgr,
  DCSDKMgr,
  Progress,
  KOSMgr,
  ShellThd,
  PostInst,
  StrRes,
  ToolMgr,
  About,
  ModVer,
  IDEMgr,
  Splash,
  RubyMgr,
  Unpack,
  PkgMgr;

{$R *.res}

begin
{$IF DEFINED(DEBUG) AND DEFINED(CONSOLE)}
  SetConsoleCtrlHandler(nil, True);
{$ENDIF}
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

