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
  Unpack,
  PkgMgr,
  Global,
  SysTools;

{$R *.res}

var
  LogContext: TLogMessageContext;

begin
{$IF DEFINED(DEBUG) AND DEFINED(CONSOLE)}
  SetConsoleCtrlHandler(nil, True);
{$ENDIF}
  Application.Scaled:=True;
  Application.Title:='DreamSDK Manager';
  RequireDerivedFormResource:=True;
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    LogMessage(LogContext, 'Initialize application');
    Application.Initialize;

    LogMessage(LogContext, 'Initialize splash screen');
    SplashInitialize;

    LogMessage(LogContext, 'Initialize forms');
    Application.CreateForm(TfrmMain, frmMain);
    Application.CreateForm(TfrmAbout, frmAbout);

    LogMessage(LogContext, 'Execute post-install');
    Application.ShowMainForm := not IsPostInstallMode;
    ExecutePostInstall;

    LogMessage(LogContext, 'Finalize splash screen');
    SplashFinalize;

    LogMessage(LogContext, 'Startup application');
    Application.Run;
  finally
    LogMessageExit(LogContext);
  end;
end.

