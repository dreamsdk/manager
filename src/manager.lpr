program Manager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, getver, Environ, portmgr, dcsdkmgr, Progress, output,
  kosmgr, SysTools, RunCmd;

{$R *.res}

begin
  Application.Title:='DreamSDK Manager';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmShellOutput, frmShellOutput);
  Application.Run;
end.

