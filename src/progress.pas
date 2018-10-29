unit Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TfrmProgress }

  TfrmProgress = class(TForm)
    btnAbort: TButton;
    Memo1: TMemo;
    pgbOperationProgress: TProgressBar;
    procedure btnAbortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    fFinished: Boolean;
  public
    property Finished: Boolean read fFinished write fFinished;
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.lfm}

uses
  ShellThd;

{ TfrmProgress }

procedure TfrmProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin

end;

procedure TfrmProgress.btnAbortClick(Sender: TObject);
begin
  AbortThreadOperation;
end;

procedure TfrmProgress.FormCreate(Sender: TObject);
begin
  fFinished := True;
end;

end.

