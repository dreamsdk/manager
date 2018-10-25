unit Output;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmShellOutput }

  TfrmShellOutput = class(TForm)
    btnClose: TButton;
    gbxShellOutput: TGroupBox;
    lblTitle: TLabel;
    memShellOutput: TMemo;
    procedure btnCloseClick(Sender: TObject);
  private
  public
  end;

var
  frmShellOutput: TfrmShellOutput;

procedure ShowShellOutputWindow(ResultOutput: string);

implementation

{$R *.lfm}

procedure ShowShellOutputWindow(ResultOutput: string);
begin
  with frmShellOutput do
  begin
    memShellOutput.Clear;
    memShellOutput.Lines.Add(ResultOutput);
    ShowModal;
  end;
end;

{ TfrmShellOutput }

procedure TfrmShellOutput.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

