unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TButton;
    edtComponentURL: TLabeledEdit;
    Label1: TLabel;
    lblCopyleft: TLabel;
    lblCreditsTitle: TLabel;
    memLicense: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    tvwComponents: TTreeView;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

uses
  Version;

{ TfrmAbout }

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblCreditsTitle.Caption := GetProductName;
  lblCopyleft.Caption := Format(lblCopyleft.Caption, [GetCompanyName]);
end;

end.

