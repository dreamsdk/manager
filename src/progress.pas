unit Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TfrmProgress }

  TfrmProgress = class(TForm)
    pgbOperationProgress: TProgressBar;
  private
  public
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.lfm}

end.

