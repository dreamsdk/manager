unit StrRes;

{$mode objfpc}{$H+}

interface

resourcestring
  // Main
  MailToSubject = '[KallistiOS] Question (from DreamSDK) about the %s KallistiOS Port';
  RestoreDefaultsTitle = 'Question';
  RestoreDefaultsText = 'Are you sure to restore settings to its defaults?';

  // Progress
  SendingCancelSignal = 'Sending abort signal, this can take up to 1 minute, please be patient...';
  CancelDialogCaption = 'Warning';
  CancelDialogText = 'Are you really sure to cancel? This may breaks things!';
  CloseButtonCaption = '&Close';
  CancelButtonCaption = '&Cancel';
  OperationSuccessfullyTerminated = 'Operation finished.';
  OperationAborted = 'Operation aborted.';
  OperationDoneWithErrors = 'Operation done with errors.';
  OperationErrorMemoText = '*** %s';

  // ShellThd
  InstallationProblem =
    'Your installation have problems!!!' + sLineBreak +
    'No required binaries were found in the InstallPath directory ("%s").' + sLineBreak +
    'Please check the configuration file and modify the InstallPath key if needed.';

implementation

end.

