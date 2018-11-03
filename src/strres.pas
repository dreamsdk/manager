unit StrRes;

{$mode objfpc}{$H+}

interface

resourcestring
  // Generic
  KallistiText = 'KallistiOS';
  KallistiPortsText = 'KallistiOS Ports';
  DreamcastToolText = 'Dreamcast Tool';
  DialogInformationTitle = 'Information';

  // Main
  MailToSubject = '[KallistiOS] Question (from DreamSDK) about the %s KallistiOS Port';
  RestoreDefaultsTitle = 'Question';
  RestoreDefaultsText = 'Are you sure to restore settings to its defaults?';
  UserInterfaceNotInstalledText = 'Not installed';
  UserInterfaceInstalledText = 'Installed';
  UpdateProcessEverythingUpdate = 'Everything is up-to-date.';
  UpdateProcessUpdateSuccessText = '%s was successfully updated.';
  UpdateProcessUpdateUselessText = '%s is up-to-date.';

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
  KallistiPortOperationText = 'KallistiOS Port %s %s';
  KallistiPortOperationInstallText = 'Installation of %s';
  KallistiPortOperationUpdateText = 'Update of %s';
  KallistiPortOperationUninstallText = 'Uninstallation of %s';
  KallistiOperationText = 'KallistiOS Installation/Update';
  KallistiOperationNothingNeededText = 'KallistiOS is already installed and up-to-date.';
  CloningOperation = 'Cloning %s repository...';
  UpdatingOperation = 'Updating %s repository...';
  KallistiInitializeText = 'Initialize KallistiOS environment...';
  KallistiBuildText = 'Building KallistiOS library...';
  KallistiFixNewlibText = 'Fixing SH-4 Newlib...';
  DreamcastToolInitializeText = 'Initialize Dreamcast Tool environment...';
  DreamcastToolBuildText = 'Building Dreamcast Tool components...';
  DreamcastToolInstallText = 'Installing Dreamcast Tool...';
  KallistiPortInstallText = 'Installing the KallistiOS Port...';
  KallistiPortUninstallText = 'Uninstalling the KallistiOS Port...';
  KallistiPortUpdateText = 'Updating the KallistiOS Port...';

implementation

end.

