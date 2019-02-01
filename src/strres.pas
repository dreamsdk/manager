unit StrRes;

{$mode objfpc}{$H+}

interface

resourcestring
  // Generic
  KallistiText = 'KallistiOS';
  KallistiPortsText = 'KallistiOS Ports';
  DreamcastToolText = 'Dreamcast Tool';
  DialogInformationTitle = 'Information';
  DialogQuestionTitle = 'Question';
  DialogWarningTitle = 'Warning';

  // Main
  MailToSubject = 'Question about the KallistiOS Port: %s';
  RestoreDefaultsText = 'Are you sure to restore Options and Dreamcast Tool settings to their defaults?';
  UserInterfaceNotInstalledText = 'Not installed';
  UserInterfaceInstalledText = 'OK';
  UpdateProcessEverythingUpdate = 'Everything is up-to-date.';
  UpdateProcessInstallSuccessText = '%s was successfully installed.';
  UpdateProcessUpdateSuccessText = '%s was successfully updated.';
  UpdateProcessUpdateUselessText = '%s is up-to-date.';
  UpdateProcessAllKallistiPortsInstalled = 'All KallistiOS Ports were successfully installed.';
  UpdateProcessAllKallistiPortsUninstalled = 'All KallistiOS Ports were successfully uninstalled.';
  InstallAllKallistiPorts = 'Are you sure to install all KallistiOS Ports at once?';
  UninstallAllKallistiPorts = 'Are you sure to uninstall all KallistiOS Ports at once?';
  InvalidInternetProtocolAddressFormat = 'Use 3 digits!';
  InvalidInternetProtocolAddressValue = 'Invalid IP';
  InvalidMediaAccessControlAddressFormat = 'Use 2 digits!';
  InvalidMediaAccessControlAddressValue = 'Invalid MAC';
  UninstallKallistiSinglePort = 'Are you sure to uninstall %s?';
  InstallOrUpdateRequiredDoItNow = 'A management operation of KallistiOS components is necessary. Do it now?';

  // Progress
  SendingCancelSignal = 'Sending abort signal, this can take up to one minute, please be patient...';
  CancelDialogCaption = 'Warning';
  CancelDialogText = 'Are you really sure to cancel? This may breaks things!';
  CloseButtonCaption = '&Close';
  CancelButtonCaption = '&Cancel';
  OperationSuccessfullyTerminated = 'Operation done.';
  OperationAborted = 'Operation aborted.';
  OperationDoneWithErrors = 'Operation done with errors.';
  OperationErrorMemoText = '*** %s';

  // ShellThd
  InstallationProblem =
    'Your installation have problems!!!' + sLineBreak +
    'No required binaries were found in the InstallPath directory ("%s").' + sLineBreak +
    'Please check the configuration file ("%s") and modify the InstallPath key if needed.';
  KallistiPortOperationText = 'KallistiOS Port %s %s';
  KallistiPortOperationInstallText = 'Installation of %s';
  KallistiPortOperationUpdateText = 'Update of %s';
  KallistiPortOperationUninstallText = 'Uninstallation of %s';
  KallistiOperationText = 'KallistiOS Management';
  KallistiPortsOperationText = 'KallistiOS Ports Management';
  KallistiOperationNothingNeededText = 'KallistiOS is already installed and up-to-date.';
  CloningOperation = 'Cloning %s repository...';
  UpdatingOperation = 'Updating %s repository...';
  KallistiInitializeText = 'Initialize KallistiOS environment...';
  KallistiBuildText = 'Building KallistiOS library...';
  KallistiFixNewlibText = 'Fixing SuperH Newlib...';
  DreamcastToolInitializeText = 'Initialize Dreamcast Tool environment...';
  DreamcastToolBuildText = 'Building Dreamcast Tool components...';
  DreamcastToolInstallText = 'Installing Dreamcast Tool...';
  KallistiPortInstallText = 'Installing the KallistiOS Port...';
  KallistiPortUninstallText = 'Uninstalling the KallistiOS Port...';
  KallistiPortUpdateText = 'Updating the KallistiOS Port...';
  KallistiPortsInstallText = 'Installing all KallistiOS Ports...';
  KallistiPortsUninstallText = 'Uninstalling all KallistiOS Ports...';

implementation

end.

