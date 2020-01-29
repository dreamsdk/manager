unit StrRes;

{$mode objfpc}{$H+}

interface

resourcestring
  // Generic
  KallistiText = 'KallistiOS';
  KallistiPortsText = 'KallistiOS Ports';
  DreamcastToolText = 'Dreamcast Tool';
  DreamcastToolSerialText = 'Dreamcast Tool Serial (RS-232)';
  DreamcastToolInternetProtocolText = 'Dreamcast Tool Internet Protocol (IP)';
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
  UpdateProcessUninstallSuccessText = '%s was successfully uninstalled.';
  UpdateProcessUpdateUselessText = '%s is up-to-date.';
  UpdateProcessAllKallistiPortsInstalled = 'All KallistiOS Ports were successfully installed.';
  UpdateProcessAllKallistiPortsUninstalled = 'All KallistiOS Ports were successfully uninstalled.';
  InstallAllKallistiPorts = 'Are you sure to install all KallistiOS Ports at once?';
  UninstallAllKallistiPorts = 'Are you sure to uninstall all KallistiOS Ports at once?';
  InvalidInternetProtocolAddressFormat = 'Use 3 digits!';
  InvalidInternetProtocolAddressValue = 'Invalid IP';
  InvalidMediaAccessControlAddressFormat = 'Use 2 digits!';
  InvalidMediaAccessControlAddressValue = 'Invalid MAC';
  NoNetworkAdapterAvailable = 'No connected Network Adapter was found on your system. Please verify the connection state.';
  UninstallKallistiSinglePort = 'Are you sure to uninstall %s?';
  InstallOrUpdateRequiredDoItNow = 'A management operation of KallistiOS components is necessary. Do it now?';
  UseSubversionKallistiSinglePort = 'Unable to install %s, as it needs the Subversion Client (SVN) which wasn''t detected.';
  UseSubversionAllKallistiPorts = 'Note: The Subversion Client (SVN) wasn''t detected and this may causes issues.';
  ResetRepositoryLine1 = 'This will delete the %s repository on the disk, losing all your changes if any.';
  ResetRepositoryLine2 = 'Then perform an Update operation to apply the changes.';
  ResetRepositoryLine3 = 'Are you sure to continue?';
  ResetRepositoryConfirmUpdateLine1 = '%s repository deleted. Perform the Update operation now?';
  ResetRepositoryConfirmUpdateLine2 = 'If you want to change the repository URL, answer No.';
  ResetRepositoryConfirmUpdateLine3 = 'You may do the Update operation later in the %s tab.';
  InternetConnectionNeeded = 'This feature requires Internet connection.';
  FailedToResetRepository = 'Failed to reset the %s repository. You have to delete it manually.';
  ConfirmCodeBlocksMessage = 'Are you sure to %s the Code::Blocks integration?';
  ConfirmCodeBlocksInstallation = 'install';
  ConfirmCodeBlocksReinstallation = 'reinstall';
  ConfirmCodeBlocksUninstallation = 'uninstall definitely';
  CodeBlocksInstallationDirectoryInvalid = 'The Code::Blocks installation directory doesn''t exists.';

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

