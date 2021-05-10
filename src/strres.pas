unit StrRes;

{$mode objfpc}{$H+}

interface

resourcestring
  // Generic
  KallistiText = 'KallistiOS';
  KallistiPortsText = 'KallistiOS Ports';
  DreamcastToolText = 'Dreamcast Tool';
  RubyText = 'Ruby';
  DreamcastToolSerialText = 'Dreamcast Tool Serial (RS-232)';
  DreamcastToolInternetProtocolText = 'Dreamcast Tool Internet Protocol (IP)';
  DialogInformationTitle = 'Information';
  DialogQuestionTitle = 'Question';
  DialogWarningTitle = 'Warning';
  DialogErrorTitle = 'Error';

  // Main
  ElevatedCaption = '%s [Elevated]';
  MailToSubject = 'Question about the KallistiOS Port: %s';
  RestoreDefaultsText = 'Are you sure to restore Options and Dreamcast Tool settings to their defaults?';
  UserInterfaceNotInstalledText = 'Not installed';
  UserInterfaceInstalledText = 'OK';
  EverythingText = 'Everything';
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
  ResetRepositoryQuestion = 'This will delete the %s repository on the disk, losing all your changes if any.\nThen perform an Update operation to apply the changes.\nAre you sure to continue?';
  ResetRepositoryDone = '%s repository deleted.';
  ResetRepositoryConfirmUpdate = 'Perform the Update operation now?\nIf you want to change the repository URL, answer No.\nYou may do the Update operation later in the %s tab.';
  InternetConnectionNeeded = 'This feature requires Internet connection.';
  FailedToResetRepository = 'Failed to reset the %s repository. You have to delete it manually.';
  ConfirmCodeBlocksMessage = 'Are you sure to %s the Code::Blocks integration?';
  ConfirmCodeBlocksInstallation = 'install';
  ConfirmCodeBlocksReinstallation = 'reinstall';
  ConfirmCodeBlocksUninstallation = 'uninstall definitely';
  CodeBlocksInstallationDirectoryNotExists = 'The specified Code::Blocks installation directory does not exists.';
  CodeBlocksInstallationDirectoryInvalid = 'Code::Blocks is not installed in this directory.';
  CodeBlocksIncorrectHash = 'The installed Code::Blocks version seems NOT to be the expected Code::Blocks 17.12.\nThere is no guarantee that it will work. Continue anyway?';
  UnknownElevatedTask = 'Unknown elevated task!';
  PleaseVerifyRepositories = 'Repositories settings are incorrect!';
  PleaseVerifyRubyRepository = 'Ruby repository setting is incorrect!';
  InstallRubyText = 'Ruby for the Sega Dreamcast is experimental. Are you sure to enable it?';
  UninstallRubyText = 'Are you sure to disable the Ruby support?';
  UninstallRubyFailedText = 'Failed to uninstall the Ruby directory. You may delete it manually.';
  RubySamplesNotInstalled = 'Ruby samples are not installed. Please install them by installing/reinstalling Ruby support.';
  GitNeeded = 'Git is needed to use this feature.';
  UnableToInstallRubyRuntimeText = 'Please install RubyInstaller for Windows before trying to enable Ruby.';
  UnableToInstallRubyGitText = 'Ruby online installation requires Git.';
  UnableToInstallPackageText = 'An error occured when installing the requested packages.';

  // Progress
  SendingCancelSignal = 'Sending abort signal, this can take up to one minute, please be patient...';
  CancelDialogCaption = 'Warning';
  CancelDialogText = 'Are you really sure to cancel? This may let your installation in an unpredictable state.';
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
  RubyInitializeText = 'Initialize Ruby configuration...';
  RubyBuildText = 'Building Ruby library...';

  // PackageManager | Unpack
  UnpackingText = 'Unpacking...';

implementation

end.

