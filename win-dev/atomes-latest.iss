; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId=4DA2F2FC-AD2D-414A-8197-0DD52F1593D2
AppName=Atomes
AppVerName=Atomes 1.1.5
AppPublisher=CNRS
AppPublisherURL=https://atomes.ipcms.fr/
AppSupportURL=https://atomes.ipcms.fr/
AppUpdatesURL=https://atomes.ipcms.fr/
DefaultDirName={autopf}\Atomes
DisableDirPage=yes
DefaultGroupName=Atomes
LicenseFile=COPYING
OutputDir=Setup
OutputBaseFilename=atomes-1.1-setup
SetupIconFile=setup.ico
UninstallDisplayIcon=setup.ico
Compression=lzma2
SolidCompression=yes
ChangesAssociations=yes
; "ArchitecturesAllowed=x64" specifies that Setup cannot run on
; anything but x64.
ArchitecturesAllowed=x64
; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
ArchitecturesInstallIn64BitMode=x64
WizardStyle=modern

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := TRUE;
  if RegValueExists(HKEY_LOCAL_MACHINE, 'Software\IPCMS\ATOMES\1.1.4', 'Version') then begin
    MsgBox('An older version of Atomes has been detected on your computer:' #13#13 'We recommand to uninstall this previous version before installing any other', mbConfirmation, MB_OK);
    Result := FALSE;
  end;
  if RegValueExists(HKEY_LOCAL_MACHINE, 'Software\IPCMS\ATOMES\1.1.3', 'Version') then begin
    MsgBox('An older version of Atomes has been detected on your computer:' #13#13 'We recommand to uninstall this previous version before installing any other', mbConfirmation, MB_OK);
    Result := FALSE;
  end;
  if RegValueExists(HKEY_LOCAL_MACHINE, 'Software\IPCMS\ATOMES\1.1.2', 'Version') then begin
    MsgBox('An older version of Atomes has been detected on your computer:' #13#13 'We recommand to uninstall this previous version before installing any other', mbConfirmation, MB_OK);
    Result := FALSE;
  end;
  if RegValueExists(HKEY_LOCAL_MACHINE, 'Software\IPCMS\ATOMES\1.1.1', 'Version') then begin
    MsgBox('An older version of Atomes has been detected on your computer:' #13#13 'We recommand to uninstall this previous version before installing any other', mbConfirmation, MB_OK);
    Result := FALSE;
  end;
  if RegValueExists(HKEY_LOCAL_MACHINE, 'Software\IPCMS\ATOMES\1.1.0', 'Version') then begin
    MsgBox('An older version of Atomes has been detected on your computer:' #13#13 'We recommand to uninstall this previous version before installing any other', mbConfirmation, MB_OK);
    Result := FALSE;
  end;
end;

[Registry]
Root: HKLM; Subkey: "Software\IPCMS\ATOMES"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\IPCMS\ATOMES\1.1.5"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\IPCMS\ATOMES\1.1.5"; ValueType: string; ValueName: "Path"; ValueData: "{app}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\IPCMS\ATOMES\1.1.5"; ValueType: string; ValueName: "Version"; ValueData: "1.1.5"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\IPCMS\ATOMES"; Flags: uninsdeletekeyifempty
Root: HKA; Subkey: "Software\IPCMS\ATOMES\1.1.5"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\IPCMS\ATOMES\1.1.5"; ValueType: string; ValueName: "Path"; ValueData: "{app}"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\IPCMS\ATOMES\1.1.5"; ValueType: string; ValueName: "Version"; ValueData: "1.1.5"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\IPCMS\ATOMES\1.1.5"; ValueType: string; ValueName: "Name"; ValueData: "Atomes"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\IPCMS\ATOMES\1.1.5"; ValueType: string; ValueName: "Company"; ValueData: "Institut de Physique et Chimie des Mat�riaux de Strasbourg"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\.awf\OpenWithProgids"; ValueType: string; ValueName: "AtomesWorkspaceFile.awf"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\AtomesWorkspaceFile.awf"; ValueType: string; ValueName: ""; ValueData: "Atomes Workspace File"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\AtomesWorkspaceFile.awf\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "atomes-workspace.ico"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\AtomesWorkspaceFile.awf\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\atomes.exe"" ""%1"""
Root: HKA; Subkey: "Software\Classes\.apf\OpenWithProgids"; ValueType: string; ValueName: "AtomesProjectFile.apf"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\AtomesProjectFile.awf"; ValueType: string; ValueName: ""; ValueData: "Atomes Project File"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\AtomesProjectFile.apf\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "atomes-project.ico"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\AtomesProjectFile.apf\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\atomes.exe"" ""%1"""
Root: HKA; Subkey: "Software\Classes\Applications\atomes.exe\SupportedTypes"; ValueType: string; ValueName: ".awf"; ValueData: ""; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Applications\atomes.exe\SupportedTypes"; ValueType: string; ValueName: ".apf"; ValueData: ""; Flags: uninsdeletekey
Root: HKLM; SubKey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment\"; ValueType: string; ValueName: "Path"; ValueData: "{reg:HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\,Path};{app}"

[Code]
procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Path, AppDir: string;
  Index: Integer;
begin
  if CurUninstallStep = usUninstall then
  begin
    if RegQueryStringValue(HKEY_LOCAL_MACHINE,
      'SYSTEM\CurrentControlSet\Control\Session Manager\Environment\',
      'Path', Path) then
    begin
      AppDir := ExpandConstant('{app}\bin');
      Index := Pos(AppDir, Path);
      Delete(Path, Index-1, Length(AppDir)+1);
      RegWriteStringValue(HKEY_LOCAL_MACHINE,
        'SYSTEM\CurrentControlSet\Control\Session Manager\Environment\',
        'Path', Path);
    end;
  end;
end;

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Components: main; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Components: main; Flags: unchecked

[Files]
Source: "atomes\pixmaps\*"; DestDir: "{app}\pixmaps\"; Components: main; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "atomes\library\*"; DestDir: "{app}\library"; Components: main; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "atomes\bin\*"; DestDir: "{app}\bin\"; Components: main; Flags: ignoreversion
Source: "atomes\etc\*"; DestDir: "{app}\etc\"; Components: main; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "atomes\lib\*"; DestDir: "{app}\lib\"; Components: main; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "atomes\share\*"; DestDir: "{app}\share\"; Components: main; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "atomes\var\*";  DestDir: "{app}\var\"; Components: main; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "atomes.ico"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "setup.ico"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "atomes-workspace.ico"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "atomes-project.ico"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "COPYING"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "ChangeLog"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "gtk-sources\*";  DestDir: "{app}\gtk-sources\"; Components: main; Flags: ignoreversion

; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Atomes"; Filename: "{app}\bin\atomes.exe"; WorkingDir: "{app}"; IconFilename: "{app}\atomes.ico"; Components: main
Name: "{group}\{cm:ProgramOnTheWeb,Atomes}"; Filename: "https://atomes.ipcms.fr/"; Components: main;
Name: "{group}\{cm:UninstallProgram,Atomes}"; Filename: "{uninstallexe}"; IconFilename: "{app}\atomes.ico"; Components: main;
Name: "{commondesktop}\Atomes"; Filename: "{app}\bin\atomes.exe"; Tasks: desktopicon; WorkingDir: "{app}"; IconFilename: "{app}\atomes.ico"; Components: main
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\Atomes"; Filename: "{app}\bin\atomes.exe"; WorkingDir: "{app}"; Tasks: quicklaunchicon; IconFilename: "{app}\atomes.ico"; Components: main

[Components]
Name: "main"; Description: "Atomes"; Types: full compact custom   

[Run]
Filename: "{app}\bin\atomes.exe"; Description: "{cm:LaunchProgram,Atomes}"; Flags: nowait postinstall skipifsilent

