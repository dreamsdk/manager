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
    imgLogo: TImage;
    lblIntroduction: TLabel;
    lblCopyleft: TLabel;
    lblCreditsTitle: TLabel;
    memDescription: TMemo;
    pnlSeparator1: TPanel;
    pnlSeparator2: TPanel;
    tvwComponents: TTreeView;
    procedure btnCloseClick(Sender: TObject);
    procedure edtComponentURLClick(Sender: TObject);
    procedure edtComponentURLMouseEnter(Sender: TObject);
    procedure edtComponentURLMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvwComponentsChange(Sender: TObject; Node: TTreeNode);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

uses
  LCLIntf, SysTools, Version;

type
  TComponentItem = packed record
    Text: string;
    Url: string;
  end;

resourcestring
  ComponentEnvironment = 'About environment components';
  ComponentMinGW = 'MinGW, a contraction of "Minimalist GNU for Windows", is a minimalist development environment for native Microsoft Windows applications.\n\nMinGW provides a complete Open Source programming tool set which is suitable for the development of native MS-Windows applications, and which do not depend on any 3rd-party C-Runtime DLLs. (It does depend on a number of DLLs provided by Microsoft themselves, as components of the operating system; most notable among these is MSVCRT.DLL, the Microsoft C runtime library. Additionally, threaded applications must ship with a freely distributable thread support DLL, provided as part of MinGW itself).';
  ComponentMSYS = 'MSYS is a collection of GNU utilities such as bash, make, gawk and grep to allow building of applications and programs which depend on traditionally UNIX tools to be present. It is intended to supplement MinGW and the deficiencies of the cmd shell.';
  ComponentToolchains = 'About toolchains components used for both Hitachi SuperH (SH-4) CPU and Yamaha AICA Super Intelligent Sound Processor (ARM)';
  ComponentBinutils = 'The GNU Binutils are a collection of binary tools. The main ones are:\n- ld - the GNU linker.\n- as - the GNU assembler.\n\nBut they also include:\n- addr2line - Converts addresses into filenames and line numbers.\n- ar - A utility for creating, modifying and extracting from archives.\n- c++filt - Filter to demangle encoded C++ symbols.\n- dlltool - Creates files for building and using DLLs.\n- gold - A new, faster, ELF only linker, still in beta test.\n- gprof - Displays profiling information.\n- nlmconv - Converts object code into an NLM.\n- nm - Lists symbols from object files.\n- objcopy - Copies and translates object files.\n- objdump - Displays information from object files.\n- ranlib - Generates an index to the contents of an archive.\n- readelf - Displays information from any ELF format object file.\n- size - Lists the section sizes of an object or archive file.\n- strings - Lists printable strings from files.\n- strip - Discards symbols.\n- windmc - A Windows compatible message compiler.\n- windres - A compiler for Windows resource files.';
  ComponentGCC = 'The GNU Compiler Collection includes front ends for C, C++, Objective-C, Fortran, Ada, and Go, as well as libraries for these languages (libstdc++,...). GCC was originally written as the compiler for the GNU operating system. The GNU system was developed to be 100% free software, free in the sense that it respects the user''s freedom.\n\nWe strive to provide regular, high quality releases, which we want to work well on a variety of native and cross targets (including GNU/Linux), and encourage everyone to contribute changes or help testing GCC. Our sources are readily and freely available via SVN and weekly snapshots.\n\nMajor decisions about GCC are made by the steering committee, guided by the mission statement.';
  ComponentNewlib = 'Newlib is a C library intended for use on embedded systems. It is a conglomeration of several library parts, all under free software licenses that make them easily usable on embedded products.\n\nNewlib is only available in source form. It can be compiled for a wide array of processors, and will usually work on any architecture with the addition of a few low-level routines.';
  ComponentGDB = 'GDB, the GNU Project debugger, allows you to see what is going on `inside'' another program while it executes -- or what another program was doing at the moment it crashed.\n\nGDB can do four main kinds of things (plus other things in support of these) to help you catch bugs in the act: \n- Start your program, specifying anything that might affect its behavior.\n- Make your program stop on specified conditions.\n- Examine what has happened, when your program has stopped.\n- Change things in your program, so you can experiment with correcting the effects of one bug and go on to learn about another. \nThose programs might be executing on the same machine as GDB (native), on another machine (remote), or on a simulator. GDB can run on most popular UNIX and Microsoft Windows variants, as well as on Mac OS X.';
  ComponentLibraries = 'About Sega Dreamcast development libraries';
  ComponentKallisti = 'KallistiOS is a free, BSD license-based development system for the Sega Dreamcast game console. The project was initiated in 2000 by Dan Potter, then garnering a team of free software developers over the Internet. Kallisti, from the Greek "to the fairest"; Kallisti OS (Operating System); or the abbreviation KOS, which is pronounced like "Chaos".\n\nKallistiOS is the successor of libdream, the first free developer library for Dreamcast. Libdream was also written by Dan Potter and published in mid-2000. It served as the basis of some hobby projects, but was soon replaced by KallistiOS.\n\nKallistiOS allowed the emergence of a homebrew developer community on the Dreamcast. Several free and commercial games have been created using it, such as Last Hope (video game), Feet of Fury, and Inhabitants.';
  ComponentKallistiPorts = 'KallistiOS Ports is a repository of various useful libraries that have been ported to KOS over the years. These libraries provide various tools such as image reading support, archive file reading and writing, and even a OpenGL-like environment. Each port is meant to be as self-contained as possible (which does not mean it can''t have dependent ports, of course) and should build on the current version of KallistiOS with minimal fuss.\n\nFor those of you familiar with FreeBSD at all, this works a lot like their ports collection.';
  ComponentDreamcastTool = 'Dreamcast Tool (dc-tool) is a set of programs made to send and receive data from your Sega Dreamcast system. The classic use of this tool is to send programs to the Dreamcast in order to run and debug them.\n\nTo be used, you must have a way to connect your Dreamcast console to your computer, it can be one of the following:\n- A Coders Cable (a serial cable, the historical way to do that)\n- A Broadband Adapter, often shortened as "BBA" (a 10/100Mbits network Ethernet card).\n\nThis program, originally developed by ADK/Napalm and now full part of the KallistiOS library is split in two parts:\n- dcload, the server part, started from the Sega Dreamcast;\n- dc-tool, the client part, started from the computer.\n\nDepending on the chosen method (Coders Cable or BBA) you must use the right version of the program:\n- dcload-serial for the Coders Cable version\n- dcload-ip for the BBA version';
  ComponentDependencies = 'About external dependencies used in this package';
  ComponentGit = 'Git is a free and open source distributed version control system designed to handle everything from small to very large projects with speed and efficiency. \n\nGit is easy to learn and has a tiny footprint with lightning fast performance. It outclasses SCM tools like Subversion, CVS, Perforce, and ClearCase with features like cheap local branching, convenient staging areas, and multiple workflows.';
  ComponentSubversion = 'Subversion is an open source version control system. Founded in 2000 by CollabNet, Inc., the Subversion project and software have seen incredible success over the past decade. Subversion has enjoyed and continues to enjoy widespread adoption in both the open source arena and the corporate world.';
  ComponentPython = 'Python is an interpreted high-level programming language for general-purpose programming. Created by Guido van Rossum and first released in 1991, Python has a design philosophy that emphasizes code readability, notably using significant whitespace. It provides constructs that enable clear programming on both small and large scales. In July 2018, Van Rossum stepped down as the leader in the language community after 30 years.\n\nPython features a dynamic type system and automatic memory management. It supports multiple programming paradigms, including object-oriented, imperative, functional and procedural, and has a large and comprehensive standard library.\n\nPython interpreters are available for many operating systems. CPython, the reference implementation of Python, is open source software and has a community-based development model, as do nearly all of Python''s other implementations. Python and CPython are managed by the non-profit Python Software Foundation.';
  ComponentMisc = 'About miscallenaous components';
  ComponentFreePascal = 'Free Pascal is a 32, 64 and 16 bit professional Pascal compiler. It can target many processor architectures: Intel x86 (including 8086), AMD64/x86-64, PowerPC, PowerPC64, SPARC, ARM, AArch64, MIPS and the JVM. Supported operating systems include Linux, FreeBSD, Haiku, Mac OS X/iOS/iPhoneSimulator/Darwin, DOS (16 and 32 bit), Win32, Win64, WinCE, OS/2, MorphOS, Nintendo GBA, Nintendo DS, Nintendo Wii, Android, AIX and AROS. Additionally, support for the Motorola 68k architecture is available in the development versions.';
  ComponentLazarus = 'Lazarus is a Delphi compatible cross-platform IDE for Rapid Application Development. It has variety of components ready for use and a graphical form designer to easily create complex graphical user interfaces.';
  ComponentInnoSetup = 'Inno Setup is a free installer for Windows programs by Jordan Russell and Martijn Laan. First introduced in 1997, Inno Setup today rivals and even surpasses many commercial installers in feature set and stability.';
  ComponentResources = 'Resources used in this project';
  ComponentMainIcon = 'Settings Icon made by Martz90\nIconset: Circle Icons\nLicense: CC Attribution-Noncommercial-No Derivate 4.0';
  ComponentSetupIcon = 'Setup Icon made by Roundicons from FlatIcon\nLicensed by Creative Commons BY 3.0.';

const
  COMPONENTS_INFORMATION: array[0..22] of TComponentItem = (
    (Text: ComponentEnvironment;    Url: ''), // Environment
    (Text: ComponentMinGW;          Url: 'http://mingw.org/'), // MinGW
    (Text: ComponentMSYS;           Url: 'http://mingw.org/wiki/MSYS'), // MSYS
    (Text: ComponentToolchains;     Url: ''), // Toolchains
    (Text: ComponentBinutils;       Url: 'http://www.gnu.org/software/binutils/'), // Binutils
    (Text: ComponentGCC;            Url: 'https://gcc.gnu.org/'), // GCC
    (Text: ComponentNewlib;         Url: 'http://sourceware.org/newlib/'), // Newlib
    (Text: ComponentGDB;            Url: 'https://www.gnu.org/software/gdb/'), // GDB
    (Text: ComponentLibraries;      Url: ''), // Libraries
    (Text: ComponentKallisti;       Url: 'http://cadcdev.sourceforge.net/softprj/kos/'), // KallistiOS
    (Text: ComponentKallistiPorts;  Url: 'http://cadcdev.sourceforge.net/softprj/kos/'), // KallistiOS Ports
    (Text: ComponentDreamcastTool;  Url: 'http://cadcdev.sourceforge.net/softprj/kos/'), // Dreamcast Tool
    (Text: ComponentDependencies;   Url: ''), // Dependencies
    (Text: ComponentGit;            Url: 'https://git-scm.com/'), // Git
    (Text: ComponentSubversion;     Url: 'http://subversion.apache.org/'), // Subversion
    (Text: ComponentPython;         Url: 'https://www.python.org/'),  // Python
    (Text: ComponentMisc;           Url: ''), // Misc
    (Text: ComponentFreePascal;     Url: 'https://www.freepascal.org/'), // Free Pascal
    (Text: ComponentLazarus;        Url: 'https://www.lazarus-ide.org/'), // Lazarus
    (Text: ComponentInnoSetup;      Url: 'http://www.jrsoftware.org/'), // Inno Setup
    (Text: ComponentResources;      Url: ''), // ComponentResources
    (Text: ComponentMainIcon;       Url: 'https://www.deviantart.com/martz90'), // Main Icon
    (Text: ComponentSetupIcon;      Url: 'https://www.flaticon.com/authors/roundicons') // Setup Icon
  );

{ TfrmAbout }

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.edtComponentURLClick(Sender: TObject);
begin
  if edtComponentURL.Text <> '' then
    OpenURL(edtComponentURL.Text);
end;

procedure TfrmAbout.edtComponentURLMouseEnter(Sender: TObject);
begin
  with (Sender as TLabeledEdit) do
  if IsInString('@', Text) or IsInString('http', Text) then
  begin
    Font.Underline := True;
    Font.Color := clHotLight;
    Cursor := crHandPoint;
  end;
end;

procedure TfrmAbout.edtComponentURLMouseLeave(Sender: TObject);
begin
  with (Sender as TLabeledEdit) do
  begin
    Font.Underline := False;
    Font.Color := clDefault;
    Cursor := crDefault;
  end;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblCreditsTitle.Caption := GetProductName;
  lblCopyleft.Caption := Format(lblCopyleft.Caption, [GetLegalCopyright, GetCompanyName]);
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  tvwComponents.Items[0].Selected := True;
  tvwComponents.SetFocus;
end;

procedure TfrmAbout.tvwComponentsChange(Sender: TObject; Node: TTreeNode);
var
  SelectedItem: TComponentItem;

begin
  SelectedItem := COMPONENTS_INFORMATION[Node.SelectedIndex];
  memDescription.Lines.Text := StringReplace(SelectedItem.Text, '\n', sLineBreak, [rfReplaceAll]);
  edtComponentURL.Text := SelectedItem.Url;
  edtComponentURL.Visible := edtComponentURL.Text <> '';
end;

end.


