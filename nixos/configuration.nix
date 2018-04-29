# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };
  
  #boot.loader.grub.enable = true;
  #boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  #boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  users.extraUsers.felippe = {
    isNormalUser = true;
    description = "Felippe Alves";
    extraGroups = [ "wheel" "networkmanager" "audio" "video" "power" ];
  };

  #networking.hostName = "nixos-nb"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  #networking.networkmanager.enable = true;
  
  networking = {
    hostName = "nixos-nb";
    networkmanager.enable = true;
  };

  # Select internationalisation properties.
  i18n = {
    # consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "br-abnt2";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  programs.fish.enable = true;
  users.defaultUserShell = "/var/run/current-system/sw/bin/fish";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget curl
    emacs
    dropbox
    fish
    nix-repl
    htop
    man-pages
    python3
    ripgrep
    # smartmontools
    firefox
    git
    stow
    terminator
    arandr
    rofi
    polybar
    vlc
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # Enable the X11 windowing system.
  #services.xserver.enable = true;
  #services.xserver.layout = "br";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  #services.xserver.libinput.enable = true;
  #services.xserver.libinput.tapping = false;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  #services.xserver.displayManager.lightdm.enable = true;
  #services.xserver.windowManager.i3.enable = true;

  services.xserver = {
    enable = true;
    layout = "br";
    libinput = {
      enable = true;
      tapping = false;
    };
    displayManager.lightdm.enable = true;
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };

  fonts = {
        enableFontDir = true;
        enableCoreFonts = true;
        enableGhostscriptFonts = true;
        fonts = with pkgs ; [
            dejavu_fonts
            terminus_font
            source-sans-pro
            source-code-pro
        ];
  };

  nixpkgs.config = {
    allowUnfree = true;
    firefox.enableAdobeFlash = true;
  };
  
  #powerManagement.enable = true;
  services.upower.enable = true;
  powerManagement.enable = true;
  services.logind.extraConfig = ''
    HandlePowerKey=suspend
    HandleSuspendKey=suspend
    HandleHibernateKey=suspend
    HandleLidSwitch=suspend
    HandleLidSwitchDocked=ignore
  '';


  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
