function package-disk-usage
	pacman -Qei | grep -e "Name" -e "Installed Size" $argv;
end
