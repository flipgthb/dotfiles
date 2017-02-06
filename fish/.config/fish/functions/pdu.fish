function pdu
	package-disk-usage | xargs -d \n -n 2 | awk -F" " ' {print $3,$7,$8} ' $argv
end
