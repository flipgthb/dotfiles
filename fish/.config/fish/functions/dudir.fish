function dudir
	set thisdir (readlink -e $argv[1])
	du -hs $thisdir
	for l in $thisdir
		echo $l
	end
	for d in (ls $thisdir)
		du -hs $thisdir/$d
	end
end
