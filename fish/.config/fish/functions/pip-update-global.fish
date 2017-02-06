function pip-update-global
	pip list --outdated | grep -v '^\-e' | cut -d ' ' -f 1 | xargs -n1 sudo pip install -U
end
