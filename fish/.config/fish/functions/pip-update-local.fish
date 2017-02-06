function pip-update-local
	pip list --local --outdated | grep -v '^\-e' | cut -d ' ' -f 1 | xargs -n1 pip install --local -U
end
