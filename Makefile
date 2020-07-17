help:
	@cat Makefile

ghcid:
	ghcid --command "stack ghci" --restart stack.yaml -T "Main.main"

watch:
	stack build --fast --file-watch

g: ghcid
w: watch
