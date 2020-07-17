help:
	@cat Makefile

PORT=8088

ghcid:
	ghcid --command "stack ghci" --restart stack.yaml -T "Main.main"

hlint:
	hlint src

open:
	open http://localhost:$(PORT)

watch:
	stack build --fast --file-watch

g: ghcid
h: hlint
o: open
w: watch
