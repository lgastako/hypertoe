help:
	@cat Makefile

PORT=8088

ghcid:
	ghcid --command "stack ghci" --restart stack.yaml -T "Main.main"

open:
	open http://localhost:$(PORT)

watch:
	stack build --fast --file-watch

g: ghcid
o: open
w: watch
