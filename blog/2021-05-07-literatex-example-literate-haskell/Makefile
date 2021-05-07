help:
	@echo "make ghci-nix   load the script into a REPL using Nix"
	@echo "make run-nix    run the script using Nix"
	@echo "make run-stack  run the script using Stack"
.PHONY: help

ghci-nix:
	@nix-shell \
		-p "haskellPackages.ghcWithPackages (p: [p.ttc])" \
		--run "export TERM=xterm; ghci prompt.lhs"
.PHONY: ghci-nix

run-nix:
	@nix-shell \
		-p "haskellPackages.ghcWithPackages (p: [p.ttc])" \
		--run "export TERM=xterm; runhaskell prompt.lhs"
.PHONY: run-nix

run-stack:
	@stack script --resolver lts-17.10 --package time --package ttc prompt.lhs
.PHONY: run-stack
