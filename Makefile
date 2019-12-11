all:
	happy happy/Tokens.y
	cabal new-build
