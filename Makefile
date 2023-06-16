.PHONY: all fmt test

all: test

test:
	scala-cli test .

fmt:
	scala-cli fmt .
