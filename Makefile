RUSTC ?= rustc
RUSTFLAGS ?= -O

RUSTDOC ?= rustdoc

LIB_RS = src/xml/lib.rs
ALL_RS = $(shell find ./src -type f -name '*.rs')

all: $(LIB_RS) $(ALL_RS)
	$(RUSTC) $(RUSTFLAGS) $<

test: $(LIB_RS) $(ALL_RS)
	$(RUSTC) $(RUSTFLAGS) $< -o $@ --test

run-test: test
	./test $(TEST_ARGS)

docs: $(LIB_RS) $(ALL_RS)
	$(RUSTDOC) $<
