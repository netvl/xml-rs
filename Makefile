RUSTHOME ?= $(HOME)/dev/lang/rust/dist/rust-0.10
RUSTC ?= $(RUSTHOME)/bin/rustc
RUSTFLAGS ?= -O

RUSTDOC ?= $(RUSTHOME)/bin/rustdoc

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
