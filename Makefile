ELISP_FILES := $(shell cask files)
ELC_FILES := $(patsubst %.el,%.elc,$(ELISP_FILES))

.PHONY: test compile clean
.INTERMEDIATE: .compile.intermediate

all: test

# We run clean-elc before testing because undercover.el doesn't support elc
# files.
test: clean
	cask exec buttercup -L . tests

compile: $(ELC_FILES)

$(ELC_FILES): .compile.intermediate
.compile.intermediate: $(ELISP_FILES)
	cask build

clean:
	cask clean-elc
