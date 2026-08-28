VERSION := $(shell grep -oP '^Version: \K.*' DESCRIPTION)
PKG := $(shell grep -oP '^Package: \K.*' DESCRIPTION)

.PHONY: all
all: check

.PHONY: clean
clean:
	rm -f $(PKG)_*.tar.gz
	rm -rf $(PKG).Rcheck
	
.PHONY: roxygen
roxygen:
	Rscript -e 'roxygen2::roxygenize()'
	
README.md: README.Rmd
	Rscript -e 'litedown::fuse("README.Rmd", "README.md")'

.PHONY: build
build: roxygen clean
	R CMD build .
	
.PHONY: install
install: build
	R CMD INSTALL $(PKG)_$(VERSION).tar.gz
	
.PHONY: test
test: roxygen
	Rscript -e 'tinytest::build_install_test()'
	
.PHONY: check
check: build README.md
	_R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD check --as-cran $(PKG)_$(VERSION).tar.gz
	
vignettes/performance.Rmd: vignettes/performance.Rmd.orig
	Rscript -e 'litedown::fuse("vignettes/performance.Rmd.orig", "vignettes/performance.md")'
	mv vignettes/performance.md vignettes/performance.Rmd
