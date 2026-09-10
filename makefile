VERSION := $(shell grep -oP '^Version: \K.*' DESCRIPTION)
PKG := $(shell grep -oP '^Package: \K.*' DESCRIPTION)

.PHONY: all
all: check coverage

.PHONY: clean
clean:
	rm -f $(PKG)_*.tar.gz
	rm -rf $(PKG).Rcheck
	rm -f coverage-report.html
	
.PHONY: roxygen
roxygen:
	Rscript -e 'roxygen2::roxygenize()'

.PHONY: build
build: roxygen clean
	R CMD build .
	
.PHONY: install
install: build
	R CMD INSTALL $(PKG)_$(VERSION).tar.gz
	
.PHONY: test
test: roxygen
	Rscript -e 'tinytest::build_install_test()'
	
.PHONY: lint
lint: roxygen
	Rscript -e 'lintr::lint_package()'
	
.PHONY: check
check: build lint
	_R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD check --as-cran $(PKG)_$(VERSION).tar.gz
	
README.md: README.Rmd
	Rscript -e 'litedown::fuse("README.Rmd", "README.md")'
	
vignettes/performance.Rmd: vignettes/performance.Rmd.orig
	Rscript -e 'litedown::fuse("vignettes/performance.Rmd.orig", "vignettes/performance.md")'
	mv vignettes/performance.md vignettes/performance.Rmd

.PHONY: readme
readme: install README.md

.PHONY: coverage
coverage:
	Rscript -e 'covr::report(file = "coverage-report.html")'