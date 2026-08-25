.PHONY: all
all: clean check

.PHONY: clean
clean:
	-rm sps_*.tar.gz
	-rm -rf sps.Rcheck
	
.PHONY: roxygen
roxygen:
	Rscript -e 'roxygen2::roxygenize()'
	
.PHONY: build
build: roxygen clean
	R CMD build .
	
.PHONY: install
install: build
	R CMD INSTALL sps_*.tar.gz
	
.PHONY: test
test: roxygen
	Rscript -e 'tinytest::build_install_test()'
	
.PHONY:check
check: build
	_R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD check --as-cran sps_*.tar.gz
