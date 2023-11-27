# This is a gnu makefile with several commands to build, document and test
# the package.  The actual building and installation of the package is achieved
# with the standard R commands R CMD BUILD and R CMD INSTALL.

PKGDIR=pkg
INSTALL_FLAGS=--no-multiarch --with-keep.source

OSTYPE=$(shell Rscript -e "cat(.Platform[['OS.type']])")

ifeq ($(OSTYPE), windows)
# for unknown reason R CMD check --as-cran does not work on Windows
RCHECKARG=--no-multiarch
else
RCHECKARG=--no-multiarch
endif

# Package name, Version and date from DESCIPTION
PKG=$(shell grep Package: $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2)
PKGTAR=$(PKG)_$(shell grep Version $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2).tar.gz

ifneq ($(OSTYPE), windows)
CPP=$(shell R CMD config CXX)
CPP_FLAGS=$(shell R CMD config --cppflags)
PKG_CXXFLAGS = $(shell Rscript -e "Rcpp:::CxxFlags()")
endif

help:
	@echo
	@echo "The following targets are available:"
	@echo "   help      - displays this help"
	@echo "   test      - run the tests"
	@echo "   covr      - check package coverage (package covr)"
	@echo "   check     - Run R CMD check $(PKGDIR)"
	@echo "   syntax    - check syntax .cpp files"
	@echo "   document  - run roxygen to generate Rd files and make pdf Reference manual"
	@echo "   mkpkg     - builds source package, add to drat and checks with --as-cran"
	@echo "   bin       - builds binary package in ./tmp"
	@echo "   install   - install package in .libPaths()[1]"
	@echo "   installv  - install package with vignettes in .libPaths()[1]"
	@echo "   uninstall - uninstall package from .libPaths()[1]"
	@echo "   clean     - cleans up everything"
	@echo "   flags     - display R config flags and some macros"

flags:
	@echo OSTYPE=$(OSTYPE)
	@echo PKGDIR=$(PKGDIR)
	@echo PKG=$(PKG)
	@echo PKGTAR=$(PKGTAR)
	@echo RCHECKARG=$(RCHECKARG)
ifneq ($(OSTYPE), windows)
	@echo CPP=$(CPP)
	@echo CPP_FLAGS=$(CPP_FLAGS)
	@echo PKG_CXXFLAGS=$(PKG_CXXFLAGS)
endif
	@echo libPaths:
	@R --no-save --quiet --slave -e '.libPaths()'

test:
	Rscript test.R

test_covr:
	Rscript test_covr.R

check: cleanx install_deps syntax
	@echo " *** Running R CMD check ***"
	R CMD build $(PKGDIR)
	R CMD check $(RCHECKARG) $(PKGTAR)
	@rm -f  $(PKGTAR)

syntax:
ifneq ($(OSTYPE), windows)
	$(CPP) $(CPP_FLAGS) $(PKG_CXXFLAGS) -c -fsyntax-only -Wall \
		         -pedantic $(PKGDIR)/src/*.c*
else
	@echo Syntax checking not possible on Windows
endif

cleanx:
	@rm -f $(PKGTAR)
	@rm -fr $(PKG).Rcheck
ifneq ($(OSTYPE), windows)
        # Apple Finder rubbish
	@find . -name '.DS_Store' -delete
endif

# build date of package must be at least today
# build source package for submission to CRAN
# after building do a check as CRAN does it
mkpkg: cleanx syntax install_deps
ifeq ($(OSTYPE), windows)
	@echo Please run mkpkg on Linux or MAC OSX
else
	R CMD build $(PKGDIR)
	R CMD check --as-cran $(RCHECKARG) $(PKGTAR)
	@cp -nv $(PKGTAR) archive
	./drat.sh --pkg=$(PKGTAR)
endif

bin: install_deps
	-@rm -rf tmp
	mkdir tmp
	R CMD build $(PKGDIR)
	R CMD INSTALL $(INSTALL_FLAGS) -l ./tmp --build $(PKGTAR)

document: install_deps
	-@rm -f refman.pdf
	R -e "devtools::document('"$(PKGDIR)"')"
	R CMD Rd2pdf --no-preview $(PKGDIR) -o refman.pdf 2>&1 > refman.log

install: install_deps
	-@rm -rf tmp
	R CMD INSTALL $(INSTALL_FLAGS) $(PKGDIR)

installv: install_deps
	R CMD build $(PKGDIR)
	R CMD INSTALL $(INSTALL_FLAGS) $(PKGTAR)

install_deps:
	R --slave -f install_deps.R

uninstall:
	R CMD REMOVE $(PKG)

clean:
	-rm -fr $(PKGDIR).Rcheck
	-rm -fr tmp
	-rm -f $(PKGDIR)/src/*.o
	-rm -f $(PKGDIR)/src/*.so
	-rm -f $(PKGDIR)/src/*.dll
	-rm -rf .Rd2*
	-rm -f $(PKG)_*.zip
	-rm -f $(PKG)_*.tar.gz
