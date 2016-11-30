# This is a gnu makefile. The makefile contains some commands that makes
# it easy to build and install the package, but is it NOT used to build
# the package itself (for that purpose R CMD INSTALL in used).
# On Windows, the Rtools directories (e.g. c:\Rtools\bin and 
# c:\Rtools\gcc-4.6.3\bin) should be in the path, but you should not use
# the make of c:\Rtools\bin to run this file. Instead use the standard 
# gnuwin32 make command (at the ontwikkelomgeving at the CPB, use 
# command gwmake instead of make).
PKGDIR=pkg
VIEWER=gvim
OSNAME := $(shell uname | tr A-Z a-z)
INSTALL_FLAGS=--no-multiarch --with-keep.source 
RCHECKARG=--no-multiarch

# Package name, Version and date from DESCIPTION
PKG=$(shell grep 'Package:' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2)
PKGTAR=$(PKG)_$(shell grep 'Version' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2).tar.gz
PKGDATE=$(shell grep 'Date' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2)
TODAY=$(shell date "+%Y-%m-%d")

PKG_CXXFLAGS = -std=c++0x -I $(HOME)/R/x86_64-pc-linux-gnu-library/3.3/Rcpp/include 

.PHONY: clean cleanx check install uninstall mkpkg bin pdf

help:
	@echo
	@echo "The following targets are available:"
	@echo "   help      - displays this help"
	@echo "   test      - run the tests"
	@echo "   covr      - check package coverage (package covr)"
	@echo "   check     - Run R CMD check $(PKGDIR)"
	@echo "   syntax    - check syntax .cpp files"
	@echo "   document  - run roxygen to generate Rd files and make pdf Reference manual"
	@echo "   mkpkg     - builds source package and checks with --as-cran"
	@echo "   bin       - builds binary package in ./tmp"
	@echo "   install   - install package in .libPaths()[1]"
	@echo "   uninstall - uninstall package from .libPaths()[1]"
	@echo "   clean     - cleans up everything"
	@echo "   flags     - display R config flags and some macros"

# make sure that R's variables are used
# if you don't do this you'll get make's initial values
# gives error doing syntax target
#R_CPPFLAGS=$(shell R CMD config --cppflags)
CC=$(shell R CMD config CC)
CXX=$(shell R CMD config CXX)
CPP_FLAGS=$(shell R CMD config --cppflags)

flags:
	@echo "SHELL=$(SHELL)"
	@echo "CPP_FLAGS=$(CPP_FLAGS)"
	@echo "PKGDIR=$(PKGDIR)"
	@echo "PKG=$(PKG)"
	@echo "PKGTAR=$(PKGTAR)"
	@echo "PKGDATE=$(PKGDATE)"
	@echo "R .libPaths()"
	@echo "FC=$(FC)"
	@echo "F77=$(F77)"
	@echo "CC=$(CC)"
	@echo "CPP=$(CPP)"
	@echo "CPP_FLAGS=$(CPP_FLAGS)"
	@R --no-save --quiet --slave -e '.libPaths()'


test:
	Rscript test.R

test_covr:
	Rscript test_covr.R

check: cleanx syntax
	@echo " *** Running R CMD check ***"
	R CMD build $(PKGDIR)
	R CMD check $(RCHECKARG) $(PKGTAR)
	@rm -f  $(PKGTAR)
	@echo "Today                           : $(TODAY)"
	@echo "Checked package description date: $(PKGDATE)"
# 	@Rscript -e 'cat("Installed version date          :",packageDescription("nleqslv", fields="Date"))'
	@echo ""

syntax:
	$(CXX) $(CPP_FLAGS) $(PKG_CXXFLAGS) -c -fsyntax-only -Wall -pedantic $(PKGDIR)/src/*.c*

cleanx:
# Apple Finder rubbish
	@find . -name '.DS_Store' -delete
	@rm -f $(PKGTAR)
	@rm -fr $(PKG).Rcheck

# build date of package must be at least today
# build source package for submission to CRAN
# after building do a check as CRAN does it
mkpkg: cleanx syntax
	R CMD build $(PKG)
	R CMD check --as-cran $(RCHECKARG) $(PKGTAR)
	@cp -nv $(PKGTAR) archive
	@echo "Today                           : $(TODAY)"
	@echo "Checked package description date: $(PKGDATE)"
# 	@Rscript -e 'cat("Installed version date          :",packageDescription("nleqslv", fields="Date"))'
	@echo ""

bin:
	-@rm -rf tmp
	mkdir tmp
	R CMD INSTALL -l ./tmp --build $(PKGDIR)

document:
	-@rm -f $(PKGDIR).pdf
	R -e "roxygen2::update_collate('"$(PKGDIR)"'); devtools::document('"$(PKGDIR)"')"
	R CMD Rd2pdf --batch $(PKGDIR) 2>$(PKGDIR).log

install:
	-@rm -rf tmp
	R CMD INSTALL $(INSTALL_FLAGS) $(PKGDIR)

uninstall:
	R CMD REMOVE $(PKG)

clean:
	rm -fr $(PKGDIR).Rcheck
	rm -fr tmp
	rm -f $(PKGTAR)
	rm -f $(PKGDIR).pdf
	rm -f $(PKGDIR).log
