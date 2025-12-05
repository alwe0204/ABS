# #################################################################
# ABS Main Makefile                                               #
# 
# A. Weber (a.weber@unibw.de), May 9, 2017
# Gunther Reissig, Apr 2022
#
# $Id: Makefile 2645 2022-04-19 14:29:02Z lf1agure $
#                                                                 #
# #################################################################

# #################################################################
# Definitely use bash shell                                       #
SHELL := /bin/bash
# #################################################################

# #################################################################
# Problem Compiler (directory) name
PROBLEMCOMPILERDIR := ProblemCompiler
# #################################################################

# #################################################################
# ABS Library (directory) name
ABSTRACTIONDIR := ABSLibrary
# #################################################################

# #################################################################
# System prerequisites:                                           #
# Third-party Libraries and Software in the Repository:
GMP  = gmp-6.1.2
MPFR = mpfr-3.1.5
MPFI = mpfi-1.5.1
GAPPA = gappa-1.3.1
# #################################################################

# #################################################################
# Native gcc (The compiler of AdaCore is also called gcc (why?))
CC = /usr/bin/gcc
# #################################################################

# #################################################################
# System prerequisites:                                           #
# AdaCore software collection                                     #
ADACORE = gnat-gpl-2016-x86_64-linux-bin
# #################################################################

# #################################################################
# Ada utility tools                                               #
GPRBUILD = gprbuild
GPRCLEAN = gprclean
GPRDOC   = gnatdoc
# #################################################################

# #################################################################
# System prerequisites:                                           #
# Third-party Libraries and Software Outside the Repository       #
# 1. Basic Part:                                                  #
# make immediately stops execution with error status if           #
# not available; sufficient for successfully applying ABS         #
# to solve example problems such as the one in the section        #
# "Quick Start" in programmer's manual                            #
GCC_EXISTS     := $(shell command -v $(CC) 2> /dev/null)
M4_EXISTS      := $(shell command -v m4 2> /dev/null)
FLEX_EXISTS    := $(shell command -v flex 2> /dev/null)
BISON_EXISTS   := $(shell command -v bison 2> /dev/null)
BOOST_EXISTS   := $(shell command whereis boost | grep "/usr/include/boost" 2> /dev/null ; echo $$? )
# 2. Advanced Part:                                               #
# needed only for software development                            #
DOXYGEN_EXISTS := $(shell command -v doxygen 2> /dev/null)
MMA_EXISTS     := $(shell command -v math 2> /dev/null)
# #################################################################

# #################################################################
# System specification/architecture                               #
ifndef GCC_EXISTS
$(error "Package gcc is not available. Please install it on your system. Try 'sudo apt-get install gcc'." )
endif
HOSTTARGET := $(shell $(CC) -dumpmachine)
# #################################################################


# #################################################################
# Directory where this Makefile is located: branch root           #
MKFILE       := $(abspath $(lastword $(MAKEFILE_LIST)))
BRANCH_ROOT  := $(patsubst %/,%,$(dir $(MKFILE)))
# #################################################################

# #################################################################
# PATH variable 
ORIGINALPATH := $(PATH)
PATH         := $(BRANCH_ROOT)/libs/$(ADACORE)/bin:$(PATH)
#
# We have to set it as AdaCore Software is not smart enough to
# allow a more convenient solution
# #################################################################

# #################################################################
# Gpr files for ABS Library                                       #
MAINGPRABSLIBRARY := $(BRANCH_ROOT)/spec/$(ABSTRACTIONDIR).gpr
TESTROGPRABSLIBRARY := $(BRANCH_ROOT)/spec/test_ro/$(ABSTRACTIONDIR)/test_ro.gpr
TESTRWGPRABSLIBRARY := $(BRANCH_ROOT)/spec/test_rw/$(ABSTRACTIONDIR)/test_rw.gpr
READWRITEGPRABSLIBRARY := $(BRANCH_ROOT)/spec/readwrite/$(ABSTRACTIONDIR)/readwrite.gpr
READONLYGPRABSLIBRARY := $(BRANCH_ROOT)/spec/readonly/$(ABSTRACTIONDIR)/readonly.gpr
HIDDENGPRABSLIBRARY := $(BRANCH_ROOT)/spec/hidden/$(ABSTRACTIONDIR)/hidden.gpr
# #################################################################

# #################################################################
# Makefile of Problem Compiler
MAKEFILEPROBLEMCOMPILER = $(BRANCH_ROOT)/spec/Makefile.$(PROBLEMCOMPILERDIR)
# #################################################################

# #################################################################
# Template directory and template files                           #
TEMPLATESDIR := templates
PROBLEMMAKEFILE := ProblemMakefile.template
PROBLEMGPRFILE  := ProblemGprfile.template
HIDDENGPRFILE := hiddenGprfile.template
# #################################################################

# #################################################################
# Everything related to manuals:
# 1. Location of manuals (directory):
MANUALSDIR := $(BRANCH_ROOT)/doc/Manuals/texfiles
# 2. Names of manuals (without file extension)
USERMANUAL := usermanual
PROGRAMMERMANUAL := programmermanual
# 3. temporarily set latex' shell variables
# so as to use *.sty, *.bst and *.bib files
# from within working copy of ABS:
TEXINPUTS := ./TeX_links/GReissig_Styles/:$(TEXINPUTS)
BSTINPUTS := ./TeX_links/BibTeX/:$(BSTINPUTS)
BIBINPUTS := ./TeX_links/BibTeX//:$(BIBINPUTS)
# 4. options for latexmk:
LATEXMK_OPTS := -pdfps -src-specials -interaction=nonstopmode -halt-on-error -time-
# #################################################################

# #################################################################
# Administration files
#
# information on state of project in subversion repository and
# on respective working copy; contents to be used in
# documentation and in log files:
REPOSITORYFILESYSTEMINFO := $(BRANCH_ROOT)/admin/repository_and_file_system_info.txt
# template for aforementioned file:
REPOSITORYFILESYSTEMINFO_TEMPLATE := $(BRANCH_ROOT)/templates/repository_and_file_system_info.template

# version information, maintained by project admin; see
# programmer's manual:
VERSIONINFOPRIVATE := $(BRANCH_ROOT)/admin/version_info_private.txt
# as before, but automatically generated after verification of contents:
VERSIONINFOPRIVATE_VERIFIED := $(BRANCH_ROOT)/admin/version_info_private_verified.txt

# version information, automatically generated, contents to be
# used in documentation:
VERSIONINFOPUBLIC := $(MANUALSDIR)/utils/version_info_public.txt
# template for aforementioned file:
VERSIONINFOPUBLIC_TEMPLATE := $(BRANCH_ROOT)/templates/version_info_public.template
# #################################################################

# #################################################################
# Some colors for the output                                      #
REDBOLD =\033[1;31m
RED     =\033[0;31m
GREEN   =\033[0;32m
YELLOW  =\033[0;33m
BLUE    =\033[0;34m
BLUEBOLD=\033[1;34m
NOCOLOR =\033[0m
# #################################################################

# #################################################################
# Some string constants:
# 1. Notification on hidden sources:
PRINTHIDDENSRCAVAIL:="Hidden sources available"
# 2. sed script to replace lines 2 through 6 in
# automatically generated files with a comment:
COMMENT_AUTO_GEN:="2,6c%  1 %\\n%  2 %\\n%  3 % Do not change, \
do not commit into repository !\\n%  4 % This is an automatically \
generated file; its contents is to be\\n%  5 % used \
in the documentation and in log files."
# #################################################################

# #################################################################
# Browser to use                                                  #
BROWSER=firefox
# #################################################################

# #################################################################
# The binary files of the tests                                   #
# NOT ':=' below !!!                                              #
# The file name of test binaries ALWAYS starts with "test_"       #
TESTS = $(notdir $(wildcard $(BRANCH_ROOT)/bin/$(HOSTTARGET)/debug/test_*))
# #################################################################

# #################################################################
# Flags                                                           #
GPRCLEANFLAGS := -r
# #################################################################

# #################################################################
COMPILINGMODES := debug release
# #################################################################

# #################################################################
# Remote hosts and other related staff                            #
HOSTAMDSUSELINUX = uranus
HOSTAMDGNULINUX  = vesta
REMOTE_TMP_DIR   = .ABS
TARGET           = $(HOSTTARGET)
# #################################################################

# #################################################################
# RULES
# #################################################################

# #################################################################
# Targets mentioned in programmer's manual:                       #
.PHONY: all pcompiler abslib tests testing doc readdoc manuals
.PHONY: newproblem debug release clean cleanall
# Targets that are not currently mentioned in programmer's manual,
# but actually should be:
.PHONY: getresults
# Other targets:                                                  #
.PHONY: pcompilerdebug pcompilerrelease abslibdebug abslibrelease
.PHONY: prerequisites
.PHONY: hint_manuals hint_software_ref_doc hint_readdoc
# #################################################################

# add comment!!!
LIBRARIES := prerequisites libs/$(ADACORE)/bin/gnatdoc libs/$(GMP)/.libs/libgmp.a libs/$(MPFR)/src/.libs/libmpfr.a libs/$(MPFI)/src/.libs/libmpfi.a libs/$(GAPPA)/src/ABS_gappa

all : $(LIBRARIES)
	@$(MAKE) debug ; if [ $$? -eq 0 ]; then $(MAKE) release ; else exit 1; if [ $$? -eq 0 ]; then $(MAKE) doc ; else exit 1; fi; fi; \

clean: $(LIBRARIES)
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	diff -q $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) > /dev/null; \
	if [ $$? -ne 0 ]; then \
	cp $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) ; \
	fi ; \
	find "$(BRANCH_ROOT)/body/hidden/$(ABSTRACTIONDIR)" -mindepth 1 -not -path '*/\.*' -print -quit | grep -q . ; \
	nothidden=$$?; \
	if [ $$nothidden -eq 0 ] ; then \
	sed -i 's/for Externally_Built use "true";/ -- -/g' $(HIDDENGPRABSLIBRARY); \
	echo -e "$(YELLOW)$(PRINTHIDDENSRCAVAIL)$(NOCOLOR)"; \
	fi ; \
	$(MAKE) --makefile=$(MAKEFILEPROBLEMCOMPILER) -C $(dir $(MAKEFILEPROBLEMCOMPILER)) clean DEBUG=1 ; \
	$(MAKE) --makefile=$(MAKEFILEPROBLEMCOMPILER) -C $(dir $(MAKEFILEPROBLEMCOMPILER)) clean DEBUG=0 ; \
	for i in $(COMPILINGMODES); do \
	$(GPRCLEAN) $(GPRCLEANFLAGS) -Xos=$(HOSTTARGET) -Xmode=$$i -Xv=without_main -P $(MAINGPRABSLIBRARY) ; \
	$(GPRCLEAN) $(GPRCLEANFLAGS) -Xos=$(HOSTTARGET) -Xmode=$$i -Xv=with_main    -P $(MAINGPRABSLIBRARY) ; \
	$(GPRCLEAN) $(GPRCLEANFLAGS) -Xos=$(HOSTTARGET) -Xmode=$$i -Xv=with_problem -P $(MAINGPRABSLIBRARY) ; \
	$(GPRCLEAN) $(GPRCLEANFLAGS) -Xos=$(HOSTTARGET) -Xmode=$$i -Xv=with_problem -P $(TESTRWGPRABSLIBRARY) ; \
	$(GPRCLEAN) $(GPRCLEANFLAGS) -Xos=$(HOSTTARGET) -Xmode=$$i -Xv=with_problem -P $(TESTROGPRABSLIBRARY) ; \
	done ; \
	rm -rf $(BRANCH_ROOT)/doc/$(ABSTRACTIONDIR)/* ; \
	rm -rf $(BRANCH_ROOT)/doc/$(PROBLEMCOMPILERDIR)/html ; \
	rm -rf $(BRANCH_ROOT)/gnatinspect.db ; \
	cd $(MANUALSDIR) ; \
	latexmk -bibtex -silent -c -e '$$clean_ext="ps dvi"' ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" --exclude=libs/$(ADACORE) --exclude=libs/$(GMP) --exclude=libs/$(MPFR) --exclude=libs/$(MPFI) --exclude=libs/$(GAPPA) ./ $$hostmachine:~/$(REMOTE_TMP_DIR) ; \
	sleep 1 ; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	sleep 1; \
	rsync -avz --delete -e "ssh" $$hostmachine:~/$(REMOTE_TMP_DIR)/obj $(BRANCH_ROOT) ; \
	rsync -avz --delete -e "ssh" $$hostmachine:~/$(REMOTE_TMP_DIR)/bin $(BRANCH_ROOT) ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;


cleanall: clean
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	rm -rf $(BRANCH_ROOT)/libs/$(GMP)   ; \
	rm -rf $(BRANCH_ROOT)/libs/$(MPFR)  ; \
	rm -rf $(BRANCH_ROOT)/libs/$(MPFI)  ; \
	rm -rf $(BRANCH_ROOT)/libs/$(GAPPA) ; \
	rm -rf $(BRANCH_ROOT)/libs/$(ADACORE) ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;


debug: $(LIBRARIES) pcompilerdebug abslibdebug

release: $(LIBRARIES) pcompilerrelease abslibrelease

pcompiler: $(LIBRARIES) pcompilerdebug pcompilerrelease

abslib: $(LIBRARIES)
	@$(MAKE) abslibdebug ; \
	if [ $$? -ne 0 ]; then exit 1 ; fi ; \
	$(MAKE) -s abslibrelease ; \
	if [ $$? -ne 0 ]; then exit 1 ; fi ; \

pcompilerdebug: $(LIBRARIES)
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	$(MAKE) --makefile=$(MAKEFILEPROBLEMCOMPILER) -C $(dir $(MAKEFILEPROBLEMCOMPILER)) DEBUG=1 ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" --exclude=obj --exclude=bin --exclude=libs/$(ADACORE) --exclude=libs/$(GMP) --exclude=libs/$(MPFR) --exclude=libs/$(MPFI) --exclude=libs/$(GAPPA) ./ $$hostmachine:~/$(REMOTE_TMP_DIR) ; \
	sleep 1 ; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;

pcompilerrelease: $(LIBRARIES)
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	$(MAKE) --makefile=$(MAKEFILEPROBLEMCOMPILER) -C $(dir $(MAKEFILEPROBLEMCOMPILER)) DEBUG=0 ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" --exclude=obj --exclude=bin --exclude=libs/$(ADACORE) --exclude=libs/$(GMP) --exclude=libs/$(MPFR) --exclude=libs/$(MPFI) --exclude=libs/$(GAPPA) ./ $$hostmachine:~/$(REMOTE_TMP_DIR) ; \
	sleep 1 ; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;

abslibrelease: $(LIBRARIES)
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	diff -q $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) > /dev/null; \
	if [ $$? -ne 0 ]; then \
	cp $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) ; \
	fi ; \
	find "$(BRANCH_ROOT)/body/hidden/$(ABSTRACTIONDIR)" -mindepth 1 -not -path '*/\.*' -print -quit | grep -q . ; \
	nothidden=$$?; \
	if [ $$nothidden -eq 0 ] ; then \
	sed -i 's/for Externally_Built use "true";/ -- -/g' $(HIDDENGPRABSLIBRARY); \
	echo -e "$(YELLOW)$(PRINTHIDDENSRCAVAIL)$(NOCOLOR)"; \
	fi ; \
	$(GPRBUILD) -p -Xos=$(HOSTTARGET) -c -U -Xmode=release -Xv=without_main -P $(MAINGPRABSLIBRARY) ; \
	if [ $$? -ne 0 ]; then exit 1; fi ; \
	$(GPRBUILD) -p -Xos=$(HOSTTARGET) -c -u -Xmode=release -Xv=with_main -P $(MAINGPRABSLIBRARY) problem_loading.adb main.adb ; \
	if [ $$? -ne 0 ]; then exit 1; fi ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" --exclude=obj --exclude=bin --exclude=libs/$(ADACORE) --exclude=libs/$(GMP) --exclude=libs/$(MPFR) --exclude=libs/$(MPFI) --exclude=libs/$(GAPPA) ./ $$hostmachine:~/$(REMOTE_TMP_DIR) ; \
	sleep 1 ; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;

abslibdebug: $(LIBRARIES)
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	diff -q $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) > /dev/null; \
	if [ $$? -ne 0 ]; then \
	cp $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) ; \
	fi ; \
	find "$(BRANCH_ROOT)/body/hidden/$(ABSTRACTIONDIR)" -mindepth 1 -not -path '*/\.*' -print -quit | grep -q . ; \
	nothidden=$$?; \
	if [ $$nothidden -eq 0 ] ; then \
	sed -i 's/for Externally_Built use "true";/ -- -/g' $(HIDDENGPRABSLIBRARY); \
	echo -e "$(YELLOW)$(PRINTHIDDENSRCAVAIL)$(NOCOLOR)"; \
	fi ; \
	$(GPRBUILD) -p -Xos=$(HOSTTARGET) -c -U -Xmode=debug -Xv=without_main -P $(MAINGPRABSLIBRARY) ; \
	if [ $$? -ne 0 ]; then exit 1; fi ; \
	$(GPRBUILD) -p -Xos=$(HOSTTARGET) -c -u -Xmode=debug -Xv=with_main -P $(MAINGPRABSLIBRARY) problem_loading.adb main.adb ; \
	if [ $$? -ne 0 ]; then exit 1; fi ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" --exclude=obj --exclude=bin --exclude=libs/$(ADACORE) --exclude=libs/$(GMP) --exclude=libs/$(MPFR) --exclude=libs/$(MPFI) --exclude=libs/$(GAPPA) ./ $$hostmachine:~/$(REMOTE_TMP_DIR) ; \
	sleep 1 ; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;

tests: $(LIBRARIES) pcompiler
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	diff -q $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) > /dev/null; \
	if [ $$? -ne 0 ]; then \
	cp $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) ; \
	fi ; \
	find "$(BRANCH_ROOT)/body/hidden/$(ABSTRACTIONDIR)" -mindepth 1 -not -path '*/\.*' -print -quit | grep -q . ; \
	nothidden=$$?; \
	if [ $$nothidden -eq 0 ] ; then \
	sed -i 's/for Externally_Built use "true";/ -- -/g' $(HIDDENGPRABSLIBRARY); \
	echo -e "$(YELLOW)$(PRINTHIDDENSRCAVAIL)$(NOCOLOR)"; \
	fi ; \
	for i in $(COMPILINGMODES); do \
	$(GPRBUILD) -p -Xos=$(HOSTTARGET) -Xmode=$$i -Xv=with_problem  -P $(TESTRWGPRABSLIBRARY) ;  \
	if [ $$? -ne 0 ]; then exit 1; fi ; \
	$(GPRBUILD) -p -Xos=$(HOSTTARGET) -Xmode=$$i -Xv=with_problem -P $(TESTROGPRABSLIBRARY) ;  \
	if [ $$? -ne 0 ]; then exit 1; fi ; \
	done ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" --exclude=obj --exclude=bin --exclude=libs/$(ADACORE) --exclude=libs/$(GMP) --exclude=libs/$(MPFR) --exclude=libs/$(MPFI) --exclude=libs/$(GAPPA) ./ $$hostmachine:~/$(REMOTE_TMP_DIR) ; \
	sleep 1 ; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;

testing: tests
ifndef MMA_EXISTS
	$(error "Wolfram Mathematica is not installed on your system. Please install it on your system." )
endif
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	echo "*** Starting the tests ***" ; \
	cd $(BRANCH_ROOT)/bin/$(HOSTTARGET)/debug/ ; \
	for i in $(TESTS); do ./$$i ; \
	retval=$$?; \
	if [ $$retval -ne 0 ]; then \
	echo -e "$(RED)FAILED:$(NOCOLOR) $$i (debug mode; returning $$retval)" ; \
	exit 1 ; \
	else echo -e "$(GREEN)PASSED:$(NOCOLOR) $$i (debug mode)" ; \
	fi ; \
	done ; \
	cd $(BRANCH_ROOT)/bin/$(HOSTTARGET)/release/ ; \
	for i in $(TESTS); do ./$$i ; \
	retval=$$?; \
	if [ $$retval -ne 0 ]; then \
	echo -e "$(RED)FAILED:$(NOCOLOR) $$i (release mode; returning $$retval)" ; \
	exit 1 ; \
	else echo -e "$(GREEN)PASSED:$(NOCOLOR) $$i (release mode)" ; \
	fi ; \
	done ; \
	echo "*** All tests passed ***" ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" --exclude=obj --exclude=bin --exclude=libs/$(ADACORE) --exclude=libs/$(GMP) --exclude=libs/$(MPFR) --exclude=libs/$(MPFI) --exclude=libs/$(GAPPA) ./ $$hostmachine:~/$(REMOTE_TMP_DIR) ; \
	sleep 1 ; \
	ssh $$hostmachine 'cd ~/'$(REMOTE_TMP_DIR)' ; make $@ ' ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;

# #################################################################
# Generation of manuals ###########################################
# #################################################################
# Actually generate manuals, where latexmk takes care of any
# dependencies:
#  - command `cd' is needed since older versions of latexmk
#    suffer from a bug related to the option `-cd'
manuals: $(VERSIONINFOPUBLIC) $(REPOSITORYFILESYSTEMINFO) $(VERSIONINFOPRIVATE_VERIFIED)
	cd $(MANUALSDIR) ; \
	latexmk $(LATEXMK_OPTS) ; \
	chmod 644 *.pdf ;

# verify that manually maintained file $(VERSIONINFOPRIVATE)
# is consistent; if so, copy to target and insert comment that the
# file was automatically generated; otherwise exit w/ error:
$(VERSIONINFOPRIVATE_VERIFIED): $(VERSIONINFOPRIVATE)
	@noc=`sed -n '1p' $(VERSIONINFOPRIVATE)` ; \
	((var=noc += 2)) ; \
	tmp="$$var""p" ; \
	regex=`sed -n $$tmp $(VERSIONINFOPRIVATE)` ; \
	((var += 1)) ; \
	tmp="$$var""p" ; \
	versionstring=`sed -n $$tmp $(VERSIONINFOPRIVATE)` ; \
	if [[ ! $$versionstring =~ $$regex ]]; then \
	echo -e "${REDBOLD}Documentation error: \
	${NOCOLOR}Version name does not match regular expression \
	in file $(VERSIONINFOPRIVATE).\n\
	See programmer's manual.\n" ; \
	exit 1 ; \
	else \
	rm -f $(VERSIONINFOPRIVATE_VERIFIED) ; \
	sed $(COMMENT_AUTO_GEN) $(VERSIONINFOPRIVATE) >$(VERSIONINFOPRIVATE_VERIFIED); \
	fi;

# collect some information on svn repository and working copy;
# see programmer's manual:
#  - latexmk checks dependencies using checksums rather than
#    filedates, and hence, the computational overhead of always
#    rebuilding the following target would be minor. Nevertheless, we
#    rebuild only if necessary
.PHONY: $(REPOSITORYFILESYSTEMINFO)
$(REPOSITORYFILESYSTEMINFO):
	@TMP_FILE=$(shell mktemp) ; \
	svn info --xml > $$TMP_FILE; \
	RootAddressOfRepository=`sed -n 's:.*<root>\(.*\)</root>.*:\1:p' $$TMP_FILE` ;  \
	RootAddressOfThisBranchOfProjectInRepository=`sed -n 's:.*<url>\(.*\)</url>.*:\1:p' $$TMP_FILE` ;  \
	RootDirectoryOfWorkingCopyOfThisBranch=$(BRANCH_ROOT) ; \
	DateAndTimeOfMostRecentChangeOfThisBranchInRepository=`sed -n 's:.*<date>\(.*\)</date>.*:\1:p' $$TMP_FILE` ;  \
	DateAndTimeOfMostRecentChangeOfThisBranchInRepository=`echo "$$DateAndTimeOfMostRecentChangeOfThisBranchInRepository" | sed 's#T#/#g'` ; \
	sed -i ':a;N;$$!ba;s/<commit\n   /<commit /g' $$TMP_FILE; \
	RevisionNumberOfMostRecentChangeOfThisBranchInRepository=`sed -n 's:.*<commit revision="\(.*\)">.*:\1:p' $$TMP_FILE`; \
	RootAddressOfProjectInRepository=`sed -n 's#.*<url>'$$RootAddressOfRepository'/\([^/]*\)/.*</url>#\1#p' $$TMP_FILE` ;  \
	RootAddressOfProjectInRepository=$$RootAddressOfRepository/$$RootAddressOfProjectInRepository ; \
	rm -f $$TMP_FILE ; \
	cp $(REPOSITORYFILESYSTEMINFO_TEMPLATE) $$TMP_FILE ; \
	echo "$$RootAddressOfRepository" >> $$TMP_FILE; \
	echo "$$RootAddressOfProjectInRepository" >> $$TMP_FILE; \
	echo "$$RootAddressOfThisBranchOfProjectInRepository" >> $$TMP_FILE; \
	echo "$$RootDirectoryOfWorkingCopyOfThisBranch" >> $$TMP_FILE; \
	echo "$$DateAndTimeOfMostRecentChangeOfThisBranchInRepository" >> $$TMP_FILE; \
	echo "$$RevisionNumberOfMostRecentChangeOfThisBranchInRepository" >> $$TMP_FILE; \
	cmp -s $$TMP_FILE $(REPOSITORYFILESYSTEMINFO) ; \
	if [ $$? -eq 0 ]; then \
	rm -f $$TMP_FILE ; \
	else \
	mv -f $$TMP_FILE $(REPOSITORYFILESYSTEMINFO) ; \
	fi ;

# extract some data from $(VERSIONINFOPRIVATE_VERIFIED) and
# $(REPOSITORYFILESYSTEMINFO), to be used in
# some versions of user's manual:
$(VERSIONINFOPUBLIC): $(REPOSITORYFILESYSTEMINFO) $(VERSIONINFOPRIVATE_VERIFIED) $(VERSIONINFOPUBLIC_TEMPLATE)
	@noc=`sed -n '1p' $(VERSIONINFOPRIVATE_VERIFIED)` ; \
	((var=noc += 3)) ; \
	tmp="$$var""p" ; \
	versionstring=`sed -n $$tmp $(VERSIONINFOPRIVATE_VERIFIED)` ; \
	mainversion=`echo "$$versionstring" | cut -f1 -d"." ` ; \
	rm -f $(VERSIONINFOPUBLIC) ; \
	cp $(VERSIONINFOPUBLIC_TEMPLATE) $(VERSIONINFOPUBLIC) ; \
	echo "$$mainversion" >> $(VERSIONINFOPUBLIC) ; \
	noc=`sed -n '1p' $(REPOSITORYFILESYSTEMINFO)` ; \
	((var=noc += 6)) ; \
	tmp="$$var""p" ; \
	tmp=`sed -n $$tmp $(REPOSITORYFILESYSTEMINFO)` ; \
	echo "$$tmp" >> $(VERSIONINFOPUBLIC) ; \
# END: Generation of manuals #########################################

doc: prerequisites tests doc/$(PROBLEMCOMPILERDIR)/Doxyfile $(VERSIONINFOPRIVATE_VERIFIED) manuals
ifndef DOXYGEN_EXISTS
	$(error "Package doxygen is not available. Please install it on your system. Try 'sudo apt-get install doxygen'." )
endif
	@cd doc/$(PROBLEMCOMPILERDIR) ; doxygen Doxyfile ; 
	@$(GPRDOC) -Xos=$(TARGET) -P $(MAINGPRABSLIBRARY) ;
	@$(MAKE) hint_manuals hint_software_ref_doc hint_readdoc;

hint_manuals:
	@echo -e "${BLUEBOLD}Hint: User's and Programmer's Manuals:\n${NOCOLOR}To read the manuals, open the following files with a pdf viewer:"; \
	echo "   $(MANUALSDIR)/$(USERMANUAL).pdf" ; \
	echo "   $(MANUALSDIR)/$(PROGRAMMERMANUAL).pdf" ;

hint_software_ref_doc:
	@echo -e "${BLUEBOLD}Hint: Software Reference Documentation:\n${NOCOLOR}To read the software reference documentation, open the following files with a browser:"; \
	echo "   doc/$(ABSTRACTIONDIR)/index.html" ; \
	echo "   doc/$(PROBLEMCOMPILERDIR)/html/index.html" ;

hint_readdoc:
	@echo -e "${BLUEBOLD}Hint: To open all documentation in a browser, try \"make readdoc\".${NOCOLOR}"


newproblem:
	@read -p "Please enter a name for your new control problem: " name ; \
	read -e -p "Please enter the path to an existing directory: " directory ; \
	if [ ! -d $$directory ]; then echo "Directory '$$directory' does not exist." ; exit 1 ; fi ; \
	newdirectory=$$directory/$$name ; \
	mkdir $$newdirectory ; \
	if [ $$? -ne 0 ] ; then echo "Directory '$$newdirectory' already exists." ; exit 1 ; fi ; \
	cp $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(PROBLEMMAKEFILE) $$newdirectory ; \
	cp $(BRANCH_ROOT)/$(TEMPLATESDIR)/$(PROBLEMGPRFILE) $$newdirectory ; \
	if [[ $$directory -ef $(BRANCH_ROOT)/examples ]]; then \
	inexdir=1; \
	else	inexdir=0; fi ; \
	wkdir=$(BRANCH_ROOT) ; \
	cd $$newdirectory ; \
	if [ $$inexdir -eq 1 ]; then \
	cp $(PROBLEMMAKEFILE) Makefile ; cp $(PROBLEMGPRFILE) problem_project.gpr ; else \
	sed "s|SOFTWAREDIR[[:space:]]=[[:space:]]../..|SOFTWAREDIR = $$wkdir|g" $(PROBLEMMAKEFILE) > Makefile ; \
	sed -i "s|with \"../../spec/$(ABSTRACTIONDIR).gpr\";|with \"$(MAINGPRABSLIBRARY)\";|g" $(PROBLEMGPRFILE) ; \
	sed -i "s|SOFTWAREDIR[[:space:]]:=[[:space:]]\"../..\"|SOFTWAREDIR := \"$$wkdir\"|g" $(PROBLEMGPRFILE) ; \
	sed "s|SOFTWAREDIR_REL_TO_OBJ[[:space:]]:=[[:space:]]\"../../../../..\"|SOFTWAREDIR_REL_TO_OBJ := SOFTWAREDIR|g" $(PROBLEMGPRFILE) > problem_project.gpr ; \
	fi ; \
	rm $(PROBLEMMAKEFILE) ; \
	rm $(PROBLEMGPRFILE) ; \
	$(MAKE) -s ; \
	echo -e "${REDBOLD}Specify your control problem ${NOCOLOR}in '$$newdirectory/$$name.abcs'." \

readdoc: $(BRANCH_ROOT)/doc/$(ABSTRACTIONDIR)/index.html
	$(BROWSER) $(BRANCH_ROOT)/doc/$(ABSTRACTIONDIR)/index.html $(BRANCH_ROOT)/doc/$(PROBLEMCOMPILERDIR)/html/index.html $(MANUALSDIR)/$(USERMANUAL).pdf $(MANUALSDIR)/$(PROGRAMMERMANUAL).pdf ;

getresults:
	@if [ $(HOSTTARGET) = $(TARGET) ]; then \
	echo "The target platform coincides with the local platform. Make exits with no action." ; \
	exit 1 ; \
	else \
	if [ "$(TARGET)" = "x86_64-suse-linux" ]; then \
	hostmachine=$(HOSTAMDSUSELINUX) ; \
	elif [ "$(TARGET)" = "x86_64-linux-gnu" ]; then \
	hostmachine=$(HOSTAMDGNULINUX) ; \
	elif [ "$(TARGET)" = "x86_64-pc-linux-gnu" ]; then \
	echo "Please use the string \"x86_64-linux-gnu\". (The platforms \"x86_64-linux-gnu\" and \"x86_64-pc-linux-gnu\" are equivalent.)" ; exit 1; \
	else echo "No such platform"; exit 1 ; \
	fi ; \
	echo -e "$(YELLOW)Remote connection to $(NOCOLOR)$$hostmachine"; \
	rsync -avz --delete -e "ssh" $$hostmachine:~/$(REMOTE_TMP_DIR)/obj $(BRANCH_ROOT) ; \
	rsync -avz --delete -e "ssh" $$hostmachine:~/$(REMOTE_TMP_DIR)/bin $(BRANCH_ROOT) ; \
	echo -e "$(YELLOW)Remote connection closed$(NOCOLOR)" ; \
	fi ;

gps: prerequisites
	cd $(BRANCH_ROOT)/spec; gps ;

libs/$(GMP)/.libs/libgmp.a: PATH := $(ORIGINALPATH)
libs/$(GMP)/.libs/libgmp.a: libs/$(GMP)/gmp.h
	cd libs/$(GMP)/ ; ./configure ; $(MAKE) -i ; 

libs/$(MPFR)/src/.libs/libmpfr.a: PATH := $(ORIGINALPATH)
libs/$(MPFR)/src/.libs/libmpfr.a: libs/$(MPFR)/src/mpfr.h
	cd libs/$(MPFR)/ ; ./configure -with-gmp-include=$(BRANCH_ROOT)/libs/$(GMP)/ -with-gmp-lib=$(BRANCH_ROOT)/libs/$(GMP)/.libs ; $(MAKE) -i ; 

libs/$(MPFI)/src/.libs/libmpfi.a: PATH := $(ORIGINALPATH)
libs/$(MPFI)/src/.libs/libmpfi.a: libs/$(MPFI)/src/mpfi.h
	cd libs/$(MPFI)/ ; ./configure -with-mpfr-include=$(BRANCH_ROOT)/libs/$(MPFR)/src -with-mpfr-lib=$(BRANCH_ROOT)/libs/$(MPFR)/src/.libs -with-gmp-include=$(BRANCH_ROOT)/libs/$(GMP)/ -with-gmp-lib=$(BRANCH_ROOT)/libs/$(GMP)/.libs ; $(MAKE) -i ;

libs/$(GAPPA)/src/ABS_gappa: PATH := $(ORIGINALPATH)
libs/$(GAPPA)/src/ABS_gappa: libs/$(GAPPA)/src/main.cpp $(BRANCH_ROOT)/body/readwrite/$(PROBLEMCOMPILERDIR)/misc/io.cpp
	@cd libs/$(GAPPA)/ ; \
	./configure "CPPFLAGS=-I$(BRANCH_ROOT)/libs/$(MPFR)/src -I$(BRANCH_ROOT)/libs/$(GMP)/" "LIBS=-Wl,-Bstatic -L$(BRANCH_ROOT)/libs/$(MPFR)/src/.libs -lmpfr -L$(BRANCH_ROOT)/libs/$(GMP)/.libs -lgmp -Wl,-Bdynamic" ; \
	cp $(BRANCH_ROOT)/body/readwrite/$(PROBLEMCOMPILERDIR)/misc/io.cpp $(BRANCH_ROOT)/libs/$(GAPPA)/src/numbers  ; \
	sed -i "s/-lmpfr -lgmp/ /g" Remakefile; \
	./remake ; mv src/gappa src/ABS_gappa ;


libs/$(GMP)/gmp.h:
	cd libs ; rm -rf $(GMP); tar -xvf $(GMP).tar.xz ;
libs/$(MPFR)/src/mpfr.h:
	cd libs ; rm -rf $(MPFR); tar -xvf $(MPFR).tar.gz ;
libs/$(MPFI)/src/mpfi.h:
	cd libs ; rm -rf $(MPFI); tar -xvf $(MPFI).tar.gz ;
libs/$(GAPPA)/src/main.cpp:
	cd libs ; rm -rf $(GAPPA); tar -xvf $(GAPPA).tar.gz;


libs/$(ADACORE)/bin/gnatdoc:
	cd libs ; tar -xvf $(ADACORE).tar.gz ; cp /usr/bin/ld $(ADACORE)/bin/../libexec/gcc/x86_64-pc-linux-gnu/4.9.4/ ; #ld from AdaCore has bugs.


$(BRANCH_ROOT)/doc/$(ABSTRACTIONDIR)/index.html:
	$(MAKE) doc;


prerequisites:
# Checks if prerequisites are satisfied:
ifndef M4_EXISTS
	$(error "Package m4 is not available. Please install it on your system. Try 'sudo apt-get install m4'." )
endif
ifndef FLEX_EXISTS
	$(error "Package flex is not available. Please install it on your system. Try 'sudo apt-get install flex'." )
endif
ifndef BISON_EXISTS
	$(error "Package bison is not available. Please install it on your system. Try 'sudo apt-get install bison'." )
endif
ifeq ($(BOOST_EXISTS),1)
	$(error "Package boost is not available. Please install it on your system. Try 'sudo apt-get install libboost-all-dev'." )
endif
