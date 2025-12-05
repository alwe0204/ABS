# who, when, what???
#                                                                 #
# $Id: Makefile 1155 2017-06-02 15:25:15Z lf1agure $
#                                                                 #
# #################################################################
# The variable below must hold                                    #
# the directory of the software                                   #
# 
SOFTWAREDIR = ../..
# 
# #################################################################

# #################################################################
# Definitely use bash shell                                       #
SHELL := /bin/bash
# #################################################################

# #################################################################
# Abstraction and synthesis program directory name
ABSTRACTIONDIR := ABSLibrary
# #################################################################

SOFTWAREDIRCORRECT := $(shell find $(SOFTWAREDIR)/spec/readwrite/$(ABSTRACTIONDIR) 2> /dev/null)

ifndef SOFTWAREDIRCORRECT
$(error "The software directory is not specified correctly.")
endif

# #################################################################
# Native gcc (The compiler of AdaCore is also called gcc (why?))
CC = /usr/bin/gcc
# #################################################################

# #################################################################
# AdaCore software collection                                     #
ADACORE = gnat-gpl-2016-x86_64-linux-bin
# #################################################################

# #################################################################
# Ada utility tools                                               #
GPRBUILD = gprbuild
GPRCLEAN = gprclean
# #################################################################

# #################################################################
# PATH variable 
ORIGINALPATH := $(PATH)
PATH         := $(SOFTWAREDIR)/libs/$(ADACORE)/bin:$(PATH)
#
# We have to set it as AdaCore Software is not smart enough to
# allow a more convenient solution
# #################################################################

# #################################################################
# Some colors for the output                                      #
YELLOW =\033[0;33m
NOCOLOR=\033[0m
# #################################################################

# #################################################################
# Notification on hidden sources                                  #
PRINTHIDDENSRCAVAIL:="Hidden sources available"
# #################################################################

# #################################################################
# Gpr file
PROBLEMGPRFILE := problem_project.gpr
# #################################################################

# #################################################################
HIDDENGPRABSLIBRARY := $(SOFTWAREDIR)/spec/hidden/$(ABSTRACTIONDIR)/hidden.gpr
TEMPLATESDIR := templates
HIDDENGPRFILE := hiddenGprfile.template
# #################################################################


TARGET     := $(shell $(CC) -dumpmachine)
BINDIR     := $(SOFTWAREDIR)/bin
EXECDIR    := $(BINDIR)/$(TARGET)

# #################################################################
# Directory where this Makefile is located: example directory     #
MKFILE       := $(abspath $(lastword $(MAKEFILE_LIST)))
EXAMPLE_DIR  := $(dir $(MKFILE))
CURDIR       := $(notdir $(patsubst %/,%,$(dir $(MKFILE))))
# #################################################################

PROBLEMFILE := $(CURDIR).abcs

PROBLEMFILEEXISTS := $(shell find $(EXAMPLE_DIR)$(PROBLEMFILE) 2> /dev/null ; echo $$?)

SOURCECODEDIR := $(EXAMPLE_DIR)SourceCodeFiles
LOCALBINDIR   := $(EXAMPLE_DIR)bin
LOCALOBJDIR   := $(EXAMPLE_DIR)obj
LOCALEXECDIR  := $(LOCALBINDIR)/$(TARGET)

# #################################################################
# Targets mentioned in programmer's manual                        #
.PHONY: all sourcecode solution executable clean
# Other targets:                                                  #
# #################################################################

# #################################################################
# RULES
# #################################################################

ifneq ($(PROBLEMFILEEXISTS),1)
#all: abslib sourcecode executable solution
all: solution
else
all:
	@touch $(EXAMPLE_DIR)$(PROBLEMFILE) ; 
endif

#solution: sourcecode executable $(EXAMPLE_DIR)Results/Controller.dat
solution: $(EXAMPLE_DIR)Results/Controller.dat

sourcecode: $(SOURCECODEDIR)/dynamics_specification_parameters.adb

clean:
	@rm -rf $(SOURCECODEDIR) ; rm -rf $(LOCALBINDIR) ; rm -rf $(LOCALOBJDIR) ; rm -rf Results ;

executable: abslib sourcecode
	@diff -q $(SOFTWAREDIR)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) > /dev/null; \
	if [ $$? -ne 0 ]; then \
	cp $(SOFTWAREDIR)/$(TEMPLATESDIR)/$(HIDDENGPRFILE) $(HIDDENGPRABSLIBRARY) ; \
	fi ; \
	find "$(SOFTWAREDIR)/body/hidden/$(ABSTRACTIONDIR)" -mindepth 1 -not -path '*/\.*' -print -quit | grep -q . ; \
	nothidden=$$?; \
	if [ $$nothidden -eq 0 ] ; then \
	sed -i 's/for Externally_Built use "true";/ -- -/g' $(HIDDENGPRABSLIBRARY); \
	echo -e "$(YELLOW)$(PRINTHIDDENSRCAVAIL)$(NOCOLOR)"; \
	fi ; \
	$(GPRBUILD) -ws -p -Xos=$(TARGET) -Xmode=release -Xv=with_problem -P $(PROBLEMGPRFILE) -o $(LOCALEXECDIR)/release/$(CURDIR) ; \
	$(GPRBUILD) -ws -p -Xos=$(TARGET) -Xmode=debug -Xv=with_problem -P $(PROBLEMGPRFILE) -o $(LOCALEXECDIR)/debug/$(CURDIR) ;

abslib: PATH := $(ORIGINALPATH)
abslib:
	@$(MAKE) -C $(BINDIR)/.. abslib ;

pcompiler: PATH := $(ORIGINALPATH)
pcompiler:
	$(MAKE) -C $(BINDIR)/.. pcompiler ;

$(LOCALEXECDIR)/release/$(CURDIR): executable
$(LOCALEXECDIR)/debug/$(CURDIR): executable

$(EXECDIR)/release/compiler: pcompiler

$(SOURCECODEDIR)/dynamics_specification_parameters.adb: $(EXAMPLE_DIR)$(PROBLEMFILE) $(EXECDIR)/release/compiler
	@cd $(EXECDIR)/release ; \
	./compiler -outputdir=$(SOURCECODEDIR) $(EXAMPLE_DIR)$(PROBLEMFILE) ;

$(EXAMPLE_DIR)Results/Controller.dat: $(LOCALEXECDIR)/release/$(CURDIR)
	@cd $(LOCALEXECDIR)/release ; ./$(CURDIR) ; \
	if [ $$? -eq 0 ]; then mkdir -p $(EXAMPLE_DIR)Results ; \
	mv $(LOCALEXECDIR)/release/*.* $(EXAMPLE_DIR)Results ; fi ;
