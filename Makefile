.PHONY: create-env get-repo get-testdata run-tests run-all-tests

SHELL = /bin/bash

os = $(shell uname)
env_name ?= nightly

conda ?= $(or $(CONDA_EXE),$(shell find /opt/*conda*/bin $(HOME)/*conda*/bin -type f -iname conda))
conda_base = $(patsubst %/bin/conda,%,$(conda))
conda_activate = $(conda_base)/bin/activate

# Only populate if workdir is not defined
ifeq ($(origin workdir),undefined)
# Create .tempdir if it doesn't exist
ifeq ($(wildcard $(PWD)/.tempdir),)
workdir := $(shell mktemp -d -t "build_$(pkg_name).XXXXXXXX")
$(shell echo $(workdir) > $(PWD)/.tempdir)
endif

# Read tempdir
workdir := $(shell cat $(PWD)/.tempdir)
endif

#
# packages to be installed in a conda env
#

ifeq ($(os),Linux)
mesa = "mesalib=18.3.1"
else
mesa = "mesalib=17.3.9"
endif

python_ver = "python=$(py_ver)"
pkgs ?= cdat_info cdtime cdms2 genutil cdutil dv3d vcs wk vcsaddons
#test_pkgs ?= nose coverage scipy matplotlib pcmdi_metrics cia easydev nbsphinx testsrunner myproxyclient pytest ipywidgets
test_pkgs ?= nose nbsphinx testsrunner myproxyclient pytest matplotlib ipywidgets


#
# channels to install packages from
#
channels ?= cdat/label/nightly conda-forge pcmdi/label/nightly pcmdi

#
# project repo to clone and run tests
#
organization ?= cdat
repo_name ?= cdms
repo_url = "https://github.com/$(organization)/$(repo_name).git"

#
# run_tests_options - options to pass to 'python run_tests.py' 
#
run_tests_opts ?= "-H -v2"

#
# artifacts dir - directory where tests_html and tests_png will be saved to.
#
artifact_dir ?= $(PWD)/artifacts/$(repo_name)

create-env: ## creates a conda env, installs all CDAT packages from cdat/label/nightly
	source $(conda_activate) base; \
		conda create -y -n $(env_name) $(foreach x,$(channels),-c $(x)) \
		$(foreach x,$(pkgs),"$(x)") $(foreach x,$(test_pkgs),"$(x)") \
		$(foreach x,$(extra_pkgs),"$(x)") \
		"$(mesa)" "$(python_ver)";
	source $(conda_activate) $(env_name); conda list; conda list --verbose cdms2 

get-repo: ## clone project repo
	git clone $(repo_url) $(workdir)/$(repo_name)
	mkdir -p $(workdir)/$(repo_name)/tests_png
	mkdir -p $(workdir)/$(repo_name)/tests_html

get-testdata:
ifeq ($(wildcard $(workdir)/$(repo_name)/uvcdat-testdata),)
	git clone https://github.com/CDAT/uvcdat-testdata $(workdir)/$(repo_name)/uvcdat-testdata
else
	cd $(workdir)/$(repo_name)/uvcdat-testdata; git pull
endif

run-tests: ## run tests; NOTE that this target always return 0 even if there is test failure, so that CircleCI can run the next test
	source $(conda_activate) $(env_name); \
		cd $(workdir)/$(repo_name); \
		python run_tests.py $(run_tests_opts); echo $$? > run_tests.out
	mkdir -p $(artifact_dir)
	mv $(workdir)/$(repo_name)/tests_html $(artifact_dir)/
	mv $(workdir)/$(repo_name)/tests_png $(artifact_dir)/
	exit 0

cleanup:
	source $(conda_activate) base; \
		conda env remove -n $(env_name)

