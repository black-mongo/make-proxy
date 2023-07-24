all: compile dialyzer test

###===================================================================
### build
###===================================================================
.PHONY: co compile es escriptize run

co:compile
compile:
	./rebar3 compile

es:escriptize
escriptize: clean
	./rebar3 escriptize

### clean
.PHONY: clean distclean
clean:
	./rebar3 clean

distclean:
	./rebar3 clean -a

###===================================================================
### test
###===================================================================
.PHONY: test eunit ct testclean

test: epmd dialyzer
	./rebar3 do eunit -v, ct --config test/ct/ct.config --sys_config config/test.config -v, cover

eunit: epmd
	./rebar3 do eunit -v, cover

MOD := $(m)
CT_DIR := test/ct
PWD	:= $(shell pwd)
ifneq ($(MOD),)
	ifeq ($(shell if [ -f $(PWD)/test/ct/$(MOD).erl ]; then echo "ok"; fi), ok)
		CT_DIR := test/ct
	endif
endif

ct: epmd
ifdef MOD
	./rebar3 ct -v --dir $(CT_DIR) --suite $(MOD) --config test/ct/ct.config --sys_config config/test.config
else
	./rebar3 ct -v --dir test/ct --config test/ct/ct.config --sys_config config/test.config
endif
testclean:
	@rm -fr _build/test

shell: epmd config
	./rebar3 as test shell

config: epmd
	./tool.sh replace_config

dialyzer: epmd
	./rebar3 do dialyzer

tar: epmd
	rm -rf _build/prod
	./rebar3 as prod release
	./rebar3 as prod tar
###===================================================================
### other
###===================================================================
.PHONY: epmd

epmd:
	@pgrep epmd 2> /dev/null > /dev/null || epmd -daemon || true
