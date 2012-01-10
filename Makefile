REBAR=rebar

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
clean:
	$(REBAR) clean
	rm -rf ebin/*.config
	rm -rf test/*.beam
	rm -rf test/*.config

test: all
	mkdir -p .eunit
	cp -r etc .eunit/.
	cp -r job_root .eunit/.	
	cp -r config .eunit/.
	$(REBAR) skip_deps=true eunit
	$(REBAR) ct

doc:
	$(REBAR) doc skip_deps=true

rel: relclean all
	$(REBAR) generate
	chmod u+x rel/ec_logger/bin/ec_logger

relclean:
	rm -rf rel/ec_master
	rm -rf test/*.beam

# deploy:
# 	echo 'starting loading project from github'
# 	#git clone -b 'master' 'git@github.com:RomanShestakov/ec_master'
# 	git clone -b 'master' 'git@github.com:RomanShestakov/ec_dispatcher'
# 	git clone -b 'master' 'git@github.com:RomanShestakov/ec_web'
# 	echo 'building ec_master'
# 	all
# # 	cd 'ec12'
# # 	make rel
# # 	cd ..

# 	echo 'building ec_dispatcher'
# 	cd 'ec_dispatcher'
# 	make rel
# 	cd ..

# 	echo 'building ec_web'
# 	cd 'ec_web'
# 	make rel
# 	cd ..

# 	echo 'deploying builds'
# 	mkdir -p deploy
# 	#cd 'deploy'
# 	cp -r ./ec_master/rel/ec_master ./deploy
# 	cp -r ./ec_dispatcher/rel/ec_dispatcher ./deploy
# 	cp -r ./ec_web/rel/ec_web ./deploy


APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets\
xmerl snmp public_key mnesia eunit syntax_tools compiler eunit webtool
DEPS = deps/log4erl/ebin deps/mdigraph/ebin deps/gen_leader/ebin deps/resource_discovery/ebin deps/ec_cli/ebin

COMBO_PLT = $(HOME)/.ec_master_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) $(DEPS) \

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) $(DEPS) \

dialyzer: all
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin | fgrep -v -f .dialyzer.ignore-warning

cleanplt:
	@echo
	@echo "Are you sure? It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

xref:
	rebar xref
