REBAR3 = `which rebar3`

ct:
	@( cp -r ../src/ .; $(REBAR3) ct; rm -fr src/)

coverage:
	@( cp -r ../src/ .; $(REBAR3) ct --cover; $(REBAR3) cover; rm -fr src/)

ct_clean:
	@( rm -fr _build/)

.PHONY: ct coverage ct_clean