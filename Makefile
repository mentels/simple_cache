.PHONY: compile contact1 contact2 cache1 cache2 cache3 make_dev

compile:
	erlc -o ./simple_cache/ebin ./simple_cache/src/*.erl
	erlc -o ./resource_discovery/ebin ./resource_discovery/src/*.erl

contact1:
	erl -sname contact1
contact2:
	erl -sname contact2

cache1:
	erl -sname cache1 -boot ./simple_cache -config ./sys #-detached
cache2:
	erl -sname cache2 -boot ./simple_cache -config ./sys #-detached
cache3:
	erl -sname cache3 -boot ./simple_cache -config ./sys #-detached

make_dev:
	erl -sname cache_dev -pa ./simple_cache/ebin -pa ./resource_discovery/ebin

