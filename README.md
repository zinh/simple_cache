simple_cache
=====

A caching server

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell
    erl> application:start(simple_cache).

Interface
-----

    erl> simple_cache:lookup(a_key).
    failed
    erl> simple_cache:insert(a_key, a_value).
    ok
    erl> simple_cache:lookup(a_key).
    a_value
