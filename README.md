simple_cache
=====

A caching server

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell
    > application:start(simple_cache).

Interface
-----

    > simple_cache:lookup(a_key).
    failed
    > simple_cache:insert(a_key, a_value).
    ok
    > simple_cache:lookup(a_key).
    a_value
