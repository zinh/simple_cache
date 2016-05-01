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

CLI Interface
-----

    erl> simple_cache:lookup(a_key).
    failed
    erl> simple_cache:insert(a_key, a_value).
    ok
    erl> simple_cache:lookup(a_key).
    a_value

HTTP Interface
-----

```
# Lookup
curl http://localhost/<key>

# Insert
curl -X PUT -d <value> http://localhost/<key>
```
