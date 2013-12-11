# erequest_id

Creating Heroku Request IDs

## API

``` erlang
-type request_id() :: iolist().
max_byte_size() :: pos_integer().
{ok, request_id()} = erequest_id:create(),
valid|invalid = erequest_id:validate(request_id(), max_byte_size()).
```

## Tests

Rebar needs to be in your path

```
$ rebar get-deps compile
$ rebar eunit skip_deps=true
```
