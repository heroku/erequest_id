# erequest_id

Erlang client for creating and validating Heroku Request IDs. The request IDs
are UUID-like strings that can be used as HTTP headers or parameters over APIs
to identify individual requests.

The objective is to be able to have these request IDs in logs or in TCP dumps
to be able to trace, either live or a posteriori, a given request or set of
requests through a distributed system.

Valid request IDs have all their characters in the range `[A-Za-z0-9=+/\-]`.
By default the library generates 36 bytes long strings, but any non-negative
length can be seen as valid.

## API

### Types

``` erlang
-type request_id() :: iodata().
-type min_byte_size() :: non_neg_integer().
-type max_byte_size() :: pos_integer().
{ok, request_id()} = erequest_id:create(),
valid|invalid = erequest_id:validate(request_id(), max_byte_size()).
```

### Calls

- `create()`: generates a new request id.
- `validate(RequestId) -> valid | invalid`: checks if the request ID is valid
  (requires the `RequestId` to be exactly 36 bytes long)
- `validate(RequestId, MaxSize)`: checks that the request ID is valid, but can
  be of any byte size between `0..MaxSize`, inclusively.
- `validate(RequestId, MinSize, MaxSize)`: checks that the request ID is valid
  and of any byte size between `MinSize..MaxSize`, inclusively.
- `ensure(RequestId)`: Keeps the current request id if it's valid, or returns
  a new, valid one, if the one submitted isn't.
- `ensure(RequestId, MinSize, MaxSize)`: Keeps the current request id if it's
  valid, or returns a new, valid one, if the one submitted isn't.

## Tests

[Rebar3](https://www.rebar3.org) needs to be in your path

```
$ rebar3 compile
$ rebar3 eunit
```
