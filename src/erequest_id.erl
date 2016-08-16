%%% Creates and Validates request IDs usable in HTTP headers to allow tracing
%%% and debugging through logs and tcp dumps.
%% @author Geoff Cant <nem@erlang.geek.nz>
-module(erequest_id).

-export([create/0
         ,validate/1
         ,validate/2
         ,validate/3
         ,ensure/1
         ,ensure/3
        ]).

-type request_id() :: iodata().
-type min_byte_size() :: non_neg_integer().
-type max_byte_size() :: pos_integer().

-export_type([request_id/0, min_byte_size/0, max_byte_size/0]).

%% e.g. byte_size(<<"2ab5bf90-f9a1-44dd-b996-3987638ce9e0">>)
-define(DEFAULT_ID_SIZE, 36).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public Interface %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Create a new Request ID
-spec create() -> {ok, RequestId} when
      RequestId :: request_id().
create() ->
    UuidV4 = uuid:get_v4(),
    UuidV4Bin = uuid:uuid_to_string(UuidV4, binary_standard),
    {ok, UuidV4Bin}.

%% @doc Checks that the `RequestId' is 36 chars long and all characters
%% are in the range `[A-Za-z0-9=+/\-]'.
-spec validate(any()) -> valid | invalid.
validate(RequestId) ->
    validate(RequestId, ?DEFAULT_ID_SIZE, ?DEFAULT_ID_SIZE).

%% @doc Given `RequestId' input, return a valid request id.
%% Returns the input `RequestId' if it is valid, or otherwise
%% returns a newly generated one.
-spec ensure(any()) -> request_id().
ensure(RequestId) ->
    ensure(RequestId, ?DEFAULT_ID_SIZE, ?DEFAULT_ID_SIZE).

%% @doc Given `RequestId' input, return a valid request id.
%% Returns the input `RequestId' if it is valid, or otherwise
%% returns a newly generated one.
%%
%% The caller can specify the minimal and maximal size for the request id.
-spec ensure(any(), min_byte_size(), max_byte_size()) -> request_id().
ensure(RequestId, MinSize, MaxSize) ->
    case validate(RequestId, MinSize, MaxSize) of
        valid ->
            RequestId;
        invalid ->
            {ok, NewRequestId} = create(),
            NewRequestId
    end.

%% @doc Verify that the request ID is valid, meaning that it
%% should be smaller than or equal to `Size', and all its characters are in
%% the range `[A-Za-z0-9=+/\-]'.
-spec validate(any(), Size::max_byte_size()) -> valid | invalid.
validate(RequestId, MaxSize) ->
    validate(RequestId, 0, MaxSize).

%% @doc Verify that the request ID is valid, meaning that
%% it should be in the range `MinSize..MaxSize', and that all its characters
%% are in the range `[A-Za-z0-9=+/\-]'.
-spec validate(any(), min_byte_size(), max_byte_size()) -> valid | invalid.
validate(RequestId, MinSize, MaxSize)
  when is_binary(RequestId),
       MinSize =< byte_size(RequestId),
       byte_size(RequestId) =< MaxSize ->
    validate_binary(RequestId);
validate(RequestId, MinSize, MaxSize)
  when is_list(RequestId) ->
    case iolist_size(RequestId) of
        TooBig when TooBig > MaxSize -> invalid;
        TooSmall when TooSmall < MinSize -> invalid;
        _ ->
            validate_iolist(RequestId)
    end;
validate(_, _, _) ->
    invalid.


%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%
validate_iolist([]) ->
    valid;
validate_iolist([C | Rest]) when is_integer(C) ->
    case validate_char(C) of
        valid ->
            validate_iolist(Rest);
        invalid ->
            invalid
    end;
validate_iolist([L | Rest]) when is_list(L) ->
    case validate_iolist(L) of
        valid ->
            validate_iolist(Rest);
        invalid ->
            invalid
    end;
validate_iolist([ B | Rest ]) when is_binary(B) ->
    case validate_binary(B) of
        valid ->
            validate_iolist(Rest);
        invalid ->
            invalid
    end.

validate_binary(<<>>) ->
    valid;
validate_binary(<<C, Rest/binary>>) ->
    case validate_char(C) of
        valid ->
            validate_binary(Rest);
        invalid ->
            invalid
    end.

-compile({inline, {validate_char, 1}}).
validate_char(C)
  when $A =< C, C =< $Z;
       $a =< C, C =< $z;
       $0 =< C, C =< $9;
       C =:= $-;
       C =:= $+;
       C =:= $/;
       C =:= $= ->
    valid;
validate_char(_) ->
    invalid.
