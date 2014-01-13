-module(erequest_id).

-export([create/0
         ,validate/1
         ,validate/2
         ,validate/3
         ,ensure/1
         ,ensure/3
        ]).

-type request_id() :: iolist()|binary().

-export_type([request_id/0]).

 %% e.g. byte_size(<<"2ab5bf90-f9a1-44dd-b996-3987638ce9e0">>)
-define(DEFAULT_ID_SIZE, 36).

%% @doc Create a new Request ID
-spec create() ->
                    {ok, RequestId} when
      RequestId :: request_id().
create() ->
    UuidV4 = uuid:get_v4(weak),
    UuidV4Bin = uuid:uuid_to_string(UuidV4, binary_standard),
    {ok, UuidV4Bin}.

%% @doc Checks that the RequestId is 36 chars long and all characters
%% are in the range [A-Za-z0-9\-].
%% @end
-spec validate(request_id()) -> valid | invalid.
validate(RequestId) ->
    validate(RequestId, ?DEFAULT_ID_SIZE, ?DEFAULT_ID_SIZE).

%% @doc Given RequestId input, return a valid request id. The input if
%% it is in fact valid, or a newly generated request id if not.
%% @end
-spec ensure(any()) -> request_id().
ensure(RequestId) ->
    ensure(RequestId, ?DEFAULT_ID_SIZE, ?DEFAULT_ID_SIZE).

-spec ensure(any(), non_neg_integer(), non_neg_integer()) -> request_id().
ensure(RequestId, MinSize, MaxSize) ->
    case validate(RequestId, MinSize, MaxSize) of
        valid ->
            RequestId;
        invalid ->
            {ok, NewRequestId} = create(),
            NewRequestId
    end.

%% @doc
%% Validate that the request ID is valid, that means that it
%% should be >= Size and all characters in the range [A-Za-z0-9\-].
%% @end
-spec validate(RequestId, Size) ->
                      valid |
                      invalid when
      RequestId :: request_id(),
      Size :: pos_integer().
validate(RequestId, MaxSize) ->
    validate(RequestId, 0, MaxSize).

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
       C =:= $- ->
    valid;
validate_char(_) ->
    invalid.
