-module(erequest_id).

-export([create/0
         ,validate/2]).

-type request_id() :: iolist()|binary().

-export_type([request_id/0]).

%% @doc Create a new Request ID
-spec create() ->
                    {ok, RequestId} when
      RequestId :: request_id().
create() ->
    UuidV4 = uuid:get_v4(weak),
    UuidV4Bin = uuid:uuid_to_string(UuidV4, binary_standard),
    {ok, UuidV4Bin}.

%% @doc
%% Validate that the request ID is valid, that means that it 
%% should be >= Size and all characters in the range [A-Za-z0-9\-].
%% @end
-spec validate(RequestId, Size) ->
                      valid |
                      invalid when
      RequestId :: request_id(),
      Size :: pos_integer().
validate(RequestId, Size) when is_list(RequestId),
                               length(RequestId) =< Size ->
    validate(RequestId);
validate(RequestId, Size) when is_binary(RequestId),
                               byte_size(RequestId) =< Size ->
    validate(binary_to_list(RequestId), Size);
validate(_, _) ->
    invalid.

%% Internal
validate([]) ->
    valid;
validate([C|Rest]) when $A =< C, C =< $Z;
                        $a =< C, C =< $z;
                        $0 =< C, C =< $9;
                        C =:= $- ->
    validate(Rest);
validate(_) ->
    invalid.

