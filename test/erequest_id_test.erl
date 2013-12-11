-module(erequest_id_test).
-include_lib("eunit/include/eunit.hrl").

tip_queue_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_Pid) ->
	     ok
     end,
     [
      {"Create Request ID", ?_test(t_create_request_id())}
      ,{"Validate Request ID", ?_test(t_validate_request_id())}
     ]
    }.

%% Tests
t_create_request_id() ->
    {ok, RequestId} = erequest_id:create(),
    %% And make sure it's a valid RequestID
    ?assertEqual(valid, erequest_id:validate(RequestId, 255)).

t_validate_request_id() ->
    {ok, ValidReqId} = erequest_id:create(),
    %% Check known
    ?assertEqual(valid, erequest_id:validate(ValidReqId, 255)),
    %% Check size constraints
    ?assertEqual(invalid, erequest_id:validate(ValidReqId, byte_size(ValidReqId)-1)),
    %% Check a string
    ?assertEqual(valid, erequest_id:validate(binary_to_list(ValidReqId), 255)),
    %% Check a string that has invalid characters (in this case it has "?")
    ?assertEqual(invalid, erequest_id:validate(<<"invalid??id">>, 255)).
