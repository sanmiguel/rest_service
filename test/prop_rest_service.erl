-module(prop_rest_service).

-include_lib("proper/include/proper.hrl").

-behaviour(proper_statem).

-export([prop_rest/0]).

-export([command/1]).
-export([initial_state/0]).
-export([next_state/3]).
-export([precondition/2]).
-export([postcondition/3]).

%% Functions to make http calls
-export([create_resource/2]).
-export([delete_resource/1]).
-export([get_resource/1]).
-export([update_resource/2]).

-export([http_get/1]).
-export([http_put/2]).
-export([http_post/2]).
-export([http_delete/1]).

-record(state, {
    resources = [] :: list(string())
}).

-define(CTYPE, "application/json").

%%%===================================================================
%%% Main definition of what a sequence involves
%%%===================================================================
prop_rest() ->
    ?FORALL(Cmds,
            commands(?MODULE, initial_state()),
            ?TRAPEXIT(
               begin
                   {_Hist, State, Res} = run_commands(?MODULE, Cmds),
                   cleanup(State),
                   aggregate(command_names(Cmds), Res =:= ok)
               end)
           ).

%%%===================================================================
%%% proper_statem callbacks
%%%===================================================================
initial_state() ->
    #state{}.

command(#state{resources=[]}) ->
    oneof([
           {call, ?MODULE, create_resource, [resource_id(), resource_body()]},
           {call, ?MODULE, delete_resource, [resource_id()]},
           {call, ?MODULE, get_resource, [resource_id()]},
           {call, ?MODULE, update_resource, [resource_id(), resource_body()]}
          ]);
command(#state{resources=Ss}) ->
    ?LET(RID,
         oneof([resource_id() | Ss]),
         oneof([
                {call, ?MODULE, create_resource, [RID, resource_body()]},
                {call, ?MODULE, delete_resource, [RID]},
                {call, ?MODULE, get_resource, [RID]},
                {call, ?MODULE, update_resource, [RID, resource_body()]}
               ])).

precondition(_State=#state{}, {call, _M, _F, _A}) ->
    true.

postcondition(_State=#state{resources=Resources}, {call, ?MODULE, update_resource, [RID, _D]}, {RespCode, _H, _B}) ->
    %% This is fine iff (lists:member(RID, Resources) && RespCode =:= 200) OR
    %% (not lists:member(RID, Resources) && RespCode =:= 404)
    %% This is fine if RID is a known resource
    KnownRID = lists:member(RID, Resources),
    if KnownRID -> RespCode =:= 204;
       true -> RespCode =:= 404
    end;
postcondition(_State=#state{resources=Resources}, {call, ?MODULE, delete_resource, [RID]}, {RespCode, _H, _B}) ->
    KnownRID = lists:member(RID, Resources),
    if KnownRID -> RespCode =:= 204;
       true     -> RespCode =:= 404
    end;
postcondition(_State=#state{resources=Resources}, {call, ?MODULE, create_resource, [RID, _Body]}, {201, _H, _B}) ->
    not lists:member(RID, Resources);
postcondition(_State=#state{resources=Resources}, {call, ?MODULE, create_resource, [RID, _Body]}, {204, _H, _B}) ->
    lists:member(RID, Resources);
postcondition(_State=#state{}, {call, ?MODULE, create_resource, [_RID, _Body]}, _Res) ->
    %% create_resource should ALWAYS succeed with 201/204, as it's idempotent. Any other response
    %% is a problem
    false;
postcondition(_State=#state{}, _, _) ->
    true.

next_state(#state{resources = Resources} = State, _Res, {call, ?MODULE, create_resource, [RID, _Body]}) ->
    AlreadyKnown = lists:member(RID, Resources),
    if AlreadyKnown -> State;
       true -> State#state{resources=[RID|Resources]}
    end;
next_state(#state{resources=Resources} = State, _Res, {call, ?MODULE, delete_resource, [RID]}) ->
    State#state{resources=lists:delete(RID, Resources)};
next_state(State, _Res, {call, _M, _F, _A}) ->
    State.

%%%===================================================================
%%% Generators
%%%===================================================================

%% Resource ID
%% Binary (hex)
hex_char() -> oneof([$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $a, $b, $c, $d, $e, $f]).

resource_id() -> non_empty(list(hex_char())).

resource_body() -> non_empty(list(hex_char())).

%%%===================================================================
%%% Utils
%%% TODO: These should either be exported as utils funcions for use by
%%% modules implementing this behaviour, OR they should be farmed out
%%% to a util module.
%%% This is particularly important for HTTP-mechanism-related functions
%%% as this allows us to change up which HTTP client we use easily.
%%%===================================================================

%% GET URL
http_get(URL) ->
    {ok, {{_, RespCode, _}, Headers, Body}} = httpc:request(get, {URL, [{"Content-Type", ?CTYPE}]}, [], []),
    {RespCode, Headers, Body}.

%% PUT
http_put(URL, Body) ->
    {ok, {{_, RespCode, _}, Headers, RespBody}} = httpc:request(put, {URL, [], ?CTYPE, Body}, [], []),
    {RespCode, Headers, RespBody}.


%% POST
http_post(URL, Body) ->
    {ok, {{_, RespCode, _}, Headers, RespBody}} = httpc:request(post, {URL, [], ?CTYPE, Body}, [], []),
    {RespCode, Headers, RespBody}.

%% DELETE
http_delete(URL) ->
    {ok, {{_, RespCode, _}, Headers, Body}} = httpc:request(delete, {URL, []}, [], []),
    {RespCode, Headers, Body}.


%%%===================================================================
%%% Plugin Functions
%%% TODO These should be defined as callbacks and defined in a
%%% module implementing -behaviour(rest_proper).
%%%===================================================================

cleanup(#state{resources = Ss}) ->
    [ delete_resource(S) || S <- Ss ].

url(resource, ID) ->
    %% TODO The port should not be hard-coded
    lists:flatten(io_lib:format("~s/~s", ["http://localhost:8080/resource", ID])).

create_resource(SessID, Body) ->
    {RespCode, Headers, RespBody} = http_put(url(resource, SessID), Body),
    Location = proplists:get_value("location", Headers),
    {RespCode, Location, RespBody}.

delete_resource(SessID) ->
    http_delete(url(resource, SessID)).

get_resource(SessID) ->
    http_get(url(resource, SessID)).

update_resource(SessID, Body) ->
    http_post(url(resource, SessID), Body).
