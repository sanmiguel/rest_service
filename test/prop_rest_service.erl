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
-export([create_resource/1]).
-export([delete_resource/1]).
-export([get_resource/1]).
-export([update_resource/1]).
-export([poke_resource/1]).

-export([http_get/1]).
-export([http_put/2]).
-export([http_post/2]).
-export([http_delete/1]).
-export([http_head/1]).

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
           {call, ?MODULE, poke_resource,   [resource_header()]},
           {call, ?MODULE, get_resource,    [resource_header()]},
           {call, ?MODULE, delete_resource, [resource_header()]},
           {call, ?MODULE, create_resource, [resource()]},
           {call, ?MODULE, update_resource, [resource()]}
          ]);
command(#state{resources=Ss}) ->
    ?LET(Resource, oneof([ resource_header() | Ss]),
             oneof([
                    {call, ?MODULE, poke_resource,   [Resource]},
                    {call, ?MODULE, get_resource,    [Resource]},
                    {call, ?MODULE, delete_resource, [Resource]},
                    {call, ?MODULE, create_resource, [{Resource, resource_body(Resource)}]},
                    {call, ?MODULE, update_resource, [{Resource, resource_body(Resource)}]}
                   ])).

precondition(_State=#state{}, {call, _M, _F, _A}) ->
    true.

postcondition(_State=#state{resources=Resources}, {call, ?MODULE, update_resource, [{Resource, _D}]}, {RespCode, _H, _B}) ->
    %% This is fine iff (lists:member(RID, Resources) && RespCode =:= 200) OR
    %% (not lists:member(RID, Resources) && RespCode =:= 404)
    %% This is fine if RID is a known resource
    KnownRID = lists:member(Resource, Resources),
    if KnownRID -> RespCode =:= 204;
       true -> RespCode =:= 404
    end;
postcondition(_State=#state{resources=Resources}, {call, ?MODULE, delete_resource, [Resource]}, {RespCode, _H, _B}) ->
    KnownRID = lists:member(Resource, Resources),
    if KnownRID -> RespCode =:= 204;
       true     -> RespCode =:= 404
    end;
postcondition(_State=#state{resources=Resources}, {call, ?MODULE, create_resource, [{Resource, _Body}]}, {201, _H, _B}) ->
    not lists:member(Resource, Resources);
postcondition(_State=#state{resources=Resources}, {call, ?MODULE, create_resource, [{Resource, _Body}]}, {204, _H, _B}) ->
    lists:member(Resource, Resources);
postcondition(_State=#state{}, {call, ?MODULE, create_resource, [{_Resource, _Body}]}, _Res) ->
    %% create_resource should ALWAYS succeed with 201/204, as it's idempotent. Any other response
    %% is a problem
    false;
postcondition(_State=#state{}, _, _) ->
    true.

next_state(#state{resources = Resources} = State, _Res, {call, ?MODULE, create_resource, [{Resource, _Body}]}) ->
    AlreadyKnown = lists:member(Resource, Resources),
    if AlreadyKnown -> State;
       true -> State#state{resources=[Resource|Resources]}
    end;
next_state(#state{resources=Resources} = State, _Res, {call, ?MODULE, delete_resource, [Resource]}) ->
    State#state{resources=lists:delete(Resource, Resources)};
next_state(State, _Res, {call, _M, _F, _A}) ->
    State.

%%%===================================================================
%%% Generators
%%%===================================================================

resource_header() ->
    ?LET(RType, resource_type(),
         {RType, resource_id(RType)}).

resource() ->
    ?LET(Resource, resource_header(),
         {Resource, resource_body(Resource)}).

%% Binary (hex)
hex_char() -> oneof([$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f]).

resource_type() ->
    oneof([ hexstring ]).

resource_id(hexstring) -> non_empty(list(hex_char())).

resource_body({hexstring, _}) -> non_empty(list(hex_char())).


%% Not really a generator but kinda used as such
domain() ->
    %% TODO The port should not be hard-coded
    "http://localhost:8080/resource".

url({hexstring, ID}) ->
    lists:flatten(io_lib:format("~s/~s",
                         [domain(), ID])).

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

%% HEAD
http_head(URL) ->
    {ok, {{_, RespCode, _}, Headers, Body}} = httpc:request(head, {URL, [{"Content-Type", ?CTYPE}]}, [], []),
    {RespCode, Headers, Body}.

%%%===================================================================
%%% Plugin Functions
%%% TODO These should be defined as callbacks and defined in a
%%% module implementing -behaviour(rest_proper).
%%%===================================================================

cleanup(#state{resources = Rs}) ->
    [ delete_resource({T, R}) || {T, R} <- Rs ].

poke_resource(Resource) ->
     http_head(url(Resource)).

create_resource({Resource, Body}) ->
    {RespCode, Headers, RespBody} = http_put(url(Resource), Body),
    Location = proplists:get_value("location", Headers),
    {RespCode, Location, RespBody}.

delete_resource(Resource) ->
    http_delete(url(Resource)).

get_resource(Resource) ->
    http_get(url(Resource)).

update_resource({Resource, Body}) ->
    http_post(url(Resource), Body).
