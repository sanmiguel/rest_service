%% --------------------------------------------------------------------
%% Copyright (c) 2013. Michael Coles < michael dot coles at gmail dot com >
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% File: rest_servie_sup.erl. OTP Supervisor for both web service and storage
%% facility.
%%
%% --------------------------------------------------------------------
-module(rest_service_sup).

-behaviour(supervisor).

%% API
-export([get_store/0]).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

get_store() ->
    find_store(supervisor:which_children(?MODULE)).

find_store([]) ->
    undef;
find_store([{rest_service_store, P, worker, [rest_service_store]}|_]) -> P ;
find_store([_|T]) -> find_store(T).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Dispatch = cowboy_router:compile(
      [{'_',
        [
         {"/resource/[:resource_id]", rest_service, []}
        ]}]),
    RanchOptions = [{port, application:get_env(port, 8080)}, {nodelay, true}],
    CowboyEnv = [{env, [{dispatch, Dispatch},
                                            {max_keepalive, 100},
                                            {compress, false},
                                            {max_empty_lines, 5},
                                            {max_header_name_length, 64},
                                            {max_header_value_length, 4096},
                                            {max_headers, 100},
                                            {max_keepalive, 100},
                                            {max_request_line_length, 4096},
                                            {timeout, 30000}]}],
    {ok, {{one_for_one, 5, 10},
          [
           ?CHILD(rest_service_store_sup, rest_service_store_sup, supervisor, []),
           ranch:child_spec(
                 rest_service_store_8080, % Name
                 50, % Acceptor count
                 ranch_tcp, % transport method
                 RanchOptions, % ranch options
                 cowboy_protocol, % behaviour
                 CowboyEnv % cowboy options
                )
          ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
