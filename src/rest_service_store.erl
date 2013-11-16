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
%% File: rest_service_store.erl. OTP gen_server based simple storage facility.
%%
%% --------------------------------------------------------------------
-module(rest_service_store).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([ clean/1
         , list_keys/1
         , append/3
         , is_key/2
         , fetch/2
         , delete/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {resources = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

clean(PID) ->
	gen_server:call(PID, {clean}).

list_keys(PID) ->
	gen_server:call(PID, {list_keys}).

append(PID, Key, Value) ->
    gen_server:call(PID, {append, Key, Value}).

is_key(PID, Key) ->
    gen_server:call(PID, {is_key, Key}).

fetch(PID, Key) ->
    gen_server:call(PID, {fetch, Key}).

delete(PID, Key) ->
    gen_server:call(PID, {delete, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({clean}, _From, #state{}=State) ->
	{reply, ok, State#state{resources=dict:new()}};
handle_call({list_keys}, _From, #state{resources=R}=State) ->
	{reply, dict:fetch_keys(R), State};
handle_call({is_key, Key}, _From, #state{resources=R}=State) ->
    {reply, dict:is_key(Key, R), State};
handle_call({fetch, Key}, _From, #state{resources=R}=State) ->
    {reply, dict:fetch(Key, R), State};
handle_call({delete, Key}, _From, #state{resources=R}=State) ->
    {reply, ok, State#state{resources=dict:erase(Key, R)}};
handle_call({append, Key, Value}, _From, #state{resources=R}=State) ->
    {reply, ok, State#state{resources=dict:append(Key, Value, R)}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
