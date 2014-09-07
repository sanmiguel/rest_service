%%%-------------------------------------------------------------------
%%% @author sanmiguel
%%% @copyright (C) 2014, sanmiguel
%%% @doc
%%%
%%% @end
%%% Created : 2014-02-02 16:24:24.578224
%%%-------------------------------------------------------------------
-module(rest_service_SUITE).

%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1
         ]).

%% test cases
-export([
         t_prop_statem/1
         %% TODO: test case names go here
        ]).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     {group, statem}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {statem, [], [
                   t_prop_statem
                  ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    inets:start(),
    {ok, Apps} = application:ensure_all_started(rest_service),

    [{apps, Apps} | Config].

end_per_suite(Config) ->
    Apps = lists:reverse(?config(apps, Config)),
    [ ok = application:stop(A) || A <- Apps ],
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
t_prop_statem(_Config) ->
    true = proper:quickcheck(prop_rest_service:prop_rest()).
