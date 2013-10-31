%% ------------------------------------------------------------------------
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
%% File rest_service.erl. Simple cowboy_rest-based handler.
%% ------------------------------------------------------------------------
-module(rest_service).

%behaviour(cowboy_rest).
-export([init/3]).
-export([rest_init/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

% content funs
-export([display/2]).
-export([store/2]).

-record(context, {
          store :: pid()
         }).

-type context() :: #context{}.

%%
%% Cowboy REST callbacks
-spec init(atom(), cowboy_req:req(), Paths :: list()) ->
    {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Args) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), list()) ->
    {ok, cowboy_req:req(), context()}.
rest_init(Req, _Opts) ->
    StorePID = rest_service_store_sup:get_store(),
    {ok, Req, #context{store=StorePID}}.

-spec known_methods(cowboy_req:req(), context()) ->
    {list(binary()), cowboy_req:req(), context()}.
known_methods(Req, Context) ->

    {[<<"DELETE">>, <<"GET">>, <<"PUT">>, <<"POST">>], Req, Context}.

-spec allowed_methods(cowboy_req:req(), context()) ->
    {list(binary()), cowboy_req:req(), context()}.
allowed_methods(Req, Context) ->
    {[<<"DELETE">>, <<"GET">>, <<"PUT">>, <<"POST">>], Req, Context}.

-spec content_types_provided(cowboy_req:req(), context()) ->
    {list(), cowboy_req:req(), context()}.
content_types_provided(Req, Context) ->
    {Method, Req1} = cowboy_req:method(Req),
    CTA = case Method of
              <<"GET">>    -> [{{<<"application">>, <<"json">>, '*'}, display}];
              <<"DELETE">> -> [{{<<"application">>, <<"json">>, '*'}, undef}];
              <<"POST">>   -> [{{<<"application">>, <<"json">>, '*'}, undef}];
              <<"PUT">>    -> [{{<<"application">>, <<"json">>, '*'}, undef}];
			  _ -> []
          end,
    {CTA, Req1, Context}.

-spec content_types_accepted(cowboy_req:req(), context()) ->
    {list(), cowboy_req:req(), context()}.
content_types_accepted(Req, Context) ->
    {Method, Req1} = cowboy_req:method(Req),
    CTA = case Method of
              <<"PUT">>  -> [{{<<"application">>, <<"json">>, '*'}, store}];
              <<"POST">> -> [{{<<"application">>, <<"json">>, '*'}, store}];
			  _ -> []
          end,
    {CTA, Req1, Context}.


-spec resource_exists(cowboy_req:req(), context()) ->
    {true | false, cowboy_req:req(), context()}.
resource_exists(Req0, #context{store=Store}=Context) ->
    {RsrcID, Req1} = cowboy_req:binding(resource_id, Req0),
    {rest_service_store:is_key(Store, RsrcID), Req1, Context}.

-spec delete_resource(cowboy_req:req(), context()) ->
    {true | false, cowboy_req:req(), context()}.
delete_resource(Req0, #context{store=Store}=Context0) ->
    {RsrcID, Req1} = cowboy_req:binding(resource_id, Req0),
    rest_service_store:delete(Store, RsrcID),
    {true, Req1, Context0}.
    

-spec display(cowboy_req:req(), context()) ->
    {Body :: binary(), cowboy_req:req(), context()}.
display(Req0, #context{store=Store}=Context) ->

    {RsrcID, Req1} = cowboy_req:binding(resource_id, Req0),
    Rsrc = rest_service_store:fetch(Store, RsrcID),

    {Rsrc, Req1, Context}.

-spec store(cowboy_req:req(), context()) ->
    {Body :: binary(), cowboy_req:req(), context()}.
store(Req0, #context{store=Store}=Context) ->
    {RsrcID, Req1} = cowboy_req:binding(resource_id, Req0),

    {ok, Body, Req2} = cowboy_req:body(Req1),

    rest_service_store:append(Store, RsrcID, Body),
    {true, Req2, Context}.
