rest_service
============

Simple cowboy-based REST webservice
-----------------------------------

Usage:

```bash
$ rebar get-deps compile
```

Start the service:

```erlang
1> [ application:start(A) || A <- [crypto, ranch, cowlib, cowboy] ].
[ok,ok,ok,ok]
2> application:start(rest_service).
ok
```

Interact with the service:

```erlang
1> inets:start().
ok
2> httpc:request(get, {"http://localhost:8080/resource/123", []}, [], []).
{ok,{{"HTTP/1.1",404,"Not Found"},
     [{"connection","keep-alive"},
      {"date","Thu, 31 Oct 2013 11:58:57 GMT"},
      {"server","Cowboy"},
      {"content-length","0"},
      {"content-type","application/json"}],
     []}}
3> httpc:request(put, {"http://localhost:8080/resource/123", [], "application/json", "foo"}, [], []).
{ok,{{"HTTP/1.1",204,"No Content"},
     [{"connection","keep-alive"},
      {"date","Thu, 31 Oct 2013 11:59:29 GMT"},
      {"server","Cowboy"},
      {"content-length","0"},
      {"content-type","application/json"}],
     []}}
4> httpc:request(get, {"http://localhost:8080/resource/123", []}, [], []).                           {ok,{{"HTTP/1.1",200,"OK"},
     [{"connection","keep-alive"},
      {"date","Thu, 31 Oct 2013 11:59:33 GMT"},
      {"server","Cowboy"},
      {"content-length","3"},
      {"content-type","application/json"}],
     "foo"}}
5> httpc:request(delete, {"http://localhost:8080/resource/123", []}, [], []).
{ok,{{"HTTP/1.1",204,"No Content"},
     [{"connection","keep-alive"},
      {"date","Thu, 31 Oct 2013 12:01:25 GMT"},
      {"server","Cowboy"},
      {"content-length","0"},
      {"content-type","application/json"}],
     []}}
6> httpc:request(get, {"http://localhost:8080/resource/123", []}, [], []).
{ok,{{"HTTP/1.1",404,"Not Found"},
     [{"connection","keep-alive"},
      {"date","Thu, 31 Oct 2013 12:01:26 GMT"},
      {"server","Cowboy"},
      {"content-length","0"},
      {"content-type","application/json"}],
     []}}
```
