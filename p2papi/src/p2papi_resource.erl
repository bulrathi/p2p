%%
%% @author bw <derbosebar@gmail.com>
%% @copyright 2014 Sergey Vinogradov.
%%

-module(p2papi_resource).
-author('Sergey Vinogradov <derbosebar@gmail.com>').

%% external api
-export([from_http_response/2, get_http_response/3]).

-include("restfulierl.hrl").

%%
%% External API
%%

% extract a resource from a http response
from_http_response(Uri, HttpResponse) ->
  {ok, {{_HttpVersion, _StatusCode, _Message}, _Headers, Body}} = HttpResponse,

  {Xml, _Rest} = xmerl_scan:string(Body),

  _Resource = from_xml(Uri, Xml).

% get a http response from a http resquest of resource's state against uri
get_http_response(HttpMethod, Uri, ResourceState) ->
  Headers = [],
  ContentType = "application/xml",
  Body = to_xml(ResourceState),
  HttpOptions = [],
  Options = [{body_format, string}],

  _HttpResponse = http:request(HttpMethod,
                    {Uri, Headers, ContentType, Body}, HttpOptions, Options).

%%
%% Internal API
%%

% parse a xml to resource
from_xml(Uri, Xml) ->
  _Resource = restfulierl_xml_unmarshaler:to_resource(Uri, Xml).

% parse a resource's state to xml
to_xml(ResourceState) ->
  _Xml = restfulierl_xml_marshaler:to_xml(ResourceState).
