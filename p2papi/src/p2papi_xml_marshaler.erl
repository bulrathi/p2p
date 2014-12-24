%%
%% @author bw <derbosebar@gmail.com>
%% @copyright 2014 Sergey Vinogradov.
%%

%% @doc marshal a resource to a xml.

-module(p2papi_xml_marshaler).
-author('Sergey Vinogradov <derbosebar@gmail.com>').

%% external api
-export([to_xml/1]).

-include("restfulierl.hrl").

-include_lib("xmerl/include/xmerl.hrl").

%%
%% External API
%%

%% marshal a resource record to a xml
to_xml(ResourceState) ->
  _Xml = lists:flatten(xmerl:export_simple([ResourceState], xmerl_xml)).

%%
%% Internal APIs
%%
