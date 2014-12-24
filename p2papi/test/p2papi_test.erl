%%
%% @author bw <derbosebar@gmail.com>
%% @copyright 2014 Sergey Vinogradov.
%%

-module(p2papi_test).
-author('Sergey Vinogradov <derbosebar@gmail.com>').

-include("p2papi.hrl").

-include_lib("eunit/include/eunit.hrl").

describe_restfulierl_test_() ->
  {"p2papi",
    [
      {"before tests",
        {setup, fun before_all/0, []}},

      {"when started",
        [
          {"should get a resource from a valid uri",
            fun should_get_a_resource_from_a_valid_uri/0},
          {"should post a new resource to its uri",
            fun should_post_a_new_resource_to_its_uri/0},
          {"should post a new resource to a valid uri",
            fun should_post_a_new_resource_to_a_valid_uri/0},
          {"should post resource to one of its transitions",
            fun should_post_resource_to_one_of_its_transitions/0}
        ]},

      {"after tests",
        {setup, fun after_all/0, []}}
    ]}.

%%
%% Setup
%%

-define(RESOURCE_URI, "http://p2p/orders/11.xml").

before_all() ->
  restfulierl:start().

after_all() ->
  ok.

%%
%% Scenary
%%

should_get_a_resource_from_a_valid_uri() ->
  Resource = p2papi:get_resource(?RESOURCE_URI),

  % {resource,"http://p2p/orders/11.xml",
  %       {create_account,[],
  %           [{'created-at',[],["2010-01-01T05:15:45Z"]},
  %            {'account-name',[],["Sergey Vinogradov"]},
  %            {id,[],["11"]},
  %            {status,[],["create"]},
  %            {'updated-at',[],["2014-10-03T14:07:18Z"]},
  %            "\n"]},
  %       [{transition,latest,
  %            "http://p2p/orders/11"}]}}

  % matching uri
  ?assertEqual(Resource#resource.uri, ?RESOURCE_URI),

  % matching state
  CreateState = Resource#resource.state,

  {order, [], OrderAttributes} = CreateState,

  [CreatedAt | NextOrderAttributes1] = OrderAttributes,
  ?assertMatch({'created-at',[],["2010-01-01T05:15:45Z"]}, CreatedAt),

  [CustomerName | NextOrderAttributes2] = NextOrderAttributes1,
  ?assertMatch({'account-name',[],["Sergey Vinogradov"]}, CustomerName),

  [Id | NextOrderAttributes3] = NextOrderAttributes2,
  ?assertMatch({id,[],["11"]}, Id),

  [Status | NextOrderAttributes4] = NextOrderAttributes3,
  ?assertMatch({status,[],["create"]}, Status),

  [UpdateAt | _] = NextOrderAttributes4,
  ?assertMatch({'updated-at',[],["2014-10-03T14:07:18Z"]}, UpdateAt),

  % matching transitions
  OrderTransitions = Resource#resource.transitions,

  [Latest | _NextOrderTransitions] = OrderTransitions,
  ?assertMatch({transition, latest, "http://p2p/orders/11"}, Latest).

should_post_a_new_resource_to_its_uri() ->
  Resource = #resource{
                  uri = ?RESOURCE_URI,
                  state = {order, [], []}},

  Response = p2papi:post_resource(Resource),

  ?assertMatch({ok, _}, Response).

should_post_a_new_resource_to_a_valid_uri() ->
  Resource = #resource{state = {order, [], []}},

  Response = p2papi:post_resource(Resource, ?RESOURCE_URI),

  ?assertMatch({ok, _}, Response).

should_post_resource_to_one_of_its_transitions() ->
  Resource = #resource{state = {order, [], []}},

  Response = p2papi:post_resource(Resource, pay),

  ?assertMatch(yet_not_implemented, Response).

