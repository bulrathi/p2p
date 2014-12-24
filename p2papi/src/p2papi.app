%%
%% @author bw <derbosebar@gmail.com>
%% @copyright 2014 Sergey Vinogradov.
%%

{application, p2papi,
 [{description, "p2papi"},
  {vsn,"0.0.1"},
  {modules, [
    p2papi,
    p2papi_deps
    ]},
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib, inets, xmerl]}]}.
