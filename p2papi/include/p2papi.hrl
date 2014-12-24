%%
%% @author bw <derbosebar@gmail.com>
%% @copyright 2014 Sergey Vinogradov.
%%

-record(resource, {uri, state = {}, transitions = []}).

-record(transition, {name, uri}).
