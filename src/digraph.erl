%% Copyright
-module(digraph).
-author("yngui").

%% API
-export([new_acyclic/0, add_vertex/2, add_edge/3, get_path/3, in_neighbours/2, reachable/2]).

-record(digraph, {vertices = sets:new(), neighbours = dict:new(), cyclic}).

new_acyclic() ->
    #digraph{cyclic = false}.

add_vertex(V, #digraph{vertices = Vs0} = G) ->
    Vs1 = sets:add_element(V, Vs0),
    G#digraph{vertices = Vs1}.

add_edge(V1, V2, #digraph{vertices = Vs, neighbours = Ns0, cyclic = C} = G) ->
    case sets:is_element(V1, Vs) of
        true ->
            case sets:is_element(V2, Vs) of
                true ->
                    if
                        C orelse V1 =/= V2 ->
                            case get_path(V2, V1, G) of
                                false ->
                                    E = {V1, V2},
                                    Ns1 = dict:append({out, V1}, E, Ns0),
                                    Ns2 = dict:append({in, V2}, E, Ns1),
                                    G#digraph{neighbours = Ns2};
                                Path ->
                                    {error, {bad_edge, Path}}
                            end;
                        true ->
                            {error, {bad_edge, [V1, V2]}}
                    end;
                _ ->
                    {error, {bad_vertex, V2}}
            end;
        _ ->
            {error, {bad_vertex, V1}}
    end.

get_path(V1, V2, #digraph{neighbours = Ns, cyclic = false}) ->
    case dict:find({out, V1}, Ns) of
        {ok, Es} ->
            get_path(Es, V2, Ns, [V1]);
        _ ->
            false
    end.

get_path([{_, V} | _], V, _, Acc) ->
    lists:reverse([V | Acc]);
get_path([{_, V1} | Es], V2, Ns, Acc) ->
    case dict:find({out, V1}, Ns) of
        {ok, Es1} ->
            case get_path(Es1, V2, Ns, [V1 | Acc]) of
                false ->
                    get_path(Es, V2, Ns, Acc);
                Path ->
                    Path
            end;
        _ ->
            get_path(Es, V2, Ns, Acc)
    end;
get_path([], _, _, _) ->
    false.

in_neighbours(V, #digraph{neighbours = Ns}) ->
    case dict:find({in, V}, Ns) of
        {ok, Es} ->
            [V1 || {V1, _} <- Es];
        _ ->
            []
    end.

reachable(V, #digraph{neighbours = Ns, cyclic = false}) ->
    reachable(V, Ns, []).

reachable(V, Ns, Acc) ->
    case dict:find({out, V}, Ns) of
        {ok, Es} ->
            Fun = fun({_, V2}, Acc1) -> reachable(V2, Ns, Acc1) end,
            lists:foldl(Fun, [V | Acc], Es);
        _ ->
            [V | Acc]
    end.
