%% Copyright
-module(fun_name_gen).
-author("yngui").

%% API
-export([new/0, handle_event/2, generate/1]).

-record(name_gen, {count = 0, names}).

new() ->
    #name_gen{names = sets:new()}.

handle_event({forms, Fs}, _) ->
    Ns = collect_forms_names(Fs, sets:new()),
    #name_gen{names = Ns};
handle_event(_, NG) ->
    NG.

generate(#name_gen{count = C, names = Ns}) ->
    generate(C, Ns).

generate(C, Ns) ->
    Name = list_to_atom(lists:concat([f, C])),
    case sets:is_element(Name, Ns) of
        true ->
            generate(C + 1, Ns);
        _ ->
            {Name, #name_gen{count = C + 1, names = Ns}}
    end.

collect_forms_names(_Fs, Ns) ->
    Ns.
