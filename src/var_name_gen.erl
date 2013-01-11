%% Copyright
-module(var_name_gen).
-author("yngui").

%% API
-export([new/0, handle_event/2, generate/1]).

-record(name_gen, {count = 0, names}).

new() ->
    #name_gen{names = sets:new()}.

handle_event({record_attr_expr, E}, _) ->
    Ns = collect_expr_names(E, sets:new()),
    #name_gen{names = Ns};
handle_event({fun_clause, FC}, _) ->
    Ns1 = collect_clause_names(FC, sets:new()),
    #name_gen{names = Ns1};
handle_event(_, NG) ->
    NG.

generate(#name_gen{count = C, names = Ns}) ->
    generate(C, Ns).

generate(C, Ns) ->
    Name = list_to_atom(lists:concat(['V', C])),
    case sets:is_element(Name, Ns) of
        true ->
            generate(C + 1, Ns);
        _ ->
            {Name, #name_gen{count = C + 1, names = Ns}}
    end.

collect_clauses_names([C | Cs], Ns0) ->
    Ns1 = collect_clause_names(C, Ns0),
    collect_clauses_names(Cs, Ns1);
collect_clauses_names([], Ns) ->
    Ns.

collect_clause_names({clause, _, Ps, _, B}, Ns0) ->
    Ns1 = collect_patterns_names(Ps, Ns0),
    collect_exprs_names(B, Ns1).

collect_exprs_names([E | Es], Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_exprs_names(Es, Ns1);
collect_exprs_names([], Ns) ->
    Ns.

collect_expr_names({match, _, P, E}, Ns0) ->
    Ns1 = collect_pattern_names(P, Ns0),
    collect_expr_names(E, Ns1);
collect_expr_names({tuple, _, Es}, Ns) ->
    collect_exprs_names(Es, Ns);
collect_expr_names({cons, _, Eh, Et}, Ns0) ->
    Ns1 = collect_expr_names(Eh, Ns0),
    collect_expr_names(Et, Ns1);
collect_expr_names({bin, _, BEs}, Ns) ->
    collect_bin_expr_elems_names(BEs, Ns);
collect_expr_names({op, _, _, E1, E2}, Ns0) ->
    Ns1 = collect_expr_names(E1, Ns0),
    collect_expr_names(E2, Ns1);
collect_expr_names({op, _, _, E}, Ns) ->
    collect_expr_names(E, Ns);
collect_expr_names({record, _, _, RFs}, Ns) ->
    collect_record_expr_fields_names(RFs, Ns);
collect_expr_names({record, _, E, _, RFs}, Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_record_expr_fields_names(RFs, Ns1);
collect_expr_names({record_field, _, E, _, Field}, Ns0) ->
    Ns1 = collect_expr_names(Field, Ns0),
    collect_expr_names(E, Ns1);
collect_expr_names({'catch', _, E}, Ns) ->
    collect_expr_names(E, Ns);
collect_expr_names({call, _, {remote, _, Em, E}, Es}, Ns0) ->
    Ns1 = collect_expr_names(Em, Ns0),
    Ns2 = collect_expr_names(E, Ns1),
    collect_exprs_names(Es, Ns2);
collect_expr_names({call, _, E, Es}, Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_exprs_names(Es, Ns1);
collect_expr_names({lc, _, E, Ws}, Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_lc_expr_gens_names(Ws, Ns1);
collect_expr_names({bc, _, E, Ws}, Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_bc_expr_gens_names(Ws, Ns1);
collect_expr_names({block, _, B}, Ns) ->
    collect_exprs_names(B, Ns);
collect_expr_names({'if', _, ICs}, Ns) ->
    collect_clauses_names(ICs, Ns);
collect_expr_names({'case', _, E, CCs}, Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_clauses_names(CCs, Ns1);
collect_expr_names({'try', _, B, CCs, TCs}, Ns0) ->
    Ns1 = collect_exprs_names(B, Ns0),
    Ns2 = collect_clauses_names(CCs, Ns1),
    collect_clauses_names(TCs, Ns2);
collect_expr_names({'try', _, B, CCs, TCs, A}, Ns0) ->
    Ns1 = collect_exprs_names(B, Ns0),
    Ns2 = collect_clauses_names(CCs, Ns1),
    Ns3 = collect_clauses_names(TCs, Ns2),
    collect_exprs_names(A, Ns3);
collect_expr_names({'receive', _, CCs}, Ns) ->
    collect_clauses_names(CCs, Ns);
collect_expr_names({'receive', _, CCs, E, Bt}, Ns0) ->
    Ns1 = collect_exprs_names(Bt, Ns0),
    Ns2 = collect_expr_names(E, Ns1),
    collect_clauses_names(CCs, Ns2);
collect_expr_names({'fun', _, {function, Module, Name, Arity}}, Ns0) ->
    Ns1 = collect_expr_names(Module, Ns0),
    Ns2 = collect_expr_names(Name, Ns1),
    collect_expr_names(Arity, Ns2);
collect_expr_names({'fun', _, {clauses, FCs}}, Ns) ->
    collect_clauses_names(FCs, Ns);
collect_expr_names(_, Ns) ->
    Ns.

collect_bin_expr_elems_names([{bin_element, _, E, Size, _} | BEs], Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    Ns2 = collect_expr_names(Size, Ns1),
    collect_bin_expr_elems_names(BEs, Ns2);
collect_bin_expr_elems_names([], Ns) ->
    Ns.

collect_record_expr_fields_names([{record_field, _, Field, E} | RFs], Ns0) ->
    Ns1 = collect_expr_names(Field, Ns0),
    Ns2 = collect_expr_names(E, Ns1),
    collect_record_expr_fields_names(RFs, Ns2);
collect_record_expr_fields_names([], Ns) ->
    Ns.

collect_lc_expr_gens_names([{generate, _, P, E} | Ws], Ns0) ->
    Ns1 = collect_pattern_names(P, Ns0),
    Ns2 = collect_expr_names(E, Ns1),
    collect_lc_expr_gens_names(Ws, Ns2);
collect_lc_expr_gens_names([E | Ws], Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_lc_expr_gens_names(Ws, Ns1);
collect_lc_expr_gens_names([], Ns) ->
    Ns.

collect_bc_expr_gens_names([{b_generate, _, P, E} | Ws], Ns0) ->
    Ns1 = collect_pattern_names(P, Ns0),
    Ns2 = collect_expr_names(E, Ns1),
    collect_bc_expr_gens_names(Ws, Ns2);
collect_bc_expr_gens_names([E | Ws], Ns0) ->
    Ns1 = collect_expr_names(E, Ns0),
    collect_bc_expr_gens_names(Ws, Ns1);
collect_bc_expr_gens_names([], Ns) ->
    Ns.

collect_patterns_names([P | Ps], Ns0) ->
    Ns1 = collect_pattern_names(P, Ns0),
    collect_patterns_names(Ps, Ns1);
collect_patterns_names([], Ns) ->
    Ns.

collect_pattern_names({match, _, P1, P2}, Ns0) ->
    Ns1 = collect_pattern_names(P1, Ns0),
    collect_pattern_names(P2, Ns1);
collect_pattern_names({var, _, A}, Ns) when A =/= '_' ->
    sets:add_element(A, Ns);
collect_pattern_names({tuple, _, Ps}, Ns) ->
    collect_patterns_names(Ps, Ns);
collect_pattern_names({cons, _, Ph, Pt}, Ns0) ->
    Ns1 = collect_pattern_names(Ph, Ns0),
    collect_pattern_names(Pt, Ns1);
collect_pattern_names({bin, _, BEs}, Ns) ->
    collect_bin_pattern_elems_names(BEs, Ns);
collect_pattern_names({op, _, _, P1, P2}, Ns0) ->
    Ns1 = collect_pattern_names(P1, Ns0),
    collect_pattern_names(P2, Ns1);
collect_pattern_names({op, _, _, P}, Ns) ->
    collect_pattern_names(P, Ns);
collect_pattern_names({record, _, _, RFs}, Ns) ->
    collect_record_pattern_fields_names(RFs, Ns);
collect_pattern_names({record_index, _, _, Field}, Ns) ->
    collect_pattern_names(Field, Ns);
collect_pattern_names(_, Ns) ->
    Ns.

collect_bin_pattern_elems_names([{bin_element, _, P, Size, _} | BEs], Ns0) ->
    Ns1 = collect_pattern_names(P, Ns0),
    Ns2 = collect_pattern_names(Size, Ns1),
    collect_bin_pattern_elems_names(BEs, Ns2);
collect_bin_pattern_elems_names([], Ns) ->
    Ns.

collect_record_pattern_fields_names([{record_field, _, Field, P} | RFs], Ns0) ->
    Ns1 = collect_pattern_names(Field, Ns0),
    Ns2 = collect_pattern_names(P, Ns1),
    collect_record_pattern_fields_names(RFs, Ns2);
collect_record_pattern_fields_names([], Ns) ->
    Ns.
