%% Copyright
-module(polyrec_trans).
-author("yngui").

%% API
-export([parse_transform/2]).

-define(FUN_NAME_GEN_MOD, fun_name_gen).
-define(VAR_NAME_GEN_MOD, var_name_gen).
-define(MULTIPLE_RECORD_EXTENSION, "multiple record extension").
-define(CYCLIC_RECORD_EXTENSION, "cyclic record extension").
-define(RECORD_UNDEFINED, "record ~w undefined").
-define(BAD_RECORD_EXTENSION, "bad record extension").
-define(ERROR(Message, Line), {error, {Line, erl_parse, Message}}).
-define(ERROR(Message, Args, Line), {error, {Line, erl_parse, io_lib:format(Message, Args)}}).
-define(CALL(Name, Args, Line), {call, Line, {atom, Line, Name}, Args}).
-define(CALL(Module, Name, Args, Line), {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Name}}, Args}).

-record(expand, {fun_name_gen_mod = ?FUN_NAME_GEN_MOD, fun_name_gen,
    var_name_gen_mod = ?VAR_NAME_GEN_MOD, var_name_gen, acc = []}).

parse_transform(Forms, _Options) ->
    FNG = ?FUN_NAME_GEN_MOD:new(),
    VNG = ?VAR_NAME_GEN_MOD:new(),
    St = #expand{fun_name_gen = FNG, var_name_gen = VNG},
    transform(Forms, St).

transform(Fs0, St0) ->
    {Fs1, G} = take_extensions(Fs0),
    Fs2 = extend_records(Fs1, G),
    St1 = handle_event({forms, Fs2}, St0),
    St2 = expand_forms(Fs2, G, St1),
    lists:reverse(St2#expand.acc).

take_extensions(Fs) ->
    take_extensions(Fs, digraph:new_acyclic(), []).

take_extensions([{attribute, _, record, {Name, _}} = F | Fs], G0, Acc) ->
    G1 = digraph:add_vertex(Name, G0),
    take_extensions(Fs, G1, [F | Acc]);
take_extensions([{attribute, Line, record_extends, {Name1, Name2}} | Fs], G0, Acc0)
    when is_atom(Name1), is_atom(Name2) ->
    case digraph:in_neighbours(Name1, G0) of
        [] ->
            case digraph:add_edge(Name2, Name1, G0) of
                {error, {bad_edge, _}} ->
                    Acc1 = [?ERROR(?CYCLIC_RECORD_EXTENSION, Line) | Acc0],
                    take_extensions(Fs, G0, Acc1);
                {error, {bad_vertex, Name1}} ->
                    Acc1 = [?ERROR(?RECORD_UNDEFINED, [Name1], Line) | Acc0],
                    take_extensions(Fs, G0, Acc1);
                {error, {bad_vertex, Name2}} ->
                    Acc1 = [?ERROR(?RECORD_UNDEFINED, [Name2], Line) | Acc0],
                    take_extensions(Fs, G0, Acc1);
                G1 ->
                    take_extensions(Fs, G1, Acc0)
            end;
        _ ->
            Acc1 = [?ERROR(?MULTIPLE_RECORD_EXTENSION, Line) | Acc0],
            take_extensions(Fs, G0, Acc1)
    end;
take_extensions([{attribute, Line, record_extends, _} | Fs], G, Acc0) ->
    Acc1 = [?ERROR(?BAD_RECORD_EXTENSION, Line) | Acc0],
    take_extensions(Fs, G, Acc1);
take_extensions([F | Fs], G, Acc) ->
    take_extensions(Fs, G, [F | Acc]);
take_extensions([], G, Acc) ->
    {lists:reverse(Acc), G}.

extend_records(Fs, G) ->
    extend_records(Fs, G, dict:new(), []).

extend_records([{attribute, Line, record, {Name, Vs0}} = F | Fs], G, Recs0, Acc0) ->
    case digraph:in_neighbours(Name, G) of
        [BaseName] ->
            case dict:find(BaseName, Recs0) of
                {ok, BaseVs} ->
                    Vs1 = set_record_attr_fields_line(BaseVs, Line) ++ Vs0,
                    Recs1 = dict:store(Name, Vs1, Recs0),
                    Acc1 = [{attribute, Line, record, {Name, Vs1}} | Acc0],
                    extend_records(Fs, G, Recs1, Acc1);
                _ ->
                    Acc1 = [?ERROR(?RECORD_UNDEFINED, [BaseName], Line) | Acc0],
                    extend_records(Fs, G, Recs0, Acc1)
            end;
        _ ->
            Recs1 = dict:store(Name, Vs0, Recs0),
            extend_records(Fs, G, Recs1, [F | Acc0])
    end;
extend_records([F | Fs], G, Recs, Acc) ->
    extend_records(Fs, G, Recs, [F | Acc]);
extend_records([], _, _, Acc) ->
    lists:reverse(Acc).

set_record_attr_fields_line(Vs, Line) ->
    set_record_attr_fields_line(Vs, Line, []).

set_record_attr_fields_line([{record_field, _, A0} | Vs], Line, Acc0) ->
    A1 = set_expr_line(A0, Line),
    Acc1 = [{record_field, Line, A1} | Acc0],
    set_record_attr_fields_line(Vs, Line, Acc1);
set_record_attr_fields_line([{record_field, _, A0, E0} | Vs], Line, Acc0) ->
    A1 = set_expr_line(A0, Line),
    E1 = set_expr_line(E0, Line),
    Acc1 = [{record_field, Line, A1, E1} | Acc0],
    set_record_attr_fields_line(Vs, Line, Acc1);
set_record_attr_fields_line([], _, Acc) ->
    lists:reverse(Acc).

set_clauses_line(Cs, Line) ->
    set_clauses_line(Cs, Line, []).

set_clauses_line([{clause, _, Ps0, Gs0, B0} | Cs], Line, Acc0) ->
    Ps1 = set_patterns_line(Ps0, Line),
    Gs1 = [set_guard_tests_line(Gts, Line) || Gts <- Gs0],
    B1 = set_exprs_line(B0, Line),
    Acc1 = [{clause, Line, Ps1, Gs1, B1} | Acc0],
    set_clauses_line(Cs, Line, Acc1);
set_clauses_line([], _, Acc) ->
    lists:reverse(Acc).

set_exprs_line(Es, Line) ->
    set_exprs_line(Es, Line, []).

set_exprs_line([E | Es], Line, Acc0) ->
    Acc1 = [set_expr_line(E, Line) | Acc0],
    set_exprs_line(Es, Line, Acc1);
set_exprs_line([], _, Acc) ->
    lists:reverse(Acc).

set_expr_line({integer, _, L}, Line) ->
    {integer, Line, L};
set_expr_line({char, _, L}, Line) ->
    {char, Line, L};
set_expr_line({float, _, L}, Line) ->
    {float, Line, L};
set_expr_line({string, _, Cs}, Line) ->
    {string, Line, Cs};
set_expr_line({atom, _, L}, Line) ->
    {atom, Line, L};
set_expr_line({match, _, P0, E0}, Line) ->
    P1 = set_pattern_line(P0, Line),
    E1 = set_expr_line(E0, Line),
    {match, Line, P1, E1};
set_expr_line({var, _, A}, Line) ->
    {var, Line, A};
set_expr_line({tuple, _, Es0}, Line) ->
    Es1 = set_exprs_line(Es0, Line),
    {tuple, Line, Es1};
set_expr_line({nil, _}, Line) ->
    {nil, Line};
set_expr_line({cons, _, Eh0, Et0}, Line) ->
    Eh1 = set_expr_line(Eh0, Line),
    Et1 = set_expr_line(Et0, Line),
    {cons, Line, Eh1, Et1};
set_expr_line({bin, _, BEs0}, Line) ->
    BEs1 = set_bin_expr_elems_line(BEs0, Line),
    {bin, Line, BEs1};
set_expr_line({op, _, Op, E10, E20}, Line) ->
    E11 = set_expr_line(E10, Line),
    E21 = set_expr_line(E20, Line),
    {op, Line, Op, E11, E21};
set_expr_line({op, _, Op, E0}, Line) ->
    E1 = set_expr_line(E0, Line),
    {op, Line, Op, E1};
set_expr_line({record, _, Name, RFs0}, Line) ->
    RFs1 = set_record_expr_fields_line(RFs0, Line),
    {record, Line, Name, RFs1};
set_expr_line({record, _, E0, Name, RFs0}, Line) ->
    E1 = set_expr_line(E0, Line),
    RFs1 = set_record_expr_fields_line(RFs0, Line),
    {record, Line, E1, Name, RFs1};
set_expr_line({record_index, _, Name, Field0}, Line) ->
    Field1 = set_expr_line(Field0, Line),
    {record_index, Line, Name, Field1};
set_expr_line({record_field, _, E0, Name, Field0}, Line) ->
    E1 = set_expr_line(E0, Line),
    Field1 = set_expr_line(Field0, Line),
    {record_field, Line, E1, Name, Field1};
set_expr_line({'catch', _, E0}, Line) ->
    E1 = set_expr_line(E0, Line),
    {'catch', Line, E1};
set_expr_line({call, _, {remote, _, Em0, E0}, Es0}, Line) ->
    Em1 = set_expr_line(Em0, Line),
    E1 = set_expr_line(E0, Line),
    Es1 = set_exprs_line(Es0, Line),
    {call, Line, {remote, Line, Em1, E1}, Es1};
set_expr_line({call, _, E0, Es0}, Line) ->
    E1 = set_expr_line(E0, Line),
    Es1 = set_exprs_line(Es0, Line),
    {call, Line, E1, Es1};
set_expr_line({lc, _, E0, Ws0}, Line) ->
    E1 = set_expr_line(E0, Line),
    Ws1 = set_lc_expr_gens_line(Ws0, Line),
    {lc, Line, E1, Ws1};
set_expr_line({bc, _, E0, Ws0}, Line) ->
    E1 = set_expr_line(E0, Line),
    Ws1 = set_bc_expr_gens_line(Ws0, Line),
    {bc, Line, E1, Ws1};
set_expr_line({block, _, B0}, Line) ->
    B1 = set_exprs_line(B0, Line),
    {block, Line, B1};
set_expr_line({'if', _, ICs0}, Line) ->
    ICs1 = set_clauses_line(ICs0, Line),
    {'if', Line, ICs1};
set_expr_line({'case', _, E0, CCs0}, Line) ->
    E1 = set_expr_line(E0, Line),
    CCs1 = set_clauses_line(CCs0, Line),
    {'case', Line, E1, CCs1};
set_expr_line({'try', _, B0, CCs0, TCs0, A0}, Line) ->
    B1 = set_exprs_line(B0, Line),
    CCs1 = set_clauses_line(CCs0, Line),
    TCs1 = set_clauses_line(TCs0, Line),
    A1 = set_exprs_line(A0, Line),
    {'try', Line, B1, CCs1, TCs1, A1};
set_expr_line({'receive', _, CCs0}, Line) ->
    CCs1 = set_clauses_line(CCs0, Line),
    {'receive', Line, CCs1};
set_expr_line({'receive', _, CCs0, E0, Bt0}, Line) ->
    CCs1 = set_clauses_line(CCs0, Line),
    E1 = set_expr_line(E0, Line),
    Bt1 = set_exprs_line(Bt0, Line),
    {'receive', Line, CCs1, E1, Bt1};
set_expr_line({'fun', _, {function, Name, Arity}}, Line) ->
    {'fun', Line, {function, Name, Arity}};
set_expr_line({'fun', _, {function, Module0, Name0, Arity0}}, Line) ->
    Module1 = set_expr_line(Module0, Line),
    Name1 = set_expr_line(Name0, Line),
    Arity1 = set_expr_line(Arity0, Line),
    {'fun', Line, {function, Module1, Name1, Arity1}};
set_expr_line({'fun', _, {clauses, FCs0}}, Line) ->
    FCs1 = set_clauses_line(FCs0, Line),
    {'fun', Line, {clauses, FCs1}}.

set_record_expr_fields_line(RFs, Line) ->
    set_record_expr_fields_line(RFs, Line, []).

set_record_expr_fields_line([{record_field, _, Field0, E0} | RFs], Line, Acc0) ->
    Field1 = set_expr_line(Field0, Line),
    E1 = set_expr_line(E0, Line),
    Acc1 = [{record_field, Line, Field1, E1} | Acc0],
    set_record_expr_fields_line(RFs, Line, Acc1);
set_record_expr_fields_line([], _, Acc) ->
    lists:reverse(Acc).

set_bin_expr_elems_line(BEs, Line) ->
    set_bin_expr_elems_line(BEs, Line, []).

set_bin_expr_elems_line([{bin_element, _, V0, default, TSL} | BEs], Line, Acc0) ->
    V1 = set_expr_line(V0, Line),
    Acc1 = [{bin_element, Line, V1, default, TSL} | Acc0],
    set_bin_expr_elems_line(BEs, Line, Acc1);
set_bin_expr_elems_line([{bin_element, _, V0, Size0, TSL} | BEs], Line, Acc0) ->
    V1 = set_expr_line(V0, Line),
    Size1 = set_expr_line(Size0, Line),
    Acc1 = [{bin_element, Line, V1, Size1, TSL} | Acc0],
    set_bin_expr_elems_line(BEs, Line, Acc1);
set_bin_expr_elems_line([], _, Acc) ->
    lists:reverse(Acc).

set_lc_expr_gens_line(Ws, Line) ->
    set_lc_expr_gens_line(Ws, Line, []).

set_lc_expr_gens_line([{generate, _, P0, E0} | Ws], Line, Acc0) ->
    P1 = set_pattern_line(P0, Line),
    E1 = set_expr_line(E0, Line),
    Acc1 = [{generate, Line, P1, E1} | Acc0],
    set_lc_expr_gens_line(Ws, Line, Acc1);
set_lc_expr_gens_line([E | Ws], Line, Acc0) ->
    Acc1 = [set_expr_line(E, Line) | Acc0],
    set_lc_expr_gens_line(Ws, Line, Acc1);
set_lc_expr_gens_line([], _, Acc) ->
    lists:reverse(Acc).

set_bc_expr_gens_line(Ws, Line) ->
    set_bc_expr_gens_line(Ws, Line, []).

set_bc_expr_gens_line([{b_generate, _, P0, E0} | Ws], Line, Acc0) ->
    P1 = set_pattern_line(P0, Line),
    E1 = set_expr_line(E0, Line),
    Acc1 = [{generate, Line, P1, E1} | Acc0],
    set_bc_expr_gens_line(Ws, Line, Acc1);
set_bc_expr_gens_line([E | Ws], Line, Acc0) ->
    Acc1 = [set_expr_line(E, Line) | Acc0],
    set_bc_expr_gens_line(Ws, Line, Acc1);
set_bc_expr_gens_line([], _, Acc) ->
    lists:reverse(Acc).

set_patterns_line(Ps, Line) ->
    set_patterns_line(Ps, Line, []).

set_patterns_line([P | Ps], Line, Acc0) ->
    Acc1 = [set_pattern_line(P, Line) | Acc0],
    set_patterns_line(Ps, Line, Acc1);
set_patterns_line([], _, Acc) ->
    lists:reverse(Acc).

set_pattern_line({integer, _, L}, Line) ->
    {integer, Line, L};
set_pattern_line({char, _, L}, Line) ->
    {char, Line, L};
set_pattern_line({float, _, L}, Line) ->
    {float, Line, L};
set_pattern_line({string, _, Cs}, Line) ->
    {string, Line, Cs};
set_pattern_line({atom, _, L}, Line) ->
    {atom, Line, L};
set_pattern_line({match, _, P10, P20}, Line) ->
    P11 = set_pattern_line(P10, Line),
    P21 = set_pattern_line(P20, Line),
    {match, Line, P11, P21};
set_pattern_line({var, _, A}, Line) ->
    {var, Line, A};
set_pattern_line({tuple, _, Ps0}, Line) ->
    Ps1 = set_patterns_line(Ps0, Line),
    {tuple, Line, Ps1};
set_pattern_line({nil, _}, Line) ->
    {nil, Line};
set_pattern_line({cons, _, Ph0, Pt0}, Line) ->
    Ph1 = set_pattern_line(Ph0, Line),
    Pt1 = set_pattern_line(Pt0, Line),
    {cons, Line, Ph1, Pt1};
set_pattern_line({bin, _, BEs0}, Line) ->
    BEs1 = set_bin_pattern_elems_line(BEs0, Line),
    {bin, Line, BEs1};
set_pattern_line({op, _, Op, P10, P20}, Line) ->
    P11 = set_pattern_line(P10, Line),
    P21 = set_pattern_line(P20, Line),
    {op, Line, Op, P11, P21};
set_pattern_line({op, _, Op, P0}, Line) ->
    P1 = set_pattern_line(P0, Line),
    {op, Line, Op, P1};
set_pattern_line({record, _, Name, RFs0}, Line) ->
    RFs1 = set_record_pattern_elems_line(RFs0, Line),
    {record, Line, Name, RFs1};
set_pattern_line({record_index, _, Name, Field0}, Line) ->
    Field1 = set_pattern_line(Field0, Line),
    {record_index, Line, Name, Field1}.

set_bin_pattern_elems_line(BEs, Line) ->
    set_bin_pattern_elems_line(BEs, Line, []).

set_bin_pattern_elems_line([{bin_element, _, P0, default, TSL} | BEs], Line, Acc0) ->
    P1 = set_pattern_line(P0, Line),
    Acc1 = [{bin_element, Line, P1, default, TSL} | Acc0],
    set_bin_pattern_elems_line(BEs, Line, Acc1);
set_bin_pattern_elems_line([{bin_element, _, P0, Size0, TSL} | BEs], Line, Acc0) ->
    P1 = set_pattern_line(P0, Line),
    Size1 = set_pattern_line(Size0, Line),
    Acc1 = [{bin_element, Line, P1, Size1, TSL} | Acc0],
    set_bin_pattern_elems_line(BEs, Line, Acc1);
set_bin_pattern_elems_line([], _, Acc) ->
    lists:reverse(Acc).

set_record_pattern_elems_line(RFs, Line) ->
    set_record_pattern_elems_line(RFs, Line, []).

set_record_pattern_elems_line([{record_field, _, Field0, P0} | RFs], Line, Acc0) ->
    Field1 = set_pattern_line(Field0, Line),
    P1 = set_pattern_line(P0, Line),
    Acc1 = [{record_field, Line, Field1, P1} | Acc0],
    set_record_pattern_elems_line(RFs, Line, Acc1);
set_record_pattern_elems_line([], _, Acc) ->
    lists:reverse(Acc).

set_guard_tests_line(Gts, Line) ->
    set_guard_tests_line(Gts, Line, []).

set_guard_tests_line([Gt | Gts], Line, Acc0) ->
    Acc1 = [set_guard_test_line(Gt, Line) | Acc0],
    set_guard_tests_line(Gts, Line, Acc1);
set_guard_tests_line([], _, Acc) ->
    lists:reverse(Acc).

set_guard_test_line({integer, _, L}, Line) ->
    {integer, Line, L};
set_guard_test_line({char, _, L}, Line) ->
    {char, Line, L};
set_guard_test_line({float, _, L}, Line) ->
    {float, Line, L};
set_guard_test_line({string, _, Cs}, Line) ->
    {string, Line, Cs};
set_guard_test_line({atom, _, L}, Line) ->
    {atom, Line, L};
set_guard_test_line({var, _, A}, Line) ->
    {var, Line, A};
set_guard_test_line({tuple, _, Gts0}, Line) ->
    Gts1 = set_guard_tests_line(Gts0, Line),
    {tuple, Line, Gts1};
set_guard_test_line({nil, _}, Line) ->
    {nil, Line};
set_guard_test_line({cons, _, Gth0, Gtt0}, Line) ->
    Gth1 = set_guard_test_line(Gth0, Line),
    Gtt1 = set_guard_test_line(Gtt0, Line),
    {cons, Line, Gth1, Gtt1};
set_guard_test_line({bin, _, BEs0}, Line) ->
    BEs1 = set_bin_guard_test_elems_line(BEs0, Line),
    {bin, Line, BEs1};
set_guard_test_line({op, _, Op, Gt10, Gt20}, Line) ->
    Gt11 = set_guard_test_line(Gt10, Line),
    Gt21 = set_guard_test_line(Gt20, Line),
    {op, Line, Op, Gt11, Gt21};
set_guard_test_line({op, _, Op, Gt0}, Line) ->
    Gt1 = set_guard_test_line(Gt0, Line),
    {op, Line, Op, Gt1};
set_guard_test_line({record, _, Name, RFs0}, Line) ->
    RFs1 = set_record_guard_test_fields_line(RFs0, Line),
    {record, Line, Name, RFs1};
set_guard_test_line({record_index, _, Name, Field0}, Line) ->
    Field1 = set_guard_test_line(Field0, Line),
    {record_index, Line, Name, Field1};
set_guard_test_line({record_field, _, Gt0, Name, Field0}, Line) ->
    Gt1 = set_guard_test_line(Gt0, Line),
    Field1 = set_guard_test_line(Field0, Line),
    {record_field, Line, Gt1, Name, Field1};
set_guard_test_line({call, _, {remote, _, Am0, A0}, Gts0}, Line) ->
    Am1 = set_guard_test_line(Am0, Line),
    A1 = set_guard_test_line(A0, Line),
    Gts1 = set_guard_tests_line(Gts0, Line),
    {call, Line, {remote, Line, Am1, A1}, Gts1};
set_guard_test_line({call, _, A0, Gts0}, Line) ->
    A1 = set_guard_test_line(A0, Line),
    Gts1 = set_guard_tests_line(Gts0, Line),
    {call, Line, A1, Gts1}.

set_bin_guard_test_elems_line(BEs, Line) ->
    set_bin_guard_test_elems_line(BEs, Line, []).

set_bin_guard_test_elems_line([{bin_element, _, Gt0, default, TSL} | BEs], Line, Acc0) ->
    Gt1 = set_guard_test_line(Gt0, Line),
    Acc1 = [{bin_element, Line, Gt1, default, TSL} | Acc0],
    set_bin_guard_test_elems_line(BEs, Line, Acc1);
set_bin_guard_test_elems_line([{bin_element, _, Gt0, Size0, TSL} | BEs], Line, Acc0) ->
    Gt1 = set_guard_test_line(Gt0, Line),
    Size1 = set_guard_test_line(Size0, Line),
    Acc1 = [{bin_element, Line, Gt1, Size1, TSL} | Acc0],
    set_bin_guard_test_elems_line(BEs, Line, Acc1);
set_bin_guard_test_elems_line([], _, Acc) ->
    lists:reverse(Acc).

set_record_guard_test_fields_line(RFs, Line) ->
    set_record_guard_test_fields_line(RFs, Line, []).

set_record_guard_test_fields_line([{record_field, _, Field0, Gt0} | RFs], Line, Acc0) ->
    Field1 = set_guard_test_line(Field0, Line),
    Gt1 = set_guard_test_line(Gt0, Line),
    Acc1 = [{record_field, Line, Field1, Gt1} | Acc0],
    set_record_guard_test_fields_line(RFs, Line, Acc1);
set_record_guard_test_fields_line([], _, Acc) ->
    lists:reverse(Acc).

expand_forms([{attribute, Line, record, {Name, Vs0}} | Fs], G, St0) ->
    {Vs1, St1} = expand_record_attr_fields(Vs0, G, St0),
    Acc = [{attribute, Line, record, {Name, Vs1}} | St1#expand.acc],
    expand_forms(Fs, G, St1#expand{acc = Acc});
expand_forms([{function, Line, Name, Arity, FCs0} | Fs], G, St0) ->
    {FCs1, St1} = expand_fun_clauses(FCs0, G, St0),
    Acc = [{function, Line, Name, Arity, FCs1} | St1#expand.acc],
    expand_forms(Fs, G, St1#expand{acc = Acc});
expand_forms([F | Fs], G, St) ->
    Acc = [F | St#expand.acc],
    expand_forms(Fs, G, St#expand{acc = Acc});
expand_forms([], _, St) ->
    St.

expand_record_attr_fields(Vs, G, St) ->
    expand_record_attr_fields(Vs, G, St, []).

expand_record_attr_fields([{record_field, Line, A0} | Vs], G, St0, Acc0) ->
    {A1, St1} = expand_record_attr_expr(A0, G, St0),
    Acc1 = [{record_field, Line, A1} | Acc0],
    expand_record_attr_fields(Vs, G, St1, Acc1);
expand_record_attr_fields([{record_field, Line, A0, E0} | Vs], G, St0, Acc0) ->
    {A1, St1} = expand_record_attr_expr(A0, G, St0),
    {E1, St2} = expand_record_attr_expr(E0, G, St1),
    Acc1 = [{record_field, Line, A1, E1} | Acc0],
    expand_record_attr_fields(Vs, G, St2, Acc1);
expand_record_attr_fields([], _, St, Acc) ->
    {lists:reverse(Acc), St}.

expand_record_attr_expr(E0, G, St0) ->
    St1 = handle_event({record_attr_expr, E0}, St0),
    case expand_expr(E0, G, St1) of
        {_, #expand{var_name_gen = NG}} = Res when NG =:= St1#expand.var_name_gen ->
            Res;
        {E1, St2} ->
            {N, St3} = generate_fun_name(St2),
            Line = element(2, E1),
            F = {function, Line, N, 0, [{clause, Line, [], [], [E1]}]},
            A = {attribute, Line, compile, {inline, [{N, 0}]}},
            Acc = [F, A | St2#expand.acc],
            {?CALL(N, [], Line), St3#expand{acc = Acc}}
    end.

generate_fun_name(St) ->
    {A, NG} = (St#expand.fun_name_gen_mod):generate(St#expand.fun_name_gen),
    {A, St#expand{fun_name_gen = NG}}.

expand_fun_clauses(FCs, G, St) ->
    expand_fun_clauses(FCs, G, St, []).

expand_fun_clauses([{clause, Line, Ps0, Gs0, B0} = FC | FCs], G, St0, Acc0) ->
    St1 = handle_event({fun_clause, FC}, St0),
    {B1, St2} = expand_exprs(B0, G, St1),
    L1 = product([expand_pattern(P, G) || P <- Ps0]),
    L2 = product([product([expand_guard_test(Gt, G) || Gt <- Gts]) || Gts <- Gs0]),
    Acc1 = lists:reverse([{clause, Line, Ps1, Gs1, B1} || Ps1 <- L1, Gs1 <- L2], Acc0),
    expand_fun_clauses(FCs, G, St2, Acc1);
expand_fun_clauses([], _, St, Acc) ->
    {lists:reverse(Acc), St}.

handle_event(E, St) ->
    FNG = (St#expand.fun_name_gen_mod):handle_event(E, St#expand.fun_name_gen),
    VNG = (St#expand.var_name_gen_mod):handle_event(E, St#expand.var_name_gen),
    St#expand{fun_name_gen = FNG, var_name_gen = VNG}.

expand_clauses(Cs, G, St) ->
    expand_clauses(Cs, G, St, []).

expand_clauses([{clause, Line, Ps0, Gs0, B0} | Cs], G, St0, Acc0) ->
    {B1, St1} = expand_exprs(B0, G, St0),
    L1 = product([expand_pattern(P, G) || P <- Ps0]),
    L2 = product([product([expand_guard_test(Gt, G) || Gt <- Gts]) || Gts <- Gs0]),
    Acc1 = lists:reverse([{clause, Line, Ps1, Gs1, B1} || Ps1 <- L1, Gs1 <- L2], Acc0),
    expand_clauses(Cs, G, St1, Acc1);
expand_clauses([], _, St, Acc) ->
    {lists:reverse(Acc), St}.

expand_exprs(Es, G, St) ->
    expand_exprs(Es, G, St, []).

expand_exprs([E0 | Es], G, St0, Acc) ->
    {E1, St1} = expand_expr(E0, G, St0),
    expand_exprs(Es, G, St1, [E1 | Acc]);
expand_exprs([], _, St, Acc) ->
    {lists:reverse(Acc), St}.

expand_expr({match, Line, P0, E0}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    case expand_pattern(P0, G) of
        [_] ->
            {{match, Line, P0, E1}, St1};
        L ->
            {A, St2} = generate_var_name(St1),
            V = {var, Line, A},
            B = [V],
            CCs = [{clause, Line, [{match, Line, V, P1}], [], B} || P1 <- L],
            CC = {clause, Line, [V], [],
                [?CALL(erlang, error, [{tuple, Line, [{atom, Line, badmatch}, V]}], Line), {match, Line, P0, V}]},
            {{'case', Line, E1, CCs ++ [CC]}, St2}
    end;
expand_expr({tuple, Line, Es0}, G, St0) ->
    {Es1, St1} = expand_exprs(Es0, G, St0),
    {{tuple, Line, Es1}, St1};
expand_expr({cons, Line, Eh0, Et0}, G, St0) ->
    {Eh1, St1} = expand_expr(Eh0, G, St0),
    {Et1, St2} = expand_expr(Et0, G, St1),
    {{cons, Line, Eh1, Et1}, St2};
expand_expr({bin, Line, BEs0}, G, St0) ->
    {BEs1, St1} = expand_bin_elem_exprs(BEs0, G, St0),
    {{bin, Line, BEs1}, St1};
expand_expr({op, Line, Op, E10, E20}, G, St0) ->
    {E11, St1} = expand_expr(E10, G, St0),
    {E21, St2} = expand_expr(E20, G, St1),
    {{op, Line, Op, E11, E21}, St2};
expand_expr({op, Line, Op, E0}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    {{op, Line, Op, E1}, St1};
expand_expr({record, Line, Name, RFs0}, G, St0) ->
    {RFs1, St1} = expand_record_field_exprs(RFs0, G, St0),
    {{record, Line, Name, RFs1}, St1};
expand_expr({record, Line, E0, Name0, RFs0}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    {RFs1, St2} = expand_record_field_exprs(RFs0, G, St1),
    case digraph:reachable(Name0, G) of
        [_] ->
            {{record, Line, E1, Name0, RFs1}, St2};
        L ->
            {A, St3} = generate_var_name(St2),
            V = {var, Line, A},
            Ps = [V],
            CCs = [{clause, Line, Ps, [[?CALL(erlang, is_record, [V, {atom, Line, Name1}], Line)]],
                [{record, Line, V, Name1, RFs1}]} || Name1 <- L],
            CC = {clause, Line, [{var, Line, '_'}], [],
                [?CALL(erlang, error, [{tuple, Line, [{atom, Line, badrecord}, {atom, Line, Name0}]}], Line)]},
            {{'case', Line, E1, CCs ++ [CC]}, St3}
    end;
expand_expr({record_index, Line, Name, Field0}, G, St0) ->
    {Field1, St1} = expand_expr(Field0, G, St0),
    {{record_index, Line, Name, Field1}, St1};
expand_expr({record_field, Line, E0, Name0, Field0}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    {Field1, St2} = expand_expr(Field0, G, St1),
    case digraph:reachable(Name0, G) of
        [_] ->
            {{record_field, Line, E1, Name0, Field1}, St2};
        L ->
            {A, St3} = generate_var_name(St2),
            V = {var, Line, A},
            Ps = [V],
            CCs = [{clause, Line, Ps, [[?CALL(erlang, is_record, [V, {atom, Line, Name1}], Line)]],
                [{record_field, Line, V, Name1, Field1}]} || Name1 <- L],
            CC = {clause, Line, [{var, Line, '_'}], [],
                [?CALL(erlang, error, [{tuple, Line, [{atom, Line, badrecord}, {atom, Line, Name0}]}], Line)]},
            {{'case', Line, E1, CCs ++ [CC]}, St3}
    end;
expand_expr({'catch', Line, E0}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    {{'catch', Line, E1}, St1};
expand_expr({call, Line1, {remote, _, {atom, _, erlang}, {atom, _, is_record}} = Rem,
    [E0, {atom, Line5, Name0}]}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    case digraph:reachable(Name0, G) of
        [_] ->
            {{call, Line1, Rem, [E1, {atom, Line5, Name0}]}, St1};
        L ->
            {A, St2} = generate_var_name(St1),
            V = {var, Line1, A},
            Ps = [V],
            B = [{atom, Line1, 'true'}],
            CCs = [{clause, Line1, Ps, [[{call, Line1, Rem, [V, {atom, Line5, Name1}]}]], B} || Name1 <- L],
            CC = {clause, Line1, [{var, Line1, '_'}], [], [{atom, Line1, 'false'}]},
            {{'case', Line1, E1, CCs ++ [CC]}, St2}
    end;
expand_expr({call, Line1, {remote, Line2, Em0, E0}, Es0}, G, St0) ->
    {Em1, St1} = expand_expr(Em0, G, St0),
    {E1, St2} = expand_expr(E0, G, St1),
    {Es1, St3} = expand_exprs(Es0, G, St2),
    {{call, Line1, {remote, Line2, Em1, E1}, Es1}, St3};
expand_expr({call, Line1, {atom, _, is_record} = E, [E0, {atom, Line3, Name0}]}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    case digraph:reachable(Name0, G) of
        [_] ->
            {{call, Line1, E, [E1, {atom, Line3, Name0}]}, St1};
        L ->
            {A, St2} = generate_var_name(St1),
            V = {var, Line1, A},
            Ps = [V],
            B = [{atom, Line1, 'true'}],
            CCs = [{clause, Line1, Ps, [[{call, Line1, E, [V, {atom, Line3, Name1}]}]], B} || Name1 <- L],
            CC = {clause, Line1, [{var, Line1, '_'}], [], [{atom, Line1, 'false'}]},
            {{'case', Line1, E1, CCs ++ [CC]}, St2}
    end;
expand_expr({call, Line, E0, Es0}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    {Es1, St2} = expand_exprs(Es0, G, St1),
    {{call, Line, E1, Es1}, St2};
expand_expr({block, Line, B0}, G, St0) ->
    {B1, St1} = expand_exprs(B0, G, St0),
    {{block, Line, B1}, St1};
expand_expr({'if', Line, ICs0}, G, St0) ->
    {ICs1, St1} = expand_clauses(ICs0, G, St0),
    {{'if', Line, ICs1}, St1};
expand_expr({'case', Line, E0, CCs0}, G, St0) ->
    {E1, St1} = expand_expr(E0, G, St0),
    {CCs1, St2} = expand_clauses(CCs0, G, St1),
    {{'case', Line, E1, CCs1}, St2};
expand_expr({'try', Line, B0, CCs0, TCs0, A0}, G, St0) ->
    {B1, St1} = expand_exprs(B0, G, St0),
    {CCs1, St2} = expand_clauses(CCs0, G, St1),
    {TCs1, St3} = expand_clauses(TCs0, G, St2),
    {A1, St4} = expand_exprs(A0, G, St3),
    {{'try', Line, B1, CCs1, TCs1, A1}, St4};
expand_expr({'receive', Line, CCs0}, G, St0) ->
    {CCs1, St1} = expand_clauses(CCs0, G, St0),
    {{'receive', Line, CCs1}, St1};
expand_expr({'receive', Line, CCs0, E0, Bt0}, G, St0) ->
    {CCs1, St1} = expand_clauses(CCs0, G, St0),
    {E1, St2} = expand_expr(E0, G, St1),
    {Bt1, St3} = expand_exprs(Bt0, G, St2),
    {{'receive', Line, CCs1, E1, Bt1}, St3};
expand_expr({'fun', Line, {function, Module0, Name0, Arity0}}, G, St0) ->
    {Module1, St1} = expand_expr(Module0, G, St0),
    {Name1, St2} = expand_expr(Name0, G, St1),
    {Arity1, St3} = expand_expr(Arity0, G, St2),
    {{'fun', Line, {function, Module1, Name1, Arity1}}, St3};
expand_expr({'fun', Line, {clauses, FCs0}}, G, St0) ->
    {FCs1, St1} = expand_clauses(FCs0, G, St0),
    {{'fun', Line, {clauses, FCs1}}, St1};
expand_expr(E, _, St) ->
    {E, St}.

generate_var_name(St) ->
    {N, NG} = (St#expand.var_name_gen_mod):generate(St#expand.var_name_gen),
    {N, St#expand{var_name_gen = NG}}.

expand_bin_elem_exprs(BEs, G, St) ->
    expand_bin_elem_exprs(BEs, G, St, []).

expand_bin_elem_exprs([{bin_element, Line, V0, Size0, TSL} | BEs], G, St0, Acc0) ->
    {V1, St1} = expand_expr(V0, G, St0),
    {Size1, St2} = expand_expr(Size0, G, St1),
    Acc1 = [{bin_element, Line, V1, Size1, TSL} | Acc0],
    expand_bin_elem_exprs(BEs, G, St2, Acc1);
expand_bin_elem_exprs([], _, St, Acc) ->
    {lists:reverse(Acc), St}.

expand_record_field_exprs(RFs, G, St) ->
    expand_record_field_exprs(RFs, G, St, []).

expand_record_field_exprs([{record_field, Line, Field0, E0} | RFs], G, St0, Acc0) ->
    {Field1, St1} = expand_expr(Field0, G, St0),
    {E1, St2} = expand_expr(E0, G, St1),
    Acc1 = [{record_field, Line, Field1, E1} | Acc0],
    expand_record_field_exprs(RFs, G, St2, Acc1);
expand_record_field_exprs([], _, St, Acc) ->
    {lists:reverse(Acc), St}.

expand_pattern({match, Line, P10, P20}, G) ->
    L1 = expand_pattern(P10, G),
    L2 = expand_pattern(P20, G),
    [{match, Line, P11, P21} || P11 <- L1, P21 <- L2];
expand_pattern({tuple, Line, Ps0}, G) ->
    L = product([expand_pattern(P, G) || P <- Ps0]),
    [{tuple, Line, Ps1} || Ps1 <- L];
expand_pattern({cons, Line, Ph0, Pt0}, G) ->
    L1 = expand_pattern(Ph0, G),
    L2 = expand_pattern(Pt0, G),
    [{cons, Line, Ph1, Pt1} || Ph1 <- L1, Pt1 <- L2];
expand_pattern({bin, Line, BEs0}, G) ->
    L1 = product([case E of
        {bin_element, Line, P0, Size0, TSL} ->
            L2 = expand_pattern(P0, G),
            L3 = expand_pattern(Size0, G),
            [{bin_element, Line, P1, Size1, TSL} || P1 <- L2, Size1 <- L3]
    end || E <- BEs0]),
    [{bin, Line, BEs1} || BEs1 <- L1];
expand_pattern({op, Line, Op, P10, P20}, G) ->
    L1 = expand_pattern(P10, G),
    L2 = expand_pattern(P20, G),
    [{op, Line, Op, P11, P21} || P11 <- L1, P21 <- L2];
expand_pattern({op, Line, Op, P0}, G) ->
    L = expand_pattern(P0, G),
    [{op, Line, Op, P1} || P1 <- L];
expand_pattern({record, Line, Name0, RFs0}, G) ->
    L1 = digraph:reachable(Name0, G),
    L2 = product([case RF of
        {record_field, Line, Field0, P0} ->
            L3 = expand_pattern(Field0, G),
            L4 = expand_pattern(P0, G),
            [{record_field, Line, Field1, P1} || Field1 <- L3, P1 <- L4]
    end || RF <- RFs0]),
    [{record, Line, Name1, RFs1} || Name1 <- L1, RFs1 <- L2] ;
expand_pattern({record_index, Line, Name, Field0}, G) ->
    L = expand_pattern(Field0, G),
    [{record_index, Line, Name, Field1} || Field1 <- L];
expand_pattern(P, _) ->
    [P].

expand_guard_test({tuple, Line, Gts0}, G) ->
    L = product([expand_guard_test(Gt, G) || Gt <- Gts0]),
    [{tuple, Line, Gts1} || Gts1 <- L];
expand_guard_test({cons, Line, Gth0, Gtt0}, G) ->
    L1 = expand_guard_test(Gth0, G),
    L2 = expand_guard_test(Gtt0, G),
    [{cons, Line, Gth1, Gtt1} || Gth1 <- L1, Gtt1 <- L2];
expand_guard_test({bin, Line, BEs0}, G) ->
    L1 = product([case E of
        {bin_element, Line, Gt0, Size0, TSL} ->
            L2 = expand_guard_test(Gt0, G),
            L3 = expand_guard_test(Size0, G),
            [{bin_element, Line, Gt1, Size1, TSL} || Gt1 <- L2, Size1 <- L3]
    end || E <- BEs0]),
    [{bin, Line, BEs1} || BEs1 <- L1];
expand_guard_test({op, Line, Op, Gt10, Gt20}, G) ->
    L1 = expand_guard_test(Gt10, G),
    L2 = expand_guard_test(Gt20, G),
    [{op, Line, Op, Gt11, Gt22} || Gt11 <- L1, Gt22 <- L2];
expand_guard_test({op, Line, Op, Gt0}, G) ->
    L = expand_guard_test(Gt0, G),
    [{op, Line, Op, Gt1} || Gt1 <- L];
expand_guard_test({record, Line, Name, RFs0}, G) ->
    L1 = product([case RF of
        {record_field, Line, Field0, Gt0} ->
            L2 = expand_guard_test(Field0, G),
            L3 = expand_guard_test(Gt0, G),
            [{record_field, Line, Field1, Gt1} || Field1 <- L2, Gt1 <- L3]
    end || RF <- RFs0]),
    [{record, Line, Name, RFs1} || RFs1 <- L1] ;
expand_guard_test({record_index, Line, Name, Field0}, G) ->
    L = expand_guard_test(Field0, G),
    [{record_index, Line, Name, Field1} || Field1 <- L];
expand_guard_test({record_field, Line, Gt0, Name0, Field0}, G) ->
    L1 = expand_guard_test(Gt0, G),
    L2 = digraph:reachable(Name0, G),
    L3 = expand_guard_test(Field0, G),
    [{record_field, Line, Gt1, Name1, Field1} || Gt1 <- L1, Name1 <- L2, Field1 <- L3];
expand_guard_test({call, Line1, {remote, _, {atom, _, erlang}, {atom, _, is_record}} = Rem,
    [Gt0, {atom, Line5, Name0}]}, G) ->
    L1 = expand_guard_test(Gt0, G),
    L2 = digraph:reachable(Name0, G),
    [{call, Line1, Rem, [Gt1, {atom, Line5, Name1}]} || Gt1 <- L1, Name1 <- L2];
expand_guard_test({call, Line1, {remote, Line2, Am0, A0}, Gts0}, G) ->
    L1 = expand_guard_test(Am0, G),
    L2 = expand_guard_test(A0, G),
    L3 = product([expand_guard_test(Gt, G) || Gt <- Gts0]),
    [{call, Line1, {remote, Line2, Am1, A1}, Gts1} || Am1 <- L1, A1 <- L2, Gts1 <- L3];
expand_guard_test({call, Line1, {atom, _, is_record} = Gt, [Gt0, {atom, Line3, Name0}]}, G) ->
    L1 = expand_guard_test(Gt0, G),
    L2 = digraph:reachable(Name0, G),
    [{call, Line1, Gt, [Gt1, {atom, Line3, Name1}]} || Gt1 <- L1, Name1 <- L2];
expand_guard_test({call, Line, A0, Gts0}, G) ->
    L1 = expand_guard_test(A0, G),
    L2 = product([expand_guard_test(Gt, G) || Gt <- Gts0]),
    [{call, Line, A1, Gts1} || A1 <- L1, Gts1 <- L2];
expand_guard_test(Gt, _) ->
    [Gt].

product(L) ->
    product(lists:reverse(L), [[]]).

product([H | T], Acc) ->
    product(T, [[X | L] || X <- H, L <- Acc]);
product([], Acc) ->
    Acc.
