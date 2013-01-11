%% Copyright
-module(polyrec_trans_tests).
-author("yngui").

-include_lib("eunit/include/eunit.hrl").

%% API
-define(H1, "
            -record(r1, {f1, f2}).
            -record(r2, {f1, f2, f3, f4}).
            -record(r3, {f1, f2, f5, f6}).").
-define(H2, "
            -record(r1, {f1, f2}).
            -record(r2, {f3, f4}).
            -extends(r1).
            -record(r3, {f5, f6}).
            -extends(r1).").

parse_transform_test_() ->
    [
        ?_test(assertEqual("
            -record(r1, {f1}).
            -record(r2, {f1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = 1}).
            -record(r2, {f1 = 1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = 1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = $a}).
            -record(r2, {f1 = $a, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = $a}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = 1.0}).
            -record(r2, {f1 = 1.0, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = 1.0}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = \"abc\"}).
            -record(r2, {f1 = \"abc\", f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = \"abc\"}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = a}).
            -record(r2, {f1 = a, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = a}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = 1 = f()}).
            -record(r2, {f1 = 1 = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = 1 = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = $a = f()}).
            -record(r2, {f1 = $a = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = $a = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = 1.0 = f()}).
            -record(r2, {f1 = 1.0 = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = 1.0 = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = \"abc\" = f()}).
            -record(r2, {f1 = \"abc\" = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = \"abc\" = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = a = f()}).
            -record(r2, {f1 = a = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = a = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = (1 = 1) = f()}).
            -record(r2, {f1 = (1 = 1) = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = (1 = 1) = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = _ = f()}).
            -record(r2, {f1 = _ = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = _ = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = {1, a} = f()}).
            -record(r2, {f1 = {1, a} = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = {1, a} = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = [] = f()}).
            -record(r2, {f1 = [] = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = [] = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = [1, a] = f()}).
            -record(r2, {f1 = [1, a] = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = [1, a] = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = <<1, 1:4/integer>> = f()}).
            -record(r2, {f1 = <<1, 1:4/integer>> = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = <<1, 1:4/integer>> = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = 1 + 2 = f()}).
            -record(r2, {f1 = 1 + 2 = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = 1 + 2 = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = -1 = f()}).
            -record(r2, {f1 = -1 = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = -1 = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = #r{f1 = 1, f2 = a} = f()}).
            -record(r2, {f1 = #r{f1 = 1, f2 = a} = f(), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = #r{f1 = 1, f2 = a} = f()}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = #r.f1 = 1}).
            -record(r2, {f1 = #r.f1 = 1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = #r.f1 = 1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = {1, a}}).
            -record(r2, {f1 = {1, a}, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = {1, a}}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = []}).
            -record(r2, {f1 = [], f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = []}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = [1, a]}).
            -record(r2, {f1 = [1, a], f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = [1, a]}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = <<1, 1:4/integer>>}).
            -record(r2, {f1 = <<1, 1:4/integer>>, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = <<1, 1:4/integer>>}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = 1 + 2}).
            -record(r2, {f1 = 1 + 2, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = 1 + 2}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = -1}).
            -record(r2, {f1 = -1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = -1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = #r{f1 = 1, f2 = a}}).
            -record(r2, {f1 = #r{f1 = 1, f2 = a}, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = #r{f1 = 1, f2 = a}}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = (f())#r{f1 = 1, f2 = a}}).
            -record(r2, {f1 = (f())#r{f1 = 1, f2 = a}, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = (f())#r{f1 = 1, f2 = a}}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = #r.f1}).
            -record(r2, {f1 = #r.f1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = #r.f1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = (f())#r.f1}).
            -record(r2, {f1 = (f())#r.f1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = (f())#r.f1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = (catch f())}).
            -record(r2, {f1 = (catch f()), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = (catch f())}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = f(1, a)}).
            -record(r2, {f1 = f(1, a), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = f(1, a)}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = m:f(1, a)}).
            -record(r2, {f1 = m:f(1, a), f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = m:f(1, a)}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = begin 1, a end}).
            -record(r2, {f1 = begin 1, a end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = begin 1, a end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = if true -> ok end}).
            -record(r2, {f1 = if true -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = if true -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 -> ok end}).
            -record(r2, {f1 = case f() of 1 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 -> ok; a -> ok end}).
            -record(r2, {f1 = case f() of 1 -> ok; a -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 -> ok; a -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when 1, 1; 1 -> ok end}).
            -record(r2, {f1 = case f() of 1 when 1, 1; 1 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when 1, 1; 1 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when $a -> ok end}).
            -record(r2, {f1 = case f() of 1 when $a -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when $a -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when 1.0 -> ok end}).
            -record(r2, {f1 = case f() of 1 when 1.0 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when 1.0 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when \"abc\" -> ok end}).
            -record(r2, {f1 = case f() of 1 when \"abc\" -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when \"abc\" -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when a -> ok end}).
            -record(r2, {f1 = case f() of 1 when a -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when a -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when {1, a} -> ok end}).
            -record(r2, {f1 = case f() of 1 when {1, a} -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when {1, a} -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when [] -> ok end}).
            -record(r2, {f1 = case f() of 1 when [] -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when [] -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when [1, a] -> ok end}).
            -record(r2, {f1 = case f() of 1 when [1, a] -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when [1, a] -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when <<1, 1:4/integer>> -> ok end}).
            -record(r2, {f1 = case f() of 1 when <<1, 1:4/integer>> -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when <<1, 1:4/integer>> -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when 1 + 2 -> ok end}).
            -record(r2, {f1 = case f() of 1 when 1 + 2 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when 1 + 2 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when -1 -> ok end}).
            -record(r2, {f1 = case f() of 1 when -1 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when -1 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when #r{f1 = 1, f2 = a} -> ok end}).
            -record(r2, {f1 = case f() of 1 when #r{f1 = 1, f2 = a} -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when #r{f1 = 1, f2 = a} -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when #r.f1 -> ok end}).
            -record(r2, {f1 = case f() of 1 when #r.f1 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when #r.f1 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when (#r{})#r.f1 -> ok end}).
            -record(r2, {f1 = case f() of 1 when (#r{})#r.f1 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when (#r{})#r.f1 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when element(1, {1, a}) -> ok end}).
            -record(r2, {f1 = case f() of 1 when element(1, {1, a}) -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when element(1, {1, a}) -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = case f() of 1 when erlang:element(1, {1, a}) -> ok end}).
            -record(r2, {f1 = case f() of 1 when erlang:element(1, {1, a}) -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = case f() of 1 when erlang:element(1, {1, a}) -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = try f() catch _:_ -> ok end}).
            -record(r2, {f1 = try f() catch _:_ -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = try f() catch _:_ -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = try f() of 1 -> ok catch _:_ -> ok end}).
            -record(r2, {f1 = try f() of 1 -> ok catch _:_ -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = try f() of 1 -> ok catch _:_ -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = try f() catch _:_ -> ok after ok end}).
            -record(r2, {f1 = try f() catch _:_ -> ok after ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = try f() catch _:_ -> ok after ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = try f() of 1 -> ok catch _:_ -> ok after ok end}).
            -record(r2, {f1 = try f() of 1 -> ok catch _:_ -> ok after ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = try f() of 1 -> ok catch _:_ -> ok after ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = receive 1 -> ok end}).
            -record(r2, {f1 = receive 1 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = receive 1 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = receive 1 -> ok after 1 -> ok end}).
            -record(r2, {f1 = receive 1 -> ok after 1 -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = receive 1 -> ok after 1 -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = fun f/1}).
            -record(r2, {f1 = fun f/1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = fun f/1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = fun m:f/1}).
            -record(r2, {f1 = fun m:f/1, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = fun m:f/1}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        )),
        ?_test(assertEqual("
            -record(r1, {f1 = fun (1, a) -> ok end}).
            -record(r2, {f1 = fun (1, a) -> ok end, f2}).
            test(#r2{}) -> ok;
            test(#r1{}) -> ok.",
            "
            -record(r1, {f1 = fun (1, a) -> ok end}).
            -record(r2, {f2}).
            -record_extends({r2, r1}).
            test(#r1{}) -> ok."
        ))
%% ,

%%         ?_test(assertEqual(?H1 ++ "
%%             test(#r3{}) -> ok;
%%             test(#r2{}) -> ok;
%%             test(#r1{}) -> ok.", ?H2 ++ "
%%             test(#r1{}) -> ok.")),
%%         ?_test(assertEqual(?H1 ++ "
%%             test(A) ->
%%                 case A of
%%                     #r3{} -> ok;
%%                     #r2{} -> ok;
%%                     #r1{} -> ok
%%                 end.", ?H2 ++ "
%%             test(A) ->
%%                 case A of
%%                     #r1{} -> ok
%%                 end.")),
%%         ?_test(assertEqual(?H1 ++ "
%%             test(A) when is_record(A, r3) -> ok;
%%             test(A) when is_record(A, r2) -> ok;
%%             test(A) when is_record(A, r1) -> ok.", ?H2 ++ "
%%             test(A) when is_record(A, r1) -> ok."))
    ].

assertEqual(S1, S2) ->
    {ok, Ts1, _} = erl_scan:string(S1),
    {ok, Ts2, _} = erl_scan:string(S2),
    ?assertEqual(erl_prettypr:format(erl_syntax:form_list(parse_forms(Ts1))),
        erl_prettypr:format(erl_syntax:form_list(polyrec_trans:parse_transform(parse_forms(Ts2), [])))).

parse_forms(Ts) ->
    parse_forms(Ts, [], []).

parse_forms([{dot, _} = T | Ts], Acc1, Acc2) ->
    {ok, F} = erl_parse:parse_form(lists:reverse([T | Acc1])),
    parse_forms(Ts, [], [F | Acc2]);
parse_forms([T | Ts], Acc1, Acc2) ->
    parse_forms(Ts, [T | Acc1], Acc2);
parse_forms([], [], Acc2) ->
    lists:reverse(Acc2).
