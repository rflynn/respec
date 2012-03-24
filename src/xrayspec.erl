% ex: set ts=4 et:

-module(xrayspec).
-export([check/2, test/0, tests/0, pl_cmp/2, main/1]).

check_proplist_([], [], ErrAcc) -> ErrAcc;
check_proplist_([], VL, ErrAcc) -> [VL|ErrAcc];
check_proplist_([{optional, _}|TTail], [], ErrAcc) ->
    check_proplist_(TTail, [], ErrAcc);
check_proplist_([{optional, T}|TTail], VL=[V|_], ErrAcc) when T < V ->
    check_proplist_(TTail, VL, ErrAcc);
check_proplist_([{optional, T}|TTail], [V|VTail], ErrAcc) when T >= V ->
    ErrAcc2 =
        case check(T, V) of
            ok -> ErrAcc;
            Error -> [Error|ErrAcc]
        end,
    check_proplist_(TTail, VTail, ErrAcc2);
check_proplist_([T|TTail], [V|VTail], ErrAcc) ->
    ErrAcc2 =
        case check(T, V) of
            ok -> ErrAcc;
            Error -> [Error|ErrAcc]
        end,
    check_proplist_(TTail, VTail, ErrAcc2).

check_proplist(T, V) ->
    case check_proplist_(T, V, []) of
        [] -> ok;
        Else -> Else
    end.

check_(any, _) -> ok;
check_(bool, V) when is_boolean(V) -> ok;
check_(atom, V) when is_atom(V) -> ok;
check_(int, V) when is_integer(V) -> ok;
check_(binary, V) when is_binary(V) -> ok;
check_({proplist, T}, V) when is_list(T) and is_list(V) ->
    check_proplist(
        lists:sort(fun xrayspec:pl_cmp/2, T),
        lists:sort(V));
check_(T, V) when is_list(T) and is_list(V) ->
    Res = [check_(T0, V0) || {T0,V0} <- lists:zip(T, V) ],
    case lists:all(fun(R)-> R == ok end, Res) of
        true -> ok;
        _ -> Res
    end;
check_(T, V) when is_tuple(T) and is_tuple(V) ->
    case check_(tuple_to_list(T), tuple_to_list(V)) of
        ok -> ok;
        Fail -> list_to_tuple(Fail)
    end;
check_(Exp, Val) when Exp =:= Val -> ok; % type-strict literal match
check_(Exp, Val) -> {Val,Exp}.

check(Exp, Val) ->
    try
        check_(Exp, Val)
    catch
        _:_ -> {Val,Exp}
    end.

% proplist item cmp for sort; ignore potential {optional, ...} enclosure
pl_cmp({optional, X}, Y) -> pl_cmp(X, Y);
pl_cmp(X, {optional, Y}) -> pl_cmp(X, Y);
pl_cmp(X, Y) when X =< Y -> true;
pl_cmp(_, _) -> false.

tests() ->
    {tests,
        [
            {plcmp_0,      (fun()-> lists:sort(fun xrayspec:pl_cmp/2, [{a,0},{b,1}]) == [{a,0},{b,1}] end)() },
            {plcmp_1,      (fun()-> lists:sort(fun xrayspec:pl_cmp/2, [{optional,{a,0}},{b,1}]) == [{optional,{a,0}},{b,1}] end)() },
            {plcmp_2,      (fun()-> lists:sort(fun xrayspec:pl_cmp/2, [{optional,{b,1}},{a,0}]) == [{a,0},{optional,{b,1}}] end)() },
            {atom_pass0,   (fun()-> check(atom,x) == ok end)()                                      },
            {atom_fail0,   (fun()-> check(atom,0) == {0,atom} end)()                                },
            {int_pass0,    (fun()-> check(int,0) == ok end)()                                       },
            {int_fail0,    (fun()-> check(int,x) == {x,int} end)()                                  },
            {int_pass1,    (fun()-> check(0,0) == ok end)()                                         },
            {list_pass0,   (fun()-> check([],[]) == ok end)()                                       },
            {list_fail0,   (fun()-> check([],{}) == {{},[]} end)()                                  },
            {list_pass1,   (fun()-> check([int],[0]) == ok end)()                                   },
            {tuple_pass0,  (fun()-> check({},{}) == ok end)()                                       },
            {tuple_pass1,  (fun()-> check({atom},{x}) == ok end)()                                  },
            {tuple_fail1,  (fun()-> check({atom},{0}) == {{0,atom}} end)()                          },
            {tuple_pass2,  (fun()-> check({atom,atom},{x,y}) == ok end)()                           },
            {tuple_fail2,  (fun()-> check({atom,atom},{0,1}) == {{0,atom},{1,atom}} end)()          },
            {tuple_fail3,  (fun()-> check({int},{0,1}) == {{0,1},{int}} end)()                      },
            {pl_lit_pass0, (fun()-> check({proplist,[]},[]) == ok end)()                            },
            {pl_lit_pass1, (fun()-> check({proplist,[{a,0}]},[{a,0}]) == ok end)()                  },
            {pl_lit_pass2, (fun()-> check({proplist,[{optional,{a,0}}]},[{a,0}]) == ok end)()       },
            {pl_lit_pass3, (fun()-> check({proplist,[{optional,{a,0}}]},[]) == ok end)()            },
            {pl_lit_pass4, (fun()-> check({proplist,[{optional,{a,0}},{b,1}]},[{b,1}]) == ok end)() },
            {pl_lit_pass5, (fun()-> check({proplist,[{a,0},{b,1},{c,2}]},[{a,0},{b,1},{c,2}]) == ok end)() },
            {pl_lit_pass6, (fun()-> check({proplist,[{a,0},{optional,{b,1}},{c,2}]},[{a,0},{c,2}]) == ok end)() },
            {pl_type_pass0,(fun()-> check({proplist,[{atom,int}]},[{a,0}]) == ok end)() },

            {pl_real_pass0,
                (fun()-> check(
                    {proplist,
                        [
                            {<<"a">>,int},
                            {<<"b">>,binary},
                            {<<"c">>,int}
                        ]
                    },
                    [
                        {<<"a">>,0},
                        {<<"b">>,<<"foo">>},
                        {<<"c">>,0}
                    ]
                ) == ok end)() }

        ]}.

test() ->
    {tests, Results} = tests(),
    Failed = lists:filter(fun({_Name, R})-> R =/= true end, Results),
    if
        Failed == [] -> ok;
        true -> Failed
    end.

main(_Args) ->
    io:format("test(): ~w~n", [test()]).

