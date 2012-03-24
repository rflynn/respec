% ex: set ts=4 et:

% @spec for your data
% see ../README.md

-module(respec).
-export([check/2, test/0, tests/0, pl_cmp/2, main/1]).

check_proplist_([], [], ErrAcc) -> lists:reverse(ErrAcc);
check_proplist_([], VL, ErrAcc) -> lists:reverse([VL|ErrAcc]);
check_proplist_([{optional, _}|TTail], [], ErrAcc) ->
    check_proplist_(TTail, [], ErrAcc);
check_proplist_([{optional, {TK,_}}|TTail], VL=[{VK,_}|_], ErrAcc) when TK < VK ->
    check_proplist_(TTail, VL, ErrAcc);
check_proplist_([{optional, T}|TTail], [V|VTail], ErrAcc) ->
    ErrAcc2 =
        case check(T, V) of
            ok -> ErrAcc;
            _ -> [{V,T}|ErrAcc] % return full key/val pair for any error
        end,
    check_proplist_(TTail, VTail, ErrAcc2);
check_proplist_([T|TTail], [V|VTail], ErrAcc) ->
    ErrAcc2 =
        case check(T, V) of
            ok -> ErrAcc;
            _ -> [{V,T}|ErrAcc] % return full key/val pair for any error
        end,
    check_proplist_(TTail, VTail, ErrAcc2).

check_proplist(T, V) ->
    case check_proplist_(T, V, []) of
        [] -> ok;
        Else -> Else
    end.

check_(any, _) -> ok;
check_(bool, V) when is_boolean(V) -> ok;
check_(int, V) when is_integer(V) -> ok;
check_(binary, V) when is_binary(V) -> ok;
check_(atom, V) when is_atom(V) -> ok;
check_(float, V) when is_float(V) -> ok;
check_({proplist, T}, V) when is_list(T) and is_list(V) ->
    check_proplist(
        lists:sort(fun ?MODULE:pl_cmp/2, T),
        lists:sort(V));
check_([_], []) -> ok;
    % an empty value list always passes
check_([T], V) when is_list(V) ->
    % a single type in a list is applied to all values
    Res = [check_(T, V0) || V0 <- V ],
    Failed = lists:filter(fun(R)-> R =/= ok end, Res),
    if
        Failed == [] -> ok;
        true -> Failed
    end;
check_(T, V) when is_list(T) and is_list(V) ->
    % multiple types in a list are applied one-by-one
    Res = [check_(T0, V0) || {T0,V0} <- lists:zip(T, V) ],
    Failed = lists:filter(fun(R)-> R =/= ok end, Res),
    if
        Failed == [] -> ok;
        true -> Failed
    end;
check_(T, V) when is_tuple(T) and is_tuple(V) ->
    LT = tuple_to_list(T),
    LV = tuple_to_list(V),
    % because we convert tuples to a list and because lists with length(1)
    % are treated differently than different-sized tuples (which never match)
    % we must verify the size here
    if
        length(LT) =/= length(LV) -> {V,T};
        true ->
            case check_(LT, LV) of
                ok -> ok;
                Fail -> list_to_tuple(Fail)
            end
    end;
check_(Exp, Val) when Exp =:= Val -> ok; % type-strict literal match
check_(Exp, Val) -> {Val,Exp}.

check(Exp, Val) ->
    try
        case check_(Exp, Val) of
            ok -> ok;
            Details -> {error, Details}
        end
    catch
        _:_ -> {error, {Val,Exp}}
    end.

% proplist item cmp for sort; ignore potential {optional, ...} enclosure
pl_cmp({optional, X}, Y) -> pl_cmp(X, Y);
pl_cmp(X, {optional, Y}) -> pl_cmp(X, Y);
pl_cmp(X, Y) when X =< Y -> true;
pl_cmp(_, _) -> false.

tests() ->
    {tests,
        [
            {plcmp_0,      (fun()-> lists:sort(fun ?MODULE:pl_cmp/2, [{a,0},{b,1}]) == [{a,0},{b,1}] end)() },
            {plcmp_1,      (fun()-> lists:sort(fun ?MODULE:pl_cmp/2, [{optional,{a,0}},{b,1}]) == [{optional,{a,0}},{b,1}] end)() },
            {plcmp_2,      (fun()-> lists:sort(fun ?MODULE:pl_cmp/2, [{optional,{b,1}},{a,0}]) == [{a,0},{optional,{b,1}}] end)() },
            {atom_pass0,   (fun()-> check(atom,x) == ok end)()                                      },
            {atom_fail0,   (fun()-> check(atom,0) == {error,{0,atom}} end)()                        },
            {bool_pass0,   (fun()-> check(bool,true) == ok end)()                                   },
            {bool_fail0,   (fun()-> check(bool,x) == {error,{x,bool}} end)()                        },
            {int_pass0,    (fun()-> check(int,0) == ok end)()                                       },
            {int_fail0,    (fun()-> check(int,x) == {error,{x,int}} end)()                          },
            {int_pass1,    (fun()-> check(0,0) == ok end)()                                         },
            {list_pass0,   (fun()-> check([],[]) == ok end)()                                       },
            {list_fail0,   (fun()-> check([],{}) == {error,{{},[]}} end)()                          },
            {list_pass1,   (fun()-> check([int],[0]) == ok end)()                                   },
            {list_pass2,   (fun()-> check([int],[0,1,2]) == ok end)()                               },
            {list_pass3,   (fun()-> check([int],[0,1,x]) == {error,[{x,int}]} end)()                },
            {tuple_pass0,  (fun()-> check({},{}) == ok end)()                                       },
            {tuple_pass1,  (fun()-> check({atom},{x}) == ok end)()                                  },
            {tuple_fail1,  (fun()-> check({atom},{0}) == {error,{{0,atom}}} end)()                  },
            {tuple_pass2,  (fun()-> check({atom,atom},{x,y}) == ok end)()                           },
            {tuple_fail2,  (fun()-> check({atom,atom},{0,1}) == {error,{{0,atom},{1,atom}}} end)()  },
            {tuple_fail3,  (fun()-> check({int},{0,1}) == {error,{{0,1},{int}}} end)()              },
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
                        [{<<"a">>,int},{<<"b">>,binary},{<<"c">>,int}]},
                    [{<<"a">>,0},{<<"b">>,<<"foo">>},{<<"c">>,0}]
                ) == ok end)() },

            {pl_real_pass1,
                (fun()-> check(
                    {proplist,
                        [{optional,{<<"b">>,<<"foo">>}}]},
                    [{<<"b">>,<<"foo">>}]
                ) == ok end)() },

            % check that a single optional key is compared properly
            {pl_real_pass2,
                (fun()-> check(
                    {proplist,
                        [{optional,{<<"b">>,binary}}] },
                    [{<<"b">>,<<"foo">>}]
                ) == ok end)() },

            {pl_real_pass3,
                (fun()-> check(
                    {proplist, [{<<"a">>,int},{optional,{<<"b">>,binary}},{<<"c">>,int}]},
                    [{<<"a">>,0},{<<"b">>,<<"foo">>},{<<"c">>,0}]
                ) == ok end)() },

            {pl_real_fail4,
                (fun()-> check(
                    {proplist,[{<<"foo">>,int},{<<"bar">>,binary}]},
                    [{<<"foo">>,5},{<<"bar">>,5}]
                ) == {error, [{{<<"bar">>,5},{<<"bar">>,binary}}]} end)() },

            % missing field
            {pl_real_fail5,
                (fun()-> check(
                    {proplist,[{<<"foo">>,any}]},
                    [{<<"bar">>,5}]
                ) == {error, [{{<<"bar">>,5},{<<"foo">>,any}}]} end)() }

        ]}.

test() ->
    {tests, Results} = tests(),
    Failed = lists:filter(fun({_Name, R})-> R =/= true end, Results),
    if
        Failed == [] -> ok;
        true -> Failed
    end.

main(_Args) ->
    io:format("~s:test(): ~w~n", [?MODULE, test()]).

