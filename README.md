# respec

Hierachical @spec for your Erlang data.
Broadly applicable; built to verify incoming messages from untrusted source.

Reduces any matching data structure to 'ok', or the elements that do not match
the specification and the unmatched spec type, making it easy to return
a sensible and specific error message.

## Example

```erlang
1> respec:check(atom, x).
ok
2> respec:check(atom, 5).
{5,atom}
3> respec:check([atom], [x,y,z]).
ok
4> respec:check([atom], [x,y,5]).
[{5,atom}]
5> respec:check([int], [x,y,5]). 
[{x,int},{y,int}]
6> respec:check([atom,atom,int], [x,y,5]).
ok
```

It also handles proplists and allows for optional items.
It returns the full {key,val} pair and spec on any error.

```erlang
7> respec:check({proplist,[{<<"foo">>,int},{optional,{<<"bar">>,binary}}]}, [{<<"foo">>,5}]).  
ok
8> respec:check({proplist,[{<<"foo">>,int},{<<"bar">>,binary}]}, [{<<"foo">>,5},{<<"bar">>,5}]).
{error,[{{<<"bar">>,5},{<<"bar">>,binary}}]}
```

### References
1. Lindahl and Sagonas, "EEP 8: Types and function specifications" http://www.erlang.org/eeps/eep-0008.html 2 Dec 2007

