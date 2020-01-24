-module(mymath).

-export([factorial/1, newTree/0, insert/2]).

factorialInternal(0, Container) -> Container;

factorialInternal(N, Container) -> factorialInternal(N-1, N*Container).

factorial(N) -> factorialInternal(N, 1).

newTree() -> {btreeNode, nil, nil, nil}.

insert({btreeNode, nil, nil, nil}, NewValue) -> {btreeNode, NewValue, nil, nil};

insert({btreeNode, Value, nil, nil}, NewValue) ->
if Value < NewValue then
	{btreeNode, Value, insert(newTree(), NewValue), nil}
else
	{btreeNode, Value, nil, insert(newTree(), NewValue)}
end.