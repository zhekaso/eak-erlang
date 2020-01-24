-module(bTree).

-export([newTree/1, insert/2, find/2, delete/2, insertList/2]).

-define(EMPTY_NODE, {bTreeNode, null}).

%Root: {bTreeRoot, RootNode :: bTreeNode, CompareFunction :: fun(Element, Element)}
%Node: {bTreeNode, null | {Element :: userType, LeftNode :: bTreeNode, RightNode :: bTreeNode}}


newTree(CompareFunction) when is_function(CompareFunction, 2) -> {bTreeRoot, ?EMPTY_NODE, CompareFunction}.

insertList(Tree, List) -> insertInternal(Tree, List).

insertInternal(Tree, []) -> Tree;
insertInternal(Tree, [H|Tail]) -> insertInternal(insert(Tree, H), Tail).

insert({bTreeRoot, RootNode, CompareFunction}, Element) ->
	{bTreeRoot, insert(RootNode, Element, CompareFunction), CompareFunction}.

insert(?EMPTY_NODE, Element, _) -> {bTreeNode, {Element, ?EMPTY_NODE, ?EMPTY_NODE}};

insert({bTreeNode, {Element, LeftNode, RightNode}}, NewElement, Cmp) ->
	case Cmp(NewElement, Element) of
		true -> {bTreeNode, {Element, insert(LeftNode, NewElement, Cmp), RightNode}};
		false ->
			case Cmp(Element, NewElement) of
				true -> {bTreeNode, {Element, LeftNode, insert(RightNode, NewElement, Cmp)}};
				false -> {bTreeNode, {NewElement, LeftNode, RightNode}}
			end
	end.

find({bTreeRoot, RootNode, CompareFunction}, Element) ->
	find(RootNode, Element, CompareFunction).

find(?EMPTY_NODE, _, _) -> {err, null};

find({bTreeNode, {Element, LeftNode, RightNode}}, SearchElem, Cmp) ->
	case Cmp(SearchElem, Element) of
		true -> find(LeftNode, SearchElem, Cmp);
		false ->
			case Cmp(Element, SearchElem) of
				true -> find(RightNode, SearchElem, Cmp);
				false -> {ok, Element}
			end
	end.

delete({bTreeRoot, RootNode, CompareFunction}, Element) ->
	{bTreeRoot, delete(RootNode, Element, CompareFunction), CompareFunction}.

delete(?EMPTY_NODE, _, _) -> ?EMPTY_NODE;

delete({bTreeNode, {Element, ?EMPTY_NODE, ?EMPTY_NODE}}, DeleteElem, Cmp) ->
	case {Cmp(Element, DeleteElem), Cmp(DeleteElem, Element)} of
		{false, false} -> ?EMPTY_NODE;
		{_, _} -> {bTreeNode, {Element, ?EMPTY_NODE, ?EMPTY_NODE}}
	end;

delete({bTreeNode, {Element, LeftNode, RightNode}}, DeleteElem, Cmp) ->
	case Cmp(DeleteElem, Element) of
		true -> {bTreeNode, {Element, delete(LeftNode, DeleteElem, Cmp), RightNode}};
		false ->
			case Cmp(Element, DeleteElem) of
				true -> {bTreeNode, {Element, LeftNode, delete(RightNode, DeleteElem, Cmp)}};
				false -> LeftNode,RightNode
			end
	end.

