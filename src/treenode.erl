-module(treenode).

-record(treenode, {
          coordinate :: coordinate(),
          value :: any(),
          left :: #treenode{} | undefined,
          right :: #treenode{} | undefined
         }).

-export([
         new/2,
         new/4,
         coordinate/1,
         value/1,
         left/1,
         right/1,
         from_h3/2
        ]).

-export_type([coordinate/0, treenode/0]).

-type coordinate() :: {float(), float()}.
-type treenode() :: #treenode{}.

-spec new(coordinate(), any()) -> treenode().
new(Coordinate, Value) ->
    #treenode{
       coordinate=Coordinate,
       value=Value,
       left=undefined,
       right=undefined
      }.

-spec new(coordinate(), any(), treenode(), treenode()) -> treenode().
new(Coordinate, Value, Left, Right) ->
    #treenode{
       coordinate=Coordinate,
       value=Value,
       left=Left,
       right=Right
      }.

-spec from_h3(h3:index(), any()) -> treenode().
from_h3(H3Index, Value) ->
    #treenode{
       coordinate=h3:to_geo(H3Index),
       value=Value,
       left=undefined,
       right=undefined
      }.

-spec coordinate(treenode()) -> coordinate().
coordinate(TreeNode) ->
    TreeNode#treenode.coordinate.

-spec value(treenode()) -> any().
value(TreeNode) ->
    TreeNode#treenode.value.

-spec left(treenode()) -> undefined | treenode().
left(TreeNode) ->
    TreeNode#treenode.left.

-spec right(treenode()) -> undefined | treenode().
right(TreeNode) ->
    TreeNode#treenode.right.
