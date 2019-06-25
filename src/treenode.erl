-module(treenode).

-record(treenode, {
          location :: coordinate(),
          value :: any(),
          left :: #treenode{} | undefined,
          right :: #treenode{} | undefined
         }).

-export([
         new/2,
         new/4,
         location/1,
         value/1,
         left/1,
         right/1
        ]).

-export_type([coordinate/0, treenode/0]).

-type coordinate() :: {float(), float()}.
-type treenode() :: #treenode{}.

-spec new(coordinate(), any()) -> treenode().
new(Coordinate, Value) ->
    #treenode{
       location=Coordinate,
       value=Value,
       left=undefined,
       right=undefined
      }.

-spec new(coordinate(), any(), treenode(), treenode()) -> treenode().
new(Coordinate, Value, Left, Right) ->
    #treenode{
       location=Coordinate,
       value=Value,
       left=Left,
       right=Right
      }.

-spec location(treenode()) -> coordinate().
location(TreeNode) ->
    TreeNode#treenode.location.

-spec value(treenode()) -> any().
value(TreeNode) ->
    TreeNode#treenode.value.

-spec left(treenode()) -> undefined | treenode().
left(TreeNode) ->
    TreeNode#treenode.left.

-spec right(treenode()) -> undefined | treenode().
right(TreeNode) ->
    TreeNode#treenode.right.
