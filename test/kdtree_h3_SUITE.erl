-module(kdtree_h3_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([
         basic_test/1,
         add_test/1,
         remove_test/1
        ]).

all() -> [
          basic_test,
          add_test,
          remove_test
         ].

basic_test(_Config) ->
    % random list of h3 indices
    {ok, [List0]} = file:consult(filename:join(code:priv_dir(kdtree), "hotspots.txt")),
    List = [{H3, Name} || {H3, Name} <- List0],
    Coordinates = [{h3:to_geo(H3), Name} || {H3, Name} <- List],
    ct:pal("Coordinates: ~p", [Coordinates]),
    % build a kdtree
    Tree = kdtree:from_indices(List),
    ct:pal("Tree: ~p", [Tree]),
    {RandCoordinate, RandName} = lists:nth(rand:uniform(length(Coordinates)), Coordinates),
    ct:pal("RandCoordinate: ~p, RandName: ~p", [RandCoordinate, RandName]),
    % Verify nearest location from given location
    {NearestLoc, Distance} = kdtree:nearest(Tree, RandCoordinate),
    ct:pal("NearestLoc: ~p, Distance: ~p", [NearestLoc, Distance]),
    % Verify nearby
    Nearby = kdtree:nearby(Tree, RandCoordinate, round(Distance) + 1),
    ct:pal("Nearby: ~p", [Nearby]),
    ?assertNotEqual([], Nearby),
    ok.

add_test(_Config) ->
    % random list of h3 indices
    {ok, [List0]} = file:consult(filename:join(code:priv_dir(kdtree), "missing.txt")),
    List = [{H3, Name} || {H3, Name} <- List0],
    Coordinates = [{h3:to_geo(H3), Name} || {H3, Name} <- List],
    ct:pal("Coordinates: ~p", [Coordinates]),
    % build a kdtree
    Tree = kdtree:from_indices(List),
    ct:pal("Tree: ~p", [Tree]),
    OldList = kdtree:to_list(Tree),

    %% create a new node from h3 index
    NewNode = treenode:from_h3(631210968874529791, "mini-currant-lizard"),
    ct:pal("NewNode: ~p", [NewNode]),

    %% add node to tree
    NewTree = kdtree:add(Tree, NewNode),
    ct:pal("NewTree: ~p", [NewTree]),

    ?assertEqual(kdtree:size(Tree), kdtree:size(NewTree) - 1),
    ok.

remove_test(_Config) ->
    % random list of h3 indices
    {ok, [List0]} = file:consult(filename:join(code:priv_dir(kdtree), "hotspots.txt")),
    List = [{H3, Name} || {H3, Name} <- List0],
    Coordinates = [{h3:to_geo(H3), Name} || {H3, Name} <- List],
    ct:pal("Coordinates: ~p", [Coordinates]),
    % build a kdtree
    Tree = kdtree:from_indices(List),
    ct:pal("Tree: ~p", [Tree]),
    {RandCoordinate, RandName} = lists:nth(rand:uniform(length(Coordinates)), Coordinates),
    ct:pal("RandCoordinate: ~p, RandName: ~p", [RandCoordinate, RandName]),

    NodeToRemove = treenode:new(RandCoordinate, RandName),
    NewTree = kdtree:remove(Tree, NodeToRemove),

    ?assertEqual(kdtree:size(Tree), kdtree:size(NewTree) + 1),
    ok.
