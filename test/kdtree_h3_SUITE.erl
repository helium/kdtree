-module(kdtree_h3_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([basic_test/1]).

all() -> [basic_test].

init_per_testcase(_TestCase, Config) ->
    % random list of h3 indices
    {ok, [List0]} = file:consult(filename:join(code:priv_dir(kdtree), "hotspots.txt")),
    List = [{H3, Name} || {H3, Name} <- List0],
    Coordinates = [{h3:to_geo(H3), Name} || {H3, Name} <- List],
    % build a kdtree
    Tree = kdtree:from_indices(List),
    [{tree, Tree}, {coordinates, Coordinates} | Config].

basic_test(Config) ->
    Tree = proplists:get_value(tree, Config),
    ct:pal("Tree: ~p", [Tree]),
    Coordinates = proplists:get_value(coordinates, Config),
    ct:pal("Coordinates: ~p", [Coordinates]),
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

end_per_testcase(_, _Config) ->
    ok.
