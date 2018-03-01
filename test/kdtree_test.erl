-module(kdtree_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    % random list of GPS coordinates
    List = [{{48.55321,-125.02235}, make_ref()},
            {{45.26664,-66.06525}, make_ref()},
            {{51.64982,-121.28594}, make_ref()},
            {{51.6675,-121.29361}, make_ref()},
            {{47.811,-53.97733}, make_ref()},
            {{51.685,-121.29861}, make_ref()},
            {{63.89335,-139.1991}, make_ref()},
            {{51.84983,-121.60273}, make_ref()}],
    % build a kdtree
    Tree = kdtree:from_list(List),
    % Verify kdtree root node is not undefined
    ?assertNotEqual(undefined, Tree),
    % Some Random Coordinate from list
    % RandCoordinate = {51.84983,-121.60273},
    {RandCoordinate, _} = lists:nth(rand:uniform(length(List)), List),
    io:format("RandCoordinate: ~p", [RandCoordinate]),
    % Verify nearest location from given location
    {NearestLoc, Distance} = kdtree:nearest(Tree, RandCoordinate),
    ?assertNotEqual(undefined, NearestLoc),
    % Verify nearby
    Nearby = kdtree:nearby(Tree, RandCoordinate, round(Distance) + 1),
    % nearby list should not be empty
    ?assertNotEqual([], Nearby),
    % there should be only one coordinate in nearby list
    ?assert(length(Nearby) >= 1),
    % check identity
    ?assertEqual(lists:usort(List), lists:usort(kdtree:to_list(Tree))).

value_test() ->
    List = [{{48.55321,-125.02235}, make_ref()},
            {{45.26664,-66.06525}, make_ref()},
            {{51.64982,-121.28594}, make_ref()},
            {{51.6675,-121.29361}, 10},
            {{47.811,-53.97733}, make_ref()},
            {{51.685,-121.29861}, make_ref()},
            {{63.89335,-139.1991}, make_ref()},
            {{51.84983,-121.60273}, make_ref()}],
    % build a kdtree
    Tree = kdtree:from_list(List),
    Value = kdtree:get_value(Tree, {51.6675,-121.29361}),
    io:format("V: ~p~n", [Value]),
    ?assertEqual(10, Value).
