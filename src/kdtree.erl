-module(kdtree).

% greater than max distance between two points on earth
-define(MAXDISTANCE, 13000).
% in miles
-define(EARTHRADIUS, 3961).

-export([
         from_list/1,
         from_indices/1,
         haversine_distance/2,
         nearby/3,
         nearest/2,
         to_list/1,
         get_value/2,
         add/2,
         remove/2
        ]).

-spec from_indices([{h3:h3_index(), any()}, ...]) -> treenode:treenode().
from_indices(Indices) when length(Indices) > 0->
    CoordinateList = [{h3:to_geo(I), V} || {I, V} <- Indices],
    from_list(CoordinateList).

-spec from_list([{treenode:coordinate(), any()}, ...]) -> treenode:treenode().
from_list(CoordinateList) when length(CoordinateList) > 0 ->
    from_list(CoordinateList, 0).

-spec from_list([{treenode:coordinate(), any()}], non_neg_integer()) -> treenode:treenode().
from_list([], _Depth) ->
    undefined;
from_list([{Coordinate, Value}], _Depth) ->
    treenode:new(Coordinate, Value);
from_list(CoordinateList, Depth) ->
    K = tuple_size(lists:nth(1, CoordinateList)),
    Axis = Depth rem K,
    SortedCoordinateList = case Axis of
                               0 -> lists:keysort(1, CoordinateList);
                               1 -> lists:keysort(2, CoordinateList)
                           end,
    Median = (length(SortedCoordinateList) div 2) + 1,
    {Coordinate, Value} = lists:nth(Median, SortedCoordinateList),
    treenode:new(Coordinate,
                 Value,
                 from_list(lists:sublist(SortedCoordinateList, 1, Median - 1), Depth + 1),
                 from_list(lists:sublist(SortedCoordinateList, Median + 1, length(SortedCoordinateList)), Depth + 1)).

-spec nearest(treenode:treenode(), treenode:coordinate()) -> {treenode:coordinate(), float()}.
nearest(TreeNode, Coordinate) ->
    nearest(TreeNode, Coordinate, treenode:coordinate(TreeNode), ?MAXDISTANCE, 0).

-spec nearest(treenode:treenode(), tuple(), tuple(), non_neg_integer(), non_neg_integer()) -> tuple().
nearest(TreeNode, Coordinate, Closest, MinDist, Depth) ->
    Axis = get_axis(Depth, Coordinate),
    Coodinate  = treenode:coordinate(TreeNode),
    Value = treenode:value(TreeNode),
    Distance = haversine_distance(Coordinate, Coodinate),
    {NewClosest, NewMinDist} = case Distance < MinDist andalso Coordinate /= Coodinate of
                                   true ->
                                       {{Coodinate, Value}, Distance};
                                   false ->
                                       {Closest, MinDist}
                               end,
    TreeNodeDim = get_dimension(Axis, Coodinate),
    PointDim = get_dimension(Axis, Coordinate),
    case {PointDim > TreeNodeDim, treenode:left(TreeNode), treenode:right(TreeNode)} of
        {_, undefined, undefined} ->
            {NewClosest, NewMinDist};
        {true, undefined, Right} ->
            nearest(Right, Coordinate, NewClosest, NewMinDist, Depth + 1);
        {true, Left, Right} ->
            {NewerClosest, NewerMinDist} = nearest(Left, Coordinate, NewClosest, NewMinDist, Depth + 1),
            case (PointDim + MinDist) >= TreeNodeDim andalso Right /= undefined of
                true -> nearest(Right, Coordinate, NewerClosest, NewerMinDist, Depth + 1);
                false -> {NewerClosest, NewerMinDist}
            end;
        {false, Left, undefined} ->
            nearest(Left, Coordinate, NewClosest, NewMinDist, Depth + 1);
        {false, Left, Right} ->
            {NewerClosest, NewerMinDist} = nearest(Right, Coordinate, NewClosest, NewMinDist, Depth + 1),
            case (PointDim - MinDist) =< TreeNodeDim andalso Left /= undefined of
                true -> nearest(Left, Coordinate, NewClosest, NewMinDist, Depth + 1);
                false -> {NewerClosest, NewerMinDist}
            end
    end.

-spec nearby(treenode:treenode(), treenode:coordinate(), pos_integer()) -> list().
nearby(TreeNode, Coordinate, Range) ->
    nearby(TreeNode, Coordinate, treenode:coordinate(TreeNode), Range, 0, []).

-spec nearby(treenode:treenode() | undefined, treenode:coordinate(), treenode:coordinate(), pos_integer(), non_neg_integer(), list()) -> list().
nearby(undefined, _Coordinate, _NearbyCoordinate, _Range, _Depth, List) ->
    List;
nearby(TreeNode, Coordinate, NearbyCoordinate, Range, Depth, List) ->
    Coodinate = treenode:coordinate(TreeNode),
    Value = treenode:value(TreeNode),
    Distance = haversine_distance(Coordinate, Coodinate),
    NewList = case Distance < Range andalso Coordinate /= Coodinate of
                  true ->
                      [{Coodinate, Value} | List];
                  false ->
                      List
              end,

    nearby(treenode:left(TreeNode), Coordinate, NearbyCoordinate, Range, Depth + 1, NewList) ++
    (nearby(treenode:right(TreeNode), Coordinate, NearbyCoordinate, Range, Depth + 1, NewList) -- NewList).

-spec get_value(treenode:treenode() | undefined, treenode:coordinate()) -> undefined | any().
get_value(undefined, _Coordinate) ->
    undefined;
get_value(TreeNode, Coordinate) ->
    case Coordinate == treenode:coordinate(TreeNode) of
        true ->
            treenode:value(TreeNode);
        false ->
            case get_value(treenode:left(TreeNode), Coordinate) of
                undefined -> get_value(treenode:right(TreeNode), Coordinate);
                Value -> Value
            end
    end.

-spec get_dimension(1 | 2, treenode:coordinate()) -> float().
get_dimension(Axis, Coordinate) ->
    lists:nth(Axis, tuple_to_list(Coordinate)).

-spec haversine_distance(treenode:coordinate(), treenode:coordinate()) -> float().
haversine_distance({Lat1, Long1}, {Lat2, Long2}) ->
    V = math:pi()/180,
    DeltaLat = (Lat2 - Lat1) * V,
    DeltaLong = (Long2 - Long1) * V,
    A = math:pow(math:sin(DeltaLat/2), 2) + math:cos(Lat1 * V) * math:cos(Lat2 * V) * math:pow(math:sin(DeltaLong/2), 2),
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1-A)),
    ?EARTHRADIUS * C.

-spec get_axis(non_neg_integer(), treenode:coordinate()) -> 1 | 2.
get_axis(Depth, Coordinate) ->
    case Depth rem tuple_size(Coordinate) of
        0 -> 1;
        1 -> 2
    end.

-spec to_list(treenode:treenode()) -> [{treenode:coordinate(), any()}, ...].
to_list(TreeNode) ->
    Coodinate = treenode:coordinate(TreeNode),
    Value = treenode:value(TreeNode),
    [{Coodinate, Value} | nearby(TreeNode, Coodinate, ?MAXDISTANCE)].

-spec add(treenode:treenode(), treenode:treenode()) -> treenode:treenode().
add(Tree, NodeToAdd) ->
    List = to_list(Tree),
    from_list([{treenode:coordinate(NodeToAdd), treenode:value(NodeToAdd)} | List]).

-spec remove(treenode:treenode(), treenode:treenode()) -> treenode:treenode().
remove(Tree, NodeToRemove) ->
    OldList = to_list(Tree),
    NewList = proplists:delete(treenode:coordinate(NodeToRemove), OldList),
    from_list(NewList).
