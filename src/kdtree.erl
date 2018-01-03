-module(kdtree).

-type coordinate() :: {float(), float()}.

-record(node, {
          location :: coordinate(),
          value :: any(),
          left :: #node{} | undefined,
          right :: #node{} | undefined
         }).

% greater than max distance between two points on earth
-define(MAXDISTANCE, 13000).
% in miles
-define(EARTHRADIUS, 3961).

-export([from_list/1, haversine_distance/2, nearby/3, nearest/2, to_list/1]).

-spec from_list([{coordinate(), any()}, ...]) -> #node{}.
from_list(CoordinateList) when length(CoordinateList) > 0 ->
    from_list(CoordinateList, 0).

-spec from_list([{coordinate(), any()}], non_neg_integer()) -> #node{}.
from_list([], _Depth) ->
    undefined;
from_list([{Coordinate, Value}], _Depth) ->
    #node{
       location=Coordinate,
       value=Value,
       left=undefined,
       right=undefined
      };
from_list(CoordinateList, Depth) ->
    K = tuple_size(lists:nth(1, CoordinateList)),
    Axis = Depth rem K,
    SortedCoordinateList = case Axis of
                               0 -> lists:keysort(1, CoordinateList);
                               1 -> lists:keysort(2, CoordinateList)
                           end,
    Median = (length(SortedCoordinateList) div 2) + 1,
    {Coordinate, Value} = lists:nth(Median, SortedCoordinateList),
    #node{
       location=Coordinate,
       value=Value,
       left=from_list(lists:sublist(SortedCoordinateList, 1, Median - 1), Depth + 1),
       right=from_list(lists:sublist(SortedCoordinateList, Median + 1, length(SortedCoordinateList)), Depth + 1)
       }.

-spec nearest(#node{}, coordinate()) -> {coordinate(), float()}.
nearest(Node, Coordinate) ->
    nearest(Node, Coordinate, Node#node.location, ?MAXDISTANCE, 0).

-spec nearest(#node{}, tuple(), tuple(), non_neg_integer(), non_neg_integer()) -> tuple().
nearest(Node, Coordinate, Closest, MinDist, Depth) ->
    Axis = get_axis(Depth, Coordinate),
    Distance = haversine_distance(Coordinate, Node#node.location),
    {NewClosest, NewMinDist} = case Distance < MinDist andalso Coordinate /= Node#node.location of
                                   true ->
                                       {{Node#node.location, Node#node.value}, Distance};
                                   false ->
                                       {Closest, MinDist}
                               end,
    NodeDim = get_dimension(Axis, Node#node.location),
    PointDim = get_dimension(Axis, Coordinate),
    case {PointDim > NodeDim, Node#node.left, Node#node.right} of
        {_, undefined, undefined} ->
            {NewClosest, NewMinDist};
        {true, undefined, Right} ->
            nearest(Right, Coordinate, NewClosest, NewMinDist, Depth + 1);
        {true, Left, Right} ->
            {NewerClosest, NewerMinDist} = nearest(Left, Coordinate, NewClosest, NewMinDist, Depth + 1),
            case (PointDim + MinDist) >= NodeDim andalso Right /= undefined of
                true -> nearest(Right, Coordinate, NewerClosest, NewerMinDist, Depth + 1);
                false -> {NewerClosest, NewerMinDist}
            end;
        {false, Left, undefined} ->
            nearest(Left, Coordinate, NewClosest, NewMinDist, Depth + 1);
        {false, Left, Right} ->
            {NewerClosest, NewerMinDist} = nearest(Right, Coordinate, NewClosest, NewMinDist, Depth + 1),
            case (PointDim - MinDist) =< NodeDim andalso Left /= undefined of
                true -> nearest(Left, Coordinate, NewClosest, NewMinDist, Depth + 1);
                false -> {NewerClosest, NewerMinDist}
            end
    end.

-spec nearby(#node{}, coordinate(), pos_integer()) -> list().
nearby(Node, Coordinate, Range) ->
    nearby(Node, Coordinate, Node#node.location, Range, 0, []).

-spec nearby(#node{} | undefined, coordinate(), coordinate(), pos_integer(), non_neg_integer(), list()) -> list().
nearby(undefined, _Coordinate, _NearbyCoordinate, _Range, _Depth, List) ->
    List;
nearby(Node, Coordinate, NearbyCoordinate, Range, Depth, List) ->
    Distance = haversine_distance(Coordinate, Node#node.location),
    NewList = case Distance < Range andalso Coordinate /= Node#node.location of
                  true ->
                      [{Node#node.location, Node#node.value} | List];
                  false ->
                      List
              end,

    nearby(Node#node.left, Coordinate, NearbyCoordinate, Range, Depth + 1, NewList) ++
        (nearby(Node#node.right, Coordinate, NearbyCoordinate, Range, Depth + 1, NewList) -- NewList).

-spec get_dimension(1 | 2, coordinate()) -> float().
get_dimension(Axis, Coordinate) ->
    lists:nth(Axis, tuple_to_list(Coordinate)).

-spec haversine_distance(coordinate(), coordinate()) -> float().
haversine_distance({Lat1, Long1}, {Lat2, Long2}) ->
    V = math:pi()/180,
    DeltaLat = (Lat2 - Lat1) * V,
    DeltaLong = (Long2 - Long1) * V,
    A = math:pow(math:sin(DeltaLat/2), 2) + math:cos(Lat1 * V) * math:cos(Lat2 * V) * math:pow(math:sin(DeltaLong/2), 2),
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1-A)),
    ?EARTHRADIUS * C.

-spec get_axis(non_neg_integer(), coordinate()) -> 1 | 2.
get_axis(Depth, Coordinate) ->
    case Depth rem tuple_size(Coordinate) of
        0 -> 1;
        1 -> 2
    end.

-spec to_list(#node{}) -> [{coordinate(), any()}, ...].
to_list(Node) ->
    [{Node#node.location, Node#node.value} | nearby(Node, Node#node.location, ?MAXDISTANCE)].
