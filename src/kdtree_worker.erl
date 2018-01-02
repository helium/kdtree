-module(kdtree_worker).

-behavior(gen_server).

-record(node, {
          location,
          left,
          right
         }).

% greater than max distance between two points on earth
-define(MAXDISTANCE, 13000).
% in miles
-define(EARTHRADIUS, 3961).

-export([build/1, nearest/1]).
-export([init/1, start_link/0, handle_cast/2, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

% clienside functions
-spec build(list()) -> #node{}.
build(CoordinateList) ->
    gen_server:call(?MODULE, {build, CoordinateList}).

-spec nearest(tuple()) -> tuple().
nearest(Coordinate) ->
    gen_server:call(?MODULE, {nearest, Coordinate}).

% callback functions
handle_call(Msg, _From, State) ->
    case Msg of
        {build, List} ->
            Tree = build(List, 0),
            {reply, Tree, Tree};
        {nearest, Coordinate} ->
            Nearest = nearest(State, Coordinate),
            {reply, Nearest, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

% private functions
-spec build(list(), non_neg_integer()) -> undefined | #node{}.
build(CoordinateList, _Depth) when length(CoordinateList) == 0 ->
    undefined;
build(CoordinateList, Depth) when length(CoordinateList) == 1 ->
    #node{
       location=hd(CoordinateList),
       left=build([], Depth + 1),
       right=build([], Depth + 1)
      };
build(CoordinateList, Depth) ->
    K = tuple_size(lists:nth(1, CoordinateList)),
    Axis = Depth rem K,
    SortedCoordinateList = case Axis of
                               0 -> lists:keysort(1, CoordinateList);
                               1 -> lists:keysort(2, CoordinateList)
                           end,
    Median = (length(SortedCoordinateList) div 2) + 1,
    #node{
       location= lists:nth(Median, SortedCoordinateList),
       left=build(lists:sublist(SortedCoordinateList, 1, Median - 1), Depth + 1),
       right=build(lists:sublist(SortedCoordinateList, Median + 1, length(SortedCoordinateList)), Depth + 1)
       }.

-spec nearest(#node{} | undefined, tuple()) -> undefined | tuple().
nearest(undefined, _Coordinate) ->
    undefined;
nearest(Node, Coordinate) ->
    nearest(Node, Coordinate, Node#node.location, ?MAXDISTANCE, 0).

-spec nearest(#node{}, tuple(), tuple(), non_neg_integer(), non_neg_integer()) -> undefined | tuple().
nearest(Node, Coordinate, Closest, MinDist, Depth) ->
    Axis = case Depth rem tuple_size(Coordinate) of
               0 -> 1;
               1 -> 2
           end,

    Distance = haversine_distance(Coordinate, Node#node.location),
    {NewClosest, NewMinDist} = case Distance < MinDist andalso Coordinate /= Node#node.location of
                                   true ->
                                       {Node#node.location, Distance};
                                   false ->
                                       {Closest, MinDist}
                               end,

    case {get_dimension(Axis, Coordinate) < get_dimension(Axis, Node#node.location), Node#node.left, Node#node.right} of
        {_, undefined, undefined} ->
            {NewClosest, NewMinDist};
        {true, undefined, _} ->
            {NewClosest, NewMinDist};
        {true, Left, _} ->
            nearest(Left, Coordinate, NewClosest, NewMinDist, Depth + 1);
        {false, _, undefined} ->
            {NewClosest, NewMinDist};
        {false, _, Right} ->
            nearest(Right, Coordinate, NewClosest, NewMinDist, Depth + 1)
    end.

-spec get_dimension(1 | 2, tuple()) -> float().
get_dimension(Axis, Coordinate) ->
    lists:nth(Axis, tuple_to_list(Coordinate)).

-spec haversine_distance(tuple(), tuple()) -> float().
haversine_distance({Lat1, Long1}, {Lat2, Long2}) ->
    V = math:pi()/180,
    DeltaLat = (Lat2 - Lat1) * V,
    DeltaLong = (Long2 - Long1) * V,
    A = math:pow(math:sin(DeltaLat/2), 2) + math:cos(Lat1 * V) * math:cos(Lat2 * V) * math:pow(math:sin(DeltaLong/2), 2),
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1-A)),
    ?EARTHRADIUS * C.
