### [k-d tree](https://en.wikipedia.org/wiki/K-d_tree) implementation in Erlang

#### Motivation:
* This is the first application I've tried to build in Erlang.
* I couldn't really find any implementation in pure Erlang of a simple k-d tree for determining nearby GPS coordinates.
* This implementation is correct as far as I can tell, mistakes/suggestions are very welcome.

#### Implementation Details:
* Uses [haversine distance](https://en.wikipedia.org/wiki/Haversine_formula) to approximate the distance between two GPS coordinates.
* Building a tree from a list of coordinates takes O(nlog(n)) time complexity.
* Search takes O(log(n)) time complexity.

#### Example:
```Erlang
1> List = [{48.55321,-125.02235}, {45.26664,-66.06525}, {51.64982,-121.28594}, {51.6675,-121.29361}, {47.811,-53.97733}, {51.685,-121.29861}].
[{48.55321,-125.02235},
 {45.26664,-66.06525},
 {51.64982,-121.28594},
 {51.6675,-121.29361},
 {47.811,-53.97733},
 {51.685,-121.29861}]
2> kdtree_app:start().
{ok,<0.72.0>}
3> kdtree_worker:build(List).
{node,{51.64982,-121.28594},
      {node,{45.26664,-66.06525},
            {node,{48.55321,-125.02235},undefined,undefined},
            {node,{47.811,-53.97733},undefined,undefined}},
      {node,{51.6675,-121.29361},
            {node,{51.685,-121.29861},undefined,undefined},
            undefined}}
4> kdtree_worker:nearest({47.811,-53.0023}).
{{47.811,-53.97733},45.2682807129988}
5> 
```
