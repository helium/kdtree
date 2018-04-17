kdtree
=====

Erlang implementation of kdtree

Implementation:
-----
* Uses [haversine distance](https://en.wikipedia.org/wiki/Haversine_formula) to approximate the distance between two GPS coordinates.
* Building a tree from a list of coordinates takes O(nlog(n)) time complexity.
* I've added a placeholder on the node which allows you to put in any value/data. I use it to add PIDs as an example.
* Search takes O(log(n)) time complexity.

Build
-----

    $ make

Test
-----

    $ make test

Typecheck
-----

    $ make typecheck

#### Example:
```Erlang
1> List = [{{48.55321,-125.02235}, make_ref()},
1> {{45.26664,-66.06525}, make_ref()},
1> {{51.64982,-121.28594}, make_ref()},
1> {{51.6675,-121.29361}, make_ref()},
1> {{47.811,-53.97733}, make_ref()},
1> {{51.685,-121.29861}, make_ref()},
1> {{63.89335,-139.1991}, make_ref()},
1> {{51.84983,-121.60273}, make_ref()}].
[{{48.55321,-125.02235},
  #Ref<0.3804993279.2739142657.109428>},
 {{45.26664,-66.06525},#Ref<0.3804993279.2739142657.109429>},
 {{51.64982,-121.28594},#Ref<0.3804993279.2739142657.109430>},
 {{51.6675,-121.29361},#Ref<0.3804993279.2739142657.109431>},
 {{47.811,-53.97733},#Ref<0.3804993279.2739142657.109432>},
 {{51.685,-121.29861},#Ref<0.3804993279.2739142657.109433>},
 {{63.89335,-139.1991},#Ref<0.3804993279.2739142657.109434>},
 {{51.84983,-121.60273},
  #Ref<0.3804993279.2739142657.109435>}]
2> Tree = kdtree:from_list(List).
{node,{51.6675,-121.29361},
      #Ref<0.3804993279.2739142657.109431>,
      {node,{51.64982,-121.28594},
            #Ref<0.3804993279.2739142657.109430>,
            {node,{48.55321,-125.02235},
                  #Ref<0.3804993279.2739142657.109428>,
                  {node,{45.26664,-66.06525},
                        #Ref<0.3804993279.2739142657.109429>,undefined,undefined},
                  undefined},
            {node,{47.811,-53.97733},
                  #Ref<0.3804993279.2739142657.109432>,undefined,undefined}},
      {node,{63.89335,-139.1991},
            #Ref<0.3804993279.2739142657.109434>,
            {node,{51.685,-121.29861},
                  #Ref<0.3804993279.2739142657.109433>,undefined,undefined},
            {node,{51.84983,-121.60273},
                  #Ref<0.3804993279.2739142657.109435>,undefined,undefined}}}
3> kdtree:nearest(Tree, {51.685,-121.29861}).
{{{51.6675,-121.29361},#Ref<0.3804993279.2739142657.109431>},
 1.2286600794979432}
4> kdtree:nearby(Tree, {51.685,-121.29861}, 10).
[{{51.64982,-121.28594},
  #Ref<0.3804993279.2739142657.109430>},
 {{51.6675,-121.29361},#Ref<0.3804993279.2739142657.109431>}]
```

References
-----
[k-d tree](https://en.wikipedia.org/wiki/K-d_tree)
