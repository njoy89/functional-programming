
module type SHORTEST_PATH_FINDER =  
    functor(AvlMap : MAP) ->
        sig
            val get_the_shortest_path : 
                AvlMap(IntIntListOrderTuple).map -> AvlMap(IntIntListOrderTuple).first_cord_type -> 
                    AvlMap(IntIntOrderTuple).map * AvlMap(IntIntOrderTuple).map
        end;;

module DIJKSTRA : functor(Queue : PRIORITY_QUEUE) -> SHORTEST_PATH_FINDER = 
    functor(Queue : PRIORITY_QUEUE) ->
    functor(AvlMap : MAP) ->
        struct
            module IntIntAvl = Queue(IntIntOrder)
            module IntIntAvlMap = AvlMap(IntIntOrderTuple)
            module Graph = AvlMap(IntIntListOrderTuple)

            let _prepare_map graph starting_point sp nsp = 
                let graph_dump = Graph.print graph in
                let new_map = 
                    List.fold_left(fun map ((k, v), _, _) -> 
                        IntIntAvlMap.put map (k, sp)
                    ) IntIntAvlMap.get_empty_map graph_dump
                in
                    IntIntAvlMap.put new_map (starting_point, nsp)
            
            let _prepare_dists graph starting_point = 
                _prepare_map graph starting_point inf 0 

            let _prepare_parent graph starting_point = 
                _prepare_map graph starting_point (-1) (-1)

            let _prepare_queue starting_point = 
                IntIntAvl.insert_element IntIntAvl.get_empty_queue (0, starting_point)

            let rec _dijkstra graph queue dists parent =
                if IntIntAvl.is_empty queue then
                    (dists, parent)
                else
                    let (distance, u) = IntIntAvl.get_min_element queue in
                    let queue_without_min = IntIntAvl.remove_element queue (distance, u) in
                    let (new_queue, new_dists, new_parent) = List.fold_left(
                        fun (nqueue, ndist, nparent) (v, cap, cost) ->
                            let act_cost = (IntIntAvlMap.get_keys_value ndist u) + cost in
                            let old_cost = IntIntAvlMap.get_keys_value ndist v in
                            if act_cost < old_cost then
                                let nqueue_without_v = IntIntAvl.remove_element
                                    nqueue (old_cost, v) in
                                let nnqueue = IntIntAvl.insert_element nqueue_without_v (act_cost, v) in
                                let nndist = IntIntAvlMap.put ndist (v, act_cost) in
                                let nnparent = IntIntAvlMap.put nparent (v, u) in
                                (nnqueue, nndist, nnparent)
                            else 
                                (nqueue, ndist, nparent)
                    ) (queue_without_min, dists, parent) (Graph.get_keys_value graph u) 
                    in
                        _dijkstra graph new_queue new_dists new_parent

            let get_the_shortest_path graph starting_point =
                _dijkstra graph 
                    (_prepare_queue starting_point) 
                    (_prepare_dists graph starting_point) 
                    (_prepare_parent graph starting_point)
        end;;

module BELLMAN_FORD : SHORTEST_PATH_FINDER = 
    functor(AvlMap : MAP) ->
        struct
            module IntIntAvlMap = AvlMap(IntIntOrderTuple)
            module Graph = AvlMap(IntIntListOrderTuple)

            let _prepare_map graph starting_point sp nsp = 
                let graph_dump = Graph.print graph in
                let new_map = 
                    List.fold_left(fun map ((k, v), _, _) -> 
                        IntIntAvlMap.put map (k, sp)
                    ) IntIntAvlMap.get_empty_map graph_dump
                in
                    IntIntAvlMap.put new_map (starting_point, nsp)
            
            let _prepare_dists graph starting_point = 
                _prepare_map graph starting_point inf 0 

            let _prepare_parent graph starting_point = 
                _prepare_map graph starting_point (-1) (-1)

            let rec _bellman_ford graph dists parent ith_v number_of_vertices =
                if ith_v == number_of_vertices then
                    (dists, parent)
                else
                    let (new_dists, new_parent) = List.fold_left(
                        fun (dists_u, parent_u) ((u, neighbours), _, _) ->
                            List.fold_left(
                                fun (ndist, nparent) (v, cap, cost) ->
                                    let act_cost = (IntIntAvlMap.get_keys_value ndist u) + cost in
                                    let old_cost = IntIntAvlMap.get_keys_value ndist v in
                                    if act_cost < old_cost then
                                        let nndist = IntIntAvlMap.put ndist (v, act_cost) in
                                        let nnparent = IntIntAvlMap.put nparent (v, u) in
                                        (nndist, nnparent)
                                    else
                                        (ndist, nparent)
                            ) (dists_u, parent_u) neighbours
                    ) (dists, parent) (Graph.print graph)
                    in
                        _bellman_ford graph new_dists new_parent (ith_v+1) number_of_vertices

            let get_the_shortest_path graph starting_point =
                _bellman_ford graph 
                    (_prepare_dists graph starting_point) 
                    (_prepare_parent graph starting_point)
                    1 (Graph.get_size graph)
        end;;
