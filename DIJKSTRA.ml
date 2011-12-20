
module type DIJKSTRA_SIG =  
    functor(Queue : PRIORITY_QUEUE) ->
    functor(AvlMap : MAP) ->
        sig
            val get_the_shortest_path : 
                AvlMap(IntIntListOrderTuple).map -> AvlMap(IntIntListOrderTuple).first_cord_type -> 
                    AvlMap(IntIntOrderTuple).map * AvlMap(IntIntOrderTuple).map
        end;;

module DIJKSTRA : DIJKSTRA_SIG = 
    functor(Queue : PRIORITY_QUEUE) ->
    functor(AvlMap : MAP) ->
        struct
            module IntIntAvl = Queue(IntIntOrder)
            module IntIntAvlMap = AvlMap(IntIntOrderTuple)
            module Graph = AvlMap(IntIntListOrderTuple)

            let inf = 1000000000;;

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
