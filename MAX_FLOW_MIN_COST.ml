
module type MAX_FLOW_MIN_COST_SIG = 
    functor(AvlMap : MAP) ->
    functor(Dijkstra : SHORTEST_PATH_FINDER) ->
    functor(BellmanFord : SHORTEST_PATH_FINDER) ->
        sig
            val get_max_flow_min_cost : 
                AvlMap(IntIntListOrderTuple).map ->
                AvlMap(IntIntListOrderTuple).first_cord_type -> 
                AvlMap(IntIntListOrderTuple).first_cord_type -> 
                (int * int * AvlMap(IntIntIntResNetOrderTuple).map)
                (* result: (capacity * cost * resuidal network) *)
        end;;

module MAX_FLOW_MIN_COST =
    functor(AvlMap : MAP) ->
    functor(Dijkstra : SHORTEST_PATH_FINDER) ->
    functor(BellmanFord : SHORTEST_PATH_FINDER) ->
        struct
            module IntIntAvlMap = AvlMap(IntIntOrderTuple)
            module Graph = AvlMap(IntIntListOrderTuple)
            module ResuidalNetwork = AvlMap(IntIntIntResNetOrderTuple);;

            module DijkstraAlgorithm = Dijkstra(AvlMap);;
            module BellmanFordAlgorithm = BellmanFord(AvlMap);;
            
            (* for each edge set flow that flow through this edge equaled to 0 *)
            let _init_flow flow graph = 
                let graph_dump = Graph.print graph in
                    List.fold_left(
                        fun nflow ((u, value_list), _, _) ->
                            List.fold_left(
                                fun nnflow (b, cap, cost) ->
                                    ResuidalNetwork.put nnflow ((u, b), 0)
                            ) nflow value_list
                    ) flow graph_dump 

            (* it actualizes potential values taking into account distances a_pi computed in
             * Dijkstra algorithm *)
            let _actualize_potentials potentials dists =
                List.fold_left(
                    fun npotentials ((k, v), _, _) ->
                        IntIntAvlMap.put npotentials (k, v + IntIntAvlMap.get_keys_value dists k)
                ) IntIntAvlMap.get_empty_map (IntIntAvlMap.print potentials)

            (* it actualizes cost of edges taking into accound a new potentials value.
               result: new graph *)
            let _actualize_graph graph potentials = 
                let graph_dump = Graph.print graph in
                    List.fold_left(
                        fun ngraph ((u, value_list), _, _) ->
                            let nngraph = 
                                if Graph.is_key_a_member ngraph u then
                                    ngraph
                                else
                                    Graph.put ngraph (u, [])
                            in
                            List.fold_left(
                                fun nngraph (v, cap, cost) ->
                                    let add_num = (IntIntAvlMap.get_keys_value potentials u) 
                                        - (IntIntAvlMap.get_keys_value potentials v)
                                    in
                                        Graph.put nngraph (u, (v, cap, cost + add_num)::(Graph.get_keys_value nngraph u))
                            ) nngraph value_list
                    ) Graph.get_empty_map graph_dump

            (* before the extending f over found path, find the minimal capacity of edge belonging to this path *)
            let _get_min_cap_over_path graph parent =
                 let graph_dump = Graph.print graph in
                    List.fold_left(
                        fun mcap ((u, neight_u), _, _) ->
                            List.fold_left(
                                fun act_min_cap (v, cap, cost) ->
                                    let (pu, pv) = ((IntIntAvlMap.get_keys_value parent v), v) in
                                        if u == pu && v == pv then
                                            min act_min_cap cap
                                        else
                                            act_min_cap
                            ) mcap neight_u
                    ) inf graph_dump

            (* it extends the flow over found path *)
            let _extend_flow graph flow parent dists add_flow =
                let graph_dump = Graph.print graph in
                    List.fold_left(
                        fun aflow ((u, neight_u), _, _) ->
                            List.fold_left(
                                fun act_flow (v, cap, cost) ->
                                    let (pu, pv) = ((IntIntAvlMap.get_keys_value parent v), v) in
                                    if u == pu && v == pv then
                                        ResuidalNetwork.put act_flow ((u, v), (ResuidalNetwork.get_keys_value flow (u, v)) + add_flow)
                                    else
                                        ResuidalNetwork.put act_flow ((u, v), (ResuidalNetwork.get_keys_value flow (u, v)))
                            ) aflow neight_u
                    ) ResuidalNetwork.get_empty_map graph_dump

            let _get_flow_value graph flow s = 
                let neight_s = Graph.get_keys_value graph s in
                    List.fold_left (
                        fun act_flow_value (v, cap, cost) ->
                            act_flow_value + (ResuidalNetwork.get_keys_value flow (s, v))
                    ) 0 neight_s

            let _get_flow_cost graph flow =
                let graph_dump = Graph.print graph in
                    List.fold_left(
                        fun act_cost ((u, neight_u), _, _) ->
                            List.fold_left(
                                fun act_act_cost (v, cap, cost) ->
                                    act_act_cost + cost * (ResuidalNetwork.get_keys_value flow (u, v))
                            ) act_cost neight_u
                    ) 0 graph_dump

            let _get_tmp_graph_without_sated_edges graph flow =
                let graph_dump = Graph.print graph in
                    List.fold_left(
                        fun ngraph ((u, value_list), _, _) ->
                            let nngraph = 
                                if Graph.is_key_a_member ngraph u then
                                    ngraph
                                else
                                    Graph.put ngraph (u, [])
                            in
                            List.fold_left(
                                fun nngraph (v, cap, cost) ->
                                    if cap < ResuidalNetwork.get_keys_value flow (u, v) then
                                        nngraph
                                    else
                                        if not (Graph.is_key_a_member nngraph u) then
                                            Graph.put nngraph (u, [(v, cap, cost)])
                                        else
                                            Graph.put nngraph (u, (v, cap, cost)::(Graph.get_keys_value nngraph u))
                            ) nngraph value_list
                    ) Graph.get_empty_map graph_dump

            (* it is called whenever the extending path exists *)
            let rec _main_loop graph flow potentials s t = 
                let new_graph = _actualize_graph graph potentials in
                let tmp_graph = _get_tmp_graph_without_sated_edges graph flow in
                let (dists, parent) = DijkstraAlgorithm.get_the_shortest_path tmp_graph s in
                    if (IntIntAvlMap.get_keys_value dists t) == inf then
                        let max_flow_value = _get_flow_value new_graph flow s in
                        let max_flow_cost = _get_flow_cost new_graph flow in
                            (max_flow_value, max_flow_cost, flow)
                    else
                        let min_cap_over_path = _get_min_cap_over_path graph parent in
                        let new_flow = _extend_flow graph flow parent dists min_cap_over_path in
                        let new_potentials = _actualize_potentials potentials dists in
                            _main_loop new_graph new_flow new_potentials s t

            let get_max_flow_min_cost graph s t = 
                let flow = _init_flow ResuidalNetwork.get_empty_map graph in
                let (potentials, parent) = BellmanFordAlgorithm.get_the_shortest_path graph s in
                    _main_loop graph flow potentials s t
        end;;
