
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
 
            let _get_int_value avl_map u =
                IntIntAvlMap.get_keys_value avl_map u

            let _get_flow flow (u, v) =
                ResuidalNetwork.get_keys_value flow (u, v)


            (* for each edge set flow that flow through this edge equaled to 0 *)
            let _get_init_flow graph = 
                List.fold_left(
                    fun nflow ((u, neight_u), _, _) ->
                        List.fold_left(
                            fun nnflow (v, cap, cost) ->
                                let nnnflow = ResuidalNetwork.put nnflow ((u, v), 0) in
                                    ResuidalNetwork.put nnnflow ((v, u), 0)
                        ) nflow neight_u
                ) ResuidalNetwork.get_empty_map (Graph.print graph)

            let _make_res_network graph = 
                List.fold_left(
                    fun nres_net ((u, neight_u), _, _) ->
                        let nnres_net = 
                            if Graph.is_key_a_member nres_net u then
                                nres_net
                            else
                                Graph.put nres_net (u, [])
                        in
                            List.fold_left(
                                fun nnnres_net (v, cap, cost) ->
                                    Graph.put nnnres_net (
                                        u, 
                                        (v, cap, cost)::(Graph.get_keys_value nnnres_net u)
                                    )
                        ) nnres_net neight_u
                ) Graph.get_empty_map (Graph.print graph)

            (* it actualizes cost of edges in resuidal network taking into accound a new potentials value.
               result: new res_network *)
            let _update_costs_in_resuidal_network res_network potentials =
                List.fold_left(
                    fun nres_net ((u, neight_u), _, _) ->
                        let nnres_net = 
                            if Graph.is_key_a_member nres_net u then
                                nres_net
                            else
                                Graph.put nres_net (u, [])
                        in
                            List.fold_left(
                                fun nnnres_net (v, cap, cost) ->
                                    let add_num = (_get_int_value potentials u) - (_get_int_value potentials v)
                                    in
                                        Graph.put nnnres_net (
                                            u, 
                                            (v, cap, cost + add_num)::(Graph.get_keys_value nnnres_net u)
                                        )
                        ) nnres_net neight_u
                ) Graph.get_empty_map (Graph.print res_network)

            (* get list of edges ((u, v), cap) that are contained in augment path *)
            let _get_augment_path res_network parent s t =
                let rec _get_simple_path simple_path parent v s =
                    if v == s then
                        simple_path
                    else
                        let u = _get_int_value parent v in
                        let nsimple_path = ResuidalNetwork.put simple_path ((u, v), 0) in
                            _get_simple_path nsimple_path parent u s
                in
                    let simple_path = _get_simple_path ResuidalNetwork.get_empty_map parent t s 
                    in
                        List.fold_left(
                            fun aug_path ((u, neight_u), _, _) ->
                                List.fold_left(
                                    fun naug_path (v, cap, cost) ->
                                        if ResuidalNetwork.is_key_a_member simple_path (u, v) then
                                            ((u, v), cap, cost)::naug_path
                                        else
                                            naug_path
                                ) aug_path neight_u
                        ) [] (Graph.print res_network)

            (* before the extending f over found path, find the minimal capacity of edge belonging to this path *)
            let _get_min_cap_over_path res_network parent s t =
                let augment_path = _get_augment_path res_network parent s t
                in
                    List.fold_left(
                        fun min_cap ((u, v), cap, _) ->
                            min min_cap cap
                    ) inf augment_path

            (* it extends the flow over found path *)
            let _extend_flow res_network flow parent add_flow act_cost_max_flow s t =
                let augment_path = _get_augment_path res_network parent s t
                in
                    List.fold_left(
                        fun (nflow, ncost_max_flow) ((u, v), cap, cost) ->
                            (
                                ResuidalNetwork.put nflow ((u, v), (_get_flow nflow (u, v)) + add_flow), 
                                ncost_max_flow + add_flow * cost
                            )
                    ) (flow, act_cost_max_flow) augment_path

            (* it rebuild resuidal network after updating flow and updating edge costs *)
            let _update_resuidal_network graph res_network flow =
                List.fold_left(
                    fun nres_net ((u, neight_u), _, _) ->
                        let nnres_net = 
                            if Graph.is_key_a_member nres_net u then
                                nres_net
                            else
                                Graph.put nres_net (u, [])
                        in 
                        List.fold_left(
                            fun nnnres_net (v, cap, cost) ->
                                let u_v_flow = (_get_flow flow (u, v)) 
                                in
                                    let nnnnres_net = 
                                        if u_v_flow < cap then
                                            Graph.put nnnres_net (
                                                u, 
                                                (v, cap - u_v_flow, cost)::(Graph.get_keys_value nnnres_net u)
                                            )
                                        else
                                            nnnres_net
                                    in
                                        if u_v_flow > 0 then
                                            let tmp = 
                                                if Graph.is_key_a_member nnnnres_net v then
                                                    nnnnres_net
                                                else
                                                    Graph.put nnnnres_net (v, [])
                                            in
                                            Graph.put tmp (
                                                v, 
                                                (u, u_v_flow, -cost)::(Graph.get_keys_value tmp v)
                                            )
                                        else
                                            nnnnres_net
                        ) nnres_net neight_u
                ) Graph.get_empty_map (Graph.print graph)

            (* it actualizes potential values taking into account distances a_pi computed in
             * Dijkstra algorithm. dists[v] is a distance from s to v. *)
            let _actualize_potentials potentials dists =
                List.fold_left(
                    fun npotentials ((u, dist), _, _) ->
                        IntIntAvlMap.put npotentials (u, dist + (_get_int_value dists u))
                ) IntIntAvlMap.get_empty_map (IntIntAvlMap.print potentials)
               
            (* counts flow value *)
            let _get_flow_value graph flow s = 
                let neight_s = Graph.get_keys_value graph s in
                    List.fold_left (
                        fun act_flow_value (v, cap, cost) ->
                            act_flow_value + (_get_flow flow (s, v))
                    ) 0 neight_s

            (* counts overall flow cost *)
            let _get_flow_cost graph flow =
                List.fold_left(
                    fun ncost ((u, neight_u), _, _) ->
                        List.fold_left(
                            fun nncost (v, cap, cost) ->
                                nncost + cost * (_get_flow flow (u, v))
                        ) ncost neight_u
                ) 0 (Graph.print graph)


            (* it is called whenever the extending path exists *)
            let rec _main_loop graph res_network flow potentials max_flow_cost s t i = 
                let res_network_with_updated_costs = _update_costs_in_resuidal_network res_network potentials in
                let (dists, parent) = DijkstraAlgorithm.get_the_shortest_path res_network_with_updated_costs s in
                    if (_get_int_value dists t) == inf || i == 0 then
                        (* extending path doesn't exist *)
                        let max_flow_value = _get_flow_value graph flow s in
                        (*let max_flow_cost = _get_flow_cost graph flow in*)
                            (max_flow_value, max_flow_cost, flow, res_network)
                    else
                        let min_cap_over_path = _get_min_cap_over_path res_network_with_updated_costs parent s t in
                        let (new_flow, new_max_flow_cost) = 
                            _extend_flow res_network flow parent min_cap_over_path max_flow_cost s t in
                        let new_res_network = _update_resuidal_network graph res_network_with_updated_costs new_flow in
                        let new_potentials = _actualize_potentials potentials dists in
                            _main_loop graph new_res_network new_flow new_potentials new_max_flow_cost s t (i-1)

            let get_max_flow_min_cost graph s t = 
                let flow = _get_init_flow graph in
                let res_network = _make_res_network graph in
                let (potentials, parent) = BellmanFordAlgorithm.get_the_shortest_path graph s in
                    _main_loop graph res_network flow potentials 0 s t 1000000
        end;;
