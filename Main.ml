#use "Common.ml";;
#use "PRI_QUE_AVL.ml";;
#use "MAP_AVL.ml";;
#use "SHORTEST_PATH.ml";;
#use "MAX_FLOW_MIN_COST.ml";;

open Scanf;;
open Printf;;

module GraphIntIntList = AVL_MAP(IntIntListOrderTuple);;
module IntAvl = AVL(IntOrder);;
module IntAvlMap = AVL_MAP(IntIntOrderTuple);;
module DijkstraAlgorithm = DIJKSTRA(AVL)(AVL_MAP);;
module BellmanFordAlgorithm = BELLMAN_FORD(AVL_MAP);;
module MaxFlowMinCost = MAX_FLOW_MIN_COST(AVL_MAP)(DIJKSTRA(AVL))(BELLMAN_FORD);;
module ResuidalNetwork = AVL_MAP(IntIntIntResNetOrderTuple);;

(* it gets list of nodes and edges, then returns a graph's structure *)
let build_graph nodes edges =
    let graph_first_step = 
        List.fold_left(
            fun graph i ->
                GraphIntIntList.put graph (i, [])
        ) GraphIntIntList.get_empty_map nodes
    in
        List.fold_left (
            fun graph (a, b, cap, cost) -> 
                if not (GraphIntIntList.is_key_a_member graph a) then
                    GraphIntIntList.put graph (a, [(b, cap, cost)])
                else
                    GraphIntIntList.put graph (a, 
                        (b, cap, cost)::(GraphIntIntList.get_keys_value graph a))
        ) graph_first_step edges;;

let print_graph graph =
    let print_map = ref [] in
    begin
        print_map := GraphIntIntList.print graph;
        List.iter (fun ((a, l), h, s) -> 
            begin
                printf "key=%d h=%d s=%d:\n" a h s;
                List.iter (fun (b, cap, cost) -> 
                    printf "b=%d cap=%d cost=%d\n" b cap cost) l;
                printf "\n";
            end;
        ) !print_map;
    end;;

let main () =
        let n = ref 0 in
            scanf " %d" (fun x -> n := x);
        let m = ref 0 in
            scanf " %d" (fun x -> m := x);

        let nodes = ref [] in
            for i = 1 to !n do
                nodes := i::!nodes;
            done;

        let edges = ref [] in
            for i = 1 to !m do
                 scanf " %d %d %d %d" (fun a b cap cost -> edges := (a, b, cap, cost)::!edges);
            done;
        
        Printf.printf "\nn=%d m=%d\n" !n !m;
        List.iter (fun (a, b, cap, cost) -> printf "%d %d %d %d\n" a b cap cost) !edges;

        let graph = build_graph !nodes !edges in
        let (flow_value, flow_cost, flow, res_net) = MaxFlowMinCost.get_max_flow_min_cost graph 1 !n in
        begin
            print_graph graph;
            printf "flow_value=%d    flow_cost=%d\n" flow_value flow_cost;
            (*ResuidalNetwork.print flow;*)
        end;
        ()

(* ========================================================================================= *)
(* ===================================== TESTS ============================================= *)

let tests = [
    ( (* #0 *)
        [1;2;3;4],
        [
            (1, 2, 10, 1); 
            (1, 3, 10, 3); 
            (2, 3, 10, 1);
            (2, 4, 10, 5); 
            (3, 4, 10, 1); 
        ], 3, 20, 100, 1, 4
    );
    ( (* #1 *)
        [1;2;3;4],
        [
            (1, 2, 10, 1); 
            (1, 3, 10, 3); 
            (2, 3, 10, 1);
            (2, 4, 10, 5); 
            (3, 4, 10, 1); 
            (1, 4, 10, 2)
        ], 2, 30, 120, 1, 4
    );
    ( (* #2 *)
        [1;2;3;4;5],
        [
            (1, 2, 10, 1); 
            (2, 3, 10, 1); 
            (3, 5, 10, 1);
            (1, 5, 10, 3); 
            (1, 4, 10, 1); 
            (4, 5, 10, 1); 
            (2, 4, 10, 1);
            (3, 4, 10, 1)
        ], 2, 30, 80, 1, 5
    );
    ( (* #3 *)
        [1;2;3;4;5;6],
        [
            (1, 2, 10, 1);
            (1, 3, 10, 3);
            (1, 4, 10, 2);
            (4, 3, 10, 2);
            (2, 5, 10, 3);
            (2, 6, 10, 4);
            (3, 5, 10, 2);
            (4, 5, 10, 1);
            (3, 6, 10, 7);
            (4, 6, 10, 4);
            (5, 6, 10, 2);
            (2, 3, 10, 1);
        ], 5, 30, 180, 1, 6
    );
    ( (* #4 *)
        [1;2;3;4;5;6],
        [
            (1, 2, 4, 1);
            (1, 3, 10, 1);
            (2, 4, 6, 1);
            (3, 4, 9, 1);
            (4, 3, 5, 1);
            (2, 5, 3, 1);
            (5, 4, 5, 1);
            (4, 6, 7, 1);
            (5, 6, 10, 1);
        ], 3, 10, 30, 1, 6
    );
    ( (* #5 *)
        [1;2;3;4;5],
        [
            (1, 2, 2, 4);
            (1, 3, 1, 1);
            (2, 4, 2, 5);
            (3, 4, 2, 3);
            (4, 5, 2, 7);
        ], 11, 2, 27, 1, 5
    );
    ( (* #6 *)
        [1;2;3;4],
        [
            (1, 2, 2, 3);
            (1, 3, 2, 4);
            (2, 4, 1, 5);
            (3, 4, 3, 1);
            (2, 3, 1, 3);
            (3, 2, 1, 3);
        ], 5, 4, 25, 1, 4
    );
    (*
    ( (* doesn't work - problem with multiple edges *)
        [1;2;3],
        [
            (1, 2, 2, 5);
            (1, 2, 1, 1);
            (2, 3, 2, 5);
        ], 6, 2, 16, 1, 3
    )
    *)
    ]

let test_shortest_paths =
    List.fold_left(
        fun i (vertices, edges, min_dist, max_flow, min_cost_max_flow, s, t) ->
            let graph = build_graph vertices edges in
            let (dists_dij, parent_dij) = DijkstraAlgorithm.get_the_shortest_path graph s in
            let dist_to_last_dij = IntAvlMap.get_keys_value dists_dij (IntAvlMap.get_max_key dists_dij) in
            let (dists_bell, parent_bell) = BellmanFordAlgorithm.get_the_shortest_path graph s in
            let dist_to_last_bell = IntAvlMap.get_keys_value dists_bell (IntAvlMap.get_max_key dists_bell) in
            let (cap, cost, flow, res_net) = MaxFlowMinCost.get_max_flow_min_cost graph s t in
                (*printf "Parent:\n";
                List.iter (fun ((k, v), h, s) -> printf "(%d, %d) h=%d s=%d\n" k v h s) (IntAvlMap.print parent_dij);
                printf "\nDists:\n";
                List.iter (fun ((k, v), h, s) -> printf "(%d, %d) h=%d s=%d\n" k v h s) (IntAvlMap.print dists_dij);*)
                printf "Test %d:\n" i;
                assert (dist_to_last_dij == min_dist && dist_to_last_bell == min_dist);
                assert (cap == max_flow); 
                assert (cost == min_cost_max_flow);
                
                printf "flow: \n";
                List.iter(
                    fun (((u, v), u_v_flow), _, _) ->
                        printf "(%d, %d) -> %d\n" u v u_v_flow
                ) (ResuidalNetwork.print flow);
                printf "max-flow value: %d\n" cap;
                printf "min-cost max-flow cost: %d\n" cost;

                printf "RESULT: OK\n\n";
                (i+1)
    ) 1 tests;;

(*let (cap, cost, res_net) = *)
let test_max_flow_min_cost = 
    let which_test = 3 in
    let (nodes, edges, min_dist, max_flow, min_cost_max_flow, s, t) = (List.nth tests which_test) in
    let graph = build_graph nodes edges in
    let (cap, cost, flow, res_net) = MaxFlowMinCost.get_max_flow_min_cost graph s t in
        (cap, cost, ResuidalNetwork.print flow, GraphIntIntList.print res_net);;

(*
let test_avl1 () =
    let tree = IntAvl.get_empty_queue in
    let tree = IntAvl.insert_list_of_elements tree [5; 7; 3; 6; 10; 4; 2; 1; 8; 9] in 
    let tree = IntAvl.remove_list_of_elements tree [4; 5] in

    let print_tree = ref [] and 
        min_element = ref 0 and 
        max_element = ref 0 in
    begin
        print_tree := IntAvl.print tree;
        List.iter (fun (x, h) -> printf "x=%d h=%d\n" x h) !print_tree;
        min_element := IntAvl.get_min_element tree;
        max_element := IntAvl.get_max_element tree;
        printf "Min=%d Max=%d\n" !min_element !max_element;
    end;
    ()

let test_avl_map1 () =
    let map = IntAvlMap.get_empty_map in
    let map = IntAvlMap.put_list_of_keys map [(5, 1); (7, 2); (3, 3); (6, 4); (10, 5); 
        (4, 6); (2, 7); (1, 8); (8, 9); (9, 10)] in
    
    let print_map = ref [] and
        value = ref 0 in
    begin
        print_map := IntAvlMap.print map;
        List.iter (fun ((k, v), h, s) -> printf "(%d, %d) h=%d s=%d\n" k v h s) !print_map;
        value := IntAvlMap.get_keys_value map 3;
        printf "keys_value = %d\n" !value;
    end;
    ()

let test_avl_map2 () =
    let map = IntAvlMap.get_empty_map in
    let map = IntAvlMap.put_list_of_keys map [(5, 1); (4, 2); (3, 3); (6, 4); (7, 5)] in
    
    let print_map = ref [] in
    begin
        print_map := IntAvlMap.print map;
        List.iter (fun ((k, v), h, s) -> printf "(%d, %d) h=%d s=%d\n" k v h s) !print_map;
    end;
    ()

let test_graph1 () =
    let graph = GraphIntIntList.get_empty_map in
    let graph = GraphIntIntList.put_list_of_keys graph [(1, [(2, 5, 5); (3, 5, 5)])] in
    let graph = GraphIntIntList.put graph (1, (4, 5, 5)::(GraphIntIntList.get_keys_value graph 1)) in 
    print_graph graph;
    ()
*)
