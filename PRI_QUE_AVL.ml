
module type PRIORITY_QUEUE = functor(Order : LINEAR_ORDER) -> 
    sig
        exception Undefined
        type nodes_value_type = Order.t
        type queue

        val is_element_a_member : queue -> nodes_value_type -> bool

        val insert_element : queue -> nodes_value_type -> queue
        val remove_element : queue -> nodes_value_type -> queue

        val insert_list_of_elements : queue -> nodes_value_type list -> queue
        val remove_list_of_elements : queue -> nodes_value_type list -> queue

        val is_empty        : queue -> bool
        val get_empty_queue : queue

        val get_min_element : queue -> nodes_value_type
        val get_max_element : queue -> nodes_value_type

        val print : queue -> (nodes_value_type * int) list
    end;;

(* Methods whose name begins with the sign "_" are treated as private and
 * auxiliary methods. *)
module AVL : PRIORITY_QUEUE = functor (Order : LINEAR_ORDER) ->
    struct
        exception Undefined

        type nodes_value_type = Order.t

        (* type of a node in the queue *)
        type queue = 
            Empty |
            Node of (nodes_value_type * queue * queue * int)

        let is_empty tree = (tree == Empty)

        (* it returns an empty queue, must be used frequently *)
        let get_empty_queue = Empty

        let rec get_min_element tree = 
            match tree with
                Node(k, Empty, _, _) -> k |
                Node(_, l, _, _)     -> get_min_element l |
                Empty                -> raise Undefined 

        let rec get_max_element tree =
            match tree with
                Node(k, _, Empty, _) -> k |
                Node(_, _, r, _)     -> get_max_element r |
                Empty                -> raise Undefined

        (* it finds a given element in the queue. if it doesn't exists, it returns Empty. *)
        (* _find_key : queue -> first_cord_type -> queue *)
        let rec _find_element tree key =
            match tree with 
                Empty            -> Empty |
                Node(k, l, r, _) -> if Order.compare k key == EQUAL then 
                                        tree
                                    else if Order.compare k key == LOWER then
                                        _find_element r key
                                    else
                                        _find_element l key

        (* it checks whether exists a given element in the queue *)
        let is_element_a_member tree key =
            not (_find_element tree key == Empty)

        let _height tree = 
            match tree with
                Empty            -> 0 |
                Node(_, _, _, h) -> h

        let _make_node key left right =
            Node(key, left, right, (max (_height left) (_height right)) + 1)

        (* it restores the AVL tree order (uses rotations) *)
        let rec _balance key l r =
            let hl = _height l in
            let hr = _height r in
            if hl > hr + 1 then
                match l with
                    Empty               -> assert false |
                    Node(k, ll, lr, _)  -> 
                                    if _height ll >= _height lr then
                                        _make_node k ll (_make_node key lr r)
                                    else
                                        match lr with
                                            Empty                 -> assert false |
                                            Node(rk, lrl, lrr, _) ->
                                                _make_node rk (_make_node k ll lrl) (_make_node key lrr r)
            else if hr > hl + 1 then
                match r with
                    Empty               -> assert false |
                    Node(k, rl, rr, _)  -> 
                                    if _height rr >= _height rl then
                                        _make_node k (_make_node key l rl) rr
                                    else
                                        match rl with
                                            Empty -> assert false |
                                            Node(lk, rll, rlr, _) ->
                                                _make_node lk (_make_node key l rll) (_make_node k rlr rr)
            else
                _make_node key l r

        (* it is called only if a given element doesn't exist in the queue - as opposed to put method
         * in MAP *)
        let rec _insert_auxiliary tree key =
            match tree with
                Empty            -> Node(key, Empty, Empty, 1) |
                Node(k, l, r, _) -> if Order.compare k key == LOWER then
                                        let new_right = _insert_auxiliary r key
                                        in _balance k l new_right 
                                    else
                                        let new_left = _insert_auxiliary l key
                                        in _balance k new_left r 

        let insert_element tree key =
            if is_element_a_member tree key then
                tree
            else
                _insert_auxiliary tree key

        (* useful method, it inserts a list of elements into the queue *)
        let rec insert_list_of_elements tree keys =
            match keys with
                [] -> tree |
                h::t -> 
                    if is_element_a_member tree h then
                        tree
                    else
                        insert_list_of_elements (insert_element tree h) t

        (* it removes a given elemeent. it cares for restore the AVL order as well *)
        let rec _remove_auxiliary tree key = 
            match tree with
                Empty -> tree |
                Node(k, l, r, _) ->
                    if Order.compare k key == EQUAL then
                        match (l, r) with
                            (Empty, Empty)  -> Empty |
                            (Empty, _)      -> 
                                        let m = get_min_element r in 
                                        let new_r = _remove_auxiliary r m in
                                            _balance m l new_r |
                            _               ->
                                        let m = get_max_element l in
                                        let new_l = _remove_auxiliary l m in
                                            _balance m new_l r
                    else if Order.compare k key == LOWER then
                        let new_r = _remove_auxiliary r key in
                            _balance k l new_r
                    else
                        let new_l = _remove_auxiliary l key in
                            _balance k new_l r

        (* it removes a given element. it doesn't require the queue to store it *)
        let remove_element tree key =
            if not (is_element_a_member tree key) then
                tree
            else
                _remove_auxiliary tree key

        let rec remove_list_of_elements tree keys =
            match keys with
                [] -> tree |
                h::t ->
                    if not (is_element_a_member tree h) then
                        tree
                    else
                        remove_list_of_elements (remove_element tree h) t

        (* infix tree print *)
        let rec print tree = 
            match tree with
                Empty -> [] |
                Node(k, l, r, h) -> print l @ [(k, h)] @ print r
    end;;
