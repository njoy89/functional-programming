
module type MAP = functor(Order : LINEAR_ORDER_TUPLE) -> 
    sig
        exception Undefined

        type first_cord_type = Order.ft
        type second_cord_type = Order.st
        type tuple_type = first_cord_type * second_cord_type

        type map 

        val is_key_a_member     : map -> first_cord_type -> bool

        val put                 : map -> tuple_type -> map 
        val remove              : map -> first_cord_type -> map
        val get_keys_value      : map -> first_cord_type -> second_cord_type

        val put_list_of_keys    : map -> tuple_type list -> map
        val remove_list_of_keys : map -> first_cord_type list -> map

        val is_empty            : map -> bool
        val get_empty_map       : map 
        val get_size            : map -> int

        val get_min_key         : map -> first_cord_type
        val get_max_key         : map -> first_cord_type

        val print : map -> (tuple_type * int * int) list
    end;;

(* Methods whose name begins with the sign "_" are treated as private and
 * auxiliary methods. *)
module AVL_MAP : MAP = functor (Order : LINEAR_ORDER_TUPLE) ->
    struct
        (* this exception is raised when there is a wrong access to non-existing node *)
        exception Undefined

        type first_cord_type = Order.ft
        type second_cord_type = Order.st
        type tuple_type = first_cord_type * second_cord_type (* (key, value) *)

        (* type of a node in the map *)
        type map = 
            Empty |
            Node of (tuple_type * map * map * int * int)
            (* (key, value, left, right, height, size) *)

        let is_empty map = (map == Empty)

        (* it returns an empty map, must be used frequently *)
        let get_empty_map = Empty

        let rec get_min_key map = 
            match map with
                Node((k, v), Empty, _, _, _) -> k |
                Node((k, v), l, _, _, _)     -> get_min_key l |
                Empty                        -> raise Undefined 

        let rec get_max_key map =
            match map with
                Node((k, v), _, Empty, _, _) -> k |
                Node(_, _, r, _, _)          -> get_max_key r |
                Empty                        -> raise Undefined


        (* it returns value of node whose key equals to a given value. there must exist such node
           in the map! otherwise, the exception will be raised *)
        let rec get_keys_value map key =
            match map with 
                Empty                    -> raise Undefined |
                Node((k, v), l, r, _, _) -> if Order.compare k key == EQUAL then 
                                                v
                                            else if Order.compare k key == LOWER then
                                                get_keys_value r key
                                            else
                                                get_keys_value l key

        (* it finds node that has a given key. if it doesn't exists, it returns Empty. *)
        (* _find_key : map -> first_cord_type -> map *)
        let rec _find_key map key =
            match map with 
                Empty                    -> Empty |
                Node((k, v), l, r, _, _) -> if Order.compare k key == EQUAL then 
                                                map 
                                            else if Order.compare k key == LOWER then
                                                _find_key r key
                                            else
                                                _find_key l key

        (* it checks whether exists a given key *)
        let is_key_a_member map key =
            not (_find_key map key == Empty)

        let get_size map =
            match map with
                Empty               -> 0 |
                Node(_, _, _, _, s) -> s

        let _height map = 
            match map with
                Empty               -> 0 |
                Node(_, _, _, h, _) -> h

        let _make_node tup l r =
            Node(tup, l, r, 
                (max (_height l) (_height r)) + 1, 
                (get_size l) + (get_size r) + 1
            )

        (* it restores the AVL tree order (uses rotations) *)
        let rec _balance tup l r =
            let hl = _height l in
            let hr = _height r in
            if hl > hr + 1 then
                match l with
                    Empty                       -> assert false |
                    Node(ltup, ll, lr, _, _)    -> 
                                        if _height ll >= _height lr then
                                            _make_node ltup ll (_make_node tup lr r)
                                        else
                                            match lr with
                                                Empty                    -> assert false |
                                                Node(rk, lrl, lrr, _, _) ->
                                                    _make_node rk (_make_node ltup ll lrl) 
                                                        (_make_node tup lrr r)
            else if hr > hl + 1 then
                match r with
                    Empty                       -> assert false |
                    Node(rtup, rl, rr, _, _)    -> 
                                    if _height rr >= _height rl then
                                        _make_node rtup (_make_node tup l rl) rr
                                    else
                                        match rl with
                                            Empty -> assert false |
                                            Node(rltup, rll, rlr, _, _) ->
                                                _make_node rltup (_make_node tup l rll) 
                                                    (_make_node rtup rlr rr)
            else
                _make_node tup l r

        (* note: if node whose key equals to given key exists, this procedure changes its value,
         * as opposed to insert_element in queue *)
        let rec _put_auxiliary map (key, value) =
            match map with
                Empty                    -> Node((key, value), Empty, Empty, 1, 1) |
                Node((k, v), l, r, h, s) -> if Order.compare k key == LOWER then
                                                let new_right = _put_auxiliary r (key, value)
                                                in _balance (k, v) l new_right 
                                            else if Order.compare k key == EQUAL then
                                                Node((k, value), l, r, h, s)
                                            else
                                                let new_left = _put_auxiliary l (key, value)
                                                in _balance (k, v) new_left r 

        let put map t =
            _put_auxiliary map t

        (* useful method, it inserts a list of (key, value) into the map *)
        let rec put_list_of_keys map keys =
            match keys with
                []          -> map |
                (k, v)::t   -> 
                        if is_key_a_member map k then
                            map
                        else
                            put_list_of_keys (put map (k, v)) t

        (* it removes node whose key equals to a given key. it cares for restore the AVL order as well *)
        let rec _remove_auxiliary map key = 
            match map with
                Empty                    -> map |
                Node((k, v), l, r, _, _) ->
                        if Order.compare k key == EQUAL then
                            match (l, r) with
                                (Empty, Empty)  -> Empty |
                                (Empty, _)      -> 
                                        let m = get_min_key r in 
                                        let new_r = _remove_auxiliary r m in
                                            _balance (m, get_keys_value r m) l new_r |
                                _               ->
                                        let m = get_max_key l in
                                        let new_l = _remove_auxiliary l m in
                                            _balance (m, get_keys_value l m) new_l r
                        else if Order.compare k key == LOWER then
                            let new_r = _remove_auxiliary r key in
                                _balance (k, v) l new_r
                        else
                            let new_l = _remove_auxiliary l key in
                                _balance (k, v) new_l r

        (* it removes node whose key equals to a given key. it doesn't require the map
           to store it *)
        let remove map key =
            if not (is_key_a_member map key) then
                map
            else
                _remove_auxiliary map key

        let rec remove_list_of_keys map keys =
            match keys with
                [] -> map |
                h::t ->
                    if not (is_key_a_member map h) then
                        map
                    else
                        remove_list_of_keys (remove map h) t

        (* infix tree print *)
        let rec print map = 
            match map with
                Empty -> [] |
                Node((k, v), l, r, h, s) -> print l @ [((k, v), h, s)] @ print r
    end;;
    
