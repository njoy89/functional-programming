
(* an infinity value, used in Dijkstra algorithm *)
let inf = 1000000000;;

(* result of below comparitions *)
type comparison = LOWER | EQUAL | GREATER;;

(* used in AVL tree *)
module type LINEAR_ORDER = 
    sig
        type t
        val compare : t -> t -> comparison
    end;;

(* used in MAP, ft=key's type, st=value's type *)
module type LINEAR_ORDER_TUPLE = 
    sig
        type ft
        type st
        val compare : ft -> ft -> comparison
    end;;

module IntOrder = 
    struct
        type t = int
        let compare (x:int) (y:int) =
            if x < y then
                LOWER
            else if x == y then
                EQUAL
            else
                GREATER
    end;;

(* used in PriorityQueue in Dijkstra algorithm *)
module IntIntOrder =
    struct
        type t = int * int
        let compare ((x1, x2) : t) ((y1, y2) : t) =
            if x1 < y1 then
                LOWER
            else if x1 == y1 then
                if x2 < y2 then
                    LOWER
                else if x2 == y2 then
                    EQUAL
                else
                    GREATER
            else
                GREATER
    end;;
 
(* it is used each time whenever we want to create simple array *)
module IntIntOrderTuple = 
    struct
        type ft = int
        type st = int
        let compare (x:ft) (y:ft) =
            if x < y then
                LOWER
            else if x == y then
                EQUAL
            else
                GREATER
    end;;

(* it is used in graph representation *)
module IntIntListOrderTuple = 
    struct
        type ft = int
        type st = (int * int * int) list
        let compare (x:ft) (y:ft) =
            if x < y then
                LOWER
            else if x == y then
                EQUAL
            else
                GREATER
    end;;
   
module IntIntIntResNetOrderTuple =
    struct
        type ft = (int * int)
        type st = int
        let compare ((x1, y1):ft) ((x2, y2):ft) =
            if x1 < x2 then
                LOWER
            else if x1 == x2 then
                if y1 < y2 then
                    LOWER
                else if y1 == y2 then
                    EQUAL
                else
                    GREATER
            else
                GREATER
    end;;    

(* it is used in a data structure that store the flow - i.e. flow[(u, v, cost)] = f where 
 * e=(u,v) is en edge with cost "cost" and f is a flow that flow through this edge *)
module IntIntIntIntResNetOrderTuple =
    struct
        (* (u * v * cost) *)
        type ft = (int * int * int)
        type st = int
        let compare ((u1, v1, c1):ft) ((u2, v2, c2):ft) =
            if u1 < u2 then
                LOWER
            else if u1 == u2 then
                if v1 < v2 then
                    LOWER
                else if v1 == v2 then
                    if c1 < c2 then
                        LOWER
                    else if c1 == c2 then
                        EQUAL
                    else
                        GREATER
                else
                    GREATER
            else
                GREATER
    end;;    
