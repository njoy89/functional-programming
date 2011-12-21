
let inf = 1000000000;;

type comparison = LOWER | EQUAL | GREATER;;

module type LINEAR_ORDER = 
    sig
        type t
        val compare : t -> t -> comparison
    end;;

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
