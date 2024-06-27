`prim.spi

func_of_2_int: [int, int] -> unit

# Conflict between signature "[i64, i64] -> unit" and bond type "[i64, i64] -> i64"
func_of_2_int := (a:int, b: int) -> 
    add[a, b]