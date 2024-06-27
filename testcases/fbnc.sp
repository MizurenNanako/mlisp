`prim.spi

fbnc: [int] -> int
fbnc := (n: int) ->
      eq[n, 0] -> 1
    | eq[n, 1] -> 1
    | _ -> (
        n-1 := fbnc[sub[n, 1]] =>
        n-2 := fbnc[sub[n, 2]] =>
            add[n-1, n-2]
    )

printInt[fbnc[12]]
newline[]