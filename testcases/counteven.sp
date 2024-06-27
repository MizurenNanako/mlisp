`prim.spi

isEven := (n: int) ->
    eq[mod[n, 2], 0] -> 1
    | _ -> 0

data := {1, 2, 3, 4, 5, 6, 7}

countEven : [int, list] -> int
countEven := (acc: int, l: list) -> 
      nil[l] -> acc
    | _ -> (
          eq[isEven[hd[l]], 1] -> countEven[add[acc, 1], tl[l]]
        | _ -> countEven[acc, tl[l]]
    )

printInt[countEven[0, data]]
newline[]