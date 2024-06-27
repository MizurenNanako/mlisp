# unterminated string
`prim.spi

main := () -> 
    _ := printStr["hello
    this is
    legal,
    multiline string."] =>
    _ := printStr[
        "this is not.
    ]
# report at eof