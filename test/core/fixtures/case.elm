homeDirectory = "/root/files"
eval boolean = case boolean of
    Literal bool -> bool
    Not b        -> not (eval b)
    And b b_     -> eval b && eval b_
    Or b b_      -> eval b   || eval b_
