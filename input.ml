(fun c -> let a x = ref (c : int) in a (fun x -))
let b = ((a  ((a 3) : ref int)  ) : ref ref int) in b
