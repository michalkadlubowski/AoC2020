module Queue 

type queue<'T> =  Queue of 'T list * 'T list
    
let empty = Queue([], [])

let enqueue e q =
    match q with
    | Queue(fs, bs) -> Queue(e :: fs, bs)

let dequeue q =
    match q with
    | Queue([], []) -> failwith "Empty queue!"
    | Queue(fs, b :: bs) -> b, Queue(fs, bs)
    | Queue(fs, []) ->
        let bs = List.rev fs
        bs.Head, Queue([], bs.Tail)

let isempty q =
    match q with
    | Queue([], []) -> true
    | _ -> false

let length q = 
    match q with
    |   Queue(fs, bs) -> fs.Length + bs.Length