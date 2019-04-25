let rec f = fun n -> n + 1 in
(let rec g = fun n -> n + 2 in
let rec f = fun n -> n + 3 in f 4) + f 4 ;;