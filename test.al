exception q with string
exception something

let main =
    exception q with 1
    except | x -> 0
            | y with abc -> 0
