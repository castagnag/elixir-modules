module SMap = Map.Make(String)

let smap_of_list l =
  SMap.of_seq (List.to_seq l)

let list_of_smap m =
  List.of_seq (SMap.to_seq m)

module SSet = Set.Make(String)

let list_of_sset s =
  List.of_seq (SSet.to_seq s)
