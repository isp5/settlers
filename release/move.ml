open Definition
open Constant
open Util



(*the info relevant to each player *)
type info = ((settlement*point*hex list) list)*road list

(*the info relevant to each player *)
type playerinfo = player*info

(*Information for each player, then the rest of the board in structures then turn and next like state *)
type game = playerinfo*playerinfo*playerinfo*playerinfo*board*turn*next


let get_player_color pi = 
  match (fst pi) with 
  | c,_,_ -> c


let get_current_playerinfo (g:game) : playerinfo = 
  let (p1, p2, p3, p4, board, turn, next) = g in 
  let color = turn.active in 
  if get_player_color p1 = color then p1 
  else if get_player_color p2 = color then p2
  else if get_player_color p3 = color then p3
  else p4 

(*replaces the nth element with el in ls. If n is larger than Length of ls then el is placed at end *)
let rec replace_at_index el ls n = 
  match ls with 
  | [] -> [el]
  | hd::tl -> if n = 0 then el::tl else hd::replace_at_index el tl (n-1)


let initial_move (g:game) line = 
  let rec check_structures ps inters result acc : bool * point list =
    match ps with 
      [] -> result, acc
    | hd::tl -> 
      match List.nth inters hd with 
      | Some _ -> check_structures tl inters (result || true) acc
      | None -> check_structures tl inters (result || false) (hd::acc) in 
  let rec gen_valid_initial_move inters grab_bag = 
    let (point, rest) = pick_one grab_bag in
    match (List.nth inters point) with 
    | Some _ -> gen_valid_initial_move inters rest
    | None -> 
      let ps = adjacent_points point in
      let (result, p2s) = check_structures ps inters false [] in 
      if result then gen_valid_initial_move inters rest
      else point, (List.hd p2s) in  
(*will check validity of initial move.  If move is invalid a valid move is returned *)
  let initial_move_check (g:game) line = 
    let (point1,point2) = line in 
    let check_these =  adjacent_points point1 in 
    let (p1, p2, p3, p4, board, t, n ) = g in 
    let (map, structures, deck, discard, robber) = board in 
    let (inters, roads) = structures in
    let rec range_list a b = if a > b then [] else a::(range_list (a+1) b) in
    if not (List.mem point2 check_these) then gen_valid_initial_move inters (range_list 0 53) 
    else 
      let (result, p2s) = check_structures check_these inters false [] in
      (if result then gen_valid_initial_move inters (range_list 0 53)
       else (point1, (List.hd p2s))) in
  let (point1, point2) = initial_move_check g line in
  (*make the move -> update game to reflect the move was made *)
  let (p1, p2, p3, p4, board, turn, next) = g in
  let ((hlist, plist), (inters, roads), deck, discard, robber) = board in
  let new_road = (turn.active, (point1, point2)) in 
  let new_town = Some(turn.active, Town) in
  (*get the right player, make new info *)
  let current_player = get_current_playerinfo g in
  let current_info = snd current_player in 
  let sphl, rl = current_info in 
  (* need the hexes that correspond to this point1 *)
  let hexpoints = adjacent_pieces point1 in 
  let rec get_hex_list points acc hexList=
    match points with
      |hd::tl -> get_hex_list tl ((List.nth hexList hd)::acc) hexList
      |[]-> acc
  in
  let new_info = ((Town,point1,(get_hex_list hexpoints [] hlist))::sphl, new_road::rl) in 
  (* replace the right element of inters with the new value *) 
  let new_inters = replace_at_index new_town inters point1 in 
  let new_board = ((hlist, plist), (new_inters, (new_road::roads)), deck, discard, robber) in 
  if current_player = p1 then ((fst p1, new_info), p2, p3, p4, new_board, turn, next)
  else if current_player = p2 then (p1, (fst p2, new_info), p3, p4, new_board, turn, next)
  else if current_player = p3 then (p1, p2, (fst p3, new_info), p4, new_board, turn, next)
  else (p1, p2, p3, (fst p4, new_info), new_board, turn, next)


let robber_move g rm = failwith "here too"


let discard_move g dm = failwith "lol"


let trade_response g tr = 
  if tr then failwith "handle the trade using info in turn"
  else failwith "handle the proper trade stuff here too -> maybe an action"


