open Definition
open Constant
open Util



(*the info relevant to each player *)
type info = ((settlement*point*hex list) list)*road list

(*the info relevant to each player *)
type playerinfo = player*info

(*Information for each player, then the rest of the board in structures then turn and next like state *)
type game = playerinfo*playerinfo*playerinfo*playerinfo*board*turn*next

let print_color ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(col,r)) =
  match col with
    |Blue->print_endline "Blue"
    |Red->print_endline "Red"
    |Orange->print_endline "Orange"
    |White->print_endline "White"

let print_c c =
  match c with
    |Blue->print_endline "Blue"
    |Red->print_endline "Red"
    |Orange->print_endline "Orange"
    |White->print_endline "White"

let color_of ((pc,(inv,cards),tr),(shLst,rLst)) = pc

let structures_of ((pc,(inv,cards),tr),(shLst,rLst)) = shLst

let get_player_color pi = 
  match (fst pi) with 
  | c,_,_ -> c

let get_player_hand pi = 
  match (fst pi) with 
  | _, h, _ -> h 

let get_current_playerinfo (g:game)  = 
  let (p1, p2, p3, p4, board, turn, next) = g in 
  let color = turn.active in 
  if get_player_color p1 = color then (p1, 1) 
  else if get_player_color p2 = color then (p2, 2)
  else if get_player_color p3 = color then (p3, 3)
  else (p4, 4) 

let get_player_by_color g color = 
  match g with 
  | (p1, p2, p3, p4, _, _, _) -> 
    if get_player_color p1 = color then (p1, 1) 
    else if get_player_color p2 = color then (p2, 2)
    else if get_player_color p3 = color then (p3, 3) 
    else (p4, 4)

(*replaces the nth element with el in ls. If n is larger than Length of ls then el is placed at end *)
let rec replace_at_index el ls n = 
  match ls with 
  | [] -> [el]
  | hd::tl -> if n = 0 then el::tl else hd::replace_at_index el tl (n-1)


let initial_move g line : game = 
  print_endline "Starting initial move";
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
    | Some _ -> print_endline (string_of_int point);gen_valid_initial_move inters rest
    | None -> 
      let ps = adjacent_points point in
      let (result, p2s) = check_structures ps inters false [] in 
      if result then (print_endline (string_of_int point); gen_valid_initial_move inters rest)
      else (print_endline (string_of_int point);point, (List.hd p2s)) in  
(*will check validity of initial move.  If move is invalid a valid move is returned *)
  let initial_move_check (g:game) line = 
    let (point1,point2) = line in 
    let check_these =  adjacent_points point1 in 
    let (p1, p2, p3, p4, board, t, n ) = g in 
    let (map, structures, deck, discard, robber) = board in 
    let (inters, roads) = structures in
    let rec range_list a b = if a > b then [] else a::(range_list (a+1) b) in
    if not (List.mem point2 check_these) 
    then (print_color g;print_endline "generating move";gen_valid_initial_move inters (range_list cMIN_POINT_NUM cMAX_POINT_NUM)) 
    else if point1 < cMIN_POINT_NUM || point1 > cMAX_POINT_NUM || point2 < cMIN_POINT_NUM || point2 > cMAX_POINT_NUM then (print_color g;print_endline "generating move";gen_valid_initial_move inters (range_list cMIN_POINT_NUM cMAX_POINT_NUM))
    else 
      let (result, p2s) = check_structures check_these inters false [] in
      (if result then (print_color g;print_endline "generating move";gen_valid_initial_move inters (range_list cMIN_POINT_NUM cMAX_POINT_NUM))
       else (point1, (List.hd p2s))) in
  let (point1, point2) = initial_move_check g line in
  (*make the move -> update game to reflect the move was made *)
  let (p1, p2, p3, p4, board, turn, next) = g in
  let (color, req) = next in
  let ((hlist, plist), (inters, roads), deck, discard, robber) = board in
  let new_road = (color, (point1, point2)) in 
  let new_town = Some(color, Town) in
  (*get the right player, make new info *)
  let (current_player, num) = get_current_playerinfo g in
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
  (* blue red orange white white orange red blue *) 
  (*let ((player,(structures, roads)),ind) = (get_player_by_color g White) in*)
  let has_no_structures = ((structures_of (fst(get_player_by_color g color))) = []) in
  let other_players_have_1_structure = 
    let (pc1,(inv1,cards1),(k1,lr1,la1)),(shLst1,rLst1) = p1 in
    let (pc2,(inv2,cards2),(k2,lr2,la2)),(shLst2,rLst2) = p2 in
    let (pc3,(inv3,cards3),(k3,lr3,la3)),(shLst3,rLst3) = p3 in
    let (pc4,(inv4,cards4),(k4,lr4,la4)),(shLst4,rLst4) = p4 in
    if color = pc1 then not (shLst2 = [] || shLst3 = [] || shLst4 = [])
    else if color = pc2 then not (shLst1 = [] || shLst3 = [] || shLst4 = [])
    else if color = pc3 then not (shLst1 = [] || shLst2 = [] || shLst4 = [])
    else not (shLst1 = [] || shLst2 = [] || shLst3 = []) in

  let other_players_have_2_structures = 
    let (pc1,(inv1,cards1),(k1,lr1,la1)),(shLst1,rLst1) = p1 in
    let (pc2,(inv2,cards2),(k2,lr2,la2)),(shLst2,rLst2) = p2 in
    let (pc3,(inv3,cards3),(k3,lr3,la3)),(shLst3,rLst3) = p3 in
    let (pc4,(inv4,cards4),(k4,lr4,la4)),(shLst4,rLst4) = p4 in
    if color = pc1 then List.length shLst2 = 2 && List.length shLst3 = 2 && List.length shLst4 = 2
    else if color = pc2 then List.length shLst1 = 2 && List.length shLst3 = 2 && List.length shLst4 = 2
    else if color = pc3 then List.length shLst2 = 2 && List.length shLst1 = 2 && List.length shLst4 = 2
    else List.length shLst2 = 2 && List.length shLst3 = 2 && List.length shLst1 = 2 in
  
  let have_n_structures p n= 
    let (pc,(inv,cards),(k,lr,la)),(shLst,rLst) = p in 
    (List.length shLst) = n in

  let next_color = 
    if (has_no_structures && other_players_have_1_structure) then (color) 
    else if has_no_structures then (next_turn color)
    else if other_players_have_2_structures && (have_n_structures (fst(get_player_by_color g color)) 1) then color
    else (prev_turn color) in
  
  let looped = (have_n_structures (fst(get_player_by_color g color)) 2) && (turn.active = color) in
  let next_request = if looped then (print_endline"looped";(turn.active, ActionRequest)) else (next_color, InitialRequest) in 
  let nboard = if looped then board else new_board in 
  if num = 1 then ((fst p1, new_info), p2, p3, p4, nboard, turn, next_request)
  else if num = 2 then (p1, (fst p2, new_info), p3, p4, nboard, turn, next_request)
  else if num = 3 then (p1, p2, (fst p3, new_info), p4, nboard, turn, next_request)
  else (p1, p2, p3, (fst p4, new_info), nboard, turn, next_request)

let rec make_list c n acc = 
	match n with 
	| 0 -> acc
	| _ -> make_list c (n-1) (c::acc)

let rec convert_to_cost ls cost = 
	match ls with 
	| [] -> cost
	| hd::tl -> 
	  match hd with 
	  | Brick -> convert_to_cost tl (map_cost2 (+) cost (single_resource_cost Brick))
	  | Wool -> convert_to_cost tl (map_cost2 (+) cost (single_resource_cost Wool))
	  | Ore -> convert_to_cost tl (map_cost2 (+) cost (single_resource_cost Ore))
	  | Lumber -> convert_to_cost tl (map_cost2 (+) cost (single_resource_cost Lumber))
	  | Grain -> convert_to_cost tl (map_cost2 (+) cost (single_resource_cost Grain))
      

let discard_move g cost : game = 
  let (current_player, num) = get_current_playerinfo g in 
  let check_valid_cost g cost : bool = 
    if sum_cost cost < 0 then false 
    else (
    let (inv, cards) = get_player_hand current_player in 
    let sum = map_cost2 (-) inv cost in
    match sum with 
    | (b, w, o, l, g) ->  b < 0 || w < 0 || o < 0 || l < 0 || g < 0 )
  in 
  let gen_valid_cost g (inv:cost) : cost = 
    match inv with 
    | (b, w, o, l, g) -> 
      let inv_list = (make_list Brick b []) @ (make_list Wool w []) @ (make_list Ore o []) @ (make_list Lumber l []) @ (make_list Grain g []) in 
      (*grab a certain number from this list, convert it to a cost *) 
      let hand_size = sum_cost inv in
      let number_to_discard = if hand_size >= cMAX_HAND_SIZE then hand_size/2 else 0 in
      let rec pick_n ls n acc =  (*for this to work n must be smaller than size of the list *)
	match n with 
	| 0 -> acc 
	| _ -> let (choice, rest) = pick_one ls in
	       pick_n rest (n-1) (choice::acc) 
      in 
      let cards_to_discard = pick_n inv_list number_to_discard [] in 
      convert_to_cost cards_to_discard (0,0,0,0,0)
  in
  let handle_cost g cost : game = 
    let (p1, p2, p3, p4, b, t, n) = g in 
    let (inv, cards) = get_player_hand current_player in 
    let new_inv = map_cost2 (-) inv cost in
    let (pc, hand, trophies) = fst current_player in 
    let updated_hand = (new_inv, snd hand) in 
    let (color, req) = n in 
    let looped = ((color,req) = ((next_turn color),DiscardRequest)) in 
    let next = if looped then (t.active,ActionRequest) else (next_turn color, DiscardRequest) in
    if num = 1 then (((pc,updated_hand, trophies), snd current_player), p2, p3, p4, b, t, next) 
    else if num = 2 then (p1, ((pc, updated_hand, trophies), snd current_player), p3, p4, b, t, next)
    else if num = 3 then (p1, p2, ((pc, updated_hand, trophies), snd current_player), p4, b, t, next)
    else (p1, p2, p3, ((pc, updated_hand, trophies), snd current_player), b, t, next) 
  in 
  let (inv, cards) = get_player_hand current_player in 
  if not (check_valid_cost g cost) then 
    let new_cost = gen_valid_cost g inv in
    handle_cost g new_cost 
  else handle_cost g cost 
  
 let rec check_target ls inters color my_color result1 result2 : (bool*bool) = 
      match ls with 
      | [] -> result1, result2
      | hd::tl -> 
	match List.nth inters hd with 
	| Some (c,s) -> begin
	  match color with 
	  | Some col -> 
	    if c = col then check_target tl inters color my_color (true || result1) (true || result2)  
	    else if c = my_color then check_target tl inters color my_color (false || result1) (false || result2)
	    else check_target tl inters color my_color (false || result1) (true || result2)
	  | None -> 
	    if c = my_color then check_target tl inters color my_color (false || result1) (false || result2)
	    else check_target tl inters color my_color (false || result1) (true || result2)
	end 
	| None -> check_target tl inters color my_color (false || result1) (false || result2)
    

let robber_move g rm : game = 
  let (p1, p2, p3, p4, board, turn, next) = g in 
  let current_color = get_player_color (fst (get_current_playerinfo g)) in 
  let check_valid board rm : bool = 
    let (piece, target) = rm in 
    let (map, structures, deck, discard, robber) = board in 
    let (inters, roads) = structures in 
    if piece < 0 then false 
    else (
    let points_to_check = piece_corners piece in  
    (*piece must be on board and not where robber is currently and target (or lack of) must be valid *) 
    match target with 
    | Some color -> 
      let (right_target, valid_target) = check_target points_to_check inters (Some(color)) current_color false false in
      right_target && piece <> robber && piece <=  cMAX_PIECE_NUM && piece >= cMIN_PIECE_NUM 
    | None -> 
      let (right_target, valid_target) = check_target points_to_check inters None current_color false false in
      (not valid_target) && piece <> robber && piece <=  cMAX_PIECE_NUM && piece >= cMIN_PIECE_NUM )
  in
  let gen_valid_move g rm : robbermove = 
    (*pick random int 0-18 but it can't be where robber is now *)
    let (p1, p2, p3, p4, board, turn, next) = g in 
    let (map, structures, deck, discard, robber) = board in 
    let rec gen_ran_not_n n = 
      let result = Random.int cNUM_PIECES in 
      if result = n then gen_ran_not_n n else result
    in
    let valid_placement = gen_ran_not_n robber in
    let (inters, roads) = structures in 
    let corners = piece_corners valid_placement in
    (*check corners, if there's a possible target (not yourself) pick them else none *)
    let (current_player, num) = get_current_playerinfo g in 
    let get_target ls : color option = 
      let rec check_corners ls acc : color list = 
	match ls with 
	| [] -> acc
	| hd::tl -> 
	  match List.nth inters hd with 
	  | Some(c,s) -> 
	    if (get_player_color current_player) <> c then check_corners tl (c::acc) 
	    else check_corners tl acc 
	  | None -> check_corners tl acc 
      in 
      match check_corners corners [] with 
      | [] -> None
      | xs -> Some (fst (pick_one xs))
    in
    (valid_placement, (get_target corners) )
  in 
  let handle_valid_move g rm : game =
    let steal_from_player inv1 inv2 : cost*cost = (*p1 from p2*)
      (*make a list of p2's resources and pick one from that, afterwards come up with proper invs and return *)
      let (b1, w1, o1, l1, g1) = inv1 in 
      let (b2, w2, o2, l2, g2) = inv2 in 
      let inv2_list = (make_list Brick b2 []) @ (make_list Wool w2 []) @ (make_list Ore o2 []) @ (make_list Lumber l2 []) @ (make_list Grain g2 []) in 
      let (resource_to_steal, resulting_inv) = pick_one inv2_list in 
      let new_p2_inv = convert_to_cost resulting_inv (0,0,0,0,0) in 
      match resource_to_steal with 
      | Brick -> (((b1+1), w1, o1, l1, g1), new_p2_inv)
      | Wool -> ((b1, (w1+1), o1, l1, g1), new_p2_inv)
      | Ore -> ((b1, w1, (o1+1), l1, g1), new_p2_inv)
      | Lumber -> ((b1, w1, o1, (l1+1), g1), new_p2_inv)
      | Grain -> ((b1, w1, o1, l1, (g1+1)), new_p2_inv)  
    in
    let (placement, target) = rm in 
    let (p1, p2, p3, p4, board, turn, next) = g in 
    let (map, structures, deck, discard, robber) = board in 
    let new_board = (map, structures, deck, discard, placement) in 
    match target with 
    | Some color -> (*steal and update map *) begin
      let (current_player, num1) = get_current_playerinfo g in 
      let (target_player, num2) = get_player_by_color g color in
      let (inv1, cards1) = get_player_hand current_player in 
      let (inv2, cards2) = get_player_hand target_player in
      let (new_inv1, new_inv2) = steal_from_player inv1 inv2 in 
      let (player1, hand1, t1) = fst current_player in 
      let (player2, hand2, t2) = fst target_player in 
      let new_p1 = ((player1, (new_inv1, cards1), t1), snd current_player) in 
      let new_p2 = ((player2, (new_inv2, cards2), t2), snd target_player) in 
      let next_color = next_turn turn.active in
      match num1, num2 with (*current and target *)
      | 1,2 -> (new_p1, new_p2, p3, p4, new_board, turn, (next_color, DiscardRequest))
      | 1,3 -> (new_p1, p2, new_p2, p4, new_board, turn, (next_color, DiscardRequest))
      | 1,4 -> (new_p1, p2, p3, new_p2, new_board, turn, (next_color, DiscardRequest))
      | 2,1 -> (new_p2, new_p1, p3, p4, new_board, turn, (next_color, DiscardRequest))
      | 2,3 -> (p1, new_p1, new_p2, p4, new_board, turn, (next_color, DiscardRequest))
      | 2,4 -> (p1, new_p1, p3, new_p2, new_board, turn, (next_color, DiscardRequest))
      | 3,1 -> (new_p2, p2, new_p1, p4, new_board, turn, (next_color, DiscardRequest))
      | 3,2 -> (p1, new_p2, new_p1, p4, new_board, turn, (next_color, DiscardRequest))
      | 3,4 -> (p1, p2, new_p1, new_p2, new_board, turn, (next_color, DiscardRequest))
      | 4,1 -> (new_p2, p2, p3, new_p1, new_board, turn, (next_color, DiscardRequest))
      | 4,2 -> (p1, new_p2, p3, new_p1, new_board, turn, (next_color, DiscardRequest))
      | 4,3 -> (p1, p2, new_p2, new_p1, new_board, turn, (next_color, DiscardRequest))
      | _ -> failwith "AHHHHH"
    end 
    | None -> (p1, p2, p3, p4, new_board, turn, next) 
  in
  if check_valid board rm then handle_valid_move g rm 
  else let valid_rm = gen_valid_move g rm in handle_valid_move g valid_rm 


let trade_response g (tr:bool) : game = 
  let check_valid g : bool = 
    (* make sure both players have inventory to make it happen...might be unneccessary*)
    let (p1,p2,p3,p4,board,turn,next) = g in
    match turn.pendingtrade with 
    | Some (color, cost1, cost2) ->
	let (current_player, num1) = get_current_playerinfo g in 
	let (second_player , num2) = get_player_by_color g color in 
	let (inv1, cards1) = get_player_hand current_player in 
	let (inv2, cards2) = get_player_hand second_player in 
	let (rb1, rw1, ro1, rl1, rg1) = map_cost2 (-) cost1 inv1 in 
	let (rb2, rw2, ro2, rl2, rg2) = map_cost2 (-) cost2 inv2 in 
	rb1 >=0 && rw1 >=0 && ro1 >= 0 && rl1 >=0 && rg1 >= 0 && rb2 >=0 && rw2 >=0 && ro2 >= 0 && rl2 >=0 && rg2 >= 0 
    | None -> true
  in
  let handle_trade b g : game =
    let (p1,p2,p3,p4,board,turn,next) = g in 
    match turn.pendingtrade with 
    | Some (color, cost1, cost2) -> begin
	if b then (
	  let (current_player, num1) = get_current_playerinfo g in 
	  let (second_player , num2) = get_player_by_color g color in 
	  let (inv1, cards1) = get_player_hand current_player in 
	  let (inv2, cards2) = get_player_hand second_player in 
	  let inter_inv1 = map_cost2 (-) cost1 inv1 in 
	  let inter_inv2 = map_cost2 (-) cost2 inv2 in 
	  let final_inv1 = map_cost2 (+) cost2 inter_inv1 in 
	  let final_inv2 = map_cost2 (+) cost1 inter_inv2 in  
	  let (player1, hand1, t1) = fst current_player in 
	  let (player2, hand2, t2) = fst second_player in 
	  let new_p1 = ((player1, (final_inv1, cards1), t1), snd current_player) in 
	  let new_p2 = ((player2, (final_inv2, cards2), t2), snd second_player) in 
	  let new_trades_made = turn.tradesmade + 1 in
	  let updated_turn = {turn with pendingtrade = None ; tradesmade = new_trades_made} in 
	  match num1, num2 with (*current and target *)
	  | 1,2 -> (new_p1, new_p2, p3, p4, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 1,3 -> (new_p1, p2, new_p2, p4, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 1,4 -> (new_p1, p2, p3, new_p2, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 2,1 -> (new_p2, new_p1, p3, p4, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 2,3 -> (p1, new_p1, new_p2, p4, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 2,4 -> (p1, new_p1, p3, new_p2, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 3,1 -> (new_p2, p2, new_p1, p4, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 3,2 -> (p1, new_p2, new_p1, p4, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 3,4 -> (p1, p2, new_p1, new_p2, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 4,1 -> (new_p2, p2, p3, new_p1, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 4,2 -> (p1, new_p2, p3, new_p1, board, updated_turn, (updated_turn.active, ActionRequest))
	  | 4,3 -> (p1, p2, new_p2, new_p1, board, updated_turn, (updated_turn.active, ActionRequest))
	  | _ -> failwith "AHHHHH" )
	else 
	   let new_trades_made = turn.tradesmade + 1 in
	   let updated_turn = {turn with pendingtrade = None ; tradesmade = new_trades_made} in 
	   (p1, p2, p3, p4, board, updated_turn, (updated_turn.active, ActionRequest))
    end 
    | None ->  (p1, p2, p3, p4, board, turn, (turn.active, ActionRequest))    
  in
  if check_valid g && tr then (handle_trade tr g)
  else (handle_trade tr g)
