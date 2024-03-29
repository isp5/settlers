open Definition
open Constant
open Util
open Print



(*the info relevant to each player *)
type info = ((settlement*point*hex list) list)*road list

(*the info relevant to each player *)
type playerinfo = player*info

(*Information for each player, then the rest of the board in structures then turn and next like state *)
type game = playerinfo*playerinfo*playerinfo*playerinfo*board*turn*next

let get_player_color pi = 
  match (fst pi) with 
  | c,_,_ -> c

let get_player_hand pi = 
  match (fst pi) with 
  | _, h, _ -> h 

let get_player_trophies pi = 
  match (fst pi) with 
  | _, _, t -> t

let hide_hand pi = 
  match (fst pi) with 
  | c, (inv, cs) , t -> (c, (inv, hide cs), t), snd pi


let state_of_game g = 
  match g with 
  | p1, p2, p3, p4, b, t, n -> b, [(fst p1);(fst p2);(fst p3);(fst p4)], t, n

let rec get_intersections color ls acc coord hexList robber: (settlement*point*hex list) list= 
  let rec get_hex_list points acc hexList=
    match points with
      |hd::tl -> (
        if hd = robber then get_hex_list tl acc hexList 
        else get_hex_list tl ((List.nth hexList hd)::acc) hexList)
      |[]-> acc
   in
  match ls with 
  | [] -> acc
  | hd::tl -> 
    match hd with 
    | Some (c, s) -> 
      if c = color then get_intersections color tl ((s,coord,(get_hex_list (adjacent_pieces coord) [] hexList))::acc) (coord+1) hexList robber
      else get_intersections color tl acc (coord+1) hexList robber
    | None -> get_intersections color tl acc (coord+1) hexList robber

let rec get_roads color ls acc = 
  match ls with 
  | [] -> acc
  | hd::tl -> 
    match hd with 
    | c, l -> 
      if c = color then get_roads color tl (hd::acc)
      else get_roads color tl acc


let associate_with_player p s hlst robber: ((settlement*point*hex list) list*road list) = 
  match p with 
  | c, _, _ -> 
    match s with 
    | inters, roads -> (get_intersections c inters [] 0 hlst robber, get_roads c roads []) 


let game_of_state s = 
  match s with 
  | b, p1::p2::p3::[p4] , t, n -> begin
    match b with 
    | (hexL,_), structures , _, _, robber -> 
      let p1_info = p1, (associate_with_player p1 structures hexL robber) in
      let p2_info = p2, (associate_with_player p2 structures hexL robber) in
      let p3_info = p3, (associate_with_player p3 structures hexL robber) in
      let p4_info = p4, (associate_with_player p4 structures hexL robber) in
      p1_info, p2_info, p3_info, p4_info, b, t, n
  end 
  | _  -> failwith "something terrible has happened"

let init_game () = game_of_state (gen_initial_state())


let handle_move g m = 
  let (p1, p2, p3, p4, board, turn, next) = g in 
  let (color, request) = next in
  let action_mapper g m = 
  match request with 
  | InitialRequest -> begin
    match m with 
    | InitialMove(line) -> None, Move.initial_move g line (*no intial should win *)
    | _ -> None, Move.initial_move g (0,0)  (*will be bogus move, random valid will be generated *)
  end 
  | RobberRequest -> begin
    match m with 
    | RobberMove(robbermove) -> None, Move.robber_move g robbermove
    | _ -> None, Move.robber_move g (-1, None) (*will be bogus move, random valid will be generated *)
  end 
  | DiscardRequest -> begin 
    match m with
    | DiscardMove(cost) -> None, Move.discard_move g cost
    | _ -> None, Move.discard_move g (-1, -1, -1, -1, -1) (*will be bogus move, random valid will be generated *)
  end 
  | TradeRequest -> begin
    match m with 
    | TradeResponse(b) -> None, Move.trade_response g b 
    | _ -> None, Move.trade_response g false (*not sure what else to do *)
  end 
  | ActionRequest -> begin
    match m with 
    | Action(action) -> Action.handle_action g action
    | _ -> Action.handle_action g EndTurn 
  end in
  match action_mapper g m with
    |a -> Print.print_update color m (state_of_game (snd(a)));a
 
 
let presentation g = 
  let (p1, p2, p3, p4, (m,s,d,di,r) , t, n) = g in
  if get_player_color p1 = t.active then
    let np2 = hide_hand p2 in 
    let np3 = hide_hand p3 in 
    let np4 = hide_hand p4 in 
    p1, np2, np3, np4, (m,s,(hide d), di, r), t, n
  else if get_player_color p2 = t.active then 
    let np1 = hide_hand p1 in 
    let np3 = hide_hand p3 in 
    let np4 = hide_hand p4 in 
    np1, p2, np3, np4,(m,s,(hide d), di, r), t, n
  else if get_player_color p3 = t.active then 
    let np1 = hide_hand p1 in 
    let np2 = hide_hand p2 in 
    let np4 = hide_hand p4 in 
    np1, np2, p3, np4,(m,s,(hide d), di, r), t, n
  else
    let np1 = hide_hand p1 in 
    let np2 = hide_hand p2 in 
    let np3 = hide_hand p3 in 
    np1, np2, np3, p4,(m,s,(hide d), di, r), t, n
 
    
    
  
