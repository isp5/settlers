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

let rec get_intersections color ls acc coord hexList : (settlement*point*hex list) list= 
  let rec get_hex_list points acc hexList=
    match points with
      |hd::tl -> get_hex_list tl ((List.nth hexList hd)::acc) hexList
      |[]-> acc
   in
  match ls with 
  | [] -> acc
  | hd::tl -> 
    match hd with 
    | Some (c, s) -> 
      if c = color then get_intersections color tl ((s,coord,(get_hex_list (adjacent_pieces coord) [] hexList))::acc) (coord+1) hexList
      else get_intersections color tl acc (coord+1) hexList
    | None -> get_intersections color tl acc (coord+1) hexList

let rec get_roads color ls acc = 
  match ls with 
  | [] -> acc
  | hd::tl -> 
    match hd with 
    | c, l -> 
      if c = color then get_roads c tl (hd::acc)
      else get_roads c tl acc


let associate_with_player p s hlst: ((settlement*point*hex list) list*road list) = 
  match p with 
  | c, _, _ -> 
    match s with 
    | inters, roads -> (get_intersections c inters [] 0 hlst, get_roads c roads []) 


let game_of_state s = 
  match s with 
  | b, p1::p2::p3::[p4] , t, n -> begin
    match b with 
    | (hexL,_), structures , _, _, _ -> 
      let p1_info = p1, (associate_with_player p1 structures hexL) in
      let p2_info = p2, (associate_with_player p2 structures hexL) in
      let p3_info = p3, (associate_with_player p3 structures hexL) in
      let p4_info = p4, (associate_with_player p4 structures hexL) in
      p1_info, p2_info, p3_info, p4_info, b, t, n
  end 
  | _  -> failwith "something terrible has happened"

let init_game () = game_of_state (gen_initial_state())


let handle_move g m = 
  match m with 
  | InitialMove(line) -> None, Move.initial_move g line (*no one should ever win to start *)
  | RobberMove(robbermove) -> None, Move.robber_move g robbermove (*robber move shouldn't result in win*)
  | DiscardMove(cost) -> None, Move.discard_move g cost (*shouldn't result in a win? *)
  | TradeResponse(bool) -> None, Move.trade_response g bool (*shouldn't result in win*)
  | Action(action) -> failwith "Action.handle_action g action " (*can result in win*)

let presentation g = 
  let (p1, p2, p3, p4, b , t, n) = g in
  if get_player_color p1 = t.active then
    let np2 = hide_hand p2 in 
    let np3 = hide_hand p3 in 
    let np4 = hide_hand p4 in 
    p1, np2, np3, np4, b, t, n
  else if get_player_color p2 = t.active then 
    let np1 = hide_hand p1 in 
    let np3 = hide_hand p3 in 
    let np4 = hide_hand p4 in 
    np1, p2, np3, np4, b, t, n
  else if get_player_color p3 = t.active then 
    let np1 = hide_hand p1 in 
    let np2 = hide_hand p2 in 
    let np4 = hide_hand p4 in 
    np1, np2, p3, np4, b, t, n
  else
    let np1 = hide_hand p1 in 
    let np2 = hide_hand p2 in 
    let np3 = hide_hand p3 in 
    np1, np2, np3, p4, b, t, n
 
    
    
  
