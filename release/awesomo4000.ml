open Definition
open Registry
open Constant
open Util

(** Give your bot a 2-20 character name. *)
let name = "awesomo4000"


module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()


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


let rec print_ter_list ls = 
  match ls with 
  | [] -> print_endline "done"
  | (n,t)::tl -> 
    match t with 
    | Mountain -> print_endline "Mountain";print_ter_list tl
    | Hill -> print_endline "Hill";print_ter_list tl
    | Pasture -> print_endline "Pasture";print_ter_list tl
    | Field -> print_endline "Field";print_ter_list tl
    | Forest -> print_endline "Forest";print_ter_list tl
    | Desert -> print_endline "Desert";print_ter_list tl

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
  | _  -> failwith "something terrible has happened" (*should never hit this *)

let rec search_hexes hlist n acc = 
  (*search for hex with desirable roll *)
  match hlist with 
    [] -> acc
  | (t,r)::tl -> 
    match t with 
    | Forest -> 
      if (r = 6 || r = 8) then search_hexes tl (n+1) ((n,Forest)::acc)
      else search_hexes tl (n+1) acc 
    |  Hill-> 
      if (r = 6 || r = 8) then search_hexes tl (n+1) ((n,Hill)::acc)
      else search_hexes tl (n+1) acc 
    | Field -> 
      if (r = 6 || r = 8) then search_hexes tl (n+1) ((n,Field)::acc)
      else search_hexes tl (n+1) acc 
    | Pasture ->  
      if (r = 5 || r = 6 || r = 8 || r = 9) then search_hexes tl (n+1) ((n,Pasture)::acc)
      else search_hexes tl (n+1) acc 
    | Mountain -> 
      if (r = 5 || r = 6 || r = 8 || r = 9) then search_hexes tl (n+1) ((n,Mountain)::acc)
      else search_hexes tl (n+1) acc 
    | Desert -> search_hexes tl (n+1) acc
	  
  let decide_initial_move g = 
    let (p1, p2, p3, p4, ((hlist, plist), (inters, rds), d, di, r), t, n) = g in   
    let preference ls = 
      let comparator (n,t) (n2,t2) = 
	match t, t2 with
	| Mountain, Mountain -> 0 
	| Mountain, _ -> 1
	| Field, Mountain -> -1 
	| Field, Field -> 0
	| Field, _ -> 1 
	| Pasture, Mountain -> -1
	| Pasture, Field -> -1
	| Pasture, Pasture -> 0
	| Pasture, _ -> 1
	| Desert, _ -> -1
	| _, Desert -> 1
	| _ -> 0
      in
      (List.sort comparator ls)
    in 
     let list = preference(search_hexes hlist 0 []) in 
    let rec choose_pt lst=  
      let occupied pt intersList= 
        match List.nth intersList pt with
          |Some(a)-> true
          |None -> false in
      match list with 
        | [] -> 0,0
        | (hd, t)::(hd2, t2)::tl -> (
          let point_to_try = hd2 in
          let ps =  adjacent_points point_to_try in 
          let p2 = List.hd ps in
          if (occupied hd2 inters) then (choose_pt tl) else (point_to_try,p2) )
	| (hd, t)::[] -> (
          let point_to_try = hd in
          let ps =  adjacent_points point_to_try in 
          let p2 = List.hd ps in
          if (occupied hd inters) then (0,0) else (point_to_try,p2) )
    in
    InitialMove(choose_pt list)
    
  let get_player_by_color c g = 
    match g with 
    | (p1, p2, p3, p4, _, _, _) -> 
      if get_player_color p1 = c then p1
      else if get_player_color p2 = c then p2
      else if get_player_color p3 = c then p3
      else p4 
   
  let decide_robber_move g = 
    let (p1, p2, p3, p4, b, t, n) = g in 
    match t.active with 
    | Blue -> begin
      let (target_player,(sphl, rl)) = get_player_by_color Red g in 
      match sphl with 
      | (s, p, hl)::tl -> begin
	let pieces = adjacent_pieces p in
	match pieces with 
	| [] -> RobberMove(0, None)
	| hd::tl -> RobberMove(hd, Some(Red))
      end 
      | _ -> RobberMove(0, None) 
    end 
    | Red -> begin
      let (target_player,(sphl, rl)) = get_player_by_color Orange g in
       match sphl with 
      | (s, p, hl)::tl -> begin
	let pieces = adjacent_pieces p in
	match pieces with 
	| [] -> RobberMove(0, None)
	| hd::tl -> RobberMove(hd, Some(Orange))
      end 
      | _ -> RobberMove(0, None) 
    end 
    | Orange -> begin
      let (target_player,(sphl, rl)) = get_player_by_color White g in 
       match sphl with 
      | (s, p, hl)::tl -> begin
	let pieces = adjacent_pieces p in
	match pieces with 
	| [] -> RobberMove(0, None)
	| hd::tl -> RobberMove(hd, Some(White))
      end 
      | _ -> RobberMove(0, None)
    end 
    | White -> begin
      let (target_player,(sphl, rl)) = get_player_by_color Blue g in 
       match sphl with 
      | (s, p, hl)::tl -> begin
	let pieces = adjacent_pieces p in
	match pieces with 
	| [] -> RobberMove(0, None)
	| hd::tl -> RobberMove(hd, Some(Blue))
      end 
      | _ -> RobberMove(0, None)
    end 

  let decide_discard_move g = DiscardMove((-1,-1,-1,-1,-1)) (*let it be random*)

  let decide_trade_response g = TradeResponse(false) (*don't trade with anyone!*)


  let can_afford inv cost: bool = 
    let left = map_cost2 (-) inv cost in 
    match left with 
    | (b, w, o, g, l) -> 
      b >=0 && w >= 0 && o >=0 && l >= 0 && g >= 0

  let make_action g =
    let (p1, p2, p3, p4, ((hlist, plist), s, d, di, r), t, n) = g in
    if is_none t.dicerolled then Action(RollDice) 
    else (
      (*should be me? *)
      let me = get_player_by_color t.active g in 
      let ((color, (inv, cards), trophies), (sphl, rl)) = me in 
      if can_afford inv cCOST_CITY then (
      match sphl with 
      | (Town, p, hl)::tl -> Action(BuyBuild(BuildCity(p)))
      | (City, p, hl)::(Town, p2, hl2)::tl -> Action(BuyBuild(BuildCity(p2)))
      | (City, p, hl)::(City, p2, hl2)::tl -> 
	if can_afford inv cCOST_CARD then Action(BuyBuild(BuildCard))
	else (	
	  match cards with 
	| Reveal(cs) -> begin 
	  match List.hd cs with 
	  | Knight -> Action(PlayCard(PlayKnight(0, None)))
	  | VictoryPoint -> Action(EndTurn)
	  | RoadBuilding -> Action(PlayCard(PlayRoadBuilding((t.active,(0,0)),None))) (*effectively discard *)
	  | YearOfPlenty -> Action(PlayCard(PlayYearOfPlenty(Ore, Some(Wool))))
	  | Monopoly -> Action(PlayCard(PlayMonopoly(Grain)))
	end 
	| _ -> Action(EndTurn))
      | _ -> Action(EndTurn))
      else (
	if can_afford inv cCOST_CARD then Action(BuyBuild(BuildCard))
	else (	
	  match cards with 
	| Reveal(cs) -> begin 
	  match cs with 
	  | [] -> Action(EndTurn)
	  | hd::tl -> begin
	    match hd with 
	    | Knight -> Action(PlayCard(PlayKnight(0, None)))
	    | VictoryPoint -> Action(EndTurn)
	    | RoadBuilding -> Action(PlayCard(PlayRoadBuilding((t.active,(0,0)),None))) (*effectively discard *)
	    | YearOfPlenty -> Action(PlayCard(PlayYearOfPlenty(Ore, Some(Wool))))
	    | Monopoly -> Action(PlayCard(PlayMonopoly(Grain)))
	  end 
	end 
	| _ -> Action(EndTurn))))
    
(* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,n) : state) : move =
    let s = (b,p,t,n) in
    let (c, r) = n in
    match r with
      | InitialRequest -> decide_initial_move (game_of_state s)
      | RobberRequest -> decide_robber_move (game_of_state s)
      | DiscardRequest-> decide_discard_move (game_of_state s)
      | TradeRequest -> decide_trade_response (game_of_state s)
      | ActionRequest -> make_action (game_of_state s)
        
      
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
