open Definition
open Constant
open Util
open Print

(*the info relevant to each player *)
type playerinfo = player*structures

(*Information for each player, then the rest of the board in structures then turn and next like state *)
type game = playerinfo*playerinfo*playerinfo*playerinfo*structures*turn*next

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


let state_of_game g = failwith "A voice said look me in the stars"
let game_of_state s = failwith "And tell me truly, men of earth,"


let init_game () = game_of_state (gen_initial_state())


let handle_move s m = failwith "If all the soul-and-body scars"

let presentation g = 
  let (p1, p2, p3, p4, s , t, n) = g in
  if get_player_color p1 = t.active then begin
    let np1 = p1 in 
    let np2 = hide_hand p2 in 
    let np3 = hide_hand p3 in 
    let np4 = hide_hand p4 in 
  end 
  else if get_player_color p2 = t.active then begin
    let np1 = hide_hand p1 in 
    let np2 = p2 in 
    let np3 = hide_hand p3 in 
    let np4 = hide_hand p4 in 
  end
  else if get_player_color p3 = t.active then begin
    let np1 = hide_hand p1 in 
    let np2 = hide_hand p2 in 
    let np3 = p3 in 
    let np4 = hide_hand p4 in 
  end
  else begin
    let np1 = hide_hand p1 in 
    let np2 = hide_hand p2 in 
    let np3 = hide_hand p3 in 
    let np4 = p4 in 
  end; np1, np2, np3, np4, s, t, n
    
    
  
