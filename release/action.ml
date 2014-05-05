open Definition
open Constant
open Util
open Game

(*the info relevant to each player *)
type info = ((settlement*point*hex list) list)*road list

(*the info relevant to each player *)
type playerinfo = player*info

(*Information for each player, then the rest of the board in structures then turn and next like state *)
type game = playerinfo*playerinfo*playerinfo*playerinfo*board*turn*next

(** Returns a cost where there is one of the resource specified, and zero of all others *)
let many_of_one_resource_cost (resource : resource) num: cost = 
  match resource with
    | Brick ->  ((1*num),0,0,0,0)
    | Wool ->   (0,(1*num),0,0,0)
    | Ore ->    (0,0,(1*num),0,0)
    | Grain ->  (0,0,0,(1*num),0)
    | Lumber -> (0,0,0,0,(1*num))

let handle_action g a:(game)=
  (*all functionality necessary for the RollDice action*)
  let roll_dice  ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) = 
    let numRolled = random_roll ()in
    let rec add_resources invT setAndHexLst dRoll =
      let rec resources_to_add s hList (b,w,o,g,l) dR =
        match hList with
          |(ter,r)::tl ->(if r = dR 
            then (
              match (resource_of_terrain ter) with
                | Some Brick -> resources_to_add s tl ((b+(settlement_num_resources s)),w,o,g,l) dR
                | Some Wool -> resources_to_add s tl (b,(w+(settlement_num_resources s)),o,g,l) dR
                | Some Ore -> resources_to_add s tl (b,w,(o+(settlement_num_resources s)),g,l) dR
                | Some Grain -> resources_to_add s tl (b,w,o,(g+(settlement_num_resources s)),l) dR
                | Some Lumber -> resources_to_add s tl (b,w,o,g,(l+(settlement_num_resources s))) dR 
                | None -> resources_to_add s tl (b,w,o,g,l) dR ) 
            else resources_to_add s tl (b,w,o,g,l) dR)
          |[]-> (b,w,o,g,l) in
      match setAndHexLst with
        |(set,pt,hexList)::tl-> add_resources (map_cost2 (fun x y-> x+y) invT (resources_to_add set hexList (0,0,0,0,0) dRoll)) tl dRoll
        |[]-> invT in
    let modify_turn turn roll= {turn with dicerolled = Some(roll)} in
    let robber_roll (p1,p2,p3,p4,b,t,(nCol,req)) = (p1,p2,p3,p4,b,t,(nCol,RobberRequest)) in
    let add_player_resources ((c,(inv,cards),t),(shLst,rLst)) r = ((c,((add_resources inv shLst r),cards),t),(shLst,rLst)) in
    let p1_info roll:(playerinfo)= add_player_resources p1 roll in
    let p2_info roll:(playerinfo)= add_player_resources p2 roll in
    let p3_info roll:(playerinfo)= add_player_resources p3 roll in
    let p4_info roll:(playerinfo)= add_player_resources p4 roll in
    match numRolled with
      | r -> (
        if r = cROBBER_ROLL 
        then robber_roll (p1,p2,p3,p4,b,(modify_turn t r),n) 
        else ((p1_info r),(p2_info r),(p3_info r),(p4_info) r,b,(modify_turn t r),n)) in
  
  (*all functionality necessary for the maritime trade action*)
  let maritime_trade_action ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),((hxLst,portList),s,deck,discard,robber),t,n) (sold, bought)= 
    let rec is_port pt pLst=
      match pLst with
        |((p1,p2),r,prtResrc)::tl->if pt = p1 || pt = p2 then Some(r,prtResrc) else is_port pt tl
        |[] -> None in
    let rec exchange_rate (settlementList:(settlement*point*hex list) list) ratio= 
      match settlementList with
        |(s,pt,h)::tl->(
          match is_port pt portList with
            |Some(r,PortResource(res))-> if r < ratio && res = bought then exchange_rate tl r else exchange_rate tl ratio
            |Some(r,Any)-> if r < ratio then exchange_rate tl r else exchange_rate tl ratio
            |None-> exchange_rate tl ratio
        )
        |[]-> ratio in
    let compute_inv inv ratio= 
      let enough_resources = num_resource_in_inventory inv sold > ratio in
      if enough_resources 
      then map_cost2 (fun x y -> x+y) (single_resource_cost bought) (map_cost2 (fun x y -> x-y) inv (many_of_one_resource_cost sold ratio))
      else inv in
    let apply_mtrade ((pc,(inv,cards),t),(shLst,rLst)) c =
        if pc = c
        then ((pc,((compute_inv inv (exchange_rate shLst cMARITIME_DEFAULT_RATIO)),cards),t),(shLst,rLst))
        else ((pc,(inv,cards),t),(shLst,rLst)) in
    let np1 = apply_mtrade p1 t.active in
    let np2 = apply_mtrade p2 t.active in
    let np3 = apply_mtrade p3 t.active in
    let np4 = apply_mtrade p4 t.active in
    (np1,np2,np3,np4,((hxLst,portList),s,deck,discard,robber),t,n) in

  (*all functionality necessary for the domestic trade action*)
  let domestic_trade_action ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(nCol,req)) dt= 
    failwith "not implementd" in

  (*all functionality necessary for the build action*)
  let build_action ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) b= 
    match b with
      | BuildRoad(r) -> failwith "not implemented"
      | BuildTown(pt) -> failwith "not implemented"
      | BuildCity(pt) -> failwith "not implemented"
      | BuildCard -> failwith "not implemented" in

  let play_card_action ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) pc =
    match pc with
      | PlayKnight(robMove) -> failwith "not implemented"
      | PlayRoadBuilding(rd,Some(rd2)) -> failwith "not implemented"
      | PlayYearOfPlenty(res,Some(res2)) -> failwith "not implemented"
      | PlayRoadBuilding(rd,None) -> failwith "not implemented"
      | PlayYearOfPlenty(res,None) -> failwith "not implemented"
      | PlayMonopoly(res) -> failwith "not implemented" in

  (*all functionality necessary for the EndTurn action*)
  let end_turn ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(nCol,req)) =
    let rec new_hand hand newCards =
      match (reveal newCards) with 
        |hd::tl->  new_hand (append_card hand hd) (wrap_reveal tl)
        |[]-> hand in
    let apply_new_hand ((pc,(inv,cards),t),(shLst,rLst)) c newCards = 
      if pc = c 
      then ((pc,(inv,(new_hand cards newCards)),t),(shLst,rLst)) 
      else ((pc,(inv,cards),t),(shLst,rLst)) in
    let np1 = apply_new_hand p1 t.active t.cardsbought in
    let np2 = apply_new_hand p2 t.active t.cardsbought in
    let np3 = apply_new_hand p3 t.active t.cardsbought in
    let np4 = apply_new_hand p4 t.active t.cardsbought in
    let newTurn = new_turn (next_turn t.active) in
    let newN = (next_turn t.active,req) in
    (np1,np2,np3,np4,b,newTurn,newN) in
    

  match a with
    | RollDice -> roll_dice g
    | MaritimeTrade(mt) -> maritime_trade_action g mt
    | DomesticTrade(dt) -> domestic_trade_action g dt
    | BuyBuild(b) -> build_action g b
    | PlayCard(pc) -> play_card_action g pc
    | EndTurn -> end_turn g
