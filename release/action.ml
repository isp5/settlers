open Definition
open Constant
open Util
open Move

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

let resource_list = [(1,0,0,0,0);(0,1,0,0,0);(0,0,1,0,0);(0,0,0,1,0);(0,0,0,0,1)]

let add_to_inventory ((c,(inv,cards),tr),(shLst,rLst)) cost =
  ((c,((map_cost2 (fun x y -> x+y) inv cost),cards),tr),(shLst,rLst))

let remove_from_inventory ((c,(inv,cards),tr),(shLst,rLst)) cost =
  ((c,((map_cost2 (fun x y -> x-y) inv cost),cards),tr),(shLst,rLst))

let num_resource_in_player ((c,(inv,cards),tr),(shLst,rLst)) res =
  num_resource_in_inventory inv res

let handle_action g a:(color option*game)=
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
    let add_player_resources ((c,(inv,cards),tr),(shLst,rLst)) r = ((c,((add_resources inv shLst r),cards),tr),(shLst,rLst)) in
    let p1_info roll:(playerinfo)= add_player_resources p1 roll in
    let p2_info roll:(playerinfo)= add_player_resources p2 roll in
    let p3_info roll:(playerinfo)= add_player_resources p3 roll in
    let p4_info roll:(playerinfo)= add_player_resources p4 roll in
    match numRolled with
      | r -> (
        if r = cROBBER_ROLL 
        then robber_roll (p1,p2,p3,p4,b,(modify_turn t r),n) 
        else ((p1_info r),(p2_info r),(p3_info r),(p4_info) r,b,(modify_turn t r),(t.active, ActionRequest))) in

  (*all functionality necessary for the EndTurn action*)
  let end_turn ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(nCol,req)) =
    let rec new_hand hand newCards =
      match (reveal newCards) with 
        |hd::tl->  new_hand (append_card hand hd) (wrap_reveal tl)
        |[]-> hand in
    let apply_new_hand ((pc,(inv,cards),tr),(shLst,rLst)) c newCards = 
      if pc = c 
      then ((pc,(inv,(new_hand cards newCards)),tr),(shLst,rLst)) 
      else ((pc,(inv,cards),tr),(shLst,rLst)) in
    let np1 = apply_new_hand p1 t.active t.cardsbought in
    let np2 = apply_new_hand p2 t.active t.cardsbought in
    let np3 = apply_new_hand p3 t.active t.cardsbought in
    let np4 = apply_new_hand p4 t.active t.cardsbought in
    let newTurn = new_turn (next_turn t.active) in
    let newN = (next_turn t.active,ActionRequest) in
    let game = (p1,p2,p3,p4,b,t,(nCol,req)) in
    if is_none t.dicerolled then roll_dice game else (np1,np2,np3,np4,b,newTurn,newN) in
  
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
      then Some(map_cost2 (fun x y -> x+y) (single_resource_cost bought) (map_cost2 (fun x y -> x-y) inv (many_of_one_resource_cost sold ratio)))
      else None in
    let apply_mtrade ((pc,(inv,cards),tr),(shLst,rLst)) c =
        if pc = c
        then (
          match compute_inv inv (exchange_rate shLst cMARITIME_DEFAULT_RATIO) with
            |Some(newInv)->Some((pc,(newInv,cards),tr),(shLst,rLst))
            |None -> None)
        else Some((pc,(inv,cards),tr),(shLst,rLst)) in
    let np1 = apply_mtrade p1 t.active in
    let np2 = apply_mtrade p2 t.active in
    let np3 = apply_mtrade p3 t.active in
    let np4 = apply_mtrade p4 t.active in
    let game = (p1,p2,p3,p4,((hxLst,portList),s,deck,discard,robber),t,n) in
    match (np1,np2,np3,np4) with
      |Some(a1),Some(a2),Some(a3),Some(a4)->(a1,a2,a3,a4,((hxLst,portList),s,deck,discard,robber),t,(t.active, ActionRequest)) 
      |_-> end_turn game in

  (*all functionality necessary for the domestic trade action*)
  let domestic_trade_action ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) (color,give,ask)= 
    let compute_trade ((pc,(inv,cards),tr),(shLst,rLst)) cActive cTrade =
      let greater_then_zero (b,w,o,g,l) = b>=0 && w>=0 && o>=0 && g>=0 && l>=0 in
      let a_enough_resources = greater_then_zero (map_cost2 (fun x y -> x-y) inv give) in
      let t_enough_resources = greater_then_zero (map_cost2 (fun x y -> x-y) inv ask) in
      match (pc = cActive, pc = cTrade) with
        |(true,false)-> (
          if a_enough_resources then Some(give) else None)
        |(false,true)->(
          if t_enough_resources then Some(ask) else None)
        |(false,false) -> None
        |(true, true) -> None in
    let modify_turn turn = {turn with pendingtrade = Some(color,give,ask)} in
    let rec choose_players pList cActive (pGive, pAsk) = 
      match pList with
        |((pc,(inv,cards),tr),(shLst,rLst))::tl->(
          let player = ((pc,(inv,cards),tr),(shLst,rLst)) in
          if pc = cActive then choose_players tl cActive (player,pAsk) else(
            if pc = color then choose_players tl cActive (pGive,player) else choose_players tl cActive (pGive,pAsk)))
        |[]-> (pGive,pGive) in
    let tGive,tAsk = choose_players (p1::p2::p3::[p4]) t.active (p1,p1) in
    let game = (p1,p2,p3,p4,b,t,n) in
    if t.tradesmade <=cNUM_TRADES_PER_TURN then(
      match (compute_trade tGive t.active color),(compute_trade tAsk t.active color) with
        |Some(gp),Some(ap)->(p1,p2,p3,p4,b,(modify_turn t),(color,TradeRequest)) 
        |_-> end_turn game)
    else end_turn game in

  (*all functionality necessary for the build action*)
  let build_action ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) b= 
    match b with
      | BuildRoad(r) -> failwith "not implemented"
      | BuildTown(pt) -> failwith "not implemented"
      | BuildCity(pt) -> failwith "not implemented"
      | BuildCard -> failwith "not implemented" in

  let valid_card ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),((hxLst,portList),s,deck,discard,robber),t,n) myCard= 
        let compute_hand hand card =
          List.fold_left (fun (valid,newHand) x -> if x = card then (true,newHand) else (valid,(x::newHand))) (false,[]) hand in
        let players_hands ((pc,(inv,cards),tr),(shLst,rLst)) =
          if pc = t.active
          then (
            match compute_hand (reveal cards) myCard with
              |true,clist->Some((pc,(inv,(wrap_reveal clist)),tr),(shLst,rLst))
              |false,_-> None)
          else Some((pc,(inv,cards),tr),(shLst,rLst)) in
      let np1 = players_hands p1 in
      let np2 = players_hands p2 in
      let np3 = players_hands p3 in
      let np4 = players_hands p4 in
      let game = (p1,p2,p3,p4,((hxLst,portList),s,deck,discard,robber),t,n) in
      match np1,np2,np3,np4 with
        |Some(a1),Some(a2),Some(a3),Some(a4)-> (true,(a1,a2,a3,a4,((hxLst,portList),s,deck,(myCard::discard),robber),t,n))
        |_-> (false,game) in
  
  let play_monopoly ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) res =
    let rescource_gain = (num_resource_in_player p1 res) + (num_resource_in_player p2 res) + (num_resource_in_player p3 res) + (num_resource_in_player p4 res) in
    let compute_new_invetory ((pc,(inv,cards),tr),(shLst,rLst)) rGain= 
      let player = ((pc,(inv,cards),tr),(shLst,rLst)) in
      if pc = t.active
      then add_to_inventory player (many_of_one_resource_cost res (rGain-(num_resource_in_player player res)))
      else remove_from_inventory player (many_of_one_resource_cost res (num_resource_in_player player res)) in
    let np1 = compute_new_invetory p1 rescource_gain in
    let np2 = compute_new_invetory p2 rescource_gain in
    let np3 = compute_new_invetory p3 rescource_gain in
    let np4 = compute_new_invetory p4 rescource_gain in
    (np1,np2,np3,np4,b,t,n) in

  let play_year_of_plenty ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) res1 resOption =
    let compute_inv ((pc,(inv,cards),tr),(shLst,rLst)) = 
      let player = ((pc,(inv,cards),tr),(shLst,rLst)) in
      if pc = t.active
      then (
        match resOption with
          |Some(res2)-> add_to_inventory player (map_cost2 (fun x y -> x+y) (single_resource_cost res1) (single_resource_cost res2))
          |None-> add_to_inventory player (single_resource_cost res1))
      else player in
    let np1 = compute_inv p1 in
    let np2 = compute_inv p2 in
    let np3 = compute_inv p3 in
    let np4 = compute_inv p4 in
    (np1,np2,np3,np4,b,t,n) in

  let play_knight ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) (piece,colOpt) resCost=
    let game = (p1,p2,p3,p4,b,t,n) in
    let (hxLst,portList),(inters,rds),deck,discard,robber = b in
    let (validColor,colorsToSteal) =  check_target (piece_corners piece) inters colOpt t.active false false in
    let increment_knights ((pc,(inv,cards),(k,lr,la)),(shLst,rLst)) = (pc,(inv,cards),((k+1),lr,la)),(shLst,rLst) in
    let steal player = 
      let (pc,(inv,cards),tr),(shLst,rLst) = player in
      if pc = t.active then increment_knights (add_to_inventory player resCost)
      else if is_none colOpt then player 
      else if pc = (get_some colOpt) then remove_from_inventory player resCost
      else player in
    let np1 = steal p1 in
    let np2 = steal p2 in
    let np3 = steal p3 in
    let np4 = steal p4 in
    (* need to figure out how to factor my color into this*)
    let new_board = (hxLst,portList),(inters,rds),deck,discard,piece in
    if piece <= cMAX_PIECE_NUM && piece >= cMIN_PIECE_NUM
    then (if validColor || (is_none colOpt && not colorsToSteal) then (np1,np2,np3,np4,new_board,t,n) else end_turn game)
    else end_turn game in

  let play_road_building ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) rd1 rdOption =
    let game = (p1,p2,p3,p4,b,t,n) in
    end_turn game in

  let play_card_action game playedCard =
    match playedCard with
      | PlayKnight(robMove) -> play_knight game robMove (fst(pick_one resource_list))
      | PlayRoadBuilding(rd,rdOption) -> play_road_building game rd rdOption
      | PlayYearOfPlenty(res,resOption) -> play_year_of_plenty game res resOption
      | PlayMonopoly(res) -> play_monopoly game res in

  let calculate_victory ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),(map,(inters,rds),deck,discard,robber),t,n) =
    let (pc1,(inv1,cards1),(k1,lr1,la1)),(shLst1,rLst1) = p1 in
    let (pc2,(inv2,cards2),(k2,lr2,la2)),(shLst2,rLst2) = p2 in
    let (pc3,(inv3,cards3),(k3,lr3,la3)),(shLst3,rLst3) = p3 in
    let (pc4,(inv4,cards4),(k4,lr4,la4)),(shLst4,rLst4) = p4 in
    let hasLA= 
      match la1, la2, la3, la4 with
        |true,false,false,false-> Some(k1,pc1)
        |false,true,false,false-> Some(k2,pc2)
        |false,false,true,false-> Some(k3,pc3)
        |false,false,false,true-> Some(k4,pc4)
        |false,false,false,false-> None
        |_->failwith "multiple people have largest army trophy" in
     let hasLR =
      match lr1, lr2, lr3, lr4 with
        |true,false,false,false-> Some((longest_road pc1 rLst1 inters),pc1)
        |false,true,false,false-> Some((longest_road pc2 rLst2 inters),pc2)
        |false,false,true,false-> Some((longest_road pc3 rLst3 inters),pc3)
        |false,false,false,true-> Some((longest_road pc4 rLst4 inters),pc4)
        |false,false,false,false-> None
        |_->failwith "multiple people have longest road trophy" in
    let check_longest_army p activeK =
      let (pc,(inv,cards),(k,lr,la)),(shLst,rLst) = p in
      let kT,cT = get_some hasLA in
      if is_none hasLA then (if k >= cMIN_LARGEST_ARMY then (pc,(inv,cards),(k,lr,true)),(shLst,rLst) else p)
      else if k > kT then (pc,(inv,cards),(k,lr,true)),(shLst,rLst) 
      else if pc = cT && k<activeK then (pc,(inv,cards),(k,lr,false)),(shLst,rLst) else p in
      
    let check_longest_road p activeRd =
      let (pc,(inv,cards),(k,lr,la)),(shLst,rLst) = p in
      let laT,cT = get_some hasLR in
      let myLR = longest_road pc rLst inters in
      if is_none hasLR then (if  myLR >= cMIN_LONGEST_ROAD then (pc,(inv,cards),(k,true,la)),(shLst,rLst) else p)
      else if myLR > laT then (pc,(inv,cards),(k,lr,true)),(shLst,rLst)
      else if pc = cT && myLR<activeRd then (pc,(inv,cards),(k,false,la)),(shLst,rLst) else p in
      
    let calculate_player_victory_pts player =
      let (pc,(inv,cards),(k,lr,la)),(shLst,rLst) = player in
      let rec settlement_vpoints sList res=
        let pts s = match s with Town -> cVP_TOWN | City -> cVP_CITY in
        match sList with
          |(s,p,h)::tl -> settlement_vpoints tl ((pts s)+res)
          |[]->res in
      let rec cards_vpionts crds res = 
        match crds with
         |hd::tl ->(
           match hd with
             |VictoryPoint-> cards_vpionts tl (res+cNUM_VICTORYPOINT)
             |_-> cards_vpionts tl res)
         |[]->res in
      let trophy_vpoints = 
        if lr && la then (cVP_LONGEST_ROAD + cVP_LARGEST_ARMY)
        else if lr then cVP_LARGEST_ARMY
        else if la then cVP_LONGEST_ROAD
        else 0 in
      if ((settlement_vpoints shLst 0) + (cards_vpionts (reveal cards) 0) + trophy_vpoints) >= cWIN_CONDITION
      then Some(pc)
      else None in
    let np1 = check_longest_road (check_longest_army p1 k1) (longest_road pc1 rLst1 inters) in
    let np2 = check_longest_road (check_longest_army p2 k2) (longest_road pc2 rLst2 inters) in
    let np3 = check_longest_road (check_longest_army p3 k3) (longest_road pc3 rLst3 inters) in
    let np4 = check_longest_road (check_longest_army p4 k4) (longest_road pc4 rLst4 inters) in
    let game = (np1,np2,np3,np4,(map,(inters,rds),deck,discard,robber),t,n) in
    match (pc1 = t.active,pc2 = t.active,pc3 = t.active,pc4 = t.active) with
      |true,false,false,false-> (calculate_player_victory_pts np1),game
      |false,true,false,false-> (calculate_player_victory_pts np2),game
      |false,false,true,false-> (calculate_player_victory_pts np3),game
      |false,false,false,true-> (calculate_player_victory_pts np4),game 
      |_-> failwith "No active player" in

  match a with
    | RollDice -> calculate_victory (roll_dice g)
    | MaritimeTrade(mt) -> calculate_victory (maritime_trade_action g mt)
    | DomesticTrade(dt) -> calculate_victory (domestic_trade_action g dt)
    | BuyBuild(b) -> calculate_victory (build_action g b)
    | PlayCard(pc) ->(
      let valid,game = valid_card g (card_of_playcard pc) in
      if valid then calculate_victory (play_card_action game pc) else calculate_victory (end_turn game))
    | EndTurn -> calculate_victory (end_turn g)
