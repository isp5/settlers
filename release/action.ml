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

let handle_action g a:(game)=
  (*all functionality necessary for the RollDice action*)
  let rollD  ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,n) = 
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
        |(set,hexList)::tl-> add_resources (map_cost2 (fun x y-> x+y) invT (resources_to_add set hexList (0,0,0,0,0) dRoll)) tl dRoll
        |[]-> invT in
    let modifyTurn turn roll= {turn with dicerolled = Some(roll)} in
    let robberRoll (p1,p2,p3,p4,b,t,(nCol,req)) = (p1,p2,p3,p4,b,t,(nCol,RobberRequest)) in
    let add_player_resources ((c,(inv,cards),t),(shLst,pt,rLst)) r = ((c,((add_resources inv shLst r),cards),t),(shLst,rLst)) in
    let p1_info roll:(playerinfo)= add_player_resources p1 roll in
    let p2_info roll:(playerinfo)= add_player_resources p2 roll in
    let p3_info roll:(playerinfo)= add_player_resources p3 roll in
    let p4_info roll:(playerinfo)= add_player_resources p4 roll in
    match numRolled with
      | r -> (
        if r = cROBBER_ROLL 
        then robberRoll (p1,p2,p3,p4,b,(modifyTurn t r),n) 
        else ((p1_info r),(p2_info r),(p3_info r),(p4_info) r,b,(modifyTurn t r),n)) in
  
  (*all functionality necessary for the build maritime trade*)
  let maritimeTradeAction ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),((hxLst,portList),s,deck,discard,robber),t,(nCol,req)) (sold, bought)= 
    let rec isPort pt pLst=
      match pLst with
        |((p1,p2),r,prtResrc)::tl->if pt = p1 or pt = p2 then Some(r,prtResrc) else isPort pt tl
        |[] -> None

    let rec exchangeRate settlementList ratio= 
      match settlementList with
        |(s,pt,h)::tl->(
          match isPort pt portList with
            |Some(r,PortResource(res))-> if r < ratio and res = bought then exchangeRate tl r else exchangeRate tl ratio
            |Some(r,Any)-> if r < ratio then exchangeRate tl r else exchangeRate tl ratio
            |None-> exchangeRate tl ratio
        )
        |[]-> ratio
    let computeInv inv newInv=

  (*all functionality necessary for the build maritime trade*)
  let domesticTradeAction ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(nCol,req)) dt= 

  (*all functionality necessary for the build action*)
  let buildAction ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(nCol,req)) b= 
    match b with
      | BuildRoad(r) -> failwith "not implemented"
      | BuildTown(pt) -> failwith "not implemented"
      | BuildCity(pt) -> failwith "not implemented"
      | BuildCard -> failwith "not implemented" in

  let playCardAction ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(nCol,req)) pc =
    match pc with
      | PlayKnight(robMove) -> failwith "not implemented"
      | PlayRoadBuilding(rd,Some(rd2)) -> failwith "not implemented"
      | PlayYearOfPlenty(res,Some(res2)) -> failwith "not implemented"
      | PlayRoadBuilding(rd,None) -> failwith "not implemented"
      | PlayYearOfPlenty(res,None) -> failwith "not implemented"
      | PlayMonopoly(res) -> failwith "not implemented" in

  (*all functionality necessary for the EndTurn action*)
  let endTurn ((p1:playerinfo),(p2:playerinfo),(p3:playerinfo),(p4:playerinfo),b,t,(nCol,req)) =
    let rec newHand hand newCards =
      match (reveal newCards) with 
        |hd::tl->  newHand (append_card hand hd) (wrap_reveal tl)
        |[]-> hand in
    let applyNewHand ((pc,(inv,cards),t),(shLst,pt,rLst)) c newCards = 
      if pc = c 
      then ((pc,(inv,(newHand cards newCards)),t),(shLst,rLst)) 
      else ((pc,(inv,cards),t),(shLst,rLst)) in
    let np1 = applyNewHand p1 t.active t.cardsbought in
    let np2 = applyNewHand p2 t.active t.cardsbought in
    let np3 = applyNewHand p3 t.active t.cardsbought in
    let np4 = applyNewHand p4 t.active t.cardsbought in
    let newTurn = new_turn (next_turn t.active) in
    let newN = (next_turn t.active,req) in
    (np1,np2,np3,np4,b,newTurn,newN) in
    

  match a with
    | RollDice -> rollD g
    | MaritimeTrade(mt) -> maritimeTradeAction g mt
    | DomesticTrade(dt) -> failwith "not implemented"
    | BuyBuild(b) -> buildAction g b
    | PlayCard(pc) -> playCardAction g pc
    | EndTurn -> endTurn g
