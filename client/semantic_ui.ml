open Vdom

let list h x = String.concat " " (h :: x)

let (++) x f = match x with
  | None   -> []
  | Some k -> [f k]

(* FIXME: implement tyxml over vdom *)
let img src = elt "img" ~a:[str_prop "src" src] []

module Color = struct

  type t = [
    | `Red
    | `Orange
    | `Yellow
    | `Olive
    | `Green
    | `Teal
    | `Blue
    | `Violet
    | `Purple
    | `Pink
    | `Brown
    | `Grey
    | `Black
  ]

  let str: t -> string = function
    | `Red    -> "red"
    | `Orange -> "orange"
    | `Yellow -> "yellow"
    | `Olive  -> "olive"
    | `Green  -> "green"
    | `Teal   -> "teal"
    | `Blue   -> "blue"
    | `Violet -> "violet"
    | `Purple -> "purple"
    | `Pink   -> "pink"
    | `Brown  -> "brown"
    | `Grey   -> "grey"
    | `Black  -> "black"

end

module Social = struct

  type t = [
    | `Facebook
    | `Twitter
    | `Google_plus
    | `VK
    | `Linkedin
    | `Instagram
    | `Youtube
  ]

  let str: t -> string = function
    | `Facebook    -> "facebook"
    | `Twitter     -> "twitter"
    | `Google_plus -> "google plus"
    | `VK          -> "vk"
    | `Linkedin    -> "linkedin"
    | `Instagram   -> "instagram"
    | `Youtube     -> "youtube"

end

module Icon = struct

  type t = [
    | `Arrow of [`Right | `Left]
    | `Cloud
    | `Fork
    | `Heart
    | `Like
    | `Next
    | `Pause
    | `Shop
    | `Users
    | `Thumbs of [`Up | `Down]
    | `Colored of Color.t * t
    | `Inverted of t
    | Social.t
  ]

  let rec str: t -> string = function
    | `Arrow `Right -> "right arrow"
    | `Arrow `Left  -> "left arrow"
    | `Cloud -> "cloud"
    | `Fork  -> "fork"
    | `Like  -> "like"
    | `Heart -> "heart"
    | `Next  -> "next"
    | `Pause -> "pause"
    | `Shop  -> "shop"
    | `Users -> "users"
    | `Thumbs `Down -> "thumbs outline down"
    | `Thumbs `Up   -> "thumbs outline up"
    | `Colored (c, t) -> Color.str c ^ " " ^ str t
    | `Inverted t -> "inverted " ^ str t
    | #Social.t as s -> Social.str s

  let v t = elt "i" ~a:[class_ (list "icon" [str t])] []
  let circular t = elt "i" ~a:[class_ (list "circular icon" [str t])] []

end


module Like = struct

  let v n = elt "a" ~a:[class_ "like"] [
      Icon.v `Like;
      text (string_of_int n);
      text (if n > 1 then " Likes" else " Like");
    ]
end

module Statistic = struct

  let v ~value ~label =
    div ~a:[class_ "ui statistic"] [
      div ~a:[class_ "value"] value;
      div ~a:[class_ "label"] label;
    ]

end

module Button = struct

  type kind = [
    | `Primary
    | `Secondary
    | `Positive
    | `Negative
  ]

  let kind_str: kind -> string = function
    | `Primary   -> "primary"
    | `Secondary -> "secondary"
    | `Positive  -> "positive"
    | `Negative  -> "negative"

  type state = [
    | `Active
    | `Disabled
  ]

  let state_str: state -> string = function
    | `Active    -> "active"
    | `Disabled  -> "disabled"

  let v ?color ?(basic=false) ?kind ?hidden ?(loading=false) ?state msg body =
    let color = color ++ Color.str in
    let basic = if basic then ["basic"] else [] in
    let kind = kind ++ kind_str in
    let animated = match hidden with None -> [] | Some _ -> ["animated"] in
    let loading = if loading then ["loading"] else [] in
    let state = state ++ state_str in
    let body = match hidden with
      | None        -> body
      | Some hidden ->
        [div ~a:[class_ "visible content"] body;
         div ~a:[class_ "hidden content"] hidden]
    in
    let all = animated @ color @ basic @ kind @ loading @ state in
    elt "button" ~a:[class_ (list "ui button" all);
                     onclick msg]
      body

  module Label = struct

    let v ?color ?pointing body =
      let color = color ++ Color.str in
      let pointing = pointing ++ function
          | `Left  -> "left pointing"
          | `Right -> "right pointing"
      in
      elt "a" ~a:[class_ (list "ui basic label" (color @ pointing))] [body]

  end

  type align = [ `Left | `Right ]

  let align_str = function `Left  -> "left" | `Right -> "right"

  let labeled ?align ?pointing ~label body =
    let align = align ++ align_str in
    div ~a:[class_ (list "ui labeled button" align)] [ body; label ]

  let icon t = elt "button" ~a:[class_ "ui icon button"] [Icon.v t]

  let labeled_icon ?align ~icon msg body =
    let align = align ++ align_str in
    div ~a:[class_ (list "ui labeled icon button" align);
            str_prop "tabindex" "0";
            onclick msg] [
      Icon.v icon;
      body;
    ]

  let buttons l = div ~a:[class_ "ui buttons"] l

  let social ?(circular=false) msg s text =
    let circular = if circular then ["circular"] else [] in
    elt "button" ~a:[class_ (list "ui button" @@ Social.str s :: circular);
                     str_prop "tabindex" "0";
                     onclick msg] [
      Icon.v (s :> Icon.t);
      text
    ]

end

module Container = struct

  type attributes = [
    | `Text
    | `Left
    | `Center
    | `Right
    | `Justified
    | `Fluid ]

  let attributes = function
    | `Text       -> "text"
    | `Left       -> "left aligned"
    | `Center     -> "center aligned"
    | `Right      -> "right aligned"
    | `Justified  -> "justified"
    | `Fluid      -> "fluid"

  let v ?(a=[]) body =
    let c = list "ui main container" (List.map attributes a) in
    div ~a:[class_ c] body

end

module Feed = struct

  type 'a summary = {
    user  : 'a Vdom.vdom;
    action: string;
    date  : string;
  }

  let summary { user; action; date } =
    div ~a:[class_ "summary"] [
      elt "a" ~a:[class_ "user"] [user];
      text " ";
      text action;
      div ~a:[class_ "date"] [text date];
    ]

  type 'a meta = { meta: 'a Vdom.vdom }

  let meta { meta } = div ~a:[class_ "meta"] [meta]

  type 'a event = {
    id     : int;
    label  : 'a Vdom.vdom;
    summary: 'a summary;
    text   : 'a Vdom.vdom list;
    meta   : 'a Vdom.vdom;
  }

  let event t =
    let text = match t.text with
      | [] -> [meta {meta = t.meta}]
      | b  -> [div ~a:[class_ "extra text"] b; meta {meta = t.meta}]
    in
    div ~a:[class_ "event"] [
      div ~a:[class_ "label"] [t.label];
      div ~a:[class_ "content"] (summary t.summary :: text)
    ]

  let memo_event e = memo ~key:(string_of_int e.id) event e

  let v events = div ~a:[class_ "ui feed"] (List.map memo_event events)

end

module Menu = struct

  let fixed ?(inverted=false) ~logo items =
    let inverted = if inverted then ["inverted"] else [] in
    div ~a:[class_ (list "ui fixed menu" inverted); style "z-index" "10"] (
      div ~a:[class_ "item"] [img logo]
      :: List.map (fun i -> elt "a" ~a:[class_ "item"] [i]) items
    )

  let footer ?(inverted=false) ?(vertical=false) body =
    let inverted = if inverted then ["inverted"] else [] in
    let vertical = if vertical then ["vertical"] else [] in
    div ~a:[class_ (list "ui footer segment menu" @@ inverted @ vertical)] body

end

module Segment = struct

  let v body = div ~a:[class_ "ui segment"] body

end
