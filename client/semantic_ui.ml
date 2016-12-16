open Vdom

let list h x = String.concat " " (h :: x)

let (++) x f = match x with
  | None   -> []
  | Some k -> [f k]

(* FIXME: implement tyxml over vdom *)
let img src = elt "img" ~a:[str_prop "src" src] []

module Color = struct

  type t = [ `Red | `Blue ]

  let str: t -> string = function
    | `Red  -> "red"
    | `Blue -> "blue"

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
    | Social.t
  ]

  let str: t -> string = function
    | `Arrow `Right -> "right arrow"
    | `Arrow `Left  -> "left arrow"
    | `Cloud -> "cloud"
    | `Fork  -> "fork"
    | `Like  -> "like"
    | `Heart -> "earth"
    | `Next  -> "next"
    | `Pause -> "pause"
    | `Shop  -> "shop"
    | #Social.t as s -> Social.str s

  let v t = elt "i" ~a:[class_ (list "icon" [str t])] []

end


module Like = struct

  let v n = elt "a" ~a:[class_ "like"] [
      Icon.v `Like;
      text (string_of_int n);
      text (if n > 1 then "Likes" else "Like");
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
      | None        -> [body]
      | Some hidden ->
        [div ~a:[class_ "visible content"] [body];
         div ~a:[class_ "hidden content"] [hidden]]
    in
    let all = animated @ color @ basic @ kind @ loading @ state in
    elt "button" ~a:[class_ (list "ui button" all);
                     str_prop "tabindex" "0";
                     onclick msg]
      body

  module Label = struct

    let v ?color ?pointing body =
      let color = color ++ Color.str in
      let pointing = pointing ++ function
          | `Left  -> "pointing left"
          | `Right -> "pointing right"
      in
      elt "a" ~a:[class_ (list "ui basic" (color @ pointing))] [body]

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
    let c = list "ui container" (List.map attributes a) in
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
      text action;
      div ~a:[class_ "date"] [text date];
    ]

  type 'a meta = { meta: 'a Vdom.vdom }

  let meta { meta } = div ~a:[class_ "meta"] [meta]

  type 'a event = {
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
      div ~a:[class_ "contents"] (summary t.summary :: text)
    ]

  let v events = div ~a:[class_ "ui feed"] (List.map event events)

end
