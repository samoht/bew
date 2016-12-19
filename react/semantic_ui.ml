open Tyxml_js.Html5
module R = Tyxml_js.R.Html5
module RL = ReactiveData.RList

let list h x = String.concat " " (h :: x)

let (++) x f = match x with
  | None   -> []
  | Some k -> [f k]

let (+++) x f = match x with
  | None   -> []
  | Some k -> f k

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

  let v t = i ~a:[a_class ["icon"; str t]] []
  let circular t = i ~a:[a_class ["circular"; "icon"; str t]] []

end


module Like = struct

  let v n = div ~a:[a_class ["like"]] [
      Icon.v `Like;
      pcdata (string_of_int n);
      pcdata (if n > 1 then " Likes" else " Like");
    ]
end

module Statistic = struct

  let v ~value ~label =
    div ~a:[a_class ["ui"; "statistic"]] [
      div ~a:[a_class ["value"]] value;
      div ~a:[a_class ["label"]] label;
    ]

end


module Label = struct

  type pointing = [
    | `Left
    | `Right
  ]

  let pointing_str = function
    | `Left  -> ["left"; "pointing"]
    | `Right -> ["right"; "pointing"]

  let v ?color ?pointing body =
    let color = color ++ Color.str in
    let pointing = pointing +++ pointing_str in
    let all = ["ui"; "basic"; "label"] @ color @ pointing in
    a ~a:[a_class all] [body]

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

  let v ?color ?(basic=false) ?kind ?hidden ?(loading=React.S.const false) ?state msg body =
    let color = color ++ Color.str in
    let basic = if basic then ["basic"] else [] in
    let kind = kind ++ kind_str in
    let animated = match hidden with None -> [] | Some _ -> ["animated"] in
    let state = state ++ state_str in
    let body = match hidden with
      | None        -> body
      | Some hidden ->
        [div ~a:[a_class ["visible"; "content"]] body;
         div ~a:[a_class ["hidden"; "content"]] hidden]
    in
    let all = "ui" :: "button" :: animated @ color @ basic @ kind @ state in
    let all =
      React.S.map (fun loading ->
          if loading then "loading" :: all else all
        ) loading
    in
    div ~a:[R.a_class all; a_tabindex 0; a_onclick msg] body

  type align = [ `Left | `Right ]

  let align_str = function `Left  -> "left" | `Right -> "right"

  let labeled ?align ?pointing ~label body =
    let align = align ++ align_str in
    let pointing = pointing +++ Label.pointing_str in
    let all = [ "ui"; "labeled"; "button"] @ align @ pointing in
    div ~a:[a_class all] [ body; label ]

  let icon t = button ~a:[a_class ["ui"; "icon"; "button"]] [Icon.v t]

  let labeled_icon ?align ~icon msg body =
    let align = align ++ align_str in
    let all = ["ui"; "labeled"; "icon"; "button"] @ align in
    div ~a:[a_class all; a_tabindex 0; a_onclick msg] [
      Icon.v icon;
      body;
    ]

  let buttons l = div ~a:[a_class ["ui"; "buttons"]] l

  let social ?(circular=false) msg s text =
    let circular = if circular then ["circular"] else [] in
    let all = "ui" :: "button" :: Social.str s :: circular in
    button ~a:[a_class all; a_tabindex 0; a_onclick msg] [
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

  let v ?(align=[]) body =
    let all = "ui" :: "main" :: "container" :: List.map attributes align in
    div ~a:[a_class all] body

end

module Feed = struct

  type 'a summary = {
    user  : 'a elt;
    action: string;
    date  : string;
  }

  let summary { user; action; date } =
    div ~a:[a_class ["summary"]] [
      a ~a:[a_class ["user"]] [user];
      pcdata " ";
      pcdata action;
      div ~a:[a_class ["date"]] [pcdata date];
    ]

  let meta m = div ~a:[a_class ["meta"]] [m]

  type 'a event = {
    label  : 'a elt;
    summary: 'a summary;
    text   : 'a elt list;
    meta   : 'a elt;
  }

  let event t =
    let text = match t.text with
      | [] -> [meta t.meta]
      | b  -> [div ~a:[a_class ["extra"; "text"]] b; meta t.meta]
    in
    div ~a:[a_class ["event"]] [
      div ~a:[a_class ["label"]] [t.label];
      div ~a:[a_class ["content"]] (summary t.summary :: text)
    ]

  let v events =
    R.div ~a:[a_class ["ui"; "feed"]]
      (RL.from_signal @@ React.S.map (List.map event) events)

end

module Menu = struct

  let fixed ?(inverted=false) ~logo items =
    let inverted = if inverted then ["inverted"] else [] in
    let all = "ui" :: "fixed" :: "menu" :: inverted in
    div ~a:[a_class all; a_style "z-index:10"] (
      div ~a:[a_class ["item"]] [img ~src:logo ~alt:"logo" ()]
      :: List.map (fun i -> a ~a:[a_class ["item"]] [i]) items
    )

  let footer ?(inverted=false) ?(vertical=false) body =
    let inverted = if inverted then ["inverted"] else [] in
    let vertical = if vertical then ["vertical"] else [] in
    let all = "ui" :: "footer" :: "segment" :: "menu" :: inverted @ vertical in
    div ~a:[a_class all] body

end

module Divider = struct

  let v = div ~a:[a_class ["ui"; "divider"]] []
  let horizontal elt = div ~a:[a_class ["ui"; "horizontal"; "divider"]] [elt];

end

module Segment = struct

  let v body = div ~a:[a_class ["ui"; "segment"]] body

end
