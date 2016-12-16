open Vdom

(** Components *)

module Button = struct

  type color = [ `Red | `Blue ]
  type icon = [ `Heart | `Fork ]

  let color = function `Red -> "red" | `Blue -> "blue"
  let icon = function `Heart -> "earth" | `Fork -> "fork"

  let labeled c i left right msg =
    div ~a:[class_ "ui labeled button"; str_prop "tabindex" "0"] [
      div ~a:[class_ ("ui button " ^ color c); onclick msg] [
        elt "i" ~a:[class_ ("icon " ^ icon i)] [];
        text left
      ];
      elt "a" ~a:[class_ ("ui basic left pointing label " ^ color c)]
        [text right]
    ]

end

module Comment = struct

  let v ?(avatar="/images/avatar/small/stevie.jpg") ~author body =
    div ~a:[class_ "ui comments"] [
      div ~a:[class_ "comment"] [
        elt "a" ~a:[class_ "avatar"] [
          elt "img" ~a:[str_prop "src" avatar] [];
        ];
        div ~a:[class_ "content"] [
          elt "a" ~a:[class_ "author"] [text author];
        ];
        div ~a:[class_ "text"] body;
      ]
    ]

end


module XHR = struct

  type 'msg Vdom.Cmd.t +=
    | Http_get of {url: string; payload: string; on_success: (string -> 'msg)}

  let http_get ~url ~payload on_success = Http_get {url; payload; on_success}

  let run_http_get ~url ~payload ~on_success () =
    let open Js_browser.XHR in
    let r = create () in
    open_ r "GET" url;
    set_onreadystatechange r
      (fun () ->
         match ready_state r with
         | Done -> on_success (response_text r)
         | _ ->
           ()
      );
    send r payload

  let cmd_handler ctx = function
    | Http_get {url; payload; on_success} ->
      run_http_get ~url ~payload
        ~on_success:(fun s -> Vdom_blit.Cmd.send_msg ctx (on_success s)) ();
      true
  | _ ->
      false

  let () = Vdom_blit.(register @@ cmd {Cmd.f = cmd_handler})

end

module Post = struct

  type t = {
    title: string;
    author: string;
  }

  let t =
    let o = Jsont.objc ~kind:"post" () in
    let title = Jsont.(mem o "title" string) in
    let author = Jsont.(mem o "title" string) in
    let c = Jsont.obj ~seal:true o in
    let dec o = `Ok { author = Jsont.get author o; title = Jsont.get title o } in
    let enc t = Jsont.(new_obj c [memv author t.author; memv title t.title]) in
    Jsont.view (dec, enc) c

end

module Posts = struct

  open Jsont

  type 'a data = {
    kind: string;
    data: 'a;
  }

  let data a =
    let o = Jsont.objc ~kind:"children" () in
    let data = Jsont.(mem o "data" a) in
    let kind = Jsont.(mem o "kind" string) in
    let c = Jsont.obj ~seal:true o in
    let dec o = `Ok { data = Jsont.get data o; kind = Jsont.get kind o } in
    let enc t = Jsont.(new_obj c [memv data t.data; memv kind t.kind]) in
    Jsont.view (dec, enc) c

  type 'a children = { children: 'a }

  let children a =
    let o = Jsont.objc ~kind:"data" () in
    let children = Jsont.(mem o "children" a) in
    let c = Jsont.obj ~seal:true o in
    let dec o = `Ok { children = [] (*Jsont.get children o*) } in
    let enc t = Jsont.(new_obj c [memv children t.children]) in
    Jsont.view (dec, enc) c

  let reddit = data (children (Jsont.array Post.t))

  let of_str s =
    let e = Jsont_codec.decoder (Js.string s) in
    let e = Jsont.decoder ~loc:true e reddit in
    match Jsont.decode e with
    | `Ok (_, v)    -> v.data.children
    | `Await        -> assert false
    | `Error (_, e) -> invalid_arg (Jsont.error_to_string e)

end

type model = {
  likes   : int;
  dislikes: int;
  elts    : [`Nothing | `Loading | `Data of Post.t list] ;
}

let init = return { likes = 0; dislikes = 0; elts = `Nothing  }

let update t = function
  | `Like    -> return { t with likes = t.likes + 1 }
  | `Dislike -> return { t with dislikes = t.dislikes + 1 }
  | `Fetch   ->
    return { t with elts = `Loading } ~c:[
      XHR.http_get ~url:"http://www.reddit.com/r/ocaml.json" ~payload:""
        (fun r -> `Fetched r)
    ]
  | `Fetched json ->
    let posts = Posts.of_str json in
    return { t with elts = `Data posts }

let view t =
  let icon = match t.elts with
    | `Nothing -> ""
    | `Loading -> "loading"
    | `Data _  -> ""
  in
  let posts = match t.elts with
    | `Nothing | `Loading -> []
    | `Data p -> p
  in
  let row =
    div [
      Button.labeled `Red `Heart "YES" (string_of_int t.likes) `Like;
      Button.labeled `Blue `Fork "NOO" (string_of_int t.dislikes) `Dislike;
      Button.labeled `Red `Heart "now" icon `Fetch;
(*
      div (List.map (fun x ->
          Comment.v ~author:x.Post.author [text x.Post.title]
        ) posts);
*)
    ]
  in
  div [
    div ~a:[class_ "ui statistic"] [
      div ~a:[class_ "value"] [text (string_of_int (t.likes - t.dislikes))];
      div ~a:[class_ "label"] [row]
    ]
  ]

let app = app ~init ~view ~update ()

open Js_browser

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let () = Window.set_onload window run
