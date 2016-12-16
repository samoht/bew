open Vdom
open Semantic_ui

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
    let author = Jsont.(mem o "author" string) in
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
    let dec o = `Ok { children = Jsont.get children o } in
    let enc t = Jsont.(new_obj c [memv children t.children]) in
    Jsont.view (dec, enc) c

  let reddit = data (children (Jsont.array (data Post.t)))

  let of_str s =
    let e = Jsont_codec.decoder (Js.string s) in
    let e = Jsont.decoder ~loc:true e reddit in
    match Jsont.decode e with
    | `Ok (_, v)    -> List.map (fun {data} -> data) v.data.children
    | `Await        -> assert false
    | `Error (_, e) ->
      let err = Jsont.error_to_string e in
      Printf.printf "%s\n" err;
      invalid_arg err

end

type model = {
  likes   : int;
  dislikes: int;
  elts    : [`Nothing | `Loading | `Data of Post.t list] ;
}

let init = return { likes = 0; dislikes = 0; elts = `Nothing  }

type msg = [
  | `Like
  | `Dislike
  | `Fetch
  | `Fetched of string
]

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

let view t: msg Vdom.vdom =
  let loading = match t.elts with `Loading -> true | _ -> false in
  let fetch = match t.elts with
    | `Loading | `Nothing -> [Icon.v `Users; text "Fetch"]
    | `Data l  -> [text (string_of_int (List.length l) ^ " posts")]
  in
  let posts = match t.elts with
    | `Nothing | `Loading -> []
    | `Data p -> p
  in
  let likes = text (string_of_int t.likes) in
  let dislikes = text (string_of_int t.dislikes) in
  div [

    Menu.fixed [text "Test"]
      ~logo:"https://www.docker.com/sites/all/themes/docker/assets/images/footer_moby_icon.png"
    ;

    Container.v [

      div ~a:[style "margin-top" "7em"] [];

      Segment.v [
        Container.v ~a:[`Center] [
          Button.labeled
            ~align:`Left
            ~label:(Button.Label.v ~color:`Red ~pointing:`Left likes)
            (Button.v `Like ~color:`Red [Icon.v (`Thumbs `Up)]);

          Button.labeled
            ~align:`Left
            ~label:(Button.Label.v ~color:`Blue ~pointing:`Left dislikes)
            (Button.v `Dislike ~color:`Blue ~basic:true
               ~hidden:[Icon.v `Cloud]
               [Icon.v (`Thumbs `Down)]);
        ];

        Container.v ~a:[`Center] [
          let s = t.likes - t.dislikes in
          Statistic.v
            ~value:[text (string_of_int s)]
            ~label:[text (match s with -1 | 0 | 1 -> "like" | _ -> "likes")];
        ];
      ];

      div ~a:[class_ "ui horizontal divider"] [text "r/ocaml"];

      Container.v ~a:[`Center] [
        Button.v `Fetch ~basic:true ~loading fetch;
      ];

      Container.v ~a:[`Text] [
        Feed.v (List.map (fun x ->
            let open Feed in
            { label = img "http://semantic-ui.com/images/avatar/small/justen.jpg"
            ; summary =
                { user   = text x.Post.author
                ; action = "posted on reddit"
                ; date   = "3 days ago" }
            ; text = [text x.Post.title]
            ; meta = Like.v (Random.int 10) }
          ) posts);
      ];
    ];
  ]

let app = app ~init ~view ~update ()

open Js_browser

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let () = Window.set_onload window run
