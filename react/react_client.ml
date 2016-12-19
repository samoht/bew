open React
open Lwt.Infix
open Semantic_ui

module XHR = struct

  type 'a t = {
    v  : [`Nothing | `Loading | `Data of 'a] S.t;
    get: string -> (string -> 'a) -> unit;
  }

  let v () =
    let current, set_current = S.create `Nothing in
    let get url f =
      set_current `Loading;
      Lwt.async (fun () ->
          XmlHttpRequest.get url >|= fun r ->
          let d = f r.XmlHttpRequest.content in
          set_current (`Data d)
        )
    in
    { v = current; get }

  let get t = t.get
  let map f t = S.map f t.v

end

module Counter = struct

  type t = { v: int S.t; incr: unit -> unit }

  let v () =
    let current, set_current = S.create 0 in
    { v = current; incr = fun () -> set_current (succ (S.value current)) }

  let incr t = t.incr ()
  let map f t = S.map f t.v

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
  likes   : Counter.t;
  dislikes: Counter.t;
  elts    : Post.t list XHR.t;
}

let init = { likes = Counter.v (); dislikes = Counter.v (); elts = XHR.v () }

let update t = function
  | `Like    -> Counter.incr t.likes
  | `Dislike -> Counter.incr t.dislikes
  | `Fetch   -> XHR.get t.elts "https://www.reddit.com/r/ocaml.json" Posts.of_str

let onclick t x _ev = update t x; true

let view t =
  let module R = Tyxml_js.R.Html5 in
  let module RL = ReactiveData.RList in
  let open Tyxml_js.Html5 in
  let loading = XHR.map (function `Loading -> true | _ -> false) t.elts in
  let fetch =
    XHR.map (function
        | `Loading | `Nothing -> [Icon.v `Users; pcdata "Fetch"]
        | `Data l  -> [pcdata (string_of_int (List.length l) ^ " posts")]
      ) t.elts
    |> RL.from_signal
    |> R.div
  in
  let posts = XHR.map (function
      | `Nothing | `Loading -> []
      | `Data p -> p
    ) t.elts
  in
  let likes = R.pcdata (Counter.map string_of_int t.likes) in
  let dislikes = R.pcdata (Counter.map string_of_int t.dislikes) in
  div [

    Menu.fixed [pcdata "Test"]
      ~logo:"https://www.docker.com/sites/all/themes/docker/assets/images/footer_moby_icon.png"
    ;

    Container.v [

      div ~a:[a_style "margin-top:7em"] [];

      Segment.v [
        Container.v ~align:[`Center] [
          Button.labeled
            ~align:`Left
            ~label:(Label.v ~color:`Red ~pointing:`Left likes)
            (Button.v (onclick t `Like) ~color:`Red [Icon.v (`Thumbs `Up)]);

          Button.labeled
            ~align:`Left
            ~label:(Label.v ~color:`Blue ~pointing:`Left dislikes)
            (Button.v (onclick t `Dislike) ~color:`Blue ~basic:true
               ~hidden:[Icon.v `Cloud]
               [Icon.v (`Thumbs `Down)]);
        ];

        Container.v ~align:[`Center] [
          let s = S.l2 (-) t.likes.Counter.v t.dislikes.Counter.v in
          Statistic.v
            ~value:[R.pcdata (S.map string_of_int s)]
            ~label:[R.pcdata
                      (S.map (function -1 | 0 | 1 -> "like" | _ -> "likes") s)];
        ];
      ];

      div ~a:[a_class ["ui"; "horizontal"; "divider"]] [pcdata "r/ocaml"];

      Container.v ~align:[`Center] [
        (* FIXME: how can we lift [loading]? *)
        Button.v (onclick t `Fetch) ~basic:true ~loading [fetch]
      ];

      Container.v ~align:[`Text] [
        Feed.v (S.map (fun posts -> List.map (fun x ->
            let open Feed in
            { label = img
                  ~src:"http://semantic-ui.com/images/avatar/small/justen.jpg"
                  ~alt:"avatar" ()
            ; summary =
                { user   = pcdata x.Post.author
                ; action = "posted on reddit"
                ; date   = "3 days ago" }
            ; text = [pcdata x.Post.title]
            ; meta = Like.v (Random.int 10) }
          ) posts) posts)
      ]
    ];
  ]

let onload _ =
  let main_div = Tyxml_js.To_dom.of_node (view init) in
  Dom_html.document##.body##appendChild(main_div)
  |> fun _ -> Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload
