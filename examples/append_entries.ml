open Core.Std
open Async.Std

module Stream_node = Scow_transport_stream_server.Node

module Elt = struct
  type t = int
  let compare = Int.compare
  let to_string = Int.to_string
  let of_string str = Option.try_with (fun () -> Int.of_string str)
end

module Statem = struct
  type op = Elt.t
  type ret = unit
  type t = Int.Set.t ref

  let create () = ref Int.Set.empty

  let apply t op =
    t := Set.add !t op;
    Deferred.unit
end

module Json_codec = Scow_transport_stream_codec_json.Make(Elt)
module Log = Scow_log_memory.Make(Elt)
module Transport = Scow_transport_stream.Make(Json_codec)
module Store = Scow_store_memory.Make(Transport.Node)

module Scow = Scow.Make(Statem)(Log)(Store)(Transport)

module Scow_inst = struct
  type t = { scow   : Scow.t
           ; statem : Statem.t
           }
end

let start_transport me =
  Transport.start ~me
  >>= function
    | Ok transport -> Deferred.return transport
    | Error _      -> failwith "nyi"

let create_scow nodes me =
  start_transport me
  >>= fun transport ->
  let log = Log.create () in
  let store = Store.create () in
  let statem = Statem.create () in
  let module Ia = Scow.Init_args in
  let init_args = { Ia.me           = me
                  ;    nodes        = nodes
                  ;    statem       = statem
                  ;    transport    = transport
                  ;    log          = log
                  ;    store        = store
                  ;    timeout      = sec 1.0
                  ;    timeout_rand = sec 2.0
                  }
  in
  Scow.start init_args
  >>= function
    | Ok scow -> Deferred.return Scow_inst.({scow; statem})
    | Error _ -> failwith "nyi"

let string_of_statem statem =
  String.concat
    ~sep:", "
    (List.map ~f:Int.to_string (List.sort ~cmp:Int.compare (Set.to_list !statem)))

let print_statem_info scow_insts () =
  let print scow_inst =
    Scow.me scow_inst.Scow_inst.scow
    >>=? fun me ->
    Scow.leader scow_inst.Scow_inst.scow
    >>=? fun leader_opt ->
    let leader = Option.value (Option.map ~f:Transport.Node.to_string leader_opt) ~default:"Unknown" in
    printf "%s: %s [%s]\n%!" (Transport.Node.to_string me) leader (string_of_statem scow_inst.Scow_inst.statem);
    Deferred.return (Ok ())
  in
  Deferred.List.iter
    ~f:(fun scow_inst ->
      print scow_inst
      >>= fun _ ->
      Deferred.unit)
    scow_insts
  >>= fun _ ->
  printf "---\n";
  Deferred.unit

let append_entry next_val scow_insts () =
  let print scow_inst =
    Scow.me scow_inst.Scow_inst.scow
    >>=? fun me ->
    Scow.append_log
      scow_inst.Scow_inst.scow
      !next_val
  in
  incr next_val;
  Deferred.List.iter
    ~f:(fun scow_inst ->
      print scow_inst
      >>= fun _ ->
      Deferred.unit)
    scow_insts

let rec create_nodes = function
  | 0 -> []
  | n ->
    let node_opt = Stream_node.create ~host:"localhost" ~port:(9000 + n) in
    Option.value_exn node_opt :: create_nodes (n - 1)

let main () =
  let nodes  = create_nodes (Int.of_string Sys.argv.(1)) in
  Deferred.List.map
    ~f:(create_scow nodes)
    nodes
  >>| fun scow_insts ->
  every
    (sec 5.0)
    (Fn.compose ignore (print_statem_info scow_insts));
  after (sec 5.0)
  >>| fun () ->
  every
    (sec 3.0)
    (Fn.compose ignore (append_entry (ref 0) scow_insts))

let () =
  Random.self_init ();
  ignore (main ());
  never_returns (Scheduler.go ());
