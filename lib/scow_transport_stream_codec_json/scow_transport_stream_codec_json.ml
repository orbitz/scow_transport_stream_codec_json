open Core.Std

module type ELT = sig
  type t

  val to_string : t -> string
  val of_string : string -> t option
end

module Make = functor (Elt : ELT) -> struct
  type elt = Elt.t

  module Msg = Scow_transport_stream_codec.Msg

  module Response = Scow_transport_stream_codec.Response
  module Request = Scow_transport_stream_codec.Request

  module Build = Json_type.Build
  module Browse = Json_type.Browse

  let string_of_term = Fn.compose Int.to_string Scow_term.to_int
  let string_of_log_idx = Fn.compose Int.to_string Scow_log_index.to_int

  let term_of_string str =
    Option.value_exn
      (Option.try_with
         (fun () ->
           Option.value_exn
             (Scow_term.of_int (Int.of_string str))))

  let log_idx_of_string str =
    Option.value_exn
      (Option.try_with
         (fun () ->
           Option.value_exn
             (Scow_log_index.of_int (Int.of_string str))))

  let to_string = function
    | Msg.Resp_append_entries response ->
      let (term, success) = response.Response.payload in
      Json_io.string_of_json
        (Build.objekt
           [ ("type",       Build.string "resp_append_entries")
           ; ("request_id", Build.string response.Response.request_id)
           ; ("term",       Build.string (Int.to_string (Scow_term.to_int term)))
           ; ("success",    Build.bool success)
           ])
    | Msg.Resp_request_vote response ->
      let (term, granted) = response.Response.payload in
      Json_io.string_of_json
        (Build.objekt
           [ ("type",       Build.string "resp_request_vote")
           ; ("request_id", Build.string response.Response.request_id)
           ; ("term",       Build.string (Int.to_string (Scow_term.to_int term)))
           ; ("granted",    Build.bool granted)
           ])
    | Msg.Request_vote request ->
      let module Rv = Scow_rpc.Request_vote in
      let str = Build.string in
      let request_vote = request.Request.payload in
      Json_io.string_of_json
        (Build.objekt
           [ ("type",           str "request_vote")
           ; ("node",           str request.Request.node)
           ; ("request_id",     str request.Request.request_id)
           ; ("term",           str (string_of_term request_vote.Rv.term))
           ; ("last_log_index", str (string_of_log_idx request_vote.Rv.last_log_index))
           ; ("last_log_term",  str (string_of_term request_vote.Rv.last_log_term))
           ])
    | Msg.Append_entries request ->
      let module Ae = Scow_rpc.Append_entries in
      let str = Build.string in
      let append_entries = request.Request.payload in
      let entries =
        List.map
          ~f:(fun (term, elt) ->
            Build.objekt
              [ ("term",  str (string_of_term term))
              ; ("value", str (Elt.to_string elt))
              ])
          append_entries.Ae.entries
      in
      Json_io.string_of_json
        (Build.objekt
           [ ("type",           str "append_entries")
           ; ("node",           str request.Request.node)
           ; ("request_id",     str request.Request.request_id)
           ; ("term",           str (string_of_term append_entries.Ae.term))
           ; ("prev_log_index", str (string_of_log_idx append_entries.Ae.prev_log_index))
           ; ("prev_log_term",  str (string_of_term append_entries.Ae.prev_log_term))
           ; ("leader_commit",  str (string_of_log_idx append_entries.Ae.leader_commit))
           ; ("entries",        Build.array entries)
           ])

  let of_json_exn table = function
    | "resp_append_entries" ->
      let request_id = Browse.string (Browse.field table "request_id") in
      let term       = term_of_string (Browse.string (Browse.field table "term")) in
      let success    = Browse.bool (Browse.field table "success") in
      Msg.Resp_append_entries
        { Response.request_id = request_id
        ;          payload    = (term, success)
        }
    | "resp_request_vote" ->
      let request_id = Browse.string (Browse.field table "request_id") in
      let term       = term_of_string (Browse.string (Browse.field table "term")) in
      let granted    = Browse.bool (Browse.field table "granted") in
      Msg.Resp_request_vote
        { Response.request_id = request_id
        ;          payload    = (term, granted)
        }
    | "request_vote" ->
      let module Rv  = Scow_rpc.Request_vote in
      let node       = Browse.string (Browse.field table "node") in
      let request_id = Browse.string (Browse.field table "request_id") in
      let term       = term_of_string (Browse.string (Browse.field table "term")) in
      let last_log_index =
        log_idx_of_string (Browse.string (Browse.field table "last_log_index"))
      in
      let last_log_term =
        term_of_string (Browse.string (Browse.field table "last_log_term"))
      in
      Msg.Request_vote
        { Request.node       = node
        ;         request_id = request_id
        ;         payload    = { Rv.term           = term
                               ;    last_log_index = last_log_index
                               ;    last_log_term  = last_log_term
                               }
        }
    | "append_entries" ->
      let module Ae  = Scow_rpc.Append_entries in
      let node       = Browse.string (Browse.field table "node") in
      let request_id = Browse.string (Browse.field table "request_id") in
      let term       = term_of_string (Browse.string (Browse.field table "term")) in
      let prev_log_index =
        log_idx_of_string (Browse.string (Browse.field table "prev_log_index"))
      in
      let prev_log_term =
        term_of_string (Browse.string (Browse.field table "prev_log_term"))
      in
      let leader_commit =
        log_idx_of_string (Browse.string (Browse.field table "leader_commit"))
      in
      let entries =
        List.map
          ~f:(fun entry ->
            let table = Browse.make_table (Browse.objekt entry) in
            let term  = term_of_string (Browse.string (Browse.field table "term")) in
            let value =
              Option.value_exn
                (Elt.of_string (Browse.string (Browse.field table "value")))
            in
            (term, value))
          (Browse.array (Browse.field table "entries"))
      in
      Msg.Append_entries
        { Request.node       = node
        ;         request_id = request_id
        ;         payload    = { Ae.term           = term
                               ;    prev_log_index = prev_log_index
                               ;    prev_log_term  = prev_log_term
                               ;    leader_commit  = leader_commit
                               ;    entries        = entries
                               }
        }
    | _ ->
      failwith "unknown_type"

  let of_string_exn str =
    let objekt = Browse.objekt (Json_io.json_of_string str) in
    let table  = Browse.make_table objekt in
    let typ    = Browse.string (Browse.field table "type") in
    of_json_exn table typ

  let of_string str =
    try
      Some (of_string_exn str)
    with
        _ ->
          None
end
