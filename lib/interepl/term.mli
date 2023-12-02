val next_cmd : string list -> string
val iprint : string list -> unit
val iprint_format : ('a -> 'b, out_channel, unit) format -> 'a -> 'b
val set_raw_mode : unit -> unit