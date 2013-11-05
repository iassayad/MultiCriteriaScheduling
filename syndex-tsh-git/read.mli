exception Error of string * int * string
val library_directory : string ref
val lexerbuffer_of_file : string -> Lexing.lexbuf * in_channel
val lexerbuffer_of_library : string -> Lexing.lexbuf
val file_read :
  Lexing.lexbuf ->
  string ->
  string ->
  int ->
  Adequationtypes.graph_type option -> bool
val file_parse :
  string ->
  Adequationtypes.graph_type option -> bool
val library_parse : string -> bool
val open_syndex_file :
  string -> Adequationtypes.graph_type option
val open_syndex_library : string -> unit
