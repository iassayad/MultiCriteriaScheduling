open Camltk

let () =
  Read.library_directory := "./libs";
  Ihm_ctk.top_win_create ();
  ignore (Thread.create Tk.mainLoop ())
