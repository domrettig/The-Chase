open Gameplay
open GMain
open GdkKeysyms

val window : GWindow.window

val vbox : GPack.box

(* Makes calls to Gtk/GMain library to update displayed information *)
val update : metadata -> player -> ai -> gameboard -> unit
