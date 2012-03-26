(**
   The unit test vectors
*)

open GxlDocument;;

let parser = GxlDocument.make();;

(* Parse an attributed file *)
let () = GxlDocument.parse "/tmp/gxl-1.0-examples/instance/gxl/attredges/attredges.gxl" parser;;
let () = GxlDocument.write "/tmp/atteredges.gxl" parser;;

(* Parse an attributed file *)
let () = GxlDocument.parse "/tmp/gxl-1.0-examples/instance/gxl/complexExample/complexExample.gxl" parser;;
let () = GxlDocument.write "/tmp/complexExample.gxl" parser;;
