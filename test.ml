(**
   The unit test vectors
*)

open GxlDocument;;

let pp = GxlDocument.make();;

(* Parse an attributed file *)
(* let () = GxlDocument.parse "/tmp/gxl-1.0-examples/instance/gxl/attredges/attredges.gxl" parser;; *)
(* let () = GxlDocument.write "/tmp/g_atteredges.gxl" parser;; *)

(* (\* Parse a complex attributed file *\) *)
(* let () = GxlDocument.parse "/tmp/gxl-1.0-examples/instance/gxl/complexExample/complexExample.gxl" parser;; *)
(* let () = GxlDocument.write "/tmp/g_complexExample.gxl" parser;; *)

(* (\* Parse a hyper-graph file*\) *)
(* let () = GxlDocument.parse "/tmp/gxl-1.0-examples/instance/gxl/hypergraph/hypergraph.gxl" parser;; *)
(* let () = GxlDocument.write "/tmp/g_hypergraph.gxl" parser;; *)

(* (\* Parse a hierarchical-graph file*\) *)
let () = GxlDocument.parse "/home/amal029/AUCKLAND_WORK/Oliver/Fork_Join_Nodes_30_CCR_1.99_WeightType_Random.gxl" pp
let () = GxlDocument.write "/tmp/tutu.gxl" pp
