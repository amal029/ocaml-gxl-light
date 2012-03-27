(** This is the GXL parser and in-place manipulator
    @author Avinash Malik 
    date Sat Mar 17 17:35:52 GMT 2012 
*)

open Xml;;

(** {1 Exceptions} *)
exception CompositeTypeExn;; 
exception GXLRelendException;; 
exception GXLElementTypeExn of string;;
exception GXLLengthExn of string;;
exception NotFound;; 
exception GXLGraphException of string;;

(**
   {1 GXLValue, GXLType, and GXLLocator types}
*)

type 'a mutable_at = {mutable v : 'a}
(* type mutable_int = {mutable iv:int} *)
(* and mutable_float = {mutable fv:float} *)
(* and mutable_string = {mutable sv:string} *)
(* and mutable_bool = {mutable bv:bool} *)
(* and mutable_enum = {mutable ev:string} (\* Enum is a string type, defined by a schema *\) *)
(* A default value for record fields?? -- via arguments*)
and mutable_uri = {mutable x_type:string ; mutable uriv:string}
and mutable_type = {mutable xlink_type:string; mutable xlink_href:string}
;;

type gxl_type = 
    GXLType of mutable_type
;;

(* The GXL atomic types *)
type gxl_atomic_value = 
    GXLInt of int mutable_at
  | GXLFloat of float mutable_at
  | GXLString of string mutable_at
  | GXLBool of bool mutable_at
  | GXLEnum of string mutable_at
;;

type gxl_locator = 
    GXLLocator of mutable_uri
;;

(* The GXL composite types *)
type gxl_composite_value = 
    GXLBag of m_comp
  | GXLTup of m_comp
  | GXLSet of m_comp
  | GXLSeq of m_comp
(* This is the main top level of value types*)
and m_comp = {mutable comp_v:gxl_value list}
and gxl_value =
    GXLAtomicValue of gxl_atomic_value
  | GXLLocatorValue of gxl_locator
  | GXLCompositeValue of gxl_composite_value
;;

(**
   {1 GXLAttr type}
*)

(* Any attribute is of type *)
type mutable_attr_t = {mutable id:string option;mutable name:string;mutable kind:string option}
and m_attr_val = {mutable attr_val: gxl_value}
and m_gxl_attr = {mutable children_attr:gxl_attr list}
and gxl_attr = 
    GXLAttr of m_attr_val * mutable_attr_t * m_gxl_attr 
;;



(**
   {1 GXLElement type}
*)

type edge_mode = 
    Directed | Undirected | DefaultDirected | DefaultUndirected
and relend_direction =
    GXL_IN | GXL_OUT | GXL_NONE
;;


type children = {mutable t: gxl_type option;
		 mutable a: gxl_attr list;
		 mutable g: gxl_typed_element list}
and node_children = {node_shared:children; mutable node_tentacles: gxl_attributed_element ref list}
and edge_children = {edge_shared:children; (* and the elements it is connected from and to*)
		     mutable edge_tentacles: gxl_attributed_element array; 
		    }
and gxl_gxl = {mutable xlink:string; mutable graphs: gxl_typed_element list}
and relend_attrs = {mutable relend_role:string option;
		    mutable relend_direction: relend_direction;
		    mutable relend_startorder: int option;
		    mutable relend_endorder: int option}
and relend_children = {mutable relend_attrs:gxl_attr list;
		       mutable relend_target: gxl_graph_element ref;
		       mutable relend_local_connection: gxl_local_connection ref option
		      }
and graph_children = {mutable graph_type: gxl_type option;
		      mutable graph_attrs: gxl_attr list;
		      mutable graph_element: gxl_graph_element list;
		      (* mutable graph_rels: (rel_children * rel_attrs) list *)}
and graph_attrs = {mutable graph_id:string;
		   mutable graph_role: string option;
		   mutable graph_edgeids: bool option; (*default:false*)
		   mutable hyper_graph: bool option; (*default:false*)
		   (* 4 possibilities *)
		   mutable graph_edge_mode:edge_mode}
and node_attrs = {mutable node_id:string}
and edge_attrs = {mutable edge_id: string option; edge_fromorder: int option ref;
		  edge_toorder: int option ref ; mutable edge_isdirected: bool option}
and rel_children = {rel_shared:children; mutable rel_end: gxl_attributed_element list}
and rel_attrs = {mutable rel_id:string; mutable rel_isdirected: bool option}
and gxl_attributed_element = 
    GXLRelend of relend_children * relend_attrs
  | GXLTypedElement of gxl_typed_element
and gxl_typed_element = 
    GXLGraph of graph_children * graph_attrs
  | GXLGraphElement of gxl_graph_element
and gxl_graph_element = 
    GXLLocalConnection of gxl_local_connection
  | GXLNode of node_children * node_attrs
and gxl_local_connection = 
    GXLEdge of edge_children * edge_attrs
  | GXLRel of rel_children * rel_attrs
and gxl_element = 
    GXLValue of gxl_value
  | GXLGXL of gxl_gxl
  | GXLAttributedElement of gxl_attributed_element
;;

(*-----------------Data structures---------------*)

let node_table = Hashtbl.create 10;;


(*-----------------FUNCTIONS---------------------*)
(*-----------------------------------------------*)

(**
   {1:value_types Functions on GXLValue}
*)

let string_of_atomic_value input =
  match input with
      GXLInt x -> string_of_int x.v
    | GXLString x ->  x.v
    | GXLBool x -> string_of_bool x.v
    | GXLFloat x -> string_of_float x.v
    | GXLEnum x -> x.v
;;

(* Printing the gxl_locator type *)
let string_of_locator_value input =
  match input with
      GXLLocator x -> x.uriv
;;

(* Printing the gxl_value type *)
let string_of_gxl_value input =
  match input with
      GXLAtomicValue x -> string_of_atomic_value x
    | GXLLocatorValue x -> string_of_locator_value x
    | _ -> raise CompositeTypeExn
;;

(** 

    The iteration function that works on the composite values. This
    function is appplied to all GXLValue types in a deep iteration. For
    shallow iteration use get_composite_children

*)
let rec iter_composite_value func input =
  match input with
      GXLBag x | GXLSeq x | GXLTup x | GXLSet x -> 
	let lst =
	  (match x.comp_v with
	      (GXLAtomicValue h)::t as x -> x
	    | (GXLCompositeValue h)::t as x -> eval_composite func x
	    | (GXLLocatorValue h)::t as x -> x
	    | [] -> []) in
	(*Apply the function to the list obtained*)
	List.iter func lst
and eval_composite func x =
  match x with
      (GXLCompositeValue h)::t -> iter_composite_value func h; eval_composite func t
    | [] -> [] (*give back a gxl_value list*)
    | _ -> raise CompositeTypeExn
;;
(** 

    The map function that works on the composite values. This function
    is appplied to all GXLValue types in a deep iteration. For shallow
    iteration use get_composite_children

*)
(** The function to insert a value into a composite type*)
let insert_value bag value = 
  match bag with 
      GXLBag x | GXLSet x | GXLSeq x | GXLTup x -> 
	let ll = x.comp_v in
	let ret = ll @ [value] in
	x.comp_v <- ret
;;

(* (\** The function to remove the children  *)
(*     @return {b removes list of values} *)
(* *\) *)
(* let remove_value bag =  *)
(*   match bag with  *)
(*       GXLBag x | GXLSet x | GXLSeq x | GXLTup x ->  *)
(* 	let ret = x.comp_v in *)
(* 	x.comp_v <- []; *)
(* 	ret *)
(* ;; *)

(** The function that returns the children *)
let get_composite_children bag = 
  match bag with 
      GXLBag x | GXLSet x | GXLSeq x | GXLTup x -> x.comp_v
;;

(** The function that sets the atomic values *)
let set_atomic_value atom value = 
  match atom with 
      GXLInt x -> x.v <- int_of_string value
    | GXLFloat x -> x.v <- float_of_string value
    | GXLString x -> x.v <- value
    | GXLBool x -> x.v <- bool_of_string value
    | GXLEnum x -> x.v <- value
;;

(**
   {1:type_functions Functions on GXLType }
*)

(** The type argument kind is "simple" be default*)
let set_type t ?(kind="simple") ~ref = 
  match t with GXLType t ->
    t.xlink_type <- kind; t.xlink_href <- ref;;

let get_type t =
  match t with GXLType t ->
    let ret = (t.xlink_type , t.xlink_href) in
    ret
;;

(**
   {1:locator_function Functions on GXLLocator}
*)

(** The type argument kind is "simple" be default*)
let set_locator l ?(kind="simple") ~ref = 
  match l with GXLLocator l ->
    l.x_type <- kind; l.uriv <- ref
;;

let get_locator l =
  match l with GXLLocator l ->
    let ret = (l.x_type, l.uriv) in
    ret
;;

(** 

    {1:attr_functions Functions on GXLAttr} 

    Please observe the difference between
    {ul
    {- GXLAttr elements that attribute GXL entities.} 
    {- XML element attributes that is used to markup the GXL format.}}

*)

(**
   The function to get the associated GXLValue 
*)

let get_attr_value ~attr =
  match attr with
      GXLAttr (x,_,_) -> x.attr_val
;;

(**
   The function to set the associated GXLValue 
*)
let set_attr_value ~attr v =
  match attr with
      GXLAttr (x,_,_) -> x.attr_val <- v
;;

(**
   The function to set the associated kind 
*)
let set_attr_kind ~attr v =
  match attr with
      GXLAttr (_,y,_) -> y.kind <- Some v
;;

(**
   The function to get the associated kind
   @return Some string or None
*)

let get_attr_kind ~attr = 
  match attr with
      GXLAttr (_,y,_) -> y.kind
;;

(**
   The function to set the associated name
*)
let set_attr_name ~attr v =
  match attr with
      GXLAttr (_,y,_) -> y.name <- v
;;

(**
   The function to get the associated name
   @return Some string or None
*)

let get_attr_name ~attr = 
  match attr with
      GXLAttr (_,y,_) -> y.name
;;


(**
   The function to set the associated id
*)
let set_attr_id ~attr v =
  match attr with
      GXLAttr (_,y,_) -> y.id <- Some v
;;

(**
   The function to get the associated id
   @return Some string or None
*)

let get_attr_id ~attr = 
  match attr with
      GXLAttr (_,y,_) -> y.id
;;

(**

   The function to get the nested GXLAttr children
*)

let get_attr_children ~attr = 
  match attr with
      GXLAttr (_,_,z) -> z.children_attr
;;

(**

   The function to set the nested GXLAttr children
*)

let set_attr_children ~attr v = 
  match attr with
      GXLAttr (_,_,z) -> z.children_attr <- v
;;

(**

   The function to iterate through the attribute and all its attribute
   children

*)

let rec iter_gxl_attr func ~attr = 
  let ret = 
    (
      let lc = get_attr_children attr in
      match lc with
	  [] -> [attr]
	| _ -> attr :: eval_attr_list func lc
    ) in
  List.iter func ret
and eval_attr_list func ll =
  match ll with
      (GXLAttr (_,_,_) as h)::t -> iter_gxl_attr func h; eval_attr_list func t
    | [] -> []
;;

(**
   {1 Functions on GXLElement type}
*)

let get_local_connection_id = function
  | GXLEdge (x,y) -> (match y.edge_id with 
      None -> "None" 
      | Some x -> x)
  | GXLRel (x,y) -> y.rel_id
;;

let get_graph_element_id = function
  | GXLNode (x,y) -> y.node_id
  | GXLLocalConnection x -> get_local_connection_id x
;;

let get_typed_element_id = function
  | GXLGraph (x,y) -> y.graph_id
  | GXLGraphElement x -> get_graph_element_id x
;;

let get_attributed_element_id = function
  | GXLTypedElement x -> get_typed_element_id x
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
;;

let get_id = function 
  | GXLAttributedElement x -> get_attributed_element_id x
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
;;

let set_local_connection_id v = function
  | GXLEdge (x,y) -> y.edge_id <- Some v
  | GXLRel (x,y) -> y.rel_id <- v
;;

let set_graph_element_id v = function
  | GXLNode (x,y) -> y.node_id <- v
  | GXLLocalConnection x -> set_local_connection_id v x
;;

let set_typed_element_id v = function
  | GXLGraph (x,y) -> y.graph_id <- v
  | GXLGraphElement x -> set_graph_element_id v x
;;

let set_attributed_element_id v = function
  | GXLTypedElement x -> set_typed_element_id v x
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
;;

let set_id v = function 
  | GXLAttributedElement x -> set_attributed_element_id v x
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
;;

let get_local_connection_attr_list = function
  | GXLEdge (x,y) -> x.edge_shared.a
  | GXLRel (x,y) -> x.rel_shared.a
;;
let get_graph_element_attr_list = function
  | GXLNode (x,y) -> x.node_shared.a
  | GXLLocalConnection x -> get_local_connection_attr_list x
;;
let get_typed_element_attr_list  = function
  | GXLGraph (x,y) -> x.graph_attrs
  | GXLGraphElement x -> get_graph_element_attr_list x
;;
let get_attributed_element_attr_list = function
  | GXLTypedElement x -> get_typed_element_attr_list x
  | GXLRelend (x,y) -> x.relend_attrs
;;
let get_attr_list = function
  | GXLAttributedElement x -> get_attributed_element_attr_list x
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
;;

let rec eval_l l name = 
  match l with
      (GXLAttr (x,y,z))::t -> if (y.name = name) then x.attr_val else (eval_l t name)
    | [] -> raise NotFound
;;

let get_attr element name = 
  let l = 
    (match element with
	GXLAttributedElement x -> get_attributed_element_attr_list x
      | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
    )
  in
  let ret = eval_l l name in ret
;;

let set_attr_list v = function
  | GXLAttributedElement x -> 
    (match x with
	GXLTypedElement x -> (match x with
            GXLGraph (x,y) -> x.graph_attrs <- v
	  | GXLGraphElement x -> (match x with
              GXLNode (x,y) -> x.node_shared.a <- v
	      | GXLLocalConnection x ->
		(match x with
		    GXLEdge (x,y) -> x.edge_shared.a <- v
		  | GXLRel (x,y) -> x.rel_shared.a <- v
		)
          )
	)
      | GXLRelend (x,y) -> x.relend_attrs <- v
    )
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
;;

let rec eval_l_l l name v =
  match l with 
      (GXLAttr (x,y,z))::t -> if(y.name = name) then x.attr_val <- v else (eval_l_l t name v)
    | [] -> raise NotFound
;;

let set_attr element name v = 
  let l = (match element with
    | GXLAttributedElement x -> get_attributed_element_attr_list x
    | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
  ) in
  eval_l_l l name v
;;

let gxl_element_get_type = function
  | GXLAttributedElement x -> 
    (match x with
	GXLTypedElement x -> (match x with
            GXLGraph (x,y) -> x.graph_type
	  | GXLGraphElement x -> (match x with
              GXLNode (x,y) -> x.node_shared.t
	      | GXLLocalConnection x ->
		(match x with
		    GXLEdge (x,y) -> x.edge_shared.t
		  | GXLRel (x,y) -> x.rel_shared.t
		)
          )
	)
      | _ -> raise (GXLElementTypeExn "cannot be type GXLRelend")
    )
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel, GXLGXL")
;;

let gxl_element_set_type v = function
  | GXLAttributedElement x -> 
    (match x with
	GXLTypedElement x -> (match x with
            GXLGraph (x,y) -> x.graph_type <- v
	  | GXLGraphElement x -> (match x with
              GXLNode (x,y) -> x.node_shared.t <- v
	      | GXLLocalConnection x ->
		(match x with
		    GXLEdge (x,y) -> x.edge_shared.t <- v
		  | GXLRel (x,y) -> x.rel_shared.t <- v
		)
          )
	)
      | _ -> raise (GXLElementTypeExn "cannot be type GXLRelend")
    )
  | _ -> raise (GXLElementTypeExn "not of type: GXLNode, GXLEdge, GXLGraph, GXLRel")
;;

let get_role = function
  | GXLRelend (_,y) -> y.relend_role
  | GXLTypedElement x -> match x with GXLGraph (_,y) -> y.graph_role | _ -> raise (GXLElementTypeExn "not of type GXLGraph/GXLRelend")
;;

(**
   @param role None by default
*)
let set_role ?(role=None) = function
  | GXLRelend (_,y) -> y.relend_role <- role
  | GXLTypedElement x -> match x with GXLGraph (_,y) -> y.graph_role <- role 
      | _ -> raise (GXLElementTypeExn "not of type GXLGraph/GXLRelend")
;;

(**{1 Functions on GXLGraph}*)

let get_allowed_hypergraphs = function
  | GXLGraph (x,y) -> y.hyper_graph
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

(**
   @param allowed_hypergraphs Some false by default
*)
let set_allowed_hypergraphs ?(allowed_hypergraphs=Some false) = function
  | GXLGraph (x,y) -> y.hyper_graph <- allowed_hypergraphs
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

let get_edge_ids = function
  | GXLGraph (x,y) -> y.graph_edgeids
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

(**
   @param edge_ids Some false by default
*)
let set_edge_ids ?(edge_ids=Some false)= function
  | GXLGraph (x,y) -> y.graph_edgeids <- edge_ids
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

let get_edge_modes = function
  | GXLGraph (x,y) -> y.graph_edge_mode 
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

(**
   edge_mode is Directed by default
*)

let set_edge_modes ?(edge_mode = Directed) = function
  | GXLGraph (x,y) -> y.graph_edge_mode  <- edge_mode
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

let get_graph_element_list = function
  | GXLGraph (x,y) -> x.graph_element
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

let iter_graph_element ~func element =
  let l = get_graph_element_list element in
  List.iter func l
;;

let set_graph_element_list v = function
  | GXLGraph (x,_) -> x.graph_element <- v
  | _ -> raise (GXLElementTypeExn "not of type GXLGraph")
;;

let add_to_graph_element_list v graph = 
  (* First check directions for GXLEdge and GXLRel *)
  (try
     let isdo =
       (match v with
	 | GXLLocalConnection x -> (match x with GXLEdge (_,t) -> t.edge_isdirected | GXLRel (_,t) -> t.rel_isdirected)
	 | _ -> raise Exit) in
     let isd = match isdo with Some x -> x | None -> false in
     let gem = (match graph with GXLGraph (_,t) -> t.graph_edge_mode | _ -> raise (GXLElementTypeExn "Not of type GXLGraph")) in
     if (gem = Directed) && (not isd) then raise (GXLGraphException "Cannot be added since directions don't match")
     else if (gem = Undirected) && (isd) then raise (GXLGraphException "Cannot be added since directions don't match");
   with
     | Exit -> ());
  let cl = get_graph_element_list graph in
  set_graph_element_list (cl @ [v]) graph
;;

(**
   {1:gxlrel_ten Functions on GXLRelend}
*)

let gxl_relend_get_direction = function
  | GXLRelend (_,y) -> y.relend_direction
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_set_direction v = function
  | GXLRelend (_,y) -> y.relend_direction <- v
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_get_target = function
  | GXLRelend (x,_) -> !(x.relend_target)
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_set_target v = function
  | GXLRelend (x,_) -> x.relend_target := v
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_get_target_id = function
  | GXLRelend (x,_) -> get_graph_element_id (!(x.relend_target))
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_get_local_connection = function
  | GXLRelend (x,_) -> (match x.relend_local_connection with Some x -> !x | None -> raise GXLRelendException)
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_set_local_connection v = function
  | GXLRelend (x,_) -> (match x.relend_local_connection with Some x -> x := v | None -> raise GXLRelendException)
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_get_target_incidence_order = function
  | GXLRelend (_,x) -> x.relend_endorder
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_set_target_incidence_order v = function
  | GXLRelend (_,x) -> x.relend_endorder <- v
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_get_rel_incidence_order = function
  | GXLRelend (_,x) -> x.relend_startorder
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;

let gxl_relend_set_rel_incidence_order v = function
  | GXLRelend (_,x) -> x.relend_startorder <- v
  | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
;;


(**
   {1 Functions on GXLGraphElement}
*)

let get_gxl_graph_element_graph_list = function
  | GXLNode (x,y) -> x.node_shared.g
  | GXLLocalConnection x ->
    (match x with
	GXLEdge (x,y) -> x.edge_shared.g
      | GXLRel (x,y) -> x.rel_shared.g
    )
;;

let set_gxl_graph_element_graph_list v = function
  | GXLNode (x,y) -> x.node_shared.g <- v
  | GXLLocalConnection x ->
    (match x with
	GXLEdge (x,y) -> x.edge_shared.g <- v
      | GXLRel (x,y) -> x.rel_shared.g <- v
    )
;;


let iter_gxl_graph_element_graph ~func element = 
  let l = get_gxl_graph_element_graph_list element in
  List.iter func l
;;

(** {3 Fixme: I don't understand what happens when a GXLRel is connected
    to another say GXLRel.} 
    Only GXLNode can be passed in as argument
    See {!gxlrel_ten} and {!gxlend} for getting GXLRel and GXLEdge tentacles
*)

let get_connection_list element = 
  let rl =
    (match element with
      | GXLNode (x,_) -> x.node_tentacles
      | _ -> raise (GXLElementTypeExn (get_graph_element_id element ^ " not of type GXLNode"))
    ) in
  let rrl = List.stable_sort (function x -> function y -> 
    match (!x,!y) with
      | (GXLRelend (_, x),GXLRelend(_,y)) -> compare x.relend_endorder y.relend_endorder
      | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
  ) rl in
  rrl
;;

let set_connection_list mlist = function
  | GXLNode (x,_) -> x.node_tentacles <- List.map (fun x -> ref x) mlist
  | GXLLocalConnection x -> 
    (match x with
	GXLEdge (x,_) -> x.edge_tentacles <- Array.of_list mlist
      | GXLRel (x,_) -> x.rel_end <- mlist
    )
;;

let add_to_connection_list relend = function
  | GXLNode (x,_) -> x.node_tentacles <- ((ref relend) :: x.node_tentacles)
  | GXLLocalConnection x -> 
    (match x with
	GXLEdge (x,_) -> raise (GXLElementTypeExn "not of type GXLNode/GXLRel")
      | GXLRel (x,_) -> x.rel_end <- (relend :: x.rel_end)
    )
;;

let iter_connection_list ~func element =
  let ret = get_connection_list element in
  List.iter func ret
;;

(**

   {1:gxlend Functions on GXLLocalConnection}
*)

let rec eval_edge_tentacles direction name = function
  | h::t -> (match eval_eval direction h with Some x -> x | None -> eval_edge_tentacles direction name t)
  | [] -> raise (GXLLengthExn ("Edge " ^ name ^ " is not a binary relation"))
and eval_eval direction h = 
  match h with
      GXLRelend (x,y) -> 
        (let d = gxl_relend_get_direction h in
         if (d = direction) then
           Some (gxl_relend_get_target h)
	 else
           None)
    | _ -> raise (GXLElementTypeExn "not of type GXLRelend")
and get_edge_source element = 
  match element with
    | GXLEdge (x,y) -> 
      let name = (match y.edge_id with None -> "none" | Some x -> x)in
      eval_edge_tentacles GXL_IN name (Array.to_list x.edge_tentacles)
    | _ -> raise (GXLElementTypeExn "not of type GXLEdge") 
and get_edge_target element =
  match element with
    | GXLEdge (x,y) -> 
      let name = (match y.edge_id with None -> "none" | Some x -> x)in
      eval_edge_tentacles GXL_OUT  name (Array.to_list x.edge_tentacles)
    | _ -> raise (GXLElementTypeExn "not of type GXLEdge") 

;;

let rec get_source_tentacle direction name = function
  | h::t -> if (gxl_relend_get_direction h =  direction) then h else get_source_tentacle direction name t
  | [] -> raise NotFound
and set_edge_source v element = 
  match element with
    | GXLEdge (x,y) -> 
      let name = (match y.edge_id with None -> "none" | Some x -> x)in
      let ret = ref (get_source_tentacle GXL_IN  name (Array.to_list x.edge_tentacles)) in
      ret := v
    | _ -> raise (GXLElementTypeExn "not of type GXLEdge") 
and set_edge_target v element = 
  match element with
    | GXLEdge (x,y) -> 
      let name = (match y.edge_id with None -> "none" | Some x -> x)in
      let ret = ref (get_source_tentacle GXL_OUT  name (Array.to_list x.edge_tentacles)) in
      ret := v
    | _ -> raise (GXLElementTypeExn "not of type GXLEdge") 
;;

let get_source_incidence_order = function
  | GXLEdge (x,y) -> !(y.edge_fromorder)
  | _ -> raise (GXLElementTypeExn "not of type: GXLEdge")
;;

let set_source_incidence_order v = function
  | GXLEdge (x,y) -> y.edge_fromorder := v
  | _ -> raise (GXLElementTypeExn "not of type: GXLEdge")
;;
let get_target_incidence_order = function
  | GXLEdge (x,y) -> !(y.edge_toorder)
  | _ -> raise (GXLElementTypeExn "not of type: GXLEdge")
;;

let set_target_incidence_order v = function
  | GXLEdge (x,y) -> y.edge_toorder := v
  | _ -> raise (GXLElementTypeExn "not of type: GXLEdge")
;;

(**
   {1 Functions on GXLGXL}
*)

let get_gxl_gxl_graph_list = function
  | GXLGXL x -> x.graphs
  | _ -> raise (GXLElementTypeExn "not of type GXLGXL")
;;

let set_gxl_gxl_graph_list v = function
  | GXLGXL x -> x.graphs <- v
  | _ -> raise (GXLElementTypeExn "not of type GXLGXL")
;;

let add_to_gxl_gxl_graph_list v e =
  let r = get_gxl_gxl_graph_list e in
  match e with
    | GXLGXL x -> x.graphs <- (r @ [v])
  | _ -> raise (GXLElementTypeExn "not of type GXLGXL")
;;

let iter_gxl_gxl_graphs ~func element =
  let ret = get_gxl_gxl_graph_list element in
  List.iter func ret
;;

(**
   {1 The to_xml type element}
*)

(** These guys don't have kids so no need to recurse *)
let gxl_atomic_value_to_xml = function
  | GXLInt x -> Element ("int", [], [PCData (string_of_int x.v)])
  | GXLBool x -> Element ("bool", [], [PCData (string_of_bool x.v)])
  | GXLString x -> Element ("string", [], [PCData (x.v)])
  | GXLFloat x -> Element ("float", [], [PCData (string_of_float x.v)])
  | GXLEnum x -> Element ("enum", [], [PCData (x.v)])
and gxl_type_to_xml = function
  | GXLType x -> Element ("type", [("xlink:type",x.xlink_type);("xlink:href",x.xlink_href)], [])
and gxl_locator_to_xml = function
  | GXLLocator x -> Element ("locator", [("xlink:type",x.x_type);("xlink:href",x.uriv)], [])
;;

let rec gxl_rec_composite_value_to_xml input =
  match input with
      GXLBag x | GXLSeq x | GXLTup x | GXLSet x -> 
	match x.comp_v with
	  | (GXLAtomicValue h)::t as x -> get_element x
	  | (GXLCompositeValue h)::t as x -> let rlref = ref [] in eval_composite x rlref; !rlref
	  | (GXLLocatorValue h)::t as x -> get_element x
	  | [] -> []
	(*Apply the function to the list obtained*)
and get_element x =
  List.map 
    (function x -> 
      match x with
	| GXLLocatorValue x -> gxl_locator_to_xml x
	| GXLAtomicValue x -> gxl_atomic_value_to_xml x
	| _ -> raise CompositeTypeExn
    ) x 
and eval_composite x rlref =
  match x with
      (GXLCompositeValue h)::t -> let ret = gxl_rec_composite_value_to_xml h in 
				  (*Make the composite element*)
				  let name = 
				    (match h with
				      | GXLBag _ -> "bag"
				      | GXLTup _ -> "tup"
				      | GXLSet _ -> "set"
				      | GXLSeq _ -> "seq"
				    ) in
				  let celement = Element(name,[],ret) in
				  rlref := !rlref @ [celement];
				  (*always call this*)
				  eval_composite t rlref
    | [] -> () (*give back a gxl_value list*)
    | _ -> raise CompositeTypeExn
;;

(** Composite value is recursive and uses map*)
let gxl_composite_value_to_xml input =
  let children = gxl_rec_composite_value_to_xml input in
  let name = 
  (match input with
    | GXLBag _ -> "bag"
    | GXLTup _ -> "tup"
    | GXLSet _ -> "set"
    | GXLSeq _ -> "seq"
  ) in
  Element(name,[],children)
;;  

let gxl_value_to_xml = function
  | GXLAtomicValue x -> gxl_atomic_value_to_xml x
  | GXLLocatorValue x -> gxl_locator_to_xml x
  | GXLCompositeValue x -> gxl_composite_value_to_xml x
;;

(** Attriutes have children and are implemented using recursion and
    maps*)
let rec gxl_attr_to_xml el = 
  match el with
    | GXLAttr (x,y,z) -> 
      let name = y.name in
      let attrref = ref [] in
      let lambda = (function x -> function y ->  y:= x::!y) in
      (match y.id with
	  Some x ->  lambda ("id",x) attrref
	| None -> ()
      ); 
      lambda ("name",name) attrref;
      (match y.kind with
	  Some x -> lambda ("kind",x) attrref
	| None -> ()
      );
      let children_list = ref [] in
      eval_attr_to_xml children_list (get_attr_children el);
      (* Noe evaluate the values as well *)
      children_list := (gxl_value_to_xml (get_attr_value el)) :: !children_list;
      Element(name,!attrref,!children_list)
and eval_attr_to_xml children_list children =
  match children with
    | h::t -> children_list := (gxl_attr_to_xml h)::!children_list; eval_attr_to_xml children_list t
    | [] -> ()
;;

let rec gxl_element_to_xml = function
  | GXLValue x -> gxl_value_to_xml x
  | GXLGXL x -> gxl_gxl_to_xml x
  | GXLAttributedElement x -> gxl_attributed_element_to_xml x
and gxl_graph_element_to_xml = function
  | GXLLocalConnection x -> gxl_local_connection_to_xml x
  | GXLNode (x,y) -> gxl_node_to_xml (x,y)
and gxl_local_connection_to_xml = function
  | GXLEdge (x,y) -> gxl_edge_to_xml (x,y)
  | GXLRel (x,y) -> gxl_rel_to_xml (x,y)
and gxl_attributed_element_to_xml = function
  | GXLTypedElement x -> gxl_typed_element_to_xml x
  | GXLRelend (x,y) -> gxl_relend_to_xml (x,y)
and gxl_typed_element_to_xml = function
  | GXLGraph (x,y) -> gxl_graph_to_xml (x,y)
  | GXLGraphElement x -> gxl_graph_element_to_xml x
and gxl_gxl_to_xml x = 
  let children_list = ref [] in
  let children = get_gxl_gxl_graph_list (GXLGXL x) in
  for i=0 to (List.length children - 1) do
    children_list := gxl_typed_element_to_xml (List.nth children i) :: !children_list
  done;
  Element("gxl",[("xmlns:xlink",x.xlink)],!children_list)
and gxl_graph_to_xml p = 
  match p with
    | (x,y) ->
      begin
	let attr_list = ref [] in
	let lambda = (function x -> function y -> y := !y@[x]) in
	lambda ("id",y.graph_id) attr_list;
	(
	  match y.graph_role with
	    | Some x -> lambda ("role",x) attr_list
	    | None -> ()
	);
	(
	  match y.graph_edgeids with
	    | Some x -> lambda ("edgeids",(string_of_bool x)) attr_list
	    | None -> lambda ("edgeids",(string_of_bool false)) attr_list
	);
	(
	  match y.hyper_graph with
	    | Some x -> lambda ("hypergraph",(string_of_bool x)) attr_list
	    | None -> lambda ("hypergraph",(string_of_bool false)) attr_list
	);
	let edgemode = 
	  (
	    match y.graph_edge_mode with
	      | Directed -> "directed"
	      | Undirected -> "undirected"
	      | DefaultUndirected -> "defaultundirected"
	      | DefaultDirected -> "defaultdirected"
	  ) in lambda ("edgemode",edgemode) attr_list;
  (* Now we gotta add the children *)
	let children_list = ref [] in
	(
	  match x.graph_type with
	    | Some x -> lambda [(gxl_type_to_xml x)] children_list
	    | None -> ()
	);
	lambda (List.map gxl_attr_to_xml x.graph_attrs) children_list;
	lambda (List.map gxl_graph_element_to_xml x.graph_element) children_list;
	(* lambda (List.map gxl_rel_to_xml x.graph_rels) children_list; *)
	Element("graph",!attr_list,(List.flatten !children_list))
      end
(* Note that p = (x,y) in all these *)
and gxl_node_to_xml p =
  match p with
      (x,y) -> 
	begin
	  let lambda = (function x -> function y -> y := !y @ x) in
	  let attr_list = ref [] in
	  lambda [("id",y.node_id)] attr_list;
	  let children_list = ref [] in
	  (match x.node_shared.t with
	    | Some x -> lambda [(gxl_type_to_xml x)] children_list
	    | None -> ()
	  );
	  lambda (List.map gxl_attr_to_xml x.node_shared.a ) children_list;
	  lambda (List.map gxl_typed_element_to_xml x.node_shared.g) children_list;
	  Element("node",!attr_list,!children_list)
	end
and gxl_relend_to_xml p = 
  match p with
      (x,y) -> 
	begin
	  let attr_list = ref [] in
	  let lambda = (function x -> function y -> y:= !y @ x) in
	  lambda [("target",(get_graph_element_id (!(x.relend_target))))] attr_list;
	  (match y.relend_role with
	      Some x -> lambda [("role",x)] attr_list
	    | None -> ()
	  );
	  (
	    match y.relend_startorder with
		Some x -> lambda [("startorder",(string_of_int x))] attr_list
	      | None -> ()
	  );
	  (
	    match y.relend_endorder with
		Some x -> lambda [("endorder",(string_of_int x))] attr_list
	      | None -> ()
	  );
	  (
	    match y.relend_direction with
		GXL_IN -> lambda [("direction","in")] attr_list
	      | GXL_OUT -> lambda [("direction","out")] attr_list
	      | GXL_NONE -> lambda [("direction","none")] attr_list
	  );
	  let children_list = ref [] in
	  lambda (List.map gxl_attr_to_xml x.relend_attrs) children_list;
	  Element("relend",!attr_list,!children_list)
	end
and gxl_edge_to_xml p =
  match p with
      (x,y) -> 
	begin
	  let attr_list = ref [] in
	  let lambda = (function x -> function y -> y:= !y @ x) in
	  (match y.edge_isdirected with
	    | Some x -> lambda [("isdirected",(string_of_bool x))] attr_list
	    | None -> ()
	  );
	  (
	    match !(y.edge_toorder) with
	      | Some x -> lambda [("toorder",(string_of_int x))] attr_list
	      | None -> ()
	  );
	  (
	    match !(y.edge_fromorder) with
	      | Some x -> lambda [("fromorder",(string_of_int x))] attr_list
	      | None -> ()
	  );
	  (
	    match y.edge_id with
	      | Some x -> lambda [("id",x)] attr_list
	      | None -> ()
	  );
	  let this = (GXLEdge (x,y)) in
	  let source = get_edge_source this in
	  lambda [("from",(get_source_id source))] attr_list;
	  let target = get_edge_target this in
	  lambda [("to",(get_target_id target))] attr_list;
	  let children_list = ref [] in
	  (match x.edge_shared.t with
	    | Some x -> lambda [(gxl_type_to_xml x)] children_list
	    | None -> ()
	  );
	  lambda (List.map gxl_attr_to_xml x.edge_shared.a) children_list;
	  lambda (List.map gxl_typed_element_to_xml x.edge_shared.g) children_list;
	  Element("edge",!attr_list,!children_list)
	end
and gxl_rel_to_xml p = 
  match p with
    | (x,y) ->
      let attr_list = ref [] in
      let lambda = (function x -> function y -> y:= !y @ x) in
      lambda [("id",y.rel_id)] attr_list;
      (match y.rel_isdirected with
	| Some x -> lambda [("isdirected", (string_of_bool x))] attr_list
	| None -> ()
      );
      let children_list = ref [] in
      (match x.rel_shared.t with
	| Some x -> lambda [(gxl_type_to_xml x)] children_list
	| None -> ()
      );
      lambda (List.map gxl_attr_to_xml x.rel_shared.a) children_list;
      lambda (List.map gxl_typed_element_to_xml x.rel_shared.g) children_list;
      lambda (List.map gxl_attributed_element_to_xml x.rel_end) children_list;
      Element ("rel",!attr_list,!children_list)
(* This can go on recursing for ever, yah!!*)
and get_source_id = function
  | GXLNode (x,y) -> y.node_id
  | GXLLocalConnection x -> 
    match x with
	GXLEdge (x,y) as p -> (match y.edge_id with
				  | Some x -> x
				  | None -> (get_source_id (get_edge_source p)))
      | GXLRel (x,y) -> y.rel_id

(* This can go on recursing for ever, yah!!*)
and get_target_id = function
  | GXLNode (x,y) -> y.node_id
  | GXLLocalConnection x -> 
    match x with
	GXLEdge (x,y) as p -> (match y.edge_id with
				  | Some x -> x
				  | None -> (get_target_id (get_edge_source p)))
      | GXLRel (x,y) -> y.rel_id
;;

(**
   {1 The constructors}
*)

let gxl_type_make ~xlink_href = GXLType ({xlink_type="simple";xlink_href=xlink_href});;

let gxl_int_make ~value = GXLInt ({v=value});;

let gxl_float_make ~value = GXLFloat ({v=value});;
let gxl_string_make ~value = GXLString ({v=value});;
let gxl_bool_make ~value = GXLBool ({v=value});;
let gxl_enum_make ~value = GXLEnum ({v=value});;

let gxl_locator_make ~value = GXLLocator ({x_type="simple";uriv=value});;

let gxl_atomic_value_make ~value = GXLAtomicValue(value);;
let gxl_composite_value_make ~value = GXLCompositeValue(value);;
let gxl_locator_value_make ~value = GXLLocatorValue(value);;


let gxl_bag_make ~value = GXLBag({comp_v=value});;
let gxl_tup_make ~value = GXLTup({comp_v=value});;
let gxl_set_make ~value = GXLSet({comp_v=value});;
let gxl_seq_make ~value = GXLSeq({comp_v=value});;

(**
   @param attr_id = None by default
   @param attr_kind = None by default
   @param attr_children = [] by default
*)
let gxl_attr_make ~attr_value ?(attr_id=None) ?(attr_kind=None) ?(attr_children=[]) ~attr_name = 
  GXLAttr({attr_val=attr_value},
	  {id=attr_id;name=attr_name;kind=attr_kind},
	  {children_attr=attr_children})
;;

(**
   @param role is None by default
   @param startorder is None by default
   @param endorder is None by default
   @param direction is GXL_NONE or determined by the target by default
*)
let gxl_relend_make ?(role=None) ?(startorder=None) ?(endorder=None) ?(direction=GXL_NONE)
   ?(attrs=[]) ?(local_connection = None) ~target =
  let t = try Hashtbl.find node_table target with | Not_found -> failwith ("Node " ^ target ^ " not found") in
  let relend = 
    GXLRelend({relend_attrs= attrs;
	       relend_target = (ref t);
	       relend_local_connection = local_connection},
	      {relend_role = role;
	       relend_direction = direction;
	       relend_startorder = startorder;
	       relend_endorder = endorder}) in
  (* Need to the set the target relend reference *)
  add_to_connection_list relend t;
  relend
;;


let gxl_typed_element_make ~value = GXLTypedElement(value);;
let gxl_graph_element_make ~value = GXLGraphElement(value);;

(**
   @param role = None by default
   @param edgeids = None by default
   @param hypergraph = None by default
   @param edgemode = Directed by default
   @param gxl_type = None by default
   @param attrs = [] by default
   @param elements = [] by default
   @param rels = [] by default
*)
let gxl_graph_make ?(role=None) ?(edgeids=None) ?(hypergraph=None) ?(edgemode=Directed) ?(gxl_type=None)
    ?(attrs=[]) ?(elements=[]) (* ?(rels=[]) *) ~id = 
  GXLGraph({graph_type=gxl_type;graph_attrs=attrs;
	   graph_element = elements; (* graph_rels=rels *)},
  {graph_id=id; graph_role=role; graph_edgeids=edgeids;
  hyper_graph=hypergraph;graph_edge_mode=edgemode})
;;
  
let gxl_local_connection_make ~value = GXLLocalConnection(value);;


(**
   @param gxl_type = None by Default
   @param attrs = [] by Default
   @param graphs = [] by Default
*)
let gxl_node_make ?(gxl_type=None) ?(attrs=[]) ?(graphs=[]) ~id = 
  let node = GXLNode({node_shared={t = gxl_type; a = attrs; g=graphs};
	   node_tentacles = []},
	  {node_id=id}) in
  Hashtbl.add node_table id node;
  node
;;

(**
   @param attrs = [] by Default
   @param gxl_type = None by Default
   @param graphs = [] by Default
   @param id = None by Default
   @param fromorder = None by Default
   @param toorder = None by Default
   @param isdirected = None or depends upon from and to targets by Default
*)
let gxl_edge_make ?(gxl_type=None) ?(attrs=[]) ?(graphs=[]) ?(id=None) ?(fromorder=None)
    ?(toorder=None) ?(isdirected= None) ~from_node ~to_node = 
  (* First find the nodes that have been already made *)
  let fnode = try Hashtbl.find node_table from_node with | Not_found -> failwith ("Node " ^ from_node ^ " not found") in
  let tnode = try Hashtbl.find node_table to_node with | Not_found -> failwith ("Node " ^ to_node ^ " not found") in
  let edge = GXLEdge({edge_shared={t = gxl_type; a = attrs; g=graphs};
	   edge_tentacles = [||]},
	  {edge_id=id;edge_fromorder=(ref None);
	  edge_toorder = (ref None); edge_isdirected = isdirected}) in
  (* Now build the relend *)
  let source_tentacle = gxl_relend_make ~role:None ~startorder:None ~endorder:fromorder ~direction:GXL_IN 
    ~attrs:[] ~local_connection:(Some (ref edge)) ~target:(get_graph_element_id fnode) in
  let target_tentacle = gxl_relend_make ~role:None ~startorder:None ~endorder:toorder ~direction:GXL_OUT 
    ~attrs:[] ~local_connection:(Some (ref edge)) ~target:(get_graph_element_id tnode) in
  (match edge with GXLEdge (x,y) -> x.edge_tentacles <- [|source_tentacle; target_tentacle|];
    (* From order should be a reference to the endorder of the tentacle *)
    y.edge_fromorder := (match source_tentacle with GXLRelend (_,y) -> y.relend_endorder 
      | _ -> raise (GXLElementTypeExn "not of type GXLRelend"));
    y.edge_toorder := (match target_tentacle with GXLRelend (_,y) -> y.relend_endorder 
      | _ -> raise (GXLElementTypeExn "not of type GXLRelend"))
    | _ -> raise (GXLElementTypeExn "not of type GXLEdge"));
  edge
;;

(**
   @param gxl_type = None by Default
   @param attrs = [] by Default
   @param graphs = [] by Default
   @param isdirected = None or depends upon from and to targets by Default
   @param relends = [] by Default
*)
let gxl_rel_make ?(gxl_type=None) ?(attrs=[]) ?(graphs=[])
    ?(isdirected= None) ?(relends=[]) ~id =
  let rel = GXLRel ({rel_shared={t = gxl_type; a=attrs; g=graphs};rel_end=relends},
	  {rel_id=id; rel_isdirected=isdirected}) in
  List.iter (function x -> match x with GXLRelend (x,_) -> (match x.relend_local_connection with 
      Some x -> x := rel | _ -> () ) | _ -> raise GXLRelendException) relends;
  rel
;;

let gxl_value_make ~value = GXLValue value;;
let gxl_gxl_make ?(graphs=[]) ~xlink = GXLGXL ({xlink=xlink;graphs=graphs});;
let gxl_attributed_element_make ~value = GXLAttributedElement value;;
