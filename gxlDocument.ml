(** This is the GXL parser top-level
    @author Avinash Malik 
    date Sat Mar 17 17:35:52 GMT 2012 
*)

module GxlDocument : sig 
  exception Not_found;;
  exception GXLParseError of string ;;
  type t
  val make : unit -> t
  val parse : file:string -> t -> unit
  val get_document_element : t -> GXL.gxl_element
  val write : file:string -> t -> unit
  val dtd_validate : dtd:string -> t -> Xml.xml
end =
struct
  exception Not_found;;
  exception GXLParseError of string;;
  open GXL;;
  (* All the types of GXL *)
  type 'a internal_type =
    PGXLElement of gxl_element
  | PGXLType of gxl_type
  | PGXLValue of gxl_value
  | PGXLAttributedElement of gxl_attributed_element
  | PGXLTypedElement of gxl_typed_element
  | PGXLGraphElement of gxl_graph_element
  | PGXLLocalConnection of gxl_local_connection
  | PGXLAttr of gxl_attr
  ;;
  type t = 
    GxlParser of t_t
  and t_t = {f_validator:(gxl_element -> Buffer.t -> bool);
	     mutable gxl_gxl:gxl_element;
	     f_to_xml: (gxl_element -> Xml.xml)}
  ;;

  let validator (gxl_gxl:gxl_element) (error_buffer:Buffer.t) = true;;

  (**
     Converts from the GXL type to the Xml type
  *)

  let doc_to_xml = function
    | GXLGXL v -> gxl_gxl_to_xml v
    | _ -> raise (GXLElementTypeExn "starting element not of type GXLGXL")
  ;;

  (**
     This function returns a new GXL parser with default options
  *)
  let make () = GxlParser ({f_validator=validator;
  			    gxl_gxl=GXLGXL({xlink="http://www.w3.org/1999/xlink";graphs=[]});
  			    f_to_xml=doc_to_xml});;
  (** Get the GXLGXL type element from this document*)
  let get_document_element = function
    | GxlParser x -> x.gxl_gxl
  ;;


  (** This function can be used to validate the GXL against its dtd *)
  let dtd_validate ~dtd doc =
    try
      let gxl_gxl = get_document_element doc in
      let to_prove = (doc_to_xml (gxl_gxl)) in
      let dtd_struct = Dtd.parse_file dtd in
      let dtd_checked = Dtd.check dtd_struct in
      let ret = Dtd.prove dtd_checked "<gxl>" to_prove in
      ret
    with
    | Dtd.Check_error x as h-> raise h
    | Xml.File_not_found x as h -> raise h
    | Dtd.Prove_error x as h -> raise h
    | _ as h -> raise h
  ;;
  
  let direction_of_string = function
    | "in" -> GXL_IN
    | "out" -> GXL_OUT
    | _ ->  GXL_NONE
  ;;


  (**
     The main parser function
  *)
  let rec parse ~file (parser:t) =
    try
      let xml = Xml.parse_file file in
      match parser with
  	GxlParser x ->
  	  let ret = parse_xml xml in
	  match ret with
	  | PGXLElement t -> (match t with GXLGXL _ as h -> x.gxl_gxl <- h | _ -> raise (GXLParseError "Top node not of type GXLGXL"))
	  | _ ->raise (GXLParseError "Top node not of type GXLGXL")
    with
    | Xml.Error x as h -> print_endline (Xml.error x); raise h
  and parse_xml xml = 
    match xml with
    | Xml.Element (name,attrs_list,children_list) -> 
      (match name with
	"gxl" -> build_gxl_gxl (name,attrs_list,children_list) (*done*)
      | "graph" -> build_gxl_graph (name,attrs_list,children_list) (*done*)
      | "node" -> build_gxl_node (name,attrs_list,children_list) xml (*done*)
      | "edge" -> build_gxl_edge (name,attrs_list,children_list) xml (* done *)
      | "rel" -> build_gxl_rel (name,attrs_list,children_list) xml (*done*)
      | "relend" -> build_gxl_relend (name,attrs_list,children_list) xml (*done*)
      | "type" -> build_gxl_type (name,attrs_list,children_list)(*done*)
      | "locator" -> build_gxl_locator (name,attrs_list,children_list)(*done*)
      | "attr" -> build_gxl_attr (name,attrs_list,children_list) xml (* done *)
      | "bag" | "seq" | "set" | "tup" -> build_gxl_composite_values(name,attrs_list,children_list)(*done*)
      | "string" | "int" | "bool" | "enum" | "float" -> build_gxl_atomic_values(name,attrs_list,children_list)(*done*)
      | _ -> raise (GXLParseError "unkown GXL element, cannot parse"))
    | Xml.PCData yu -> raise (GXLParseError ("unkown GXL element" ^ yu ^ " cannot parse"))
  and build_gxl_attr (_,y,z) ele = 
    let name = try Xml.attrib ele "name" with | Xml.Not_element _ | Xml.No_attribute _ ->
      raise (GXLParseError ("GXLAttr has no name")) in
    let id = try Some (Xml.attrib ele "id") with| Xml.Not_element _ | Xml.No_attribute _ -> None in
    let kind = try Some (Xml.attrib ele "kind") with| Xml.Not_element _ | Xml.No_attribute _ -> None in
    let children = List.map parse_xml z in
    let attrs = ref [] in List.iter (fun x -> match x with PGXLAttr y -> attrs := y :: !attrs | _ -> ()) children; 
    let value = ref [] in List.iter (fun x -> match x with PGXLValue y -> value := y :: !value | _ -> ()) children;
    let v =
      if (List.length !value) <> 1 then raise (GXLParseError ("GXLAtrr " ^ name ^ " does not have exactly one GXLValue"))
      else (List.hd (!value)) in
    let () = print_endline name in
    PGXLAttr (gxl_attr_make ~attr_value:v ~attr_id:id ~attr_kind:kind ~attr_children:!attrs ~attr_name:name);
  and build_gxl_gxl (_,y,z) =
    (* First get the graphs list *)
    let parse_graphs = List.map parse_xml z in
    let graphs = List.map (function x -> match x with PGXLTypedElement x -> (match x with GXLGraph _ -> x 
      | _ -> raise 
	(GXLParseError "GXLGXL: child not of type GXLGraph"))
      | _ -> raise (GXLParseError "GXLGXL: child not of type GXLGraph"))
      parse_graphs in
    (* Now make the attributes *)
    let link =
      (let xlink = List.map (fun (x,y) -> match x with "xmlns:xlink" -> y | _ -> raise (GXLParseError "GXLGXL attr not of type xmlns:xlink"
       )) y in
       if List.length xlink > 1 then raise (GXLParseError "xlink length greater than 1")
       else if List.length xlink = 0 then "http://www.w3.org/1999//xlink"
       else List.hd xlink ) in
    PGXLElement (gxl_gxl_make ~graphs:graphs ~xlink:link)
  and build_gxl_graph (_,y,z) =
    (* First get the children *)
    let children = List.map parse_xml z in
    let gtype = ref None in List.iter (fun x -> match x with PGXLType x -> (match x with GXLType _ -> gtype := Some x )
      | _ -> ()) children; 
    (* Now taking out the atributes of the graph node *)
    let id = ref "" in
    let role = ref None in
    let edgeids = ref (Some false) in
    let hypergraph = ref (Some false) in
    let edgemode = ref Directed in
    (* Use a map to iterate and obtain the values *)
    List.iter (fun (x,y) ->
      match x with
      | "id" -> id := y
      | "role" -> role := Some y
      | "edgeids" -> edgeids := Some (bool_of_string y)
      | "hypergraph" -> hypergraph := Some (bool_of_string y)
      | "edgemode" -> edgemode := (match y with | "directed" -> Directed | "undirected" -> Undirected |
  	  "defaultdirected" -> DefaultDirected | "defaultundirected" -> DefaultUndirected 
	| _ -> raise (GXLParseError "GXLGraph error: attribute edgemode not defined"))
      | _ -> () ) y;
    if !id = "" then raise (GXLParseError "GXLGraph id undefined");
    let element_list = ref [] in
    List.iter (fun x -> match x with PGXLLocalConnection x -> 
      let isd = 
	(
	  match x with 
	  | GXLRel (_,t) -> t.rel_isdirected
	  | GXLEdge (_,t) -> t.edge_isdirected
	) in
      (match isd with
      | None -> ()
      | Some t -> if (((!edgemode = Directed) && (not t)) || ((!edgemode = Undirected) && t))
	then raise (GXLParseError "Directions do not match GXLGraph/GXLLocalConnection"));
      element_list := gxl_local_connection_make (x) :: !element_list
    | _ -> ()) children;
    List.iter (fun x -> match x with PGXLGraphElement x -> (match x with GXLNode _ ->  element_list := x :: !element_list
    | _ -> ()) | _ -> ()) children;
    let attr_list = ref [] in List.iter (fun x -> match x with PGXLAttr x -> (match x with GXLAttr _ -> attr_list :=  x::!attr_list)
      | _ -> ()) children;
    PGXLTypedElement (gxl_graph_make ~role:!role ~edgeids:!edgeids ~hypergraph:!hypergraph ~edgemode:!edgemode
  			~gxl_type:!gtype ~attrs:!attr_list ~elements:!element_list ~id:!id)
  and build_gxl_atomic_values (x,_,z) =
    (*  These guys can possibly throw a conversion exception *)
    let v = (match (x,z) with
      | ("int",[(Xml.PCData y)]) -> gxl_atomic_value_make (gxl_int_make (int_of_string y))
      | ("float",[(Xml.PCData y)]) -> gxl_atomic_value_make (gxl_float_make (float_of_string y))
      | ("string",[(Xml.PCData y)]) -> gxl_atomic_value_make (gxl_string_make y)
      | ("enum", [(Xml.PCData y)]) -> gxl_atomic_value_make (gxl_enum_make y)
      | ("bool", [(Xml.PCData y)]) -> gxl_atomic_value_make (gxl_bool_make (bool_of_string y))
      | _ -> raise (GXLParseError ("GXLAtomicValue not of correct type" ^ x))
    ) in
    PGXLValue v
  and build_gxl_type (_,y,_) =
    let (_,link) = try List.find (fun (x,y) -> x = "xlink:href") y with Not_found -> 
      raise (GXLParseError "GXLType xlink:href not found") in PGXLType (gxl_type_make link)
  and build_gxl_locator (_,y,_) =
    let (_,link) = try List.find (fun (x,y) -> x="xlink:href") y with Not_found -> 
      raise (GXLParseError "GXLLocator xlink:href missing") in PGXLValue (gxl_locator_value_make (gxl_locator_make link))
  and build_gxl_composite_values (x,_,z) =
    let children = List.map parse_xml z in
    let lambda = (function x -> match x with PGXLValue y -> y 
      | _ -> raise (GXLParseError "GXLCompositeValue children not of type GXLValue")) in
    let v =
      (match x with
      | "bag" -> gxl_composite_value_make (gxl_bag_make (List.map lambda children))
      | "tup" -> gxl_composite_value_make (gxl_tup_make (List.map lambda children))
      | "set" -> gxl_composite_value_make (gxl_set_make (List.map lambda children))
      | "seq" -> gxl_composite_value_make (gxl_seq_make (List.map lambda children))
      | _ -> raise (GXLParseError ("Unknown composite type " ^ x))
      ) in PGXLValue v
  and build_gxl_node (_,y,z) ele =
    (* First get the id *)
    let id = try Xml.attrib ele "id" with | Xml.No_attribute _ | Xml.Not_element _ -> 
      raise (GXLParseError "GXLNode id not defined") in
    (* Now get the children *)
    let children = List.map parse_xml z in
    let ntype = ref None in List.iter (fun x -> match x with PGXLType x ->  ntype := Some x | _ -> ()) children; 
    let ngraphs = ref [] in List.iter (fun x -> match x with PGXLTypedElement x -> (match x with GXLGraph _ -> 
      ngraphs := x :: !ngraphs | _ -> raise (GXLParseError ("GXLNode " ^ id ^ " cannot have anything other than graph children"))) 
      | _ -> ()) children;
    let nattrs = ref [] in List.iter (fun x -> match x with PGXLAttr x -> nattrs := x :: !nattrs | _ -> ()) children; 
    PGXLGraphElement (gxl_node_make ~gxl_type:!ntype ~attrs:!nattrs ~graphs:!ngraphs ~id:id);
  and build_gxl_rel (_,y,z) ele =
    (* First get the required attributes *)
    let id = try Xml.attrib ele "id" with | Xml.No_attribute _ | Xml.Not_element _ -> raise (GXLParseError "GXLRel has no id ")in
    let isdirected = try Some (bool_of_string (Xml.attrib ele "isdirected")) with Xml.No_attribute _ -> None 
      | Xml.Not_element _ -> raise (GXLParseError ("GXLRel " ^ id ^ " has an unknown attribute")) in
    (* Now make the children *)
    let children = List.map parse_xml z in
    let rtype = ref None in List.iter (fun x -> match x with PGXLType x -> rtype := Some x  | _ -> ()) children;
    let rattrs = ref [] in List.iter (fun x -> match x with PGXLAttr x -> rattrs := x :: !rattrs | _ -> ()) children;
    let rgraphs = ref [] in List.iter (fun x -> match x with PGXLTypedElement x -> 
      (match x with GXLGraph _ -> rgraphs := x :: !rgraphs 
      | _ -> raise (GXLParseError ("GXLRel " ^ id ^ "cannot contain graph_element_type children other than GXLGraph")))
      | _ -> ()) children;
    let isd = (match isdirected with Some x -> x | None -> true) in
    List.iter (fun x -> match x with PGXLAttributedElement x -> 
      (match x with GXLRelend _ -> 
	if ((gxl_relend_get_direction x) = GXL_NONE) && (isd) then 
	  raise (GXLParseError ("Directions of GXLRelend/GXLRel do not match " ^ id))
      | _ -> raise (GXLParseError "GXLRel cannot contain any children but GXLRelend types"))
    | _ -> ()) children;
    let rrelends = ref [] in List.iter (fun x -> match x with PGXLAttributedElement x -> rrelends := x :: !rrelends | _ -> ()) children;
    PGXLLocalConnection (gxl_rel_make ~gxl_type:!rtype ~attrs:!rattrs ~graphs:!rgraphs ~isdirected:isdirected ~relends:!rrelends ~id:id);
  and build_gxl_relend (_,y,z) ele =
    (* First get the children *)
    let children = List.map parse_xml z in
    let rattrs = ref [] in List.iter(fun x -> match x with PGXLAttr x -> rattrs := x :: !rattrs | _ -> ()) children; 
    let role = try Some (Xml.attrib ele "role") with | Xml.Not_element _ | Xml.No_attribute _ -> None in
    let startorder = try Some (int_of_string (Xml.attrib ele "startorder")) with | Xml.Not_element _ | Xml.No_attribute _ -> None in
    let endorder = try Some (int_of_string (Xml.attrib ele "endorder")) with | Xml.Not_element _ | Xml.No_attribute _ -> None in
    let direction = try (direction_of_string (Xml.attrib ele "direction")) with | Xml.Not_element _ | Xml.No_attribute _ -> GXL_NONE in
    let target = try (Xml.attrib ele "target") with | Xml.Not_element _ | Xml.No_attribute _ -> 
      raise (GXLParseError ("GXLRelend has no target specified !!")) in
    PGXLAttributedElement (gxl_relend_make ~role:role ~startorder:startorder ~endorder:endorder ~direction:direction ~attrs:!rattrs
			     ~local_connection:None ~target:target);
  and build_gxl_edge (_,y,z) ele = 
    let id = try Some (Xml.attrib ele "id") with Xml.No_attribute _ | Xml.Not_element _ -> None in
    let isdirected = try Some (bool_of_string (Xml.attrib ele "isdirected")) with Xml.No_attribute _ | Xml.Not_element _ -> None in
    let toorder = try Some (int_of_string (Xml.attrib ele "toorder")) with Xml.No_attribute _ | Xml.Not_element _ -> None in
    let fromorder = try Some (int_of_string (Xml.attrib ele "fromorder")) with Xml.No_attribute _ | Xml.Not_element _ -> None in
    let to_node = try (Xml.attrib ele "to") with Xml.No_attribute _ | Xml.Not_element _ -> 
      raise (GXLParseError ("GXLEdge's target is not defined")) in
    let from_node = try (Xml.attrib ele "from") with Xml.No_attribute _ | Xml.Not_element _ -> 
      raise (GXLParseError ("GXLEdge's target is not defined")) in
    let children = List.map parse_xml z in
    let eattrs = ref [] in List.iter (fun x -> match x with PGXLAttr x -> eattrs := x :: !eattrs | _ -> ()) children;
    let etype = ref None in List.iter (fun x -> match x with PGXLType x -> etype := Some x  | _ -> ()) children;
    let egraphs = ref [] in List.iter (fun x -> match x with PGXLTypedElement x -> 
      (match x with GXLGraph _ -> egraphs := x :: !egraphs 
      | _ -> raise (GXLParseError ("GXLEdge cannot contain graph_element_type children other than GXLGraph")))
      | _ -> ()) children;
    PGXLLocalConnection (gxl_edge_make ~gxl_type:!etype ~attrs:!eattrs ~graphs:!egraphs 
			   ~id:id ~fromorder:fromorder ~toorder:toorder 
			   ~isdirected:isdirected ~from_node:from_node ~to_node:to_node);
  ;;


  let write ~file gxl_parser =
    let gxl_gxl = get_document_element gxl_parser in
    match gxl_parser with
      GxlParser x ->
  	  (*validate the gxl arguments are gxl_gxl errors Xml.xml *)
  	let errors = Buffer.create 100 in
  	let ret = x.f_validator gxl_gxl errors in
  	if (ret = true) then
  	  let ret = Xml.to_string_fmt (x.f_to_xml gxl_gxl) in
  	    (*open a file descriptor to the named file*)
  	  let ochan = open_out file in
  	  output_string ochan ret;
  	  flush ochan; (*flush everything down*)
  	  close_out ochan
  	else
  	  prerr_endline (Buffer.contents errors)
  ;;
end
