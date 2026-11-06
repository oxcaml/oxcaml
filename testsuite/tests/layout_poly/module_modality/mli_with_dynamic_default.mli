(* The file starts with [@@ dynamic], so the whole module becomes static
   (cmi_staticity = Static) and items default to [@@ dynamic] modality. *)

@@ dynamic

(* defaults to [@@ dynamic] *)
val foo : int

(* overridden to [@@ static] *)
val bar : int @@ static
