(* The file starts with [@@ static], so the whole module becomes static
   (cmi_staticity = Static) and items default to [@@ static] modality. *)

@@ static

(* defaults to [@@ static] *)
val foo : int

(* overridden to [@@ dynamic] *)
val bar : int @@ dynamic
