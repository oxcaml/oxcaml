type error =
  | Not_included of
      { loc : Location.t;
        explanation : Includemod.explanation
      }
  | Strengthening_mismatch of
      { loc : Location.t;
        path : Longident.t;
        explanation : Includemod.explanation
      }
  | Applicative_functor_mismatch of
      { loc : Location.t;
        constrained : Longident.t;
        type_path : Path.t;
        explanation : Includemod.explanation
      }
  | Substitution_mismatch of
      { loc : Location.t;
        path : Longident.t;
        explanation : Includemod.explanation
      }
  | Functor_application_mismatch of
      { env : Env.t;
        app_name : Includemod.application_name;
        mty_f : Types.module_type;
        args :
          (Includemod.Error.functor_arg_descr
          * Types.module_type
          * Typedtree.mode_with_locks)
          list
      }
  | Type_definition_mismatch of
      { loc : Location.t;
        type_expr : Types.type_expr;
        env : Env.t;
        mismatch : Includecore.type_mismatch
      }

val diagnose : loc:Location.t -> error -> Structured_diagnostic.t option
