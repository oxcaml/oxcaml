type input =
  | Typecore_error of
      { loc : Location.t;
        env : Env.t;
        error : Typecore.error
      }
  | Typemod_error of
      { loc : Location.t;
        env : Env.t;
        error : Typemod.error
      }
  | Includemod_apply_error of
      { env : Env.t;
        app_name : Includemod.application_name;
        mty_f : Types.module_type;
        args :
          (Includemod.Error.functor_arg_descr
          * Types.module_type
          * Typedtree.mode_with_locks)
          list
      }
  | Typedecl_error of
      { loc : Location.t;
        error : Typedecl.error
      }
  | Env_lookup_error of
      { loc : Location.t;
        error : Env.lookup_error
      }
  | Unique_use_during_borrowing of
      Uniqueness_analysis.Usage.unique_use_during_borrowing_error
  | Uniqueness_error of Uniqueness_analysis.error
  | Embedded_mode_error of exn

val diagnose_without_context :
  source:Diagnostic_source.t ->
  loc:Location.t ->
  input ->
  Structured_diagnostic.t option
