Test the floating attribute rules which only check the inline code, without expanding
it.

  $ run() {
  >   cat > program.ml
  >   ./ppx.exe -no-color -null -diff-cmd 'diff -u --label "" --label ""' "$@" program.ml
  > }

Test [attr_str_floating_expect] via [@@@identity_inline].

  $ run -raise-embedded-errors <<EOF
  > [@@@identity_inline
  >   module T : sig
  >     val foo : [%str]
  >   end = struct
  >     let foo = [%suffix "apples"]
  >   end]
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -4,4 +4,6 @@
     end = struct
       let foo = [%suffix "apples"]
     end]
  +module T : sig val foo : [%str ] end =
  +  struct let foo = [%suffix "apples"] end
   [@@@end]
  [1]

Test [attr_sig_floating_expect] via [@@@identity_inline].

  $ run -raise-embedded-errors <<EOF
  > module type S = sig
  >   [@@@identity_inline:
  >     val foo : [%str]
  >     include module type of struct
  >       let foo = [%suffix "apples"]
  >     end]
  >   [@@@end]
  > end 
  > EOF
  --- 
  +++ 
  @@ -4,5 +4,8 @@
       include module type of struct
         let foo = [%suffix "apples"]
       end]
  -  [@@@end]
  +  
  +val foo : [%str ]
  +include module type of struct let foo = [%suffix "apples"] end
  +[@@@end]
   end 
  [1]
