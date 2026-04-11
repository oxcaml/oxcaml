all: extract_args

extract_args.ml: extract_args.mll
	ocamllex -o extract_args.ml extract_args.mll

extract_args: extract_args.ml
	ocamlc -o extract_args extract_args.ml

clean:
	rm -f *.cmo *.cmi *.cma extract_args extract_args.ml
