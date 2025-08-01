This style guide is useful for reviewing diff in PR. 

Line Width
-----
No line shall exceed 80 characters. You should not 


Let In
------
For single-line let statement, in should appear in the same line as let.
For multi-line let statement, in should appear on a new line with the same identation as let.

GOOD:
```
  let x = f short_argument y in
  let y = f long_argument
	    continue_long_argument
  in
  z
```

BAD:
```
  let x = f short_argument y 
  in
  let y = f long_argument
	    continue_long_argument in
  z
```


Tuple
-------
Elements of tuple, if appearing on the next line following `,`, should have the same indentation as previous element.

GOOD:
```
      f long_argument long_argument y,
      g x
```

BAD:
```
      f long_argument long_argument y,
        g x
```
        


Empty Lines
-------
There should be only a single empty line between functions.

GOOD:
```
let f x = x

let g x = x
```

BAD:
```
let f x = x


let g x = x
```

Parenthesis
-----
You should not include unnecessary parenthesis. 
Content should immediately follow an open parenthesis, not in a new line.

GOOD:
```
  | Cons x y -> 
      (match f long_argument x with
```

BAD:
```
  | Cons x y -> 
      (
         match f long_argument x with
```



