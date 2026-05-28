#!/usr/bin/env -S sed -f

# Header
1i\
<!DOCTYPE html>\
<html lang="en">\
  <head>\
    <meta charset="UTF-8">\
    <meta name="viewport" content="width=device-width,initial-scale=1">\
    <title>Scalar codegen</title>\
    <style>\
      html{overflow-x:hidden;scrollbar-width:thin}\
      body{font-family:Helvetica,sans-serif,ui-sans-serif;font-size:18px;\
        line-height:1.5;background-color:#1d1d1d;color:#c5c8c6}\
      body{margin-left:2rem;margin-right:2rem}\
      pre{margin-left:2rem}\
      summary>pre{display:inline-block;margin:0rem 0.5rem}\
      h1{font-weight:400;font-size:30px;line-height:36px;margin-top:1.5rem;\
        margin-bottom:1rem}\
    </style>\
  </head>\
  <body>\
    <h1>OxCaml Scalar codegen</h1>\
    <p>Click on a primitive to see its codegen.</p>

# Delete the TEST stanza
1,/^\*)/d

# Delete the val declarations
/^let /d

# Escape < since this is HTML
s/</\&lt;/g

# Wrap expect tests
s/\(^external.*$\)/<details><summary><pre>\1<\/pre><\/summary>/
s/^\[%%expect_asm.*$/<pre>/
s/^|\}\]$/<\/pre><\/details>/

# Footer
$a\
  </body>\
</html>
