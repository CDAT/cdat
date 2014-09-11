
#!/bin/sed -f
# Simple Sed Program to remove all comments from c program
# --------------------------------------------------------------------
# This is a free shell script under GNU GPL version 2.0 or above
# Copyright (C) 2005 nixCraft project.
# Feedback/comment/suggestions : http://cyberciti.biz/fb/
# -------------------------------------------------------------------------
# This script is part of nixCraft shell script collection (NSSC)
# Visit http://bash.cyberciti.biz/ for more information.
# -------------------------------------------------------------------------
# if no /* get next
/\/\*/!b
# here we've got an /*, append lines until get the corresponding
# */
:x
/\*\//!{
N
bx
}
# delete /*...*/
s/\/\*.*\*\///
