import vcs

list=["boxfill","meshfill","isofill","textcombined","texttable","vector"]
for obj in list:
    s=''
    tc=''
    if obj == 'textcombined':
        vcs.createtextcombined('EXAMPLE_tt', 'qa', 'EXAMPLE_tto', '7left')
        tc="'EXAMPLE_tt','EXAMPLE_tto'"
    code="s=vcs.get%s(%s)" % (obj, tc)
    exec(code)
    try:
        s.script(obj+"_script.py")
    except:
        raise("Python script creation broke on "+obj+".script()")
