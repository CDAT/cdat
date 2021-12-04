import vcs, sys, os

def attrCompare(old_dict, new_dict):
    if old_dict.keys() != new_dict.keys():
        return (False, "keys mismatch")
    for key in old_dict.keys():
        if old_dict[key] == new_dict[key]:
            continue
        elif type(old_dict[key]) is tuple:
            l = list(old_dict[key])
            if l == new_dict[key]:
                continue
            else:
                return (False, key + "not equal.")
    return (True, "All values equal")

# graphics method list
gms = ["boxfill", "meshfill", "isofill", "vector", "isoline", "1d", "taylordiagram", "3d_scalar", "3d_dual_scalar",
       "3d_vector"]
for obj in gms:
    s = vcs.creategraphicsmethod(obj, 'default', 'test_%s' % obj)
    s_dict = vcs.dumpToDict(s)[0]
    scr = obj + "_script.py"
    try:
        s.script(scr)
    except:
        print "Python script creation broke on " + obj + ".script()"
        sys.exit(1)
    else:
        # script was written successfully, now test for accuracy when it's run
        # remove the object, or scriptrun() will just use the same thing
        vcs.removeobject(s)
        try:
            vcs.scriptrun(scr) # test to see that scriptrun actually runs
        except:
            print "Scriptrun broke on " + obj
            os.remove(scr)
            sys.exit(1)
        else:
            # script has both written and run without failure, just need to check for accuracy
            try:
                new_s = vcs.getgraphicsmethod(obj, "test_%s" % obj)
            except:
                print "Scriptrun failed: test_%s does not exist" %obj
                sys.exit(1)
            else:
                new_s_dict = vcs.dumpToDict(new_s)[0]
                comp = attrCompare(s_dict, new_s_dict)
                if not comp[0]:
                    print "Scriptrun failed: " + comp[1]
                    sys.exit(1)
    if os.path.exists(scr):
        os.remove(scr)


# text objects
tt = vcs.createtexttable('test_tt')
to = vcs.createtextorientation('test_tto')
tcs = vcs.listelements('textcombined')
if len(tcs) == 0:
    tc = vcs.createtext(Tt_name="testtext", Tt_source=tt.name, To_name="testtext", To_source=to.name)
else:
    tc = vcs.gettext(tcs[0])

# other secondaries
fa = vcs.createfillarea('test_fillarea')
ma = vcs.createmarker('test_marker')
li = vcs.createline('test_line')
secondaries = [tt, to, tc, fa, ma, li]
for sec in secondaries:
    scr = sec.s_name + '_script.py'
    old_dict = vcs.dumpToDict(sec)[0]
    try:
        sec.script(scr)
    except:
        print "Python script creation broke on " + sec.s_name + ".script()"
        sys.exit(1)
    else:
        vcs.removeobject(sec)
        try:
            vcs.scriptrun(scr) # test to see that scriptrun actually runs
        except:
            print "Scriptrun broke on " + sec.s_name
            os.remove(scr)
            sys.exit(1)
        else:
            if sec.s_name == 'To':
                getfunc = vcs.gettextorientation
            elif sec.s_name == 'Tt':
                getfunc = vcs.gettexttable
            elif sec.s_name == 'Tc':
                getfunc = (vcs.gettext, "testtext", "testtext")
            elif sec.s_name == 'Tm':
                getfunc = vcs.getmarker
            elif sec.s_name == 'Tf':
                getfunc = vcs.getfillarea
            else:
                getfunc = vcs.getline
            # script has both written and run without failure, just need to check for accuracy
            try:
                if type(getfunc) is tuple:
                    new_sec = getfunc[0](getfunc[1], getfunc[2])
                else:
                    new_sec = getfunc(sec.name)
            except:
                print "Scriptrun failed: %s does not exist" % sec.name
                sys.exit(1)
            else:
                new_dict = vcs.dumpToDict(new_sec)[0]
                comp = attrCompare(old_dict, new_dict)
                if comp[0]:
                    print comp[1] + " for " + sec.s_name
                else:
                    print "Scriptrun failed: " + comp[1]
                    sys.exit(1)
    if os.path.exists(scr):
        os.remove(scr)
