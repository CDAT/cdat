from vcs.texttable import Tt
import vcs.utils
import glob
import os
import doctest


# for testing. remove when done
def find_obj(olist, name):
    for obj in olist:
        if obj.name == name:
            return obj
    print ("No object named " + name)


def cleanup():
    """Cleanup for the doctests. If some files aren't being deleted after testing, add their glob signature to the
    patterns list.
    """
    gb = glob.glob
    patterns = ["example.*", "*.svg", "ex_*", "my*", "filename.*", "*.png", "deft_box.py", "*.mpeg"]
    for pattern in patterns:
        fnames = gb(pattern)
        for name in fnames:
            try:
                os.remove(name)
            except:
                continue

f = doctest.DocTestFinder(exclude_empty=False)
runner = doctest.DocTestRunner(optionflags=doctest.ELLIPSIS|doctest.NORMALIZE_WHITESPACE)
objs = f.find(Tt) # list of objects with non-empty docstrings
failed = False
doctest_failed, no_doctest, no_docstring = [], [], []
if not os.path.isdir(vcs.sample_data):
    vcs.download_sample_data_files() # we need to have these for any example that uses slabs made from sample data
for obj in objs:
    if obj.name.split('.')[-1][0] == '_' or obj.docstring.find('.. pragma: skip-doctest') >= 0:
        continue  # this is private or has been explicitly ignored; skip it.
    elif obj.lineno is None:
        continue # this is not actually something that can be documented; most likely came from __slots__.
    if obj.docstring is '':
        no_docstring.append(obj.name) # store for empty docstring warning
        continue
    examples = obj.examples
    if len(examples) > 0:
        # There are examples. Do they run?
        results = runner.run(obj)
        if results.failed > 0:
            failed = True
            doctest_failed.append(obj.name)
    else:
        # All docstrings not specifically skipped and non-empty require a doctest
        failed = True
        no_doctest.append(obj.name)
cleanup() # testing done

# Report summary for cdash
if len(doctest_failed):
    print "FAILING DOCTESTS:"
    for name in doctest_failed:
        print "\t" + name
if len(no_doctest):
    print "MISSING DOCTESTS:"
    for name in no_doctest:
        print "\t" + name
if len(no_docstring):
    print "NO DOCUMENTATION:"
    for name in no_docstring:
        print "\t" + name

assert failed is False
print ("All doctests passed for vcs.texttable")

