def test_values_setting(gm,attributes,good_values=[],bad_values=[]):
  if isinstance(attributes,str):
    attributes=[attributes,]
  for att in attributes:
    for val in good_values:
      setattr(gm,att,val)
    for val in bad_values:
      try:
        setattr(gm,att,val)
        success = True
      except:
        success = False
      else:
        if success:
          nm = getattr(gm,"g_name",getattr(gm,"s_name"))
          raise Exception,"Should not be able to set %s attribute '%s' to %s" % (nm,att,repr(val))
          sys.exit(1)
