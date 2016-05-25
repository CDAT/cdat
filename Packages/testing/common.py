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
          if hasattr(gm,"g_name"):
            nm = gm.g_name
          elif hasattr(gm,"s_name"):
            nm = gm.s_name
          else:
            nm=gm.p_name
          raise Exception,"Should not be able to set %s attribute '%s' to %s" % (nm,att,repr(val))
          sys.exit(1)
