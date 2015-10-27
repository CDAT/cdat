from genutil.colors import rgb2str, str2rgb  # noqa


def matplotlib2vcs(cmap, vcs_name=None):
    """Convert a matplotlib colormap to a vcs colormap
    Input can be either the actual matplotlib colormap or its name
    Optional second argument: vcs_name, name of the resulting vcs colormap
    """
    import vcs
    import matplotlib
    import warnings
    if isinstance(cmap, (str, unicode)):
        try:
            cmap = matplotlib.cm.get_cmap(cmap)
        except:
            raise RuntimeError("Could not retrieve matplotlib colormap: %s" % cmap)

    if vcs_name is None:
        vcs_name = cmap.name
    i = 0
    vcs_name_final = vcs_name
    while vcs_name_final in vcs.listelements("colormap"):
        vcs_name_final = vcs_name + "_mpl_%.3i" % i
        i += 1
    if vcs_name_final != vcs_name:
        warnings.warn(
            "%s colormap name was already existing, your colormap name will be: %s" %
            (vcs_name, vcs_name_final))
    vcs_cmap = vcs.createcolormap(vcs_name_final)
    cmap_rgbs = cmap(range(0, cmap.N))
    for i in range(0, min(cmap.N, 256)):
        vcs_cmap.setcolorcell(i, *([int(x / 2.55) for x in cmap_rgbs[i][:4]]))

    return vcs_cmap
