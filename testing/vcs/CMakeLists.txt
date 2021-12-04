set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcs")

cdat_add_test(flake8_vcs
    flake8 "${cdat_BINARY_DIR}/vcs/vcs/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
  --max-line-length=120 # Reasonable line length
  --ignore=F999,E121,E123,E126,E226,E24,E704 # recent version show zillions of errors if object come from an import * line
)

cdat_add_test(test_vcs_bad_png_path
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_bad_png_path.py
)

foreach(projection polar mollweide lambert orthographic mercator polyconic robinson)
  foreach(zoom none datawc subset)
    if (zoom STREQUAL "none")
      cdat_add_test(test_vcs_boxfill_${projection}
        "${PYTHON_EXECUTABLE}"
        ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_projection.py
        "${BASELINE_DIR}/test_vcs_boxfill_${projection}.png"
        ${projection}
        ${zoom}
        )
    else()
      cdat_add_test(test_vcs_boxfill_${projection}_${zoom}
        "${PYTHON_EXECUTABLE}"
        ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_projection.py
        "${BASELINE_DIR}/test_vcs_boxfill_${projection}_${zoom}.png"
        ${projection}
        ${zoom}
        )
    endif()
  endforeach()
endforeach()

foreach(lat_0 45 90)
  cdat_add_test(test_vcs_boxfill_orthographic_${lat_0}
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_orthographic.py
    "${BASELINE_DIR}/test_vcs_boxfill_orthographic_${lat_0}.png"
    ${lat_0}
    )
endforeach()
cdat_add_test(test_vcs_extends
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_extends.py
)
cdat_add_test(test_vcs_create_get
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_create_get.py
)
cdat_add_test(test_vcs_get_set_cmap_colorcell
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_get_set_cmap_colorcell.py
)
cdat_add_test(test_vcs_init_size_keyword
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_init_size_keyword.py
)
cdat_add_test(test_vcs_fillarea_transparency
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_fillarea_transparency.py
  "${BASELINE_DIR}/test_vcs_fillarea_transparency.png"
)

cdat_add_test(test_drawLinesAndMarkersLegend
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_drawLinesAndMarkersLegend.py
    "${BASELINE_DIR}/test_drawLinesAndMarkersLegend.png"
    )

cdat_add_test(test_vcs_import
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_import.py
)
foreach(orientation h v)
    foreach(offset -0.05 0.015)
        foreach(extension 0 1 2 3)
            foreach(arrow 0.05 0.25)
                cdat_add_test(test_vcs_legend_2_${orientation}_${offset}_${extension}_${arrow}
                              "${PYTHON_EXECUTABLE}"
                              ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_legend_2.py -o ${orientation} --offset=${offset} -e ${extension} -a ${arrow} -b "${BASELINE_DIR}/test_vcs_legend_${orientation}_${offset}_${extension}_${arrow}.png"
                              )
            endforeach()
        endforeach()
    endforeach()
endforeach()

cdat_add_test(test_vcs_line_point_extension
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_line_point_extension.py
)
cdat_add_test(test_vcs_meshfill_vertices
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_meshfill_vertices.py
  "${BASELINE_DIR}/test_vcs_meshfill_vertices_issue.png"
)
cdat_add_test(test_vcs_png_metadata
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_png_metadata.py
)
cdat_add_test(test_vcs_png_set_size
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_png_set_size.py
)
cdat_add_test(test_vcs_read_old_scr
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_read_old_scr.py
  ${UVCDAT_GIT_TESTDATA_DIR}/data/vcs/old.scr
)
cdat_add_test(test_vcs_remove_objects
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_remove_objects.py
)
cdat_add_test(test_vcs_read_old_scr_2
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_read_old_scr.py
  ${UVCDAT_GIT_TESTDATA_DIR}/data/vcs/old_2.scr
)
cdat_add_test(test_vcs_set_line_attribute_from_line
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_set_line_attribute_from_line.py
)
cdat_add_test(test_vcs_text_object_as_input.py
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_text_object_as_input.py
)
cdat_add_test(test_vcs_no_extra_elements
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_no_extra_elements.py
)
cdat_add_test(test_vcs_mkscale_zero_range
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_mkscale_zero_range.py
)
if (NOT EXISTS /etc/redhat-release)
    cdat_add_test(test_vcs_antialiasing
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_antialiasing.py
)
endif()
cdat_add_test(test_vcs_init
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_init.py
)
cdat_add_test(test_vcs_mkscale_great_range
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_mkscale_great_range.py
)
cdat_add_test(test_vcs_verify_set_ext_2_on_default
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_verify_set_ext_2_on_default.py
)
cdat_add_test(test_vcs_verify_levels_extensions_order_setting
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_verify_levels_extensions_order_setting.py
  )
cdat_add_test(test_vcs_verify_read_a_isofill
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_verify_read_a_isofill.py
)
cdat_add_test(test_vcs_verify_boxfill_basics
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_verify_boxfill_basics.py
)
cdat_add_test(test_vcs_verify_proj_basics
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_verify_proj_basics.py
)
cdat_add_test(test_vcs_verify_fillarea_basics
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_fillarea_basics_no_plot.py
)

cdat_add_test(test_vcs_clear_empy_canvas
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_clear_empty_canvas.py
  )
cdat_add_test(test_vcs_interact_no_open
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_interact_no_open.py
  )
cdat_add_test(test_vcs_interact_no_plot
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_interact_no_plot.py
  )

cdat_add_test(test_vcs_colorcells
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_colorcells.py
  )

cdat_add_test(test_vcs_template_saves_associated_to_tt
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_template_saves_associated_to_tt.py
  ${cdat_SOURCE_DIR}/testing/vcs/template_test_associated_dump.json
  )
cdat_add_test(test_vcs_load_json
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_load_json.py
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_json.json
  )

cdat_add_test(test_vcs_readyx_scr
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_readyx_scr.py
  ${cdat_SOURCE_DIR}/testing/vcs/read_yxvsx.scr
  )
cdat_add_test(test_vcs_queries
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_queries.py
  )
cdat_add_test(test_vcs_save_td_json
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_save_td_json.py
  ${cdat_SOURCE_DIR}/testing/vcs/vcs_test_save_td_to_json.json.good
  ${cdat_SOURCE_DIR}/testing/vcs/vcs_test_save_td_to_py.py.good
  )
cdat_add_test(test_vcs_reload_taylor_from_json
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_reload_taylor_from_json.py
  ${cdat_SOURCE_DIR}/testing/vcs/vcs_test_save_td_to_json.json.good
  )
cdat_add_test(vcs_test_dump_json
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_dump_json.py
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_dump_json.json
  )
cdat_add_test(test_vcs_background_mode_rotate
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_background_mode_rotate.py
  )

# Some vector plot tests
FOREACH(angle -180 -135 -90 -45 0 45 135 90 135 )
  cdat_add_test(test_vcs_basic_vectors_angle_${angle}
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_vectors.py
    --angle=${angle}
    "--source=${BASELINE_DIR}/test_vcs_basic_vectors_${angle}.png"
    )
ENDFOREACH(angle)
FOREACH(scale 0.5 2)
  cdat_add_test(test_vcs_basic_vectors_45_${scale}
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_vectors.py
    --scale=${scale}
    --angle=45
    "--source=${BASELINE_DIR}/test_vcs_basic_vectors_45_${scale}.png"
    )
ENDFOREACH(scale)
cdat_add_test(test_vcs_basic_vectors_45_2_amplitude
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_vectors.py
  --scale=2
  --angle=45
  --amplitude
  "--source=${BASELINE_DIR}/test_vcs_basic_vectors_45_2_amplitude.png"
  )
cdat_add_test(test_vcs_vectors_missing
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_vectors_missing.py
  "--source=${BASELINE_DIR}/test_vcs_vectors_missing.png"
  )
cdat_add_test(test_vcs_remove_marker_none_1d
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_remove_marker_none_1d.py
  "${BASELINE_DIR}/test_vcs_remove_marker_none_1d.png"
  )
cdat_add_test(test_vcs_png_window_resize
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_png_window_resize.py
  "${BASELINE_DIR}/test_vcs_png_window_resize.png"
  )
cdat_add_test(test_vcs_draw_logo_on
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_draw_logo_on.py
  ${BASELINE_DIR}/test_vcs_draw_logo_on.png
  )
cdat_add_test(test_vcs_monotonic_decreasing_yxvsx_default
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_monotonic_decreasing_yxvsx_default.py
  ${BASELINE_DIR}/test_vcs_monotonic_decreasing_yxvsx_default.png
  )
cdat_add_test(test_vcs_taylor_template_ctl
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_taylor_template_ctl.py
  ${BASELINE_DIR}/test_vcs_taylor_template_ctl.png
  )
cdat_add_test(test_vcs_taylor_2quads
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_taylor_2quads.py
  ${BASELINE_DIR}/test_vcs_taylor_2quads.png
  )

##############################################################################
#
# These tests perform plotting but do not need sample data
#
##############################################################################
cdat_add_test(test_vcs_gen_meshfill
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_gen_meshfill.py
  "${BASELINE_DIR}/test_vcs_gen_meshfill.png"
  )
cdat_add_test(test_vcs_setcolormap
  "${PYTHON_EXECUTABLE}"
  "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_setcolormap.py"
  "${BASELINE_DIR}/test_vcs_setcolormap.png"
  )
cdat_add_test(test_vcs_1d_in_boxfill
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_1d_in_boxfill.py
  "${BASELINE_DIR}/test_vcs_1d_in_boxfill.png"
  )
cdat_add_test(test_vcs_1d_missing
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_1d_missing.py
  "${BASELINE_DIR}/test_vcs_1d_missing.png"
  )
cdat_add_test(test_vcs_infinity
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_infinity.py
  "${BASELINE_DIR}/test_vcs_infinity.png"
  )
cdat_add_test(test_vcs_1D_datawc
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_1D_datawc.py
  "${BASELINE_DIR}/test_vcs_1D_datawc.png"
  )
cdat_add_test(test_vcs_1D_datawc_missing
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_1D_datawc_missing.py
  "${BASELINE_DIR}/test_vcs_1D_datawc_missing.png"
  )
cdat_add_test(test_vcs_wmo_marker
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_wmo_marker.py
  "${BASELINE_DIR}/test_vcs_wmo_marker.png"
  )
cdat_add_test(test_vcs_wmo_markers
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_wmo_markers.py
  "${BASELINE_DIR}/test_vcs_wmo_markers.png"
  )
cdat_add_test(test_vcs_iso_celine_part1
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_iso_celine_part1.py
  "${BASELINE_DIR}/test_vcs_iso_celine_part1.png"
  )
cdat_add_test(test_vcs_iso_celine_part2
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_iso_celine_part2.py
  "${BASELINE_DIR}/test_vcs_iso_celine_part2.png"
  )
cdat_add_test(test_vcs_markers
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_markers.py
  "${BASELINE_DIR}/test_vcs_markers.png"
  )
cdat_add_test(test_vcs_star_triangle_markers
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_star_triangle_markers.py
  "${BASELINE_DIR}/test_vcs_star_triangle_markers.png"
  )
cdat_add_test(test_vcs_canvas_background
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_canvas_background.py
  "${BASELINE_DIR}/test_vcs_canvas_background.png"
  )
cdat_add_test(test_vcs_canvas_background_update
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_canvas_background_update.py
  "${BASELINE_DIR}/test_vcs_canvas_background_update.png"
  )
cdat_add_test(test_vcs_boxfill_decreasing_latitude
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_decreasing_latitude.py
  "${BASELINE_DIR}/test_vcs_boxfill_decreasing_latitude.png"
  "${UVCDAT_GIT_TESTDATA_DIR}/data/decreasing_latitude.nc"
  )
cdat_add_test(test_vcs_meshfill_no_wrapping
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_meshfill_no_wrapping.py
  "${BASELINE_DIR}/test_vcs_meshfill_no_wrapping.png"
  "${UVCDAT_GIT_TESTDATA_DIR}/data/heat.nc"
  )
cdat_add_test(test_vcs_boxfill_robinson_wrap
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_robinson_wrap.py
  "${BASELINE_DIR}/test_vcs_boxfill_robinson_wrap.png"
  )
cdat_add_test(test_vcs_meshfill_zoom
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_meshfill_zoom.py
  "${BASELINE_DIR}/test_vcs_meshfill_zoom.png"
  )
cdat_add_test(test_vcs_meshfill_zoom_flip
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_meshfill_zoom.py
  "${BASELINE_DIR}/test_vcs_meshfill_zoom_flip.png"
  flip
  )
cdat_add_test(test_vcs_boxfill_10x10_numpy
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_10x10_numpy.py
  "${BASELINE_DIR}/test_vcs_boxfill_10x10_numpy.png"
  )
cdat_add_test(test_vcs_boxfill_10x10_masked_numpy
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_10x10_masked_numpy.py
  "${BASELINE_DIR}/test_vcs_boxfill_10x10_masked_numpy.png"
  )
cdat_add_test(test_vcs_1D_with_manyDs
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_1D_with_manyDs.py
  "${BASELINE_DIR}/test_vcs_1D_with_manyDs.png"
  )
cdat_add_test(test_vcs_1d_marker_not_shown_if_xaxis_flipped
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_1d_marker_not_shown_if_xaxis_flipped.py
  "${BASELINE_DIR}/test_vcs_1d_marker_not_shown_if_xaxis_flipped.png"
  )
cdat_add_test(test_vcs_png_to_base64
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_png_to_base64.py
  )
cdat_add_test(test_vcs_missing_colorname
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_missing_colorname.py
  "${BASELINE_DIR}/test_vcs_missing_colorname.png"
  )
FOREACH(gm isofill boxfill meshfill)
  cdat_add_test(test_vcs_missing_opacity_${gm}
    "${PYTHON_EXECUTABLE}"
    "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_missing_opacity_gms.py"
    --gm_type=${gm}
    "--source=${BASELINE_DIR}/test_vcs_missing_opacity_${gm}.png"
    "--threshold=15"
    )
ENDFOREACH(gm)
cdat_add_test(test_vcs_geometry
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_geometry.py
  )
cdat_add_test(test_vcs_export_text
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_export_text.py
  )
cdat_add_test(test_vcs_canvas_switchfonts
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_canvas_switchfonts.py
  )
##############################################################################
#
# These tests perform plotting and need sample data
#
##############################################################################
if (CDAT_DOWNLOAD_SAMPLE_DATA)
  FOREACH(gm boxfill isofill meshfill isoline vector)
    FOREACH(src vcs canvas gm)
      cdat_add_test(test_vcs_colormaps_source_${gm}_${src}
        "${PYTHON_EXECUTABLE}"
        ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_colormaps_source.py
        -b ${BASELINE_DIR}/test_vcs_colormaps_source_${gm}_${src}.png
        -g ${gm}
        -s ${src}
        )
    ENDFOREACH()
  ENDFOREACH()

  cdat_add_test(test_vcs_custom_logo
      "${PYTHON_EXECUTABLE}" ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_custom_logo.py
      ${BASELINE_DIR}/test_vcs_custom_logo.png)

  cdat_add_test(test_vcs_put_png_on_canvas_1_0_0_percent_True
      "${PYTHON_EXECUTABLE}" ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_put_png_on_canvas.py
      -z 1.0
      -x 0.0
      -y 0.0
      -s ${BASELINE_DIR}/test_vcs_put_png_on_canvas_1.0_0.0_0.0_percent_True.png)
  cdat_add_test(test_vcs_put_png_on_canvas_1_0_0_pixels_False
      "${PYTHON_EXECUTABLE}" ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_put_png_on_canvas.py
      -z 1.0
      -x 0.0
      -y 0.0
      -u pixels
      -H
      -s ${BASELINE_DIR}/test_vcs_put_png_on_canvas_1.0_0.0_0.0_pixels_False.png)
  FOREACH(zoom .5 1.5)
      FOREACH(xoff -25.0 25.0)
          FOREACH(yoff 25.0 -25.0)
              cdat_add_test(test_vcs_put_png_on_canvas_${zoom}_${xoff}_${yoff}_percent_True
                  "${PYTHON_EXECUTABLE}" ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_put_png_on_canvas.py
                  -z ${zoom}
                  -x ${xoff}
                  -y ${yoff}
                  -s ${BASELINE_DIR}/test_vcs_put_png_on_canvas_${zoom}_${xoff}_${yoff}_percent_True.png)
          ENDFOREACH()
      ENDFOREACH()
      FOREACH(xoff 250.0 -250.0)
          FOREACH(yoff 250.0 -250.0)
              cdat_add_test(test_vcs_put_png_on_canvas_${zoom}_${xoff}_${yoff}_pixels_False
                  "${PYTHON_EXECUTABLE}" ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_put_png_on_canvas.py
                  -z ${zoom}
                  -x ${xoff}
                  -y ${yoff}
                  -u pixels
                  -H
                  -s ${BASELINE_DIR}/test_vcs_put_png_on_canvas_${zoom}_${xoff}_${yoff}_percent_True.png)
          ENDFOREACH()
      ENDFOREACH()
  ENDFOREACH()

  # NOTE Fix baseline name
  cdat_add_test(test_vcs_meshfill_regular_grid
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_meshfill_regular_grid.py
    "${BASELINE_DIR}/test_vcs_meshfill_regular_grid.png"
  )
  # NOTE Fix baseline name
  cdat_add_test(test_vcs_plot_unstructured_via_boxfill
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_plot_unstructured_via_boxfill.py
    "${BASELINE_DIR}/test_vcs_plot_unstructured_via_boxfill.png"
  )
  # NOTE Fix baseline name
  cdat_add_test(test_vcs_box_custom_as_def_vistrails_exts
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_box_custom_as_def_vistrails_exts.py
    "${BASELINE_DIR}/test_vcs_box_custom_as_def_vistrails_exts.png"
    )
  # NOTE Fix baseline name
  cdat_add_test(test_vcs_boxfill_custom
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_custom.py
    "${BASELINE_DIR}/test_vcs_boxfill_custom.png"
    )
  cdat_add_test(test_vcs_boxfill_datawc_time
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_datawc_time.py
    "${BASELINE_DIR}/test_vcs_boxfill_datawc_time.png"
    )
  cdat_add_test(test_vcs_boxfill_custom_non_default_levels
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_custom_non_default_levels.py
    "${BASELINE_DIR}/test_vcs_boxfill_custom_non_default_levels.png"
    )
  cdat_add_test(test_vcs_boxfill_custom_ext1
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_custom_ext1.py
    "${BASELINE_DIR}/test_vcs_boxfill_custom_ext1.png"
    )
  cdat_add_test(test_vcs_boxfill_custom_ext2
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_custom_ext2.py
    "${BASELINE_DIR}/test_vcs_boxfill_custom_ext2.png"
    )
  cdat_add_test(test_vcs_boxfill_custom_ext1_ext2
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_custom_ext1_ext2.py
    "${BASELINE_DIR}/test_vcs_boxfill_custom_ext1_ext2.png"
    )
  cdat_add_test(test_vcs_boxfill_number_color_less_than_number_levels
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_number_color_less_than_number_levels.py
    )
  cdat_add_test(test_vcs_boxfill_number_color_more_than_number_levels
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_number_color_more_than_number_levels.py
    )
  cdat_add_test(test_vcs_user_passed_date
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_user_passed_date.py
    "${BASELINE_DIR}/test_vcs_user_passed_date.png"
    )
  cdat_add_test(test_vcs_user_passed_date_as_string
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_user_passed_date_as_string.py
    "${BASELINE_DIR}/test_vcs_user_passed_date_as_string.png"
    )
  cdat_add_test(test_vcs_auto_time_labels
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_auto_time_labels.py
    "${BASELINE_DIR}/test_vcs_auto_time_labels.png"
    )
  cdat_add_test(test_vcs_isofill_data_read_north_to_south
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isofill_data_read_north_to_south.py
    "${BASELINE_DIR}/test_vcs_isofill_data_read_north_to_south.png"
    )
  # Rename baseline
  cdat_add_test(test_vcs_lon_axes_freak_out
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_lon_axes_freak_out.py
    "${BASELINE_DIR}/test_vcs_lon_axes_freak_out.png"
    )
cdat_add_test(test_vcs_set_colors_name_rgba_1d
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_settings_color_name_rgba_1d.py
    "${BASELINE_DIR}/test_vcs_settings_color_name_rgba_1d.png"
    )
cdat_add_test(test_vcs_set_colors_name_rgba_isoline
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_settings_color_name_rgba_isoline.py
    "${BASELINE_DIR}/test_vcs_settings_color_name_rgba_isoline.png"
    )
cdat_add_test(test_vcs_settings_color_name_rgba_meshfill
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_settings_color_name_rgba_meshfill.py
    "${BASELINE_DIR}/test_vcs_settings_color_name_rgba_meshfill.png"
    )
cdat_add_test(test_vcs_settings_color_name_rgba_boxfill
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_settings_color_name_rgba_boxfill.py
    "${BASELINE_DIR}/test_vcs_settings_color_name_rgba_boxfill.png"
    )
cdat_add_test(test_vcs_settings_color_name_rgba
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_settings_color_name_rgba.py
    "${BASELINE_DIR}/test_vcs_settings_color_name_rgba_isofill.png"
    )
  cdat_add_test(test_vcs_isofill_mask_cell_shift
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isofill_mask_cell_shift.py
    "${BASELINE_DIR}/test_vcs_isofill_mask_cell_shift.png"
    )
  cdat_add_test(test_vcs_bad_time_units
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_bad_time_units.py
    )
  cdat_add_test(test_vcs_plot_file_varible
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_plot_file_var.py
    )
  FOREACH(gm boxfill isofill meshfill)
    FOREACH(ori horizontal vertical)
      FOREACH(ext1 y n)
        FOREACH(ext2 y n)
          cdat_add_test(test_vcs_legend_${gm}_${ori}_ext1_${ext1}_ext2_${ext2}
            "${PYTHON_EXECUTABLE}"
            ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_legend.py
            --gm_type=${gm}
            --orientation=${ori}
            --ext1=${ext1}
            --ext2=${ext2}
            "--source=${BASELINE_DIR}/test_vcs_legend_${gm}_${ori}_ext1_${ext1}_ext2_${ext2}.png"
            )
        ENDFOREACH(ext2)
      ENDFOREACH(ext1)
    ENDFOREACH(ori)
  ENDFOREACH(gm)
  FOREACH(gm boxfill isofill isoline vector meshfill yxvsx xvsy xyvsy 1d scatter)
    cdat_add_test(test_vcs_basic_${gm}_transparent
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --transparent
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_transparent.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_zero
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --zero
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_zero.png"
      )
    cdat_add_test(test_vcs_basic_${gm}
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_masked
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --mask
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_masked.png"
      )
  ENDFOREACH(gm)
  cdat_add_test(test_vcs_show
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_show.py
    )
  FOREACH(gm boxfill isofill isoline meshfill )
   FOREACH(ptype 0 -3 aeqd)
    cdat_add_test(test_vcs_basic_${gm}_masked_${ptype}_proj
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --mask
      --projection=${ptype}
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_masked_${ptype}_proj.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_SH
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=-90
      --lat2=0
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_SH.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_SH_-180_180
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=-90
      --lat2=0
      --lon1=-180
      --lon2=180
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_SH_-180_180.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_SH_0_360
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=-90
      --lat2=0
      --lon1=0
      --lon2=360
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_SH_0_360.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_NH
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=90
      --lat2=0
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_NH.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_SH_gm
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=-90
      --lat2=0
      --range_via_gm
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_SH_via_gm.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_SH_-180_180_gm
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=-90
      --lat2=0
      --lon1=-180
      --lon2=180
      --range_via_gm
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_SH_-180_180_via_gm.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_SH_0_360_gm
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=-90
      --lat2=0
      --lon1=0
      --lon2=360
      --range_via_gm
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_SH_0_360_via_gm.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_NH_gm
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=90
      --lat2=0
      --range_via_gm
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_NH_via_gm.png"
      )
    cdat_add_test(test_vcs_basic_${gm}_${ptype}_proj_NH_gm_flip
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=${gm}
      --projection=${ptype}
      --lat1=90
      --lat2=0
      --range_via_gm
      --gm_flips_lat_range
      "--source=${BASELINE_DIR}/test_vcs_basic_${gm}_${ptype}_proj_gmflip_NH_via_gm.png"
      )
   ENDFOREACH(ptype)
  ENDFOREACH(gm)

  cdat_add_test(test_vcs_isoline_extend_attributes
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isoline_extend_attributes.py
    ${BASELINE_DIR}/test_vcs_isoline_extend_attributes.png
    )
  cdat_add_test(test_vcs_isoline_numpy
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isoline_numpy.py
    ${BASELINE_DIR}/test_vcs_isoline_numpy.png
    )
  # Rename baseline
  cdat_add_test(test_vcs_meshfill_draw_mesh
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_meshfill_draw_mesh.py
    ${BASELINE_DIR}/test_vcs_meshfill_draw_mesh.png
    )
  # @dlonie is looking into why RH6 fails for these
  # Rename baselines
  if (NOT EXISTS /etc/redhat-release)
    cdat_add_test(test_vcs_isoline_labels_multi_label_input_types
      "${PYTHON_EXECUTABLE}"
      "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isoline_labels_multi_label_input_types.py"
      "${BASELINE_DIR}/test_vcs_isoline_labels_multi_label_input_types.png"
      )
    cdat_add_test(test_vcs_isoline_labels
      "${PYTHON_EXECUTABLE}"
      "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isoline_labels.py"
      "${BASELINE_DIR}/test_vcs_isoline_labels.png"
      )
    cdat_add_test(test_vcs_isoline_labelskipdistance
      "${PYTHON_EXECUTABLE}"
      "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isoline_labelskipdistance.py"
      "${BASELINE_DIR}/test_vcs_isoline_labelskipdistance.png"
      )
    cdat_add_test(test_vcs_isofill_isoline_labels
      "${PYTHON_EXECUTABLE}"
      "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isofill_isoline_labels.py"
      "${BASELINE_DIR}/test_vcs_isofill_isoline_labels.png"
      )
    # Rename baseline
    cdat_add_test(test_vcs_isoline_width_stipple
      "${PYTHON_EXECUTABLE}"
      "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isoline_width_stipple.py"
      "${BASELINE_DIR}/test_vcs_isoline_width_stipple.png"
      )
    cdat_add_test(test_vcs_isoline_labels_background
      "${PYTHON_EXECUTABLE}"
      "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isoline_labels_background.py"
      "${BASELINE_DIR}/test_vcs_isoline_labels_background.png"
      )
  endif()
  cdat_add_test(test_vcs_oned_level_axis
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_oned_level_axis.py
    "${BASELINE_DIR}/test_vcs_oned_level_axis.png"
    )
  cdat_add_test(test_vcs_first_png_blank
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_first_png_blank.py
    "${BASELINE_DIR}/first_png_blank.png"
    )
  #    cdat_add_test(test_vcs_aspect_ratio
  #  "${PYTHON_EXECUTABLE}"
  #  ${cdat_SOURCE_DIR}/testing/vcs/test_aspect_ratio.py
  #  ${cdat_SOURCE_DIR}/testing/vcs/test_aspect_ratio.py
  #  )

  foreach(z none subset datawc0 datawc1 datawc2)
    cdat_add_test(test_vcs_polar_zoom_${z}
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_polar_zoom.py
      "${BASELINE_DIR}/test_vcs_polar_zoom_${z}.png"
      ${z})
  endforeach()

  cdat_add_test(test_vcs_boxfill_lev1_lev2
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_lev1_lev2.py
    "${BASELINE_DIR}/test_vcs_boxfill_lev1_lev2.png"
    )
  cdat_add_test(test_vcs_boxfill_lev1_lev2_ext1
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_lev1_lev2_ext1.py
    "${BASELINE_DIR}/test_vcs_boxfill_lev1_lev2_ext1.png"
    )
  cdat_add_test(test_vcs_boxfill_lev1_lev2_ext2
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_lev1_lev2_ext2.py
    "${BASELINE_DIR}/test_vcs_boxfill_lev1_lev2_ext2.png"
    )
  cdat_add_test(test_vcs_boxfill_lev1_lev2_ext1_ext2
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_lev1_lev2_ext1_ext2.py
    "${BASELINE_DIR}/test_vcs_boxfill_lev1_lev2_ext1_ext2.png"
    )

  cdat_add_test(test_vcs_hatches_patterns
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hatches_patterns.py
    "${BASELINE_DIR}/test_vcs_hatches_patterns.png"
    )
  FOREACH(gm isofill boxfill meshfill)
    FOREACH(style solid pattern hatch)
      cdat_add_test(test_vcs_${gm}_${style}_fill_non-contig
        "${PYTHON_EXECUTABLE}"
        "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_gms_patterns_hatches.py"
        --gm_type=${gm}
        --fill_style=${style}
        --non-contiguous
        "--source=${BASELINE_DIR}/test_vcs_${gm}_${style}_SH_-180_180_non-contig.png"
        "--threshold=45"
        )
      cdat_add_test(test_vcs_${gm}_${style}_fill
        "${PYTHON_EXECUTABLE}"
        "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_gms_patterns_hatches.py"
        --gm_type=${gm}
        --fill_style=${style}
        "--source=${BASELINE_DIR}/test_vcs_${gm}_${style}_SH_-180_180.png"
        "--threshold=45"
        )
      cdat_add_test(test_vcs_${gm}_${style}_fill_0_360
        "${PYTHON_EXECUTABLE}"
        "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_gms_patterns_hatches.py"
        --gm_type=${gm}
        --fill_style=${style}
        --lon1=0
        --lon2=360
        "--source=${BASELINE_DIR}/test_vcs_${gm}_${style}_SH_0_360.png"
        "--threshold=45"
        )
    ENDFOREACH(style)
  ENDFOREACH(gm)

  FOREACH(gm isofill meshfill boxfill)
    FOREACH(proj robinson)
      cdat_add_test(test_vcs_animate_projected_${gm}_${proj}
        "${PYTHON_EXECUTABLE}"
        "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_gms_animate_projected_plots.py"
        --gm_type=${gm}
        --projection_type=${proj}
        --source=${BASELINE_DIR}/test_vcs_animate_projected_${gm}_${proj}.png
        --threshold=40
        )
    ENDFOREACH(proj)
  ENDFOREACH(gm)

  FOREACH(flip None X XY Y)
    cdat_add_test(test_vcs_flip${flip}
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_flip${flip}.py
      "${BASELINE_DIR}/test_vcs_flip${flip}.png"
      )
  ENDFOREACH(flip)

  cdat_add_test(test_vcs_lambert
     "${PYTHON_EXECUTABLE}"
     ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_lambert.py
     "${BASELINE_DIR}/test_vcs_lambert.png"
    )
  # Rename baseline
  cdat_add_test(test_vcs_boxfill_lev1_lev2_ta_missing
     "${PYTHON_EXECUTABLE}"
     ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_lev1_lev2_ta_missing.py
     "${BASELINE_DIR}/test_vcs_boxfill_lev1_lev2_ta_missing.png"
    )

  cdat_add_test(test_vcs_close
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_close.py
    "${BASELINE_DIR}/test_vcs_close.png"
    )

  cdat_add_test(test_vcs_basic_isofill_bigvalues
    "${PYTHON_EXECUTABLE}"
    "${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py"
    --gm_type=isofill
    --bigvalues
    "--source=${BASELINE_DIR}/test_vcs_basic_isofill_bigvalues.png"
    )

   foreach(level 0 1 2)
     cdat_add_test(test_vcs_isofill_level${level}
       "${PYTHON_EXECUTABLE}"
       ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isofill_levels.py
       "${BASELINE_DIR}/test_vcs_isofill_level${level}.png"
       "${UVCDAT_GIT_TESTDATA_DIR}/data/HadSST1870to99.nc"
       ${level}
       )
   endforeach()

   cdat_add_test(test_vcs_isofill_level3
     "${PYTHON_EXECUTABLE}"
     ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isofill_levels.py
     "${BASELINE_DIR}/test_vcs_isofill_level3.png"
     "${UVCDAT_GIT_TESTDATA_DIR}/data/masked-file.nc"
     3
     )
   cdat_add_test(test_vcs_isofill_masked_bounds
     "${PYTHON_EXECUTABLE}"
     ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_isofill_masked_bounds.py
     "${BASELINE_DIR}/test_vcs_isofill_masked_bounds.png"
     "${UVCDAT_GIT_TESTDATA_DIR}/data/coads_climatology.nc"
     )
  cdat_add_test(test_vcs_issue_960_labels
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_issue_960_labels.py
    ${BASELINE_DIR}/test_vcs_issue_960_labels_1.png
    ${BASELINE_DIR}/test_vcs_issue_960_labels_2.png
    )
  cdat_add_test(test_vcs_animate_meshfill
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_animate_meshfill.py
    ${BASELINE_DIR}
    )
  cdat_add_test(test_vcs_animate_isofill
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_animate_isofill.py
    ${BASELINE_DIR}
    )
  cdat_add_test(test_vcs_animate_boxfill
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_animate_boxfill.py
    ${BASELINE_DIR}
    )
  cdat_add_test(test_vcs_animate_isoline
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_animate_isoline.py
    ${BASELINE_DIR}
    )
  cdat_add_test(test_vcs_animate_isoline_colored
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_animate_isoline_colored.py
    ${BASELINE_DIR}
    )
  if ( (NOT EXISTS /etc/redhat-release) AND (NOT CDAT_BUILD_OFFSCREEN))
    cdat_add_test(test_vcs_animate_isoline_text_labels
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_animate_isoline_text_labels.py
      ${BASELINE_DIR}
      )
    cdat_add_test(test_vcs_animate_isoline_text_labels_colored
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_animate_isoline_text_labels_colored.py
      ${BASELINE_DIR}
      )
    cdat_add_test(test_vcs_patterns
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_patterns.py
      "${BASELINE_DIR}/test_vcs_patterns.png"
      )
    cdat_add_test(test_vcs_vectors_robinson
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_vectors_robinson.py
      "${BASELINE_DIR}/test_vcs_vectors_robinson.png"
      )
    cdat_add_test(test_vcs_vectors_robinson_wrap
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_vectors_robinson_wrap.py
      "${BASELINE_DIR}/test_vcs_vectors_robinson_wrap.png"
      )
    cdat_add_test(test_vcs_vectors_scale_options
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_vectors_scale_options.py
      "${BASELINE_DIR}/test_vcs_vectors_scale_options_off.png"
      "${BASELINE_DIR}/test_vcs_vectors_scale_options_constant.png"
      "${BASELINE_DIR}/test_vcs_vectors_scale_options_linear.png"
      "${BASELINE_DIR}/test_vcs_vectors_scale_options_normalize.png"
      "${BASELINE_DIR}/test_vcs_vectors_scale_options_constantNLinear.png"
      "${BASELINE_DIR}/test_vcs_vectors_scale_options_constantNNormalize.png"
      )
  endif()
endif()

cdat_add_test(test_vcs_endconfigure
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_endconfigure.py
)

cdat_add_test(test_vcs_colorpicker_selection
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_colorpicker_selection.py
)

cdat_add_test(test_vcs_configurator_click_text
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_configurator_click_text.py
)
cdat_add_test(test_vcs_configurator_click_label
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_configurator_click_label.py
)
cdat_add_test(test_vcs_configurator_click_marker
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_configurator_click_marker.py
)
cdat_add_test(test_vcs_mintics
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_mintics.py
  ${BASELINE_DIR}/test_vcs_mintics.png
)
cdat_add_test(test_vcs_configurator_resize
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_configurator_resize.py
  ${BASELINE_DIR}/test_vcs_configurator_resize.png
)
cdat_add_test(vcs_test_fewer_colors_than_levels
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_fewer_colors_than_levels.py
  ${BASELINE_DIR}/test_fewer_colors_than_levels.png
)

cdat_add_test(test_vcs_colorpicker_appearance
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_colorpicker_appearance.py
  ${BASELINE_DIR}/test_vcs_colorpicker_appearance.png
)


foreach(plot a_boxfill a_mollweide_boxfill a_meshfill a_robinson_meshfill
        a_isofill a_isoline vector_default)
  string(SUBSTRING ${plot} 0 2 plot_prefix)
  if (${plot_prefix} STREQUAL "a_")
    string(SUBSTRING ${plot} 2 -1 plot_name)
  else ()
    string(REGEX MATCH "[^_]+" plot_name ${plot})
  endif ()
  cdat_add_test(test_vcs_click_info_${plot_name}
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_click_info.py
    "${BASELINE_DIR}/test_vcs_click_info_${plot_name}.png"
    ${plot}
    )
endforeach()


cdat_add_test(test_vcs_mercator_edge
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_mercator_edge.py
  ${BASELINE_DIR}/test_vcs_mercator_edge.png
)

# Rename baseline
#cdat_add_test(test_vcs_continents
#  "${PYTHON_EXECUTABLE}"
#  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_continents.py
#  ${BASELINE_DIR}/test_vcs_continents.png
#)

cdat_add_test(test_vcs_large_pattern_hatch
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_large_pattern_hatch.py
  ${BASELINE_DIR}/test_vcs_large_pattern_hatch.png
)

foreach(x_over_y 0.5 2)
 # a_meshfill does not work yet, as meshfills are wrapped which is not known to VCS
 foreach(plot a_boxfill a_mollweide_boxfill a_robinson_meshfill a_lambert_isofill a_robinson_isoline)
   foreach(mode 0 1)
     string(SUBSTRING ${plot} 2 -1 plot_name)
     cdat_add_test(test_vcs_autot_axis_titles_${mode}_${plot_name}_${x_over_y}
       "${PYTHON_EXECUTABLE}"
       ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_autot_axis_titles.py
       "${BASELINE_DIR}/test_vcs_autot_axis_titles_${plot_name}_${x_over_y}_${mode}.png"
       ${mode}
       ${plot}
       ${x_over_y}
       )
   endforeach()
 endforeach()
endforeach()

cdat_add_test(test_vcs_boxfill_lambert_crash
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_lambert_crash.py
  "${BASELINE_DIR}/test_vcs_boxfill_lambert_crash.png"
  "${UVCDAT_GIT_TESTDATA_DIR}/data/NCEP_09_climo.nc"
)

cdat_add_test(test_vcs_line_patterns
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_line_patterns.py
  "${BASELINE_DIR}/test_vcs_line_patterns.png"
)

cdat_add_test(test_vcs_init_open_sizing
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_init_open_sizing.py
)
## Rename baseline
cdat_add_test(test_vcs_matplotlib_colormap
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_matplotlib_colormap.py
  ${BASELINE_DIR}/test_vcs_matplotlib_colormap.png
)

cdat_add_test(test_vcs_no_continents
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_no_continents.py
  ${BASELINE_DIR}/test_vcs_no_continents.png
)

#cdat_add_test(test_vcs_center_longitude
#  "${PYTHON_EXECUTABLE}"
#  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_center_longitude.py
#  ${BASELINE_DIR}/test_vcs_center_longitude.png
#)

cdat_add_test(test_vcs_textextents
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_textextents.py
  ${BASELINE_DIR}/test_textextents.png
)

cdat_add_test(test_script_for_py
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_script_for_py.py
)




add_subdirectory(vtk_ui)
add_subdirectory(editors)

