#
# Test whether _bin_dir is a subdirectory of _source_dir. The result is stored
# in _build_is_in_source.
#
# Run this file as a script with TEST_check_build_out_of_source to run test
# cases:
#
# cmake -DTEST_check_build_out_of_source=ON -P CheckBuildOutOfSource.cmake
#
function(check_build_out_of_source _source_dir _bin_dir _build_is_in_source)
  # Resolve symlinks / parent entries
  get_filename_component(real_src "${_source_dir}" REALPATH)
  get_filename_component(real_bin "${_bin_dir}" REALPATH)

  string(LENGTH "${real_src}/" src_len)
  string(LENGTH "${real_bin}/" bin_len)

  if("${bin_len}" LESS "${src_len}")
      set(${_build_is_in_source} FALSE PARENT_SCOPE)
  else()
      string(SUBSTRING "${real_bin}/" 0 "${src_len}" bin_start)
      string(COMPARE EQUAL "${real_src}/" "${bin_start}" comp_val)
      if(comp_val)
        set(${_build_is_in_source} TRUE PARENT_SCOPE)
      else()
        set(${_build_is_in_source} FALSE PARENT_SCOPE)
      endif()
  endif()
endfunction()

if(TEST_check_build_out_of_source)
  # Test runner:
  function(do_test src bin expected)
    check_build_out_of_source("${src}" "${bin}" result)

    # In this case we know each argument is exactly "TRUE" or "FALSE", so
    # string comparison is fine:
    if(NOT "${result}" STREQUAL "${expected}")
      message(FATAL_ERROR "In-source build test failed for\nSrc: ${src}\n"
                          "Bin: ${bin}\n"
                          "build_is_in_source: ${result}\n"
                          "expected: ${expected}")
    endif()
  endfunction()

  # Test cases:
  do_test("/source" "/path/binary" FALSE)
  do_test("/path/source" "/binary" FALSE)
  do_test("/source" "/source/binary" TRUE)
  do_test("/binary/source" "/binary" FALSE)
  do_test("/source" "/source" TRUE)
  do_test("/a/b/source" "/a/b/source/../bin" FALSE)
  do_test("/a/b/source" "/a/b/source/c/../bin" TRUE)
endif()
