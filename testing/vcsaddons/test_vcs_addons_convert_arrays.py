import vcsaddons
import numpy

magnitudes = [1, 2, 3, 4]
thetas = [5, 6, 7, 8]
zipped_input = zip(magnitudes, thetas)
grouped_zipped = [zipped_input[:2], zipped_input[2:]]

one_array = numpy.array(zip(magnitudes, thetas))
three_d_array = numpy.array(grouped_zipped)
two_arrays = numpy.array(magnitudes), numpy.array(thetas)
two_array_groups = numpy.array([magnitudes[:2], magnitudes[2:]]), numpy.array([thetas[:2], thetas[2:]])
list_and_array = two_arrays[0], thetas
two_lists = magnitudes, thetas
lists_of_arrays = [two_arrays[0]], [two_arrays[1]]
array_and_list = magnitudes, two_arrays[1]
one_list_tuples = zip(magnitudes, thetas)
one_list_grouped_tuples = [zip(magnitudes[:2], thetas[:2]), zip(magnitudes[2:], thetas[2:])]
one_list_of_arrays = [numpy.array(zip(magnitudes[:2], thetas[:2])), numpy.array(zip(magnitudes[2:], thetas[2:]))]


def compare(input, expected):
    result = vcsaddons.polar.convert_arrays(*input)
    print "Checking", result[0:2], "vs", expected
    assert result[0] == expected[0]
    assert result[1] == expected[1]

grouped = ([magnitudes[:2], magnitudes[2:]], [thetas[:2], thetas[2:]])

compare((one_array, None), ([magnitudes], [thetas]))
compare(two_arrays, ([magnitudes], [thetas]))
compare(two_array_groups, grouped)
three_d_expected = ([[1, 2], [3, 4]], [[5, 6], [7, 8]])
compare((three_d_array, None), three_d_expected)
compare(list_and_array, ([magnitudes], [thetas]))
compare(two_lists, ([magnitudes], [thetas]))
compare(lists_of_arrays, ([magnitudes], [thetas]))
compare(array_and_list, ([magnitudes], [thetas]))
compare((one_list_tuples, None), ([[i] for i in magnitudes], [[i] for i in thetas]))
compare((one_list_grouped_tuples, None), grouped)
compare((one_list_of_arrays, None), grouped)


def test_error(input, error):
    try:
        vcsaddons.polar.convert_arrays(*input)
    except:
        print "Got", error
    else:
        assert False, "Should have raised a %s" % error

# Test error conditions

# Single arg:

# List of 3d arrays
test_error(([numpy.array([[[1, 2]]])], None), "ValueError for list of 3d arrays")
# >2 element arrays
test_error(([numpy.array([[1, 2, 3]])], None), "ValueError for list of 3-element arrays")
# <2 element arrays
test_error(([numpy.array([[1]])], None), "ValueError for list of 1-element arrays")
# Wrong-sized lists
test_error(([[(1, 2, 3)]], None), "ValueError for wrong sized lists.")


# Two args:

# Too many dimensions
test_error((numpy.array([[[1, 2]]]), numpy.array([[1, 2]])), "ValueError for too many dimensions for magnitude.")
test_error((numpy.array([[1, 2]]), numpy.array([[[1, 2]]])), "ValueError for too many dimensions for magnitude.")
