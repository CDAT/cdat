import ocgis
import cdat_info
import os

test_data_path = cdat_info.get_sampledata_path()

test_data_name = os.path.join(test_data_path,"clt.nc")

rd = ocgis.RequestDataset(uri=test_data_name)
field = rd.get()
assert field.spatial.crs is not None
