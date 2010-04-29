"""
Support for NcML - netCDF Markup Language
"""
__all__ = ["ncml","ncmlParse","cs","csParse"]

import ncml, ncmlParse, cs, csParse
from ncml import NCMLError, NetcdfNode, DimensionNode, VariableNode, ValueNode, AttributeNode
from ncmlParse import NCMLParseException, load

