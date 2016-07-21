"""Overrides urllib error handling"""
# Import this AFTER urllib

import urllib

class CDMSURLopener(urllib.FancyURLopener):

	# Override FancyURLopener error handling - raise an exception
        # Can also define function http_error_DDD where DDD is the 3-digit error code,
        # to handle specific errors.
	def http_error_default(self, url, fp, errcode, errmsg, headers):
		void = fp.read()
		fp.close()
		raise IOError, ('http error', errcode, errmsg, headers)

urllib._urlopener = CDMSURLopener()

