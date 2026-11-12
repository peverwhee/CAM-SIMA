#!/usr/bin/env python3

"""Class to aid the parsing process.
"""

import logging

# Re-export the exception class from capgen so cross-boundary
# raises / catches resolve to a single class identity.  The remainder
# of the module is the verbatim original-capgen vendor copy.
from metadata.parse_tools.parse_source import (
    CCPPError,
)

########################################################################
# CCPPError are re-exported from capgen above so the
# exception hierarchy is shared.  ParseInternalError is defined locally:
# capgen no longer ships one (it never raises or catches it), so there is
# no cross-boundary class identity to preserve -- only CAM-SIMA raises it.
########################################################################

class ParseInternalError(Exception):
    """Exception for internal parser use: not to be trapped as a user error"""
    def __init__(self, errmsg):
        """Initialize this exception"""
        logging.shutdown()
        message = f"{errmsg}"
        super().__init__(message)

########################################################################
