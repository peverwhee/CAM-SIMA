"""Flat-module shim for original capgen's ``parse_tools``.

Re-exports symbols from capgen's ``metadata.parse_tools`` package
and supplements with two helpers that live in the xml_tools submodule
but aren't re-exported at the package level
(``validate_xml_file``, ``find_schema_file``).
"""

from metadata.parse_tools import (
    CCPPError,
    init_log,
    set_log_level,
    set_log_to_null,
    read_xml_file,
    find_schema_version,
    expand_nested_suites,
    write_xml_file,
    write_if_changed,
    open_if_changed,
)

# Pushed down from capgen: the general validator + schema-file lookup.
# capgen's own validate_xml_file is now suite-only; CAM-SIMA's namelist
# (file-path schema) and registry ('registry' root) validation live here.
from xml_tools import validate_xml_file, find_schema_file

# Pushed down from capgen (which no longer ships them):
# the CAM-SIMA-only ``ParseInternalError``.
from parse_source import ParseInternalError


__all__ = [
    'CCPPError',
    'ParseInternalError',
    'init_log',
    'set_log_level',
    'set_log_to_null',
    'read_xml_file',
    'find_schema_version',
    'find_schema_file',
    'validate_xml_file',
    'expand_nested_suites',
    'write_xml_file',
    'write_if_changed',
    'open_if_changed',
]
