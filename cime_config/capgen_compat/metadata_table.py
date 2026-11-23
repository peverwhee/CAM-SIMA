"""Flat-module shim for original capgen's ``metadata_table``.

Re-exports a CAM-SIMA-shaped ``parse_metadata_file`` on top of
capgen's :func:`metadata.metadata_table.parse_metadata_file`.

Signature adaptation
--------------------
Original capgen's signature::

    parse_metadata_file(filename, known_ddts, run_env,
                        skip_ddt_check=False,
                        relative_source_path=False)

Capgen's signature::

    parse_metadata_file(file_path: str) -> List[MetadataTable]

The extra arguments served original-capgen-internal purposes
(cross-referencing DDT lookups against a pre-built ``known_ddts``
list, threading the ``CCPPFrameworkEnv`` through for logger access,
selecting between absolute and relative source-path resolution).
Capgen's parser does not need any of them -- DDT cross-references
are built lazily, logging is via the module-level logger, and source
paths are always absolute.  The shim accepts all five positional /
keyword arguments and silently discards the extras.
"""

from metadata.metadata_table import (
    parse_metadata_file as _capgen_parse_metadata_file,
    MetadataTable,  # noqa: F401
    MetadataSection,
    MetaVar,
)


# Map original-capgen ``Var.get_prop_value`` property names to the
# matching capgen ``MetaVar`` attribute.  Original capgen's
# property surface is broader than what CAM-SIMA exercises through
# direct ``MetaVar`` iteration today; we cover the names that appear
# in ``generate_registry_data.py`` / ``write_init_files.py`` plus the
# obvious neighbours (``intent`` / ``optional`` for completeness even
# though host metadata typically doesn't carry them).  A miss returns
# ``None`` -- matches original capgen's behaviour for unknown props.
_METAVAR_PROP_MAP = {
    'standard_name':  'standard_name',
    'local_name':     'local_name',
    'long_name':      'long_name',
    'units':          'units',
    'dimensions':     'dimensions',
    'type':           'type',
    'kind':           'kind',
    'intent':         'intent',
    'optional':       'optional',
    'protected':      'protected',
    'allocatable':    'allocatable',
    'active':         'active',
    'advected':       'advected',
    'constituent':    'constituent',
    'molar_mass':     'molar_mass',
    'top_at_one':     'top_at_one',
    'diagnostic_name_fixed': 'diagnostic_name_fixed',
}


def _metavar_get_prop_value(self, prop_name):
    """Original-capgen-style ``Var.get_prop_value`` on ``MetaVar``.

    Returns the matching capgen attribute, or ``None`` for unknown
    property names (matches original capgen's lenient policy).
    Monkey-patched onto :class:`MetaVar` at import time so existing
    instances pick it up automatically.
    """
    attr = _METAVAR_PROP_MAP.get(prop_name)
    if attr is None:
        return None
    return getattr(self, attr, None)


if not hasattr(MetaVar, 'get_prop_value'):
    MetaVar.get_prop_value = _metavar_get_prop_value


# ----------------------------------------------------------------------
# Additional original-capgen-style methods on MetaVar.
#
# CAM-SIMA's ``generate_registry_data.py`` iterates
# ``MetadataSection.variables`` directly and calls original capgen's
# per-Var methods on each entry, so we monkey-patch the bare
# ``MetaVar`` class.
# ----------------------------------------------------------------------

import re as _re

_HORIZ_DIM_STDS = frozenset({
    'horizontal_dimension',
    'horizontal_loop_extent',
})
_VERT_DIM_STDS = frozenset({
    'vertical_layer_dimension',
    'vertical_interface_dimension',
})
_ARRAY_REF_RE = _re.compile(r'^(\w+)\s*\(([^)]+)\)\s*$')


def _strip_bounds(dim):
    """Strip a ``lower:upper`` bound spec to the upper-bound name."""
    if ':' in dim:
        return dim.split(':', 1)[1].strip()
    return dim.strip()


def _metavar_get_dimensions(self):
    """Return a copy of the dimension standard-name list."""
    return list(self.dimensions)


def _metavar_has_horizontal_dimension(self):
    """True iff any declared dim is a horizontal standard name."""
    return any(_strip_bounds(d) in _HORIZ_DIM_STDS for d in self.dimensions)


def _metavar_has_vertical_dimension(self):
    """Return the vertical dim's standard name, or ``''``.

    Original capgen's predicate is truthy when a vertical dim is
    present and returns the name itself so the caller can use it
    directly.
    """
    for d in self.dimensions:
        stripped = _strip_bounds(d)
        if stripped in _VERT_DIM_STDS:
            return stripped
    return ''


def _metavar_array_ref(self):
    """Return a regex match for a sliced ``local_name``, or ``None``.

    Usage from write_init_files:

        aref = hvar.array_ref()
        if aref:
            dimlist = [x.strip() for x in aref.group(2).split(',')]
    """
    return _ARRAY_REF_RE.match(self.local_name)


def _metavar_intrinsic_elements(self):
    """Sub-element standard names for array-of-DDT entries.

    Returns ``None`` (not ``[]``) for plain vars; CAM-SIMA's
    ``write_init_files.py`` distinguishes via
    ``isinstance(ielem, list)``.  Capgen does not currently expose
    array-of-DDT decomposition on bare MetaVar.
    """
    return None


def _metavar_call_string(self, host_dict=None):
    """Return the Fortran expression for use at a call site.

    Bare ``MetaVar`` from ``MetadataSection.variables`` carries no
    pre-baked call expression -- the raw ``local_name`` is the right
    answer for the registry-generated metadata path (vars are
    module-level decls; the call site is just the local name).
    """
    return self.local_name


_METAVAR_ADDED_METHODS = {
    'get_dimensions':           _metavar_get_dimensions,
    'has_horizontal_dimension': _metavar_has_horizontal_dimension,
    'has_vertical_dimension':   _metavar_has_vertical_dimension,
    'array_ref':                _metavar_array_ref,
    'intrinsic_elements':       _metavar_intrinsic_elements,
    'call_string':              _metavar_call_string,
}

for _name, _fn in _METAVAR_ADDED_METHODS.items():
    if not hasattr(MetaVar, _name):
        setattr(MetaVar, _name, _fn)


# Original capgen's standard names that get classified as "loop
# variables" -- they're horizontal-bound / vertical-index axes the
# host injects per chunk.  generate_registry_data.py filters them out
# of the per-section variable list to avoid double-declaring them in
# generated host data structures.
_CCPP_LOOP_VAR_STDNAMES = frozenset({
    'horizontal_loop_extent',
    'horizontal_loop_begin',
    'horizontal_loop_end',
    'vertical_layer_index',
    'vertical_interface_index',
})


# Original capgen's "constant variables" set is the framework-supplied
# universally-available constants -- the only entry is
# ``ccpp_constant_one``.  Note this is NOT the same as the
# ``protected = True`` attribute: ``protected`` flags vars whose
# storage the host has marked read-only; ``CCPP_CONSTANT_VARS`` flags
# framework-supplied numeric constants.  The category filter in
# ``variable_list`` uses the latter, not the former.
_CCPP_CONSTANT_VAR_STDNAMES = frozenset({
    'ccpp_constant_one',
})


def _variable_list(self, recursive=False,
                   std_vars=True, loop_vars=True, consts=True):
    """Original-capgen-compat ``MetadataSection.variable_list`` method.

    capgen's :class:`metadata.metadata_table.MetadataSection`
    exposes a bare ``variables`` list attribute; CAM-SIMA's
    ``generate_registry_data.py`` (and downstream code) call
    ``mheader.variable_list(loop_vars=False, consts=False)`` to filter
    by variable category.  The category rules mirror original capgen's
    ``include_var_in_list`` predicate:

    * ``std_vars`` -- regular vars (not loop vars, not constants).
    * ``loop_vars`` -- the five fixed loop-bound / vertical-index
      standard names listed in :data:`_CCPP_LOOP_VAR_STDNAMES`.
    * ``consts`` -- framework-constant standard names listed in
      :data:`_CCPP_CONSTANT_VAR_STDNAMES` (just ``ccpp_constant_one``).
      Note: NOT the ``protected = True`` attribute; that's a separate
      storage flag.

    *recursive* is accepted for signature parity and ignored --
    capgen's MetadataSection has no parent-dict concept and the
    only CAM-SIMA call site sets ``recursive=False``.

    Monkey-patched onto :class:`MetadataSection` at import time so
    existing capgen MetadataSection instances pick it up
    automatically.
    """
    out = []
    for var in self.variables:
        stdname = var.standard_name
        is_loop = stdname in _CCPP_LOOP_VAR_STDNAMES
        is_const = stdname in _CCPP_CONSTANT_VAR_STDNAMES
        if is_loop:
            keep = loop_vars
        elif is_const:
            keep = consts
        else:
            keep = std_vars
        if keep:
            out.append(var)
    return out


# Install once per Python process.  Idempotent: skipped if already
# present (e.g. during repeated test runs in the same interpreter).
if not hasattr(MetadataSection, 'variable_list'):
    MetadataSection.variable_list = _variable_list


def parse_metadata_file(filename, known_ddts=None, run_env=None,
                        skip_ddt_check=False,
                        relative_source_path=False):
    """Original-capgen-shaped ``parse_metadata_file`` wrapper.

    Forwards *filename* to capgen's parser and silently discards
    every other argument; capgen's parser does not need them.
    """
    return _capgen_parse_metadata_file(filename)


__all__ = [
    'parse_metadata_file',
    'MetadataTable',
]
