"""CAM-SIMA XML reading/validation helpers and error classes."""

import logging
import os
import shutil
import subprocess
import xml.etree.ElementTree as ET


class ParseInternalError(Exception):
    """Exception for internal parser use: not to be trapped as a user error"""
    def __init__(self, errmsg):
        logging.shutdown()
        super().__init__(errmsg)


def read_xml_file(filename, logger=None):
    """Read <filename> and return (tree, root)."""
    if os.path.isfile(filename) and os.access(filename, os.R_OK):
        with open(filename, 'r', encoding='utf-8') as fh:
            try:
                tree = ET.parse(fh)
                root = tree.getroot()
            except ET.ParseError as perr:
                raise ValueError(f"read_xml_file: Cannot read {filename}, {perr}") from perr
    elif not os.access(filename, os.R_OK):
        raise ValueError(f"read_xml_file: Cannot open '{filename}'")
    else:
        raise ValueError(f"read_xml_file: Filename, '{filename}', does not exist")
    if logger:
        logger.debug("Reading XML file %s", filename)
    return tree, root


def find_schema_version(root):
    """Return the schema version as [major, minor] from <root>'s
    version attribute.

    >>> find_schema_version(ET.fromstring('<model name="CAM" version="1.0"></model>'))
    [1, 0]
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="1.a"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Illegal version string, '1.a'
    Format must be <integer>.<integer>
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="0.0"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Illegal version string, '0.0'
    Major version must be at least 1
    """
    if 'version' not in root.attrib:
        raise ValueError("Version attribute required")
    version = root.attrib['version']
    versplit = version.split('.')
    try:
        if len(versplit) != 2:
            raise ValueError('Major and minor version required')
        try:
            verbits = [int(x) for x in versplit]
        except ValueError as verr:
            raise ValueError(verr) from verr
        if verbits[0] < 1:
            raise ValueError('Major version must be at least 1')
        if verbits[1] < 0:
            raise ValueError('Minor version must be non-negative')
    except ValueError as verr:
        errstr = f"Illegal version string, '{version}'\n" \
                 "        Format must be <integer>.<integer>"
        if str(verr):
            errstr = f"{verr}\n{errstr}"
        raise ValueError(errstr) from verr
    return verbits


def find_schema_file(schema_root, version, schema_path=None):
    """Return <schema_root>_v<major>_<minor>.xsd under <schema_path>
    (or the current directory), or None if no such file exists."""
    verstring = '_'.join([str(x) for x in version])
    schema_filename = f"{schema_root}_v{verstring}.xsd"
    if schema_path:
        schema_file = os.path.join(schema_path, schema_filename)
    else:
        schema_file = schema_filename
    if os.path.exists(schema_file):
        return schema_file
    return None


def validate_xml_file(filename, schema_root, version, logger, schema_path=None):
    """Validate <filename> against the matching schema using xmllint.
    <schema_root> is either a schema file path or a schema-name root
    (e.g., 'registry') looked up in <schema_path>.
    """
    if not os.path.isfile(filename):
        raise ValueError(f"validate_xml_file: Filename, '{filename}', does not exist")
    if not os.access(filename, os.R_OK):
        raise ValueError(f"validate_xml_file: Cannot open '{filename}'")
    if os.path.isfile(schema_root):
        schema_file = schema_root
    else:
        schema_file = find_schema_file(schema_root, version, schema_path)
        if not (schema_file and os.path.isfile(schema_file)):
            verstring = '.'.join([str(x) for x in version])
            raise ValueError(
                f"validate_xml_file: Cannot find schema for version {verstring},\n"
                f"            {schema_file} does not exist"
            )
    if not os.access(schema_file, os.R_OK):
        raise ValueError(f"validate_xml_file: Cannot open schema, '{schema_file}'")

    xmllint = shutil.which('xmllint')
    if not xmllint:
        raise ValueError(
            f"validate_xml_file: xmllint not found, could not validate file {filename}")

    logger.debug("Checking file %s against schema %s", filename, schema_file)
    cmd = [xmllint, '--noout', '--schema', schema_file, filename]
    cproc = subprocess.run(cmd, check=False, capture_output=True)
    if cproc.returncode == 0:
        # Some xmllint builds return 0 even when validation fails
        result = b'validates' in cproc.stdout or b'validates' in cproc.stderr
    else:
        result = False
    if result:
        logger.debug(cproc.stdout)
        logger.debug(cproc.stderr)
        return result
    cmd_str = ' '.join(cmd)
    outstr = f"Execution of '{cmd_str}' failed with code: {cproc.returncode}\n"
    if cproc.stdout:
        outstr += f"{cproc.stdout.decode('utf-8', errors='replace').strip()}\n"
    if cproc.stderr:
        outstr += f"{cproc.stderr.decode('utf-8', errors='replace').strip()}\n"
    raise ValueError(outstr)
