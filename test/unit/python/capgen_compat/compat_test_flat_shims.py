"""Verify every flat-module shim exposes the surface CAM-SIMA imports."""

import unittest


class TestCcppCapgenShim(unittest.TestCase):

    def test_capgen_importable(self):
        from ccpp_capgen import capgen, CapDatabase
        self.assertTrue(callable(capgen))
        self.assertTrue(isinstance(CapDatabase, type))


class TestFrameworkEnvShim(unittest.TestCase):

    def test_class_importable(self):
        from framework_env import CCPPFrameworkEnv
        env = CCPPFrameworkEnv(None)
        self.assertIsNone(env.host_files)


class TestCcppStateMachineShim(unittest.TestCase):

    def test_singleton_importable(self):
        from ccpp_state_machine import CCPP_STATE_MACH
        phases = CCPP_STATE_MACH.transitions()
        self.assertIn('run', phases)


class TestParseToolsShim(unittest.TestCase):

    def test_re_exports(self):
        import parse_tools as pt
        for name in ('validate_xml_file', 'read_xml_file',
                     'find_schema_file', 'find_schema_version',
                     'init_log', 'CCPPError', 'ParseInternalError'):
            self.assertTrue(hasattr(pt, name))


class TestMetadataTableShim(unittest.TestCase):

    def test_imports(self):
        import metadata_table as mt
        self.assertTrue(hasattr(mt, 'parse_metadata_file'))
        self.assertTrue(hasattr(mt, 'MetadataTable'))


class TestDdtLibraryShim(unittest.TestCase):

    def test_var_ddt_importable(self):
        from ddt_library import VarDDT
        from _var_wrapper import _VarDDT, _VarWrapper
        self.assertIs(VarDDT, _VarDDT)
        # ``write_init_files.py`` uses the class only in an isinstance
        # test against a wrapper from the host dict, so it has to sit on
        # the _VarWrapper branch of the hierarchy.
        self.assertTrue(issubclass(VarDDT, _VarWrapper))


class TestFortranToolsShim(unittest.TestCase):

    def test_fortran_writer_importable(self):
        from fortran_tools import FortranWriter
        self.assertTrue(isinstance(FortranWriter, type))


if __name__ == '__main__':
    unittest.main()
