"""Verify every flat-module shim exposes the surface CAM-SIMA imports."""

import unittest


class TestCcppCapgenShim(unittest.TestCase):

    def test_capgen_importable(self):
        from ccpp_capgen import capgen
        self.assertTrue(callable(capgen))


class TestFrameworkEnvShim(unittest.TestCase):

    def test_class_importable(self):
        from framework_env import CCPPFrameworkEnv
        env = CCPPFrameworkEnv(None)
        self.assertIsNone(env.host_files)


class TestMetadataTableShim(unittest.TestCase):

    def test_imports(self):
        import metadata_table as mt
        self.assertTrue(hasattr(mt, 'parse_metadata_file'))
        self.assertTrue(hasattr(mt, 'MetadataTable'))


if __name__ == '__main__':
    unittest.main()
