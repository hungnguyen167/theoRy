"""Runtime version exposed by the Theory Engine API."""

from __future__ import annotations

from importlib.metadata import PackageNotFoundError, version

try:
    API_VERSION = version("theory-engine")
except PackageNotFoundError:
    # Running directly from a source checkout without generated package
    # metadata should still expose the version declared in pyproject.toml.
    API_VERSION = "0.2.0"
