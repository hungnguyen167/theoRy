from __future__ import annotations

from pathlib import Path

import pandas as pd


def generate_seed_data(output_dir: str | Path = "inst/extdata/") -> dict[str, Path]:
    """Generate example registry and model-state parquet files.

    Parameters
    ----------
    output_dir
        Directory where ``component_registry.parquet`` and
        ``example_model_states.parquet`` will be written.

    Returns
    -------
    dict[str, Path]
        Paths keyed by ``registry`` and ``states``.

    Example
    -------
    ``generate_seed_data("inst/extdata/")``
    """

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    registry_path = output_dir / "component_registry.parquet"
    states_path = output_dir / "example_model_states.parquet"

    nodes = [
        {
            "comp_id": "C0001",
            "type": "node",
            "source": "SolarRad",
            "target": None,
            "direction": None,
            "description": "Solar radiation level",
        },
        {
            "comp_id": "C0002",
            "type": "node",
            "source": "Temp",
            "target": None,
            "direction": None,
            "description": "Ambient temperature",
        },
        {
            "comp_id": "C0003",
            "type": "node",
            "source": "Pressure",
            "target": None,
            "direction": None,
            "description": "Atmospheric pressure",
        },
        {
            "comp_id": "C0004",
            "type": "node",
            "source": "Humidity",
            "target": None,
            "direction": None,
            "description": "Relative humidity",
        },
        {
            "comp_id": "C0005",
            "type": "node",
            "source": "WindSpeed",
            "target": None,
            "direction": None,
            "description": "Wind speed",
        },
        {
            "comp_id": "C0006",
            "type": "node",
            "source": "Precip",
            "target": None,
            "direction": None,
            "description": "Precipitation amount",
        },
        {
            "comp_id": "C0007",
            "type": "node",
            "source": "CloudCover",
            "target": None,
            "direction": None,
            "description": "Cloud cover percentage",
        },
        {
            "comp_id": "C0008",
            "type": "node",
            "source": "Evap",
            "target": None,
            "direction": None,
            "description": "Evaporation rate",
        },
        {
            "comp_id": "C0009",
            "type": "node",
            "source": "Runoff",
            "target": None,
            "direction": None,
            "description": "Surface runoff",
        },
        {
            "comp_id": "C0010",
            "type": "node",
            "source": "SoilMoist",
            "target": None,
            "direction": None,
            "description": "Soil moisture content",
        },
        {
            "comp_id": "C0011",
            "type": "node",
            "source": "Elevation",
            "target": None,
            "direction": None,
            "description": "Terrain elevation",
        },
        {
            "comp_id": "C0012",
            "type": "node",
            "source": "Latitude",
            "target": None,
            "direction": None,
            "description": "Latitude coordinate",
        },
        {
            "comp_id": "C0013",
            "type": "node",
            "source": "SeasonIdx",
            "target": None,
            "direction": None,
            "description": "Seasonal index",
        },
        {
            "comp_id": "C0014",
            "type": "node",
            "source": "AirDensity",
            "target": None,
            "direction": None,
            "description": "Air density",
        },
        {
            "comp_id": "C0015",
            "type": "node",
            "source": "DewPoint",
            "target": None,
            "direction": None,
            "description": "Dew point temperature",
        },
        {
            "comp_id": "C0016",
            "type": "node",
            "source": "Visibility",
            "target": None,
            "direction": None,
            "description": "Visibility distance",
        },
    ]

    edges = [
        {
            "comp_id": "C0017",
            "type": "edge",
            "source": "SolarRad",
            "target": "Temp",
            "direction": "->",
            "description": "Solar radiation drives temperature",
        },
        {
            "comp_id": "C0018",
            "type": "edge",
            "source": "Temp",
            "target": "Pressure",
            "direction": "->",
            "description": "Temperature affects pressure",
        },
        {
            "comp_id": "C0019",
            "type": "edge",
            "source": "Temp",
            "target": "Evap",
            "direction": "->",
            "description": "Temperature drives evaporation",
        },
        {
            "comp_id": "C0020",
            "type": "edge",
            "source": "Humidity",
            "target": "Precip",
            "direction": "->",
            "description": "Humidity leads to precipitation",
        },
        {
            "comp_id": "C0021",
            "type": "edge",
            "source": "WindSpeed",
            "target": "Evap",
            "direction": "->",
            "description": "Wind speed increases evaporation",
        },
        {
            "comp_id": "C0022",
            "type": "edge",
            "source": "Temp",
            "target": "DewPoint",
            "direction": "<->",
            "description": "Temperature and dew point interact",
        },
        {
            "comp_id": "C0023",
            "type": "edge",
            "source": "Pressure",
            "target": "WindSpeed",
            "direction": "->",
            "description": "Pressure gradient drives wind",
        },
        {
            "comp_id": "C0024",
            "type": "edge",
            "source": "Elevation",
            "target": "Temp",
            "direction": "->",
            "description": "Elevation affects temperature",
        },
        {
            "comp_id": "C0025",
            "type": "edge",
            "source": "CloudCover",
            "target": "SolarRad",
            "direction": "<->",
            "description": "Cloud cover and solar radiation interact",
        },
        {
            "comp_id": "C0026",
            "type": "edge",
            "source": "Precip",
            "target": "SoilMoist",
            "direction": "->",
            "description": "Precipitation increases soil moisture",
        },
        {
            "comp_id": "C0027",
            "type": "edge",
            "source": "SoilMoist",
            "target": "Evap",
            "direction": "<->",
            "description": "Soil moisture and evaporation feedback",
        },
        {
            "comp_id": "C0028",
            "type": "edge",
            "source": "Temp",
            "target": "Humidity",
            "direction": "<->",
            "description": "Temperature and humidity interact",
        },
    ]

    registry_data = nodes + edges
    registry_df = pd.DataFrame(registry_data)
    registry_df.to_parquet(registry_path, index=False)

    states = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0001", "comp_id": "C0004", "status": "unknown", "timing": None},
        {"model_id": "M0001", "comp_id": "C0007", "status": "unknown", "timing": None},
        {"model_id": "M0001", "comp_id": "C0008", "status": "causal", "timing": 4},
        {"model_id": "M0001", "comp_id": "C0011", "status": "causal", "timing": 5},
        {"model_id": "M0002", "comp_id": "C0001", "status": "unknown", "timing": None},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0004", "status": "causal", "timing": 3},
        {"model_id": "M0002", "comp_id": "C0005", "status": "causal", "timing": 4},
        {"model_id": "M0002", "comp_id": "C0006", "status": "causal", "timing": 5},
        {"model_id": "M0002", "comp_id": "C0007", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0008", "status": "unknown", "timing": None},
        {"model_id": "M0002", "comp_id": "C0011", "status": "causal", "timing": 6},
        {"model_id": "M0003", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0003", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0003", "comp_id": "C0007", "status": "unknown", "timing": None},
        {"model_id": "M0003", "comp_id": "C0011", "status": "causal", "timing": 4},
        {"model_id": "M0003", "comp_id": "C0014", "status": "causal", "timing": 5},
        {"model_id": "M0004", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0004", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0004", "comp_id": "C0005", "status": "causal", "timing": 3},
        {"model_id": "M0004", "comp_id": "C0006", "status": "causal", "timing": 4},
        {"model_id": "M0004", "comp_id": "C0007", "status": "causal", "timing": 4},
        {"model_id": "M0004", "comp_id": "C0008", "status": "unknown", "timing": None},
        {"model_id": "M0005", "comp_id": "C0001", "status": "unknown", "timing": None},
        {"model_id": "M0005", "comp_id": "C0002", "status": "causal", "timing": 2},
        {
            "model_id": "M0005",
            "comp_id": "C0004",
            "status": "non-causal",
            "timing": None,
        },
        {"model_id": "M0005", "comp_id": "C0015", "status": "causal", "timing": 5},
        {"model_id": "M0005", "comp_id": "C0016", "status": "causal", "timing": 6},
        {"model_id": "M0006", "comp_id": "C0001", "status": "unknown", "timing": None},
        {"model_id": "M0006", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0006", "comp_id": "C0004", "status": "unknown", "timing": None},
        {"model_id": "M0006", "comp_id": "C0007", "status": "unknown", "timing": None},
        {"model_id": "M0006", "comp_id": "C0011", "status": "causal", "timing": 3},
        {"model_id": "M0006", "comp_id": "C0013", "status": "causal", "timing": 4},
        {"model_id": "M0007", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0007", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0007", "comp_id": "C0004", "status": "causal", "timing": 3},
        {
            "model_id": "M0007",
            "comp_id": "C0006",
            "status": "non-causal",
            "timing": None,
        },
        {"model_id": "M0007", "comp_id": "C0010", "status": "causal", "timing": 4},
    ]

    states_df = pd.DataFrame(states)
    states_df.to_parquet(states_path, index=False)

    return {"registry": registry_path, "states": states_path}
