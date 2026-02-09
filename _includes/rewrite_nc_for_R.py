"""
TITLE: NetCDF CF Reshaper
PURPOSE: Convert NetCDF variables to a GIS-friendly CF layout and optionally batch-process
folders. Supports optional fast mode and preserves source fill values when available.

OWNER: Ning Liu <ning.liu@csiro.au>
CREATED: 2026-02-10
LAST_MODIFIED: 2026-02-10
STATUS: working

EXAMPLES:
python rewrite_nc_for_R.py ./OzWALD.EVI.2005.nc --fast
python rewrite_nc_for_R.py ./OzWALD.EVI.2005.nc ./out.nc --var EVI
python rewrite_nc_for_R.py ./nc_folder --fast
python rewrite_nc_for_R.py ./nc_folder ./out_folder --var EVI
"""

from pathlib import Path
import time
from typing import Optional
import numpy as np
import xarray as xr

def convert_evi_nc(
    in_nc: str,
    out_nc: str,
    var: Optional[str] = None,
    debug: bool = False,
    decode_times: bool = False,
    mask_and_scale: bool = True,
    ensure_desc_lat: bool = True,
) -> None:
    """
    Convert an EVI NetCDF to CF-friendly layout for GIS:
      - ensure dims order: (time, latitude, longitude)
      - ensure latitude is descending (north->south)
      - write as float32 with NaN fill (no ubyte/scale_factor quantization)
    """
    in_path = Path(in_nc).expanduser().resolve()
    out_path = Path(out_nc).expanduser().resolve()

    if not in_path.exists():
        raise FileNotFoundError(f"Input file not found: {in_path}")

    if debug:
        t0 = time.perf_counter()
        print(f"[DEBUG] Input file:  {in_path}")
        print(f"[DEBUG] Output file: {out_path}")
        print(f"[DEBUG] Variable:    {var if var is not None else '<auto>'}")
        print(f"[DEBUG] decode_times={decode_times}, mask_and_scale={mask_and_scale}, ensure_desc_lat={ensure_desc_lat}")

    try:
        ds_cm = xr.open_dataset(in_path, decode_times=decode_times, mask_and_scale=mask_and_scale)
    except PermissionError as exc:
        raise PermissionError(
            f"{exc}\n"
            f"macOS blocked access to: {in_path}\n"
            "Allow Downloads access for your terminal/IDE Python process, "
            "or move the .nc file into a folder your process can read."
        ) from exc

    with ds_cm as ds:
        selected_var: Optional[str] = None
        if debug:
            print(f"[DEBUG] Dataset variables: {list(ds.data_vars)}")
            print(f"[DEBUG] Dataset coords:    {list(ds.coords)}")

        if not ds.data_vars:
            raise ValueError(f"No data variables found in dataset: {in_path}")

        selected_var = var
        if selected_var is None:
            selected_var = "EVI" if "EVI" in ds.data_vars else next(iter(ds.data_vars))
            if debug:
                print(f"[DEBUG] Auto-selected variable: {selected_var}")

        if selected_var not in ds:
            raise KeyError(f"Variable '{selected_var}' not found. Available: {list(ds.data_vars)}")

        da = ds[selected_var]

        if debug:
            print(f"[DEBUG] '{selected_var}' dims before rename: {da.dims}")
            if not mask_and_scale and any(k in da.attrs for k in ("scale_factor", "add_offset")):
                print("[DEBUG] Warning: scale_factor/add_offset exist while mask_and_scale=False.")

        # Ensure coord names are exactly 'latitude' and 'longitude' (common variants)
        rename = {}
        if "lat" in da.dims or "lat" in da.coords:
            rename["lat"] = "latitude"
        if "lon" in da.dims or "lon" in da.coords:
            rename["lon"] = "longitude"
        if rename:
            da = da.rename(rename)
            if debug:
                print(f"[DEBUG] Renamed coordinates: {rename}")

        # Enforce dimension order
        dims = list(da.dims)
        if all(d in dims for d in ["time", "latitude", "longitude"]):
            target_dims = ("time", "latitude", "longitude")
            if da.dims != target_dims:
                da = da.transpose(*target_dims)
        elif all(d in dims for d in ["latitude", "longitude"]):
            target_dims = ("latitude", "longitude")
            if da.dims != target_dims:
                da = da.transpose(*target_dims)
        else:
            raise ValueError(f"Unexpected dims for {selected_var}: {da.dims}")

        if debug:
            print(f"[DEBUG] '{selected_var}' dims after transpose: {da.dims}")

        # Force latitude descending (north -> south) with a cheap reverse slice
        if ensure_desc_lat:
            lat = da["latitude"]
            if lat.size > 1 and np.issubdtype(lat.dtype, np.number):
                if lat.values[0] < lat.values[-1]:
                    da = da.isel(latitude=slice(None, None, -1))
                    if debug:
                        print("[DEBUG] Latitude was ascending; reversed latitude index.")

        # Preserve source fill value when available.
        fill_value = da.encoding.get("_FillValue", None)
        if fill_value is None:
            fill_value = da.attrs.get("_FillValue", da.attrs.get("missing_value", None))
        if isinstance(fill_value, (list, tuple, np.ndarray)):
            fill_value = np.asarray(fill_value).ravel()
            fill_value = fill_value[0] if fill_value.size else None
        try:
            fill_value = np.float32(fill_value) if fill_value is not None else np.float32(np.nan)
        except (TypeError, ValueError):
            fill_value = np.float32(np.nan)

        # Build output dataset
        out = da.astype("float32", copy=False).to_dataset(name=selected_var)
        out.attrs = dict(ds.attrs)

        # Clean serialization attrs that can conflict with xarray encoding.
        for k in ["scale_factor", "add_offset", "_FillValue", "missing_value"]:
            if k in out[selected_var].attrs:
                out[selected_var].attrs.pop(k, None)
        out[selected_var].encoding.clear()

        # CF-ish coord attrs
        out["longitude"].attrs.update({"standard_name": "longitude", "units": "degrees_east"})
        out["latitude"].attrs.update({"standard_name": "latitude", "units": "degrees_north"})
        if "time" in out.coords:
            out["time"].attrs.update({"standard_name": "time"})

        # Encoding: keep float32, do not quantize to ubyte.
        enc = {selected_var: {"dtype": "float32", "_FillValue": fill_value, "zlib": False}}
        enc["longitude"] = {"dtype": "float64"}
        enc["latitude"] = {"dtype": "float64"}

        if debug:
            print(f"[DEBUG] Output _FillValue: {fill_value!r}")

        out.to_netcdf(
            out_path,
            engine="netcdf4",
            format="NETCDF4_CLASSIC",
            encoding=enc,
            unlimited_dims=("time",) if "time" in out.dims else None,
        )

    if debug:
        print(f"[DEBUG] Done in {time.perf_counter() - t0:.2f}s")
    print(f"Wrote: {out_path} ({selected_var})")


def _default_output_file(in_file: Path, out_dir: Optional[Path]) -> Path:
    if out_dir is None:
        return in_file.with_name(f"{in_file.stem}_reshaped{in_file.suffix}")
    return out_dir / in_file.name


def convert_path(
    in_path: str,
    out_path: Optional[str] = None,
    var: Optional[str] = None,
    debug: bool = False,
    decode_times: bool = False,
    mask_and_scale: bool = True,
    ensure_desc_lat: bool = True,
) -> None:
    src = Path(in_path).expanduser().resolve()
    dst = Path(out_path).expanduser().resolve() if out_path else None

    if not src.exists():
        raise FileNotFoundError(f"Input path not found: {src}")

    if src.is_dir():
        nc_files = sorted(p for p in src.iterdir() if p.is_file() and p.suffix.lower() == ".nc")
        if not nc_files:
            raise FileNotFoundError(f"No .nc files found in folder: {src}")

        out_dir = None
        if dst is not None:
            if dst.suffix.lower() == ".nc":
                raise ValueError("For folder input, out_path must be a folder, not a .nc file.")
            out_dir = dst
            out_dir.mkdir(parents=True, exist_ok=True)

        print(f"Found {len(nc_files)} .nc file(s) in {src}")
        for i, in_file in enumerate(nc_files, start=1):
            out_file = _default_output_file(in_file, out_dir)
            print(f"[{i}/{len(nc_files)}] {in_file.name} -> {out_file.name}")
            convert_evi_nc(
                str(in_file),
                str(out_file),
                var=var,
                debug=debug,
                decode_times=decode_times,
                mask_and_scale=mask_and_scale,
                ensure_desc_lat=ensure_desc_lat,
            )
        return

    # Single input file
    if dst is None:
        out_file = _default_output_file(src, None)
    elif dst.suffix.lower() == ".nc":
        out_file = dst
    else:
        dst.mkdir(parents=True, exist_ok=True)
        out_file = _default_output_file(src, dst)

    convert_evi_nc(
        str(src),
        str(out_file),
        var=var,
        debug=debug,
        decode_times=decode_times,
        mask_and_scale=mask_and_scale,
        ensure_desc_lat=ensure_desc_lat,
    )


if __name__ == "__main__":
    import argparse
    p = argparse.ArgumentParser(
        description="Reshape one NetCDF file or all .nc files in a folder.",
        epilog=(
            "Examples:\n"
            "  python ncf.py ./OzWALD.EVI.2005.nc --fast\n"
            "  python ncf.py ./OzWALD.EVI.2005.nc ./out.nc --var EVI\n"
            "  python ncf.py ./nc_folder --fast\n"
            "  python ncf.py ./nc_folder ./out_folder --var EVI\n"
        ),
        formatter_class=argparse.RawTextHelpFormatter,
    )
    p.add_argument("in_path", help="Input .nc file or folder containing .nc files.")
    p.add_argument(
        "out_path",
        nargs="?",
        default=None,
        help="Optional output .nc file (single input) or output folder (folder input). "
             "If omitted, writes beside input with suffix '_reshaped'.",
    )
    p.add_argument(
        "--var",
        default=None,
        help="Variable name to convert. If omitted, auto-detect (prefer 'EVI', else first data variable).",
    )
    p.add_argument("--debug", action="store_true")
    p.add_argument("--decode-times", action="store_true", help="Decode time coordinates to datetime objects.")
    p.add_argument("--no-mask-and-scale", action="store_true", help="Skip scale/mask decoding for speed.")
    p.add_argument("--skip-lat-reorder", action="store_true", help="Skip forcing latitude to descending order.")
    p.add_argument(
        "--fast",
        action="store_true",
        help="Fast mode: disables mask/scale decode and time decode (use only if data are already unscaled).",
    )
    args = p.parse_args()
    decode_times = args.decode_times
    mask_and_scale = not args.no_mask_and_scale
    ensure_desc_lat = not args.skip_lat_reorder
    if args.fast:
        decode_times = False
        mask_and_scale = False

    convert_path(
        args.in_path,
        args.out_path,
        var=args.var,
        debug=args.debug,
        decode_times=decode_times,
        mask_and_scale=mask_and_scale,
        ensure_desc_lat=ensure_desc_lat,
    )


