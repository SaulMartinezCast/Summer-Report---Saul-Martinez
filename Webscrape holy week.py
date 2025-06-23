import os
import time
import pandas as pd
import requests
from datetime import datetime, timedelta
from dateutil.easter import easter
from dotenv import load_dotenv
from pathlib import Path
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

# === Load API Key ===
load_dotenv()
API_KEY = os.getenv("API_KEY")
if not API_KEY:
    raise ValueError("API_KEY not found in .env file")

# === Constants ===
START_YEAR = 1929
END_YEAR = 1930
OUTPUT_DIR = Path("holy_week_data")
OUTPUT_DIR.mkdir(exist_ok=True)
CHUNK_SIZE = 10

# === Helper: get Palm Sunday and Easter Sunday range ===
def get_holy_week_range(year):
    easter_sunday = easter(year)
    palm_sunday = easter_sunday - timedelta(days=7)
    return palm_sunday, easter_sunday

# === Requests session with retries ===
session = requests.Session()
retries = Retry(
    total=5,
    backoff_factor=1,
    status_forcelist=[429, 500, 502, 503, 504],
    raise_on_status=False
)
session.mount("https://", HTTPAdapter(max_retries=retries))

# === Get Station List ===
STATION_LIST_URL = f"https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/todasestaciones/?api_key={API_KEY}"
try:
    station_resp = session.get(STATION_LIST_URL)
    station_meta_url = station_resp.json().get("datos")
    if not station_meta_url:
        raise Exception("Could not fetch station metadata URL.")
    stations_data = session.get(station_meta_url).json()
    stations_df = pd.DataFrame(stations_data)
    stations_df = stations_df[stations_df['indicativo'].notna()]
    station_codes = stations_df['indicativo'].tolist()
except Exception as e:
    raise RuntimeError(f"Failed to retrieve station list: {e}")

# Split into smaller chunks
station_chunks = [station_codes[i:i + CHUNK_SIZE] for i in range(0, len(station_codes), CHUNK_SIZE)]

# === Main Loop ===
for year in range(START_YEAR, END_YEAR + 1):
    print(f"\n>>> Fetching Holy Week data for year {year}")
    start_date, end_date = get_holy_week_range(year)
    fecha_ini = start_date.strftime("%Y-%m-%dT00:00:00UTC")
    fecha_fin = end_date.strftime("%Y-%m-%dT00:00:00UTC")
    yearly_data = []

    for chunk in station_chunks:
        station_param = ",".join(chunk)
        url = f"https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/{fecha_ini}/fechafin/{fecha_fin}/estacion/{station_param}?api_key={API_KEY}"
        headers = {"Accept": "application/json"}

        try:
            meta_resp = session.get(url, headers=headers)
            if meta_resp.status_code != 200:
                print(f"  Metadata fetch failed ({meta_resp.status_code}) for chunk. Response: {meta_resp.text}")
                continue

            try:
                data_url = meta_resp.json().get("datos")
            except Exception as e:
                print(f"  Failed to parse metadata response as JSON: {e}")
                print(f"  Raw response: {meta_resp.text}")
                continue

            if not data_url:
                print(f"  No data URL returned for chunk: {chunk[:3]}...")
                continue

            data_resp = session.get(data_url)
            if data_resp.status_code != 200:
                print(f"  Data fetch failed ({data_resp.status_code}) for chunk.")
                continue

            try:
                chunk_data = data_resp.json()
                yearly_data.extend(chunk_data)
            except Exception as e:
                print(f"  Failed to parse data JSON: {e}")
                print(f"  Raw data: {data_resp.text[:200]}...")
                continue

            time.sleep(1.5)  # to respect AEMET API limits

        except Exception as e:
            print(f"  Error processing chunk: {e}")
            continue

    if yearly_data:
        df = pd.DataFrame(yearly_data)
        df.to_csv(OUTPUT_DIR / f"precip_holy_week_{year}.csv", index=False)
        print(f"  ✅ Saved data for {year} ({len(df)} rows)")
    else:
        print(f"  ⚠️ No data found for {year}")
