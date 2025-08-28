"""
Streamlit app to explore football match attendance and fan-forecast data.

CSV columns expected:
- team1, team2, goals1, goals2, date
- attendance, homeatt, awayatt
- home.forecasts, away.forecasts

Assumptions:
- team1 is the home team, team2 is the away team.
- Forecasts are counts of home/away fans expected (not probabilities).

Tip: add to requirements.txt -> streamlit pandas numpy altair python-dateutil
"""
from __future__ import annotations

import math
from datetime import date, datetime
from typing import Dict

import numpy as np
import pandas as pd
import streamlit as st

try:
    import altair as alt  # optional; used for charts
except Exception:
    alt = None

st.set_page_config(
    page_title="Match Attendance & Fan Forecasts",
    page_icon="📊",
    layout="wide",
)

# ---------------------------
# Utility functions
# ---------------------------

@st.cache_data(show_spinner=False)
def load_csv(path: str, date_col: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    df = df.dropna(subset=["home.forecasts", "away.forecasts"])
    df["home.forecasts"] = df["home.forecasts"].apply(np.ceil).astype(int)
    df["away.forecasts"] = df["away.forecasts"].apply(np.ceil).astype(int)
    
    # Assume a ±10% uncertainty for illustration
    df["home_forecast_lower"] = np.ceil(df["home.forecasts"] * 0.9).astype(int)
    df["home_forecast_upper"] = np.ceil(df["home.forecasts"] * 1.1).astype(int)
    df["away_forecast_lower"] = np.ceil(df["away.forecasts"] * 0.9).astype(int)
    df["away_forecast_upper"] = np.ceil(df["away.forecasts"] * 1.1).astype(int)
    
    df["forecast_total_lower"] = df["home_forecast_lower"] + df["away_forecast_lower"]
    df["forecast_total_upper"] = df["home_forecast_upper"] + df["away_forecast_upper"]
    df[date_col] = pd.to_datetime(df[date_col], errors="coerce").dt.tz_localize(None)
    return df


def standardize_columns(df: pd.DataFrame, mapping: Dict[str, str]) -> pd.DataFrame:
    rename_map = {v: k for k, v in mapping.items() if v in df.columns and k != v}
    return df.rename(columns=rename_map)


def compute_errors(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    # drop matches with no forecast at all
    df = df.dropna(subset=["home.forecasts", "away.forecasts"])

    df["home.forecasts"] = df["home.forecasts"].apply(np.ceil).astype(int)
    df["away.forecasts"] = df["away.forecasts"].apply(np.ceil).astype(int)
    df["forecast_total"] = df["home.forecasts"] + df["away.forecasts"]

    df["error_home"] = df["homeatt"] - df["home.forecasts"]
    df["error_away"] = df["awayatt"] - df["away.forecasts"]
    df["error_total"] = df["attendance"] - df["forecast_total"]

    for col in ["home", "away", "total"]:
        df[f"abs_error_{col}"] = df[f"error_{col}"].abs()

    df["ape_home"] = np.where(
        df["homeatt"].abs() > 0, (df["abs_error_home"] / df["homeatt"]) * 100, np.nan
    )
    df["ape_away"] = np.where(
        df["awayatt"].abs() > 0, (df["abs_error_away"] / df["awayatt"]) * 100, np.nan
    )
    df["ape_total"] = np.where(
        df["attendance"].abs() > 0, (df["abs_error_total"] / df["attendance"]) * 100, np.nan
    )
    return df


def summary_metrics(df: pd.DataFrame) -> pd.DataFrame:
    metrics = {
        "MAE (Home fans)": df["abs_error_home"].mean(),
        "MAE (Away fans)": df["abs_error_away"].mean(),
        "MAE (Total crowd)": df["abs_error_total"].mean(),
        "MAPE (Home fans, %)": df["ape_home"].mean(),
        "MAPE (Away fans, %)": df["ape_away"].mean(),
        "MAPE (Total crowd, %)": df["ape_total"].mean(),
    }
    out = pd.DataFrame(
        {"Metric": list(metrics.keys()), "Value": [None if pd.isna(v) else v for v in metrics.values()]}
    )
    return out


# ---------------------------
# Config
# ---------------------------

DEFAULT_CSV_PATH = "home-away-fan-forecasts.csv"  # <-- change this to the actual path of your CSV file
DEFAULT_CLUB = "Oldham Athletic"

expected = {
    "team1": "team1",
    "team2": "team2",
    "goals1": "goals1",
    "goals2": "goals2",
    "date": "date",
    "attendance": "attendance",
    "homeatt": "homeatt",
    "awayatt": "awayatt",
    "home.forecasts": "home.forecasts",
    "away.forecasts": "away.forecasts",
}

venue_choice = st.sidebar.radio("Venue", ["Both", "Home", "Away"], index=0, horizontal=True)
view_choice = st.sidebar.radio("Period", ["Future fixtures", "Past matches"], index=0)
today_override = st.sidebar.date_input(
    "Treat ‘today’ as", value=date.today(), help="Used to split past vs future."
)

# ---------------------------
# Load data
# ---------------------------

raw = load_csv(DEFAULT_CSV_PATH, expected["date"])
std = standardize_columns(raw, expected)

std = std.copy()
std["match_date"] = std["date"].dt.date
std["home_team"] = std["team1"]
std["away_team"] = std["team2"]
std["forecast_total"] = std["home.forecasts"].fillna(0) + std["away.forecasts"].fillna(0)

clubs = sorted(pd.unique(pd.concat([std["home_team"], std["away_team"]], ignore_index=True)).astype(str).tolist())

# Default to Oldham Athletic if present
if DEFAULT_CLUB in clubs:
    default_index = clubs.index(DEFAULT_CLUB)
else:
    default_index = 0 if clubs else None

club = st.sidebar.selectbox("Club", options=clubs, index=default_index)

if not club:
    st.warning("No clubs found in your data.")
    st.stop()

std["club"] = club
std["venue"] = np.where(std["home_team"] == club, "Home", np.where(std["away_team"] == club, "Away", None))
std = std[std["venue"].notna()].copy()

if venue_choice != "Both":
    std = std[std["venue"] == venue_choice]

# Past vs Future split
today_dt = pd.Timestamp(datetime.combine(today_override, datetime.min.time()))
if view_choice == "Future fixtures":
    view_df = std[std["date"] >= today_dt].copy()
    historic_df = std[std["date"] < today_dt].copy()
else:
    view_df = std[std["date"] < today_dt].copy()

# ---------------------------
# Header
# ---------------------------

st.title("📊 Match Attendance & Fan Forecasts")
st.markdown(
    """
    This app produces forecasts for attendances at football matches. It is based on the dataset and modelling in 
    [Humphreys, B., Reade, J. J., Schreyer, D., & Singleton (2024), "Separating the crowds: Examining home and away attendances at football matches." In "Essays on Sports Economics in Memory of Stefan Kesenne", University of Oviedo Publishing.](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4275076)
    
    **Forecast methodology:**  
    Home and away fan forecasts are produced using historical (home/away) attendance patterns,
    recent team performance, expected match significance, the distance between the two clubs,
    whether or not the match was on a public holiday plus other scheduling factors. 
    Forecasts are rounded up to the nearest whole fan.
    
    If you spot errors, or have suggestions/requests, please contact [James Reade](mailto:j.j.reade@reading.ac.uk?subject=Attendance%20Forecast%20App)
    """
)
st.subheader(f"{club} — {venue_choice} — {view_choice}")

# ---------------------------
# FUTURE FIXTURES VIEW
# ---------------------------
if view_choice == "Future fixtures":
    if view_df.empty:
        st.info("No future fixtures match the current filters.")
    else:
        future_cols = [
            "match_date",
            "home_team",
            "away_team",
            "venue",
            "forecast_total", "forecast_total_lower", "forecast_total_upper",
            "home.forecasts", "home_forecast_lower", "home_forecast_upper",
            "away.forecasts", "away_forecast_lower", "away_forecast_upper",
        ]
        show_df = view_df[future_cols].sort_values("match_date")
        show_df = show_df.rename(
            columns={
                "match_date": "Date",
                "home_team": "Home",
                "away_team": "Away",
                "forecast_total": "Forecast total crowd",
                "forecast_total_lower": "Forecast total lower bound", 
                "forecast_total_upper": "Forecast total upper bound",
                "home.forecasts": "Forecast home fans",
                "home_forecast_lower": "Home fans lower bound", 
                "home_forecast_upper": "Home fans upper bound",
                "away.forecasts": "Forecast away fans",
                "away_forecast_lower": "Away fans lower bound", 
                "away_forecast_upper": "Away fans upper bound",
            }
        )
        st.dataframe(show_df, use_container_width=True, height=450)

        if alt is not None and not show_df.empty:
            st.markdown("**Forecast total crowd over time**")
            chart_df = show_df.rename(columns={"Date": "date"})
            calc_df = compute_errors(historic_df)
            mape_home = calc_df["ape_home"].mean()
            mape_away = calc_df["ape_away"].mean()
            mape_total = calc_df["ape_total"].mean()
            # Assume a ±10% uncertainty for illustration
            chart_df["Home fans lower bound"] = np.ceil(chart_df["Forecast home fans"] * (1-mape_home/100)).astype(int)
            chart_df["Home fans upper bound"] = np.ceil(chart_df["Forecast home fans"] * (1+mape_home/100)).astype(int)
            chart_df["Away fans lower bound"] = np.ceil(chart_df["Forecast away fans"] * (1-mape_away/100)).astype(int)
            chart_df["Away fans upper bound"] = np.ceil(chart_df["Forecast away fans"] * (1+mape_away/100)).astype(int)
            chart_df["Forecast total lower bound"] = np.ceil(chart_df["Forecast total crowd"] * (1-mape_total/100)).astype(int)
            chart_df["Forecast total upper bound"] = np.ceil(chart_df["Forecast total crowd"] * (1+mape_total/100)).astype(int)
            # c = (
            #     alt.Chart(chart_df)
            #     .mark_line(point=True)
            #     .encode(x="date:T", y="Forecast total crowd:Q", tooltip=list(chart_df.columns))
            #     .properties(height=300)
            # )
            # Shaded band for home fan forecast ± bounds
            band_total = (
                alt.Chart(chart_df)
                .mark_area(opacity=0.2, color="black")
                .encode(
                    x="date:T",
                    y="Forecast total lower bound:Q",
                    y2="Forecast total upper bound:Q",
                )
            )
            
            # Line for point forecast
            line_total = (
                alt.Chart(chart_df)
                .mark_line(point=True, color="black")
                .encode(
                    x="date:T",
                    y="Forecast total crowd:Q",
                    tooltip=list(chart_df.columns),
                )
            )
            
            # Combine shaded band and line
            c_total = band_total + line_total
            st.altair_chart(c_total, use_container_width=True)

            st.markdown("**Forecast home vs away fans over time**")
            # c_home = (
            #     alt.Chart(chart_df)
            #     .mark_line(point=True, color="blue")
            #     .encode(x="date:T", y="Forecast home fans:Q", tooltip=list(chart_df.columns))
            #     .properties(height=300)
            # )
            # Shaded band for home fan forecast ± bounds
            band_home = (
                alt.Chart(chart_df)
                .mark_area(opacity=0.2, color="blue")
                .encode(
                    x="date:T",
                    y="Home fans lower bound:Q",
                    y2="Home fans upper bound:Q",
                )
            )
            
            # Line for point forecast
            line_home = (
                alt.Chart(chart_df)
                .mark_line(point=True, color="blue")
                .encode(
                    x="date:T",
                    y="Forecast home fans:Q",
                    tooltip=list(chart_df.columns),
                )
            )
            
            # Combine shaded band and line
            c_home = band_home + line_home
            # c_away = (
            #     alt.Chart(chart_df)
            #     .mark_line(point=True, color="red")
            #     .encode(x="date:T", y="Forecast away fans:Q", tooltip=list(chart_df.columns))
            #     .properties(height=300)
            # )
            band_away = (
                alt.Chart(chart_df)
                .mark_area(opacity=0.2, color="red")
                .encode(
                    x="date:T",
                    y="Away fans lower bound:Q",
                    y2="Away fans upper bound:Q",
                )
            )
            
            # Line for point forecast
            line_away = (
                alt.Chart(chart_df)
                .mark_line(point=True, color="red")
                .encode(
                    x="date:T",
                    y="Forecast away fans:Q",
                    tooltip=list(chart_df.columns),
                )
            )
            
            # Combine shaded band and line
            c_away = band_away + line_away
            
            st.altair_chart(c_home, use_container_width=True)
            st.altair_chart(c_away, use_container_width=True)

        csv = show_df.to_csv(index=False).encode("utf-8")
        st.download_button("Download future fixtures (CSV)", csv, file_name="future_fan_forecasts.csv")

# ---------------------------
# PAST MATCHES VIEW
# ---------------------------
else:
    if view_df.empty:
        st.info("No past matches match the current filters.")
    else:
        past_df = compute_errors(view_df)
        past_cols = [
            "match_date",
            "home_team",
            "away_team",
            "venue",
            "home.forecasts",
            "away.forecasts",
            "forecast_total",
            "homeatt",
            "awayatt",
            "attendance",
            "error_home",
            "error_away",
            "error_total",
            "abs_error_total",
            "ape_total",
        ]
        show_df = past_df[past_cols].sort_values("match_date")
        show_df = show_df.rename(
            columns={
                "match_date": "date",
                "home_team": "Home",
                "away_team": "Away",
                "home.forecasts": "Forecast home fans",
                "away.forecasts": "Forecast away fans",
                "forecast_total": "Forecast total crowd",
                "homeatt": "Actual home fans",
                "awayatt": "Actual away fans",
                "attendance": "Actual total crowd",
                "error_home": "Error (home)",
                "error_away": "Error (away)",
                "error_total": "Error (total)",
                "abs_error_total": "Absolute error (total)",
                "ape_total": "APE (total, %)",
            }
        )

        st.markdown("### Per-match details")
        st.dataframe(show_df, use_container_width=True, height=500)

        st.markdown("### Summary metrics")
        smry = summary_metrics(past_df)
        st.dataframe(smry, use_container_width=True, height=250)

        if alt is not None and not show_df.empty:
            cc1 = (
                alt.Chart(show_df.rename(columns={"Date": "date"}))
                .mark_point()
                .encode(
                    x="Forecast total crowd:Q",
                    y="Actual total crowd:Q",
                    tooltip=list(show_df.columns),
                )
                .properties(height=300)
            )
            line = alt.Chart(pd.DataFrame({"x": [show_df["Forecast total crowd"].min(), show_df["Forecast total crowd"].max()]})).mark_line().encode(x="x:Q", y="x:Q")
            st.markdown("**Actual vs Forecast (total crowd)**")
            # st.write("Columns in show_df:", show_df.columns)

            st.altair_chart(cc1 + line, use_container_width=True)

            st.markdown("**Errors over time (total)**")
            cc2 = (
                alt.Chart(show_df.rename(columns={"Date": "date"}))
                .mark_line(point=True)
                .encode(x="date:T", y="Error (total):Q", tooltip=list(show_df.columns))
                .properties(height=300)
            )
            st.altair_chart(cc2, use_container_width=True)

            st.markdown("**Home vs Away fans over time**")
            c_home = (
                alt.Chart(show_df.rename(columns={"Date": "date"}))
                .mark_line(point=True, color="blue")
                .encode(x="date:T", y="Actual home fans:Q", tooltip=list(show_df.columns))
                .properties(height=300)
            )
            c_away = (
                alt.Chart(show_df.rename(columns={"Date": "date"}))
                .mark_line(point=True, color="red")
                .encode(x="date:T", y="Actual away fans:Q", tooltip=list(show_df.columns))
                .properties(height=300)
            )
            st.altair_chart(c_home, use_container_width=True)
            st.altair_chart(c_away, use_container_width=True)

        csv = show_df.to_csv(index=False).encode("utf-8")
        st.download_button("Download past matches (CSV)", csv, file_name="past_matches_with_errors.csv")
