import altair as alt
import pandas as pd

df = pd.DataFrame({
    "date": pd.date_range("2025-01-01", periods=5),
    "Forecast home fans": [1200, 1300, 1250, 1400, 1350],
    "home_forecast_lower": [1100, 1200, 1150, 1300, 1250],
    "home_forecast_upper": [1300, 1400, 1350, 1500, 1450]
})

band = alt.Chart(df).mark_area(opacity=0.2, color="blue").encode(
    x="date:T",
    y="home_forecast_lower:Q",
    y2="home_forecast_upper:Q"
)

line = alt.Chart(df).mark_line(point=True, color="blue").encode(
    x="date:T",
    y="Forecast home fans:Q"
)

band + line
