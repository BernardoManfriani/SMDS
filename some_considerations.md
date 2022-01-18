# Some considerations about every points

Point 1:
  - We have deleted some rows
    - long
    - lat
    - codice_regione
    - denominazion_regione
    -
  -

---

Point 2:
  - some issues about new hospitalized e new occupied places in intensive care
    - ingressi_terapia_intensiva doesn't cover every day
    - nuovi_ospedalizzati can't be inferred by data 
  - model that can be used
    - linear regression
    - glm
    - cart
      - loess
      - kernel regression 
      - smouth
    - gam 
    - regression tree
    - Time series
      - Moving average
      - Exponential smoothing
      - ARIMA
