box::use(
  src/signals/triage_signals,
  src/utils/get_env,
)

triage_signals$triage_signals(
  indicator_id = get_env$get_env("INDICATOR_ID"),
  n_campaigns = 0,
  test = get_env$get_env("TEST")
)
