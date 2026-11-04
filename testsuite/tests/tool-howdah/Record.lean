import Howdah.Import

def imported := howdah% "record.json"

#eval imported |> repr |> Lean.logInfo
