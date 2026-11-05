import Howdah.Import
import Howdah.Eval

def imported : JsonImport := howdah% "record.json"

#eval imported |> repr |> Lean.logInfo

#eval imported.implementation.pureEval! |> repr |> Lean.logInfo
