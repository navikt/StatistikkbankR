run_with_exported_helper_mocks <- function(code, overrides = list()) {
  ns <- asNamespace("StatistikkbankR")
  replacement_names <- names(overrides)

  originals <- lapply(replacement_names, function(nm) get(nm, envir = ns, inherits = FALSE))
  names(originals) <- replacement_names
  was_locked <- vapply(replacement_names, function(nm) bindingIsLocked(nm, ns), logical(1))

  on.exit({
    for (nm in rev(replacement_names)) {
      if (bindingIsLocked(nm, ns)) {
        unlockBinding(nm, ns)
      }
      assign(nm, originals[[nm]], envir = ns)
      if (was_locked[[nm]]) {
        lockBinding(nm, ns)
      }
    }
  }, add = TRUE)

  for (nm in replacement_names) {
    if (bindingIsLocked(nm, ns)) {
      unlockBinding(nm, ns)
    }
    assign(nm, overrides[[nm]], envir = ns)
    if (was_locked[[nm]]) {
      lockBinding(nm, ns)
    }
  }

  eval(substitute(code), parent.frame())
}
