dso_filename = ModelCompiled@dso@dso_filename
loaded_dlls = getLoadedDLLs()
if (dso_filename %in% names(loaded_dlls)) {
  message("Unloading DLL for model dso ", dso_filename)
  model.dll = loaded_dlls[[dso_filename]][['path']]
  dyn.unload(model.dll)
} else {
  message("No loaded DLL for model dso ", dso_filename)
}

loaded_dlls = getLoadedDLLs()
loaded_dlls = loaded_dlls[str_detect(names(loaded_dlls), '^file')]
if (length(loaded_dlls) > 10) {
  for (dll in head(loaded_dlls, -10)) {
    message("Unloading DLL ", dll[['name']], ": ", dll[['path']])
    dyn.unload(dll[['path']])
  }
}
message("DLL Count = ", length(getLoadedDLLs()), ": [", str_c(names(loaded_dlls), collapse = ","), "]")