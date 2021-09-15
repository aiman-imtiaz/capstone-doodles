module Ghc_cfa (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

plugin_pass :: [ModGuts] -> CoreM [ModGuts]
plugin_pass modGuts = do
  dyn <- getDynFlags
  putMsgS (showPpr dyn (mg_binds modGuts))
  putMsgS "Hello! from plugin pass"
  return modGuts

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  dyn <- getDynFlags
  putMsgS (showPpr dyn todo)
  putMsgS "Hello!"
  let plugin_todo = CoreDoPluginPass "ghc_cfa" plugin_pass
  return (plugin_todo : todo)