module Ghc_cfa (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }


pluginPass :: ModGuts -> CoreM ModGuts
pluginPass modGuts = do
  dyn <- getDynFlags
  let binds = mg_binds modGuts
  putMsgS (showPpr dyn binds)
  printDots binds
  putMsgS "Hello! from plugin pass"
  return modGuts
  
printDotBindExpr :: CoreExpr -> CoreM ()
printDotBindExpr (Var id) = do
  putMsgS "."
  return ()
printDotBindExpr (Lit literal) = do
  putMsgS "."
  return ()
printDotBindExpr (App exprb argb) = do
  putMsgS "."
  return ()
printDotBindExpr (Lam b exprB) = do
  putMsgS "."
  return ()
printDotBindExpr (Let bindB exprB) = do
  putMsgS "."
  return ()
printDotBindExpr (Case exprB b t altB) = do
  putMsgS "."
  return ()
printDotBindExpr (Cast exprB coercion) = do
  putMsgS "."
  return ()
printDotBindExpr (Tick tickishId exprB) = do
  putMsgS "."
  return ()
printDotBindExpr (Type t) = do
  putMsgS "."
  return ()
printDotBindExpr (Coercion coercion) = do
  putMsgS "."
  return ()
  
printDotsBind :: CoreBind -> CoreM ()
printDotsBind (NonRec b e) = do
  printDotBindExpr e
  putMsgS "."
  return ()
printDotsBind (Rec bList) = do
  putMsgS "."
  return ()

printDots :: [CoreBind] -> CoreM ()
printDots (b : t) = do
  printDotsBind b
  printDots t
printDots [] = do
  return ()
  

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  dyn <- getDynFlags
  putMsgS (showPpr dyn todo)
  putMsgS "Hello!"
  let plugin_todo = CoreDoPluginPass "ghc_cfa" pluginPass
  return (plugin_todo : todo)