module Ghc_cfa (plugin) where
import GhcPlugins
import Data.Map (Map)
import qualified Data.Map as Map

type Address = String
type Env = Map Id Address
type VStore = Map Address Value
type KAddress = String
type KStore = Map KAddress Continuation

data Configuration = In CoreExpr Env VStore KAddress KStore| Out Value VStore KAddress KStore

data Continuation = AnyCnt | Arg CoreExpr Env KAddress | Fn Value Env KAddress
data Value = AnyValue | Closure Id CoreExpr Env
defaultAddr = error "defaultAddr"

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }
  
allocK :: Configuration -> KAddress
allocK c = error "AllocK"

allocV :: Configuration -> KAddress
allocV c = error "AllocV"
  
eval :: Configuration -> [Configuration]
eval (In (Var id) e s k ks) =
  let value = Map.findWithDefault AnyValue (Map.findWithDefault defaultAddr id e) s
  in [Out value s k ks]
eval (In (Lam b exprB) env s k ks) =
  let closure = Closure b exprB env
  in [Out closure s k ks]
eval c@(In (App exprB argb) env s k ks) =
  let a = allocK c
      ks' = Map.insert a (Arg argb env k) ks
  in [In exprB env s a ks']
eval c@(Out value s k ks) =
  let k' = Map.findWithDefault AnyCnt k ks in
    case k' of 
      AnyCnt -> [Out value s k ks]
      Arg e env kAddress -> 
        let k'' = Fn value env kAddress
            a = allocK c
            ks' = Map.insert a (k'') ks
        in [In e env s a ks']  
      Fn value env b ->
        case value of
          AnyValue -> [Out AnyValue s k ks]
          Closure id expr env ->
            let b' = allocV c
                env' = Map.insert id b' env
                s' = Map.insert b' expr  s
            in
              [In expr env' s' b ks]


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
printDotBindExpr (App exprB argb) = do
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