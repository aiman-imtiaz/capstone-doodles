module Ghc_cfa (plugin) where
import GhcPlugins
import Data.Map (Map)
import qualified Data.Map as Map

type Address = Var
type Env = Map Id Address
type VStore = Map Address Value
type KAddress = Int
type KStore = Map KAddress Continuation

data Configuration = In CoreExpr Env VStore KAddress KStore| Out Value VStore KAddress KStore

data Continuation = MtContinuation | Arg CoreExpr Env KAddress 
                    | Fn Value Env KAddress | LetExpr CoreExpr Env Address KAddress
                    | LetRec CoreExpr [CoreExpr] Env Address [Address] KAddress 
                    | CaseContinuation Address [Alt Id] Env
data Value = AnyValue | Closure Id CoreExpr Env | DataConValue DataCon [Address]
             | LiteralValue Literal 
 
 
          
defaultAddress :: Address
defaultAddress = error "defaultAddress"

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }
  
allocK :: KStore -> KAddress
allocK ks = Map.size ks + 1

allocV :: Id -> Address
allocV b = b
  
eval :: Configuration -> [Configuration]
eval (In (Lit lit) env s k ks) =
  [Out (LiteralValue lit) s k ks]
eval (In (Var b) env s k ks) =
  let value = Map.findWithDefault AnyValue (Map.findWithDefault defaultAddress b env) s
  in [Out value s k ks]
eval (In (Lam b exprB) env s k ks) =
  let closure = Closure b exprB env
  in [Out closure s k ks]
eval (In (App exprB argb) env s k ks) =
  let a = allocK ks
      ks' = Map.insert a (Arg argb env k) ks
  in [In exprB env s a ks']
eval (In (Let (NonRec b e1) e2) env s k ks) =
  let a = allocK ks
      bAddress = allocV b
      env' = Map.insert b bAddress env
      ks' = Map.insert a (LetExpr e2 env' bAddress k) ks
  in [In e1 env s a ks']
eval (In (Let (Rec bindList) body) env s k ks) =
   let (env', bAddrList) = foldr 
          (\(id, _) (envi, bAddresses) -> 
          let bAddress = allocV id
          in (Map.insert id bAddress envi, bAddress : bAddresses)) 
          (env, []) bindList
       (e1 : es) = map snd bindList
       a1 : as = bAddrList
       a = allocK ks
       ks' = Map.insert a (LetRec body es env' a1 as k) ks
   in [In e1 env' s a ks']
eval (In (Case e b _ altBList)  env s k ks) =
   let bAddress = allocV b
       env' = Map.insert b bAddress env
       a = allocK ks
       ks' = Map.insert a (CaseContinuation bAddress altBList env) ks
    in [In e env s k ks']
eval (Out value s k ks) =
  let k' = Map.findWithDefault MtContinuation k ks in
    case k' of 
      MtContinuation -> [Out value s k ks]
      Arg e env kAddress -> 
        let k'' = Fn value env kAddress
            a = allocK ks
            ks' = Map.insert a k'' ks
        in [In e env s a ks']  
      Fn val  _ b ->
        case val of
          AnyValue -> [Out AnyValue s k ks]
          Closure id expr env ->
            let b' = allocV id
                env'' = Map.insert id b' env
                s' = Map.insert b' value s
            in [In expr env'' s' b ks]
      LetExpr e env bAddress k'' ->
        let vs' = Map.insert bAddress value s
        in [In e env vs' k'' ks]
      LetRec body exprList env bAddress bAddrList k'' ->
        let vs' = Map.insert bAddress value s
        in case exprList of
          [] -> [In body env vs' k'' ks]
          e : es ->
              let (b: bs) = bAddrList
                  newk = LetRec body es env b bs k''
                  a = allocK ks
                  ks' = Map.insert a newk ks
              in [In e env vs' a ks']
      CaseContinuation bAddress altBList env ->
        let vs' = Map.insert bAddress value s
            matched = compareAlts value env altBList
--          -- make the configurations using map
        in []
            
            
compareAlts :: Value -> Env -> [Alt Id] -> [(CoreExpr, Env)]
compareAlts v@(LiteralValue lv) env altList =
 case altList of
   (DEFAULT, [], rhs): ls -> (rhs, env) : compareAlts v env ls
   (LitAlt l, [ ], rhs) : ls -> 
      if l == lv
       then (rhs, env) : compareAlts v env ls
      else compareAlts v env ls
   [] -> []
compareAlts v@(DataConValue valD idList) env altList =
  case altList of
    (DEFAULT, [], rhs): ls -> (rhs, env) : compareAlts v env ls
    (DataAlt dc, dcList, rhs) : ls -> 
          if dc == valD && (length dcList == length idList)
           then 
             let zipped = zip dcList idList
                 env' = foldr
                  (\(id, addr) envi -> Map.insert id addr envi) env zipped
             in (rhs, env) : compareAlts v env' ls
          else compareAlts v env ls
    [] -> []
       
  
printConfig :: [Configuration] -> CoreM ()
printConfig (In {} : t) =
  putMsgS "In"
printConfig (Out {} : t) =
  putMsgS "Out"
printConfig [] =
  putMsgS ""

inject :: CoreBind -> Env ->Configuration
inject bind@(NonRec id e) env =
  let k = MtContinuation
      a0 = 1
      ks = Map.singleton 1 k
      vs = Map.empty
  in In e env vs a0 ks
inject bind@(Rec ((id, e) : t)) env =
 let k = MtContinuation
     a0 = 1
     ks = Map.singleton 1 k
     vs = Map.empty
 in In e env vs a0 ks


initialiseEnv :: [CoreBind] -> Env -> Env
initialiseEnv t env = foldl (flip addToEnv) env t
  
addToEnv :: CoreBind -> Env -> Env
addToEnv (NonRec b e) env =
  Map.insert b (allocV b) env
addToEnv (Rec bindList) env =
  foldr (\(b, _) en -> Map.insert b (allocV b) en) 
        env bindList
        
driver :: [CoreBind] -> CoreM ()
driver binds@(b : _)  = do
   let env = initialiseEnv binds Map.empty
       config = inject b env
       evaluated = eval config
    in printConfig evaluated
driver [] = do
  putMsgS "nothing here"

analysisPass :: ModGuts -> CoreM ModGuts
analysisPass modGuts = do
  dyn <- getDynFlags
  let binds = mg_binds modGuts
  putMsgS (showPpr dyn binds)
  driver binds
  putMsgS "Hello! from plugin pass"
  return modGuts


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  dyn <- getDynFlags
  putMsgS (showPpr dyn todo)
  putMsgS "Hello!"
  let plugin_todo = CoreDoPluginPass "ghc_cfa" analysisPass
  return (plugin_todo : todo)