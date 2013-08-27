module AlgorithmW 
    ( Exp (..)
    , Type (..)
    , ti -- :: TypeEnv -> Exp -> (Subst, Type)
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint as P

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

data Exp
    = EVar String
    | ELit Lit
    | EApp Exp Exp
    | EAbs String Exp
    | ELet String Exp Exp
    deriving (Eq, Ord)

data Lit
    = LInt Integer
    | LBool Bool
    deriving (Eq, Ord)

data Type 
    = TVar String 
    | TInt
    | TBool
    | TFun Type Type
    deriving (Eq, Ord)

data Scheme = Scheme [String] Type

class Types a where
    ftv     :: a -> S.Set String
    apply   :: Subst -> a -> a

instance Types Type where
    ftv (TVar n) = S.singleton n
    ftv TInt = S.empty
    ftv TBool = S.empty
    ftv (TFun t1 t2) = ftv t1 `S.union` ftv t2
    apply s (TVar n) = 
        case M.lookup n s of
            Nothing -> TVar n
            Just t -> t
    apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
    apply s t = t

instance Types Scheme where
    ftv (Scheme vars t) = (ftv t) `S.difference` (S.fromList vars)
    apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv l = foldr S.union S.empty (map ftv l)


type Subst = M.Map String Type

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (M.map (apply s1) s2) `M.union` s1

newtype TypeEnv = TypeEnv (M.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList ((ftv t) `S.difference` (ftv env))

data TIEnv = TIEnv {}
data TIState = TIState
    { tiSupply :: Int
    , tiSubst  :: Subst
    }

type TI a = ErrorT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = do
    (res, st) <- runStateT (runReaderT (runErrorT t)) initTIState
    return (res, st)
  where initTIEnv = TIEnv {}
        initTIState = TIState { tiSupply = 0, tiSubst = M.empty }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s{ tiSupply = tiSupply s + 1 }
    return (TVar (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = M.fromList (zip vars nvars)
    return $ apply s t

mgu :: Type -> Type -> TI Subst

varBind :: String -> Type -> TI Subst

tiLit :: TypeEnv -> Lit -> TI (Subst, Type)

ti :: TypeEnv -> Exp -> TI (Subst, Type)

typeInference :: M.Map String Scheme -> Exp -> TI Type
