module Utils where


import AbsLatte


class Typeable a where
    getType :: a -> Type

instance Typeable Type where
    getType t = t

instance Typeable Arg where
    getType (Arg type_ _) = type_

instance Typeable FnDef where
    getType (FnDef t ident args _) = Fun t (map getType args)

instance Typeable Field where
    getType (Field t _) = t


getIdent :: Field -> Ident
getIdent (Field _ ident) = ident
