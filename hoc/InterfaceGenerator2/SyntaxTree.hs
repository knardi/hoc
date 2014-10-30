{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module SyntaxTree where

import Data.Generics hiding (Generic)
import Data.Binary
import GHC.Generics (Generic)
import SrcPos

type ParsedHeader = [ DeclarationAndPos ]

data Declaration =
        ForwardClass [String]
    |   ForwardProtocol [String]
    |   SelectorList SelectorListHeader [(SrcPos, SelectorListItem)]
    |   Typedef CType String
    |   CTypeDecl CType
    |   ExternVar CType String
    |   ExternFun Selector
    deriving (Show,Eq,Ord)

type DeclarationAndPos = (SrcPos, Declaration)

data SelectorListHeader = 
        Interface String (Maybe String) [String]
    |   Protocol String [String]
    |   Category String String [String]
    deriving (Show,Eq,Ord)

data SelectorListItem =
        InstanceMethod Selector
    |   ClassMethod Selector
    |   LocalDecl Declaration
    |   PropertyDecl CType String [PropertyAttribute]
    |   Required Bool
    deriving (Show,Eq,Ord)

data Selector =
        Selector {
            selName :: String,
            selRetType :: CType,
            selArgTypes :: [CType],
            selVarArg :: Bool
        }
    deriving (Read,Show,Eq,Ord,Typeable,Data,Generic)
instance Binary Selector
    
data PropertyAttribute =
        Getter String
    |   Setter String
    |   ReadOnly
    |   ReadWrite
    |   Assign
    |   Retain
    |   Copy
    deriving (Show, Eq, Ord)
                       
    
data EnumValue = NextValue | GivenValue Integer | TooComplicatedValue String
    deriving (Read, Show, Eq, Ord,Typeable,Data,Generic)
instance Binary EnumValue
    
data CType = CTIDType [String {- protocols -}]
           | CTSimple String
           | CTPointer CType
           | CTFunction CType [CType] Bool
           | CTUnknown
           | CTEnum String [(String, EnumValue)]
           | CTStruct String [(CType, String)]
           | CTUnion String [(CType, String)]
           | CTBuiltin (Maybe Bool {- signed/unsigned? -}) (Maybe Length) String
    deriving (Read,Show,Eq,Ord,Typeable,Data,Generic)
instance Binary CType
    
data Length = LongLong | Long | Short
    deriving (Read,Show,Eq,Ord,Typeable,Data,Generic)
instance Binary Length

cTypeInt = CTBuiltin Nothing Nothing "int"