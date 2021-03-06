module HOC (
        SEL,
        ID,
        nil,
        Object(..),
        Class,
        MetaClass,
        ClassAndObject,
        ClassObject,
        classObject,
        ( # ), ( #. ),
        withExportedArray,
        castObject,
        declareClass,
        declareSelector,
        declareRenamedSelector,
        Covariant,
        CovariantInstance,
        Allocated,
        Inited,

        getSelectorForName,
        
        withAutoreleasePool,

        isNil,
        
        IVar,
        getIVar,
        setIVar,
        getAndSetIVar,
        getInstanceMVar,

        ClassMember(..),
        exportClass,
        
        NewlyAllocated,
        
        SuperClass,
        SuperTarget,
        super,
        castSuper,
        
        CEnum(..),
        declareCEnum,
        declareAnonymousCEnum,
        
        declareExternConst,
        declareExternFun,
        
        sel,
        
        ObjCArgument(..),
        declareStorableObjCArgument,
        declareMarshalledObjectType,
        
        FFITypeable(..),
        makeStructType,
        
        WrappedNSException(..),
        
        declareCStruct,
        declareCStructWithTag,
        
        -- debugging & statistics:
        
        objectMapStatistics
    ) where

import HOC.CBits
import HOC.Base
import HOC.Arguments
import HOC.ID
import HOC.MessageTarget
import HOC.Class
import HOC.DeclareClass
import HOC.DeclareSelector
import HOC.ExportClass
import HOC.Utilities
import HOC.NewlyAllocated
import HOC.Super
import HOC.CEnum
import HOC.ExternConstants
import HOC.ExternFunctions
import HOC.Selectors
import HOC.Exception
import HOC.FFICallInterface
import HOC.CStruct
