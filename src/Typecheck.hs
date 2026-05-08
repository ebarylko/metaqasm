module Typecheck
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


 -- newtype NonNegative {v:: Int}
 -- 
 -- type Identifier = String
 -- 
 -- type Index = NonNegative
 -- 
 -- -- This data type represents the values an expression can take on,
 -- -- being either a reference to another term or an attempt to obtain a bit or qubit from a
 -- -- collection of registers
 -- data Expression = Identifier | RegisterAccess{registerName::Identifier,  registerNumber::Index}
 -- 
 -- newtype RegistersInfo = RegistersInfo{registerName:: Identifier, numOfRegisters:: NonNegative}
 -- 
 -- 
 -- -- This data type represents the possible commands a user can execute, including the creation of
 -- -- n classical or quantum registers accessible under a certain name
 -- data Command = DeclareQuantumRegisters RegistersInfo | DeclareClassicalRegisters RegistersInfo
 -- 
 -- data termType = Bit | Qbit | 
