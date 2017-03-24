{------------------------------------------------------------------------------
    uPuppet: Catalog rendering in JSON
------------------------------------------------------------------------------}

module UPuppet.ShowJSON ( showJSON ) where

import UPuppet.CState
import UPuppet.Catalog
import UPuppet.AST

import Data.Aeson
import Data.Text (Text,pack)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)

-- would be nice if a type like this was used everywhere for resources.
-- creating a local version here so we can use it with the JSON typeclass
-- (paul)

data ResourceT = ResourceT (Name, Name, [(Attri, UPuppet.AST.Value)])

showJSON :: CState -> Catalog -> String
showJSON st catalog = unpack $ toStrict $ encode (map ResourceT catalog)


instance ToJSON ResourceT where
         -- only emit parameter map if nonempty
        toJSON ( ResourceT ( typeString, titleString, [] ) ) =
			object [ typeJSON, titleJSON ]
		where
			typeJSON = paramToJSON ( "type", ValueString typeString )
			titleJSON = paramToJSON( "title", ValueString titleString )
	toJSON ( ResourceT ( typeString, titleString, params ) ) =
			object [ typeJSON, titleJSON, paramsJSON ]
		where
			typeJSON = paramToJSON ( "type", ValueString typeString )
			titleJSON = paramToJSON( "title", ValueString titleString )
			paramsJSON = (pack "parameters") .= 
				( object $ map paramToJSON params )

paramToJSON :: (Attri, UPuppet.AST.Value) -> (Text,Data.Aeson.Value)
paramToJSON (attr,value) = (pack attr) .= toJSON value

instance ToJSON UPuppet.AST.Value where
	toJSON (ValueInt n) = toJSON n
	toJSON (ValueBool b) = toJSON b
	toJSON (ValueString str) = toJSON str
	toJSON (ValueFloat f) = toJSON f
	toJSON (ValueArray vs) = toJSON vs
	toJSON (ValueHash vvs) = object $ map hashPairToJSON vvs 
        toJSON (ValueRef rn t) = toJSON (rn ++ "[" ++ t ++ "]")
        
-- I think that the type definitions are wrong here?
-- surely, the hash keys should be strings (only) ?
-- (paul)

hashPairToJSON :: (UPuppet.AST.Value, UPuppet.AST.Value) -> (Text,Data.Aeson.Value)
hashPairToJSON (attrValue,value) = case attrValue of
	(ValueString s) -> (pack s) .= toJSON value
	otherwise -> error "hash key is not a string???"
