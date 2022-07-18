{-# language EmptyDataDeriving #-}
{-# language CPP #-}
{-# language GADTs #-}
{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- this is all vendored from kadena-io/openapi3, to avoid dependencies on generics-sop and optics.
-- TODO: perhaps give them a patch making those dependencies optional. seems feasible.

module Network.Wai.Middleware.OpenApi where

import           Control.Applicative
import           Control.Lens          hiding ((.=), allOf, anyOf)
import           Control.Monad
import           Data.Aeson            hiding (Encoding, Result(..))
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap     as KeyMap
#else
import           Data.HashMap.Strict   (HashMap)
#endif
import           Data.Aeson.TH
import qualified Data.Aeson.Types      as JSON
import           Data.Char
import           Data.Foldable
import           Data.Data             (Constr, Data (..), DataType, Fixity (..), Typeable,
                                        constrIndex, mkConstr, mkDataType)
import           Data.Hashable         (Hashable (..))
import           Data.HashSet.InsOrd   (InsOrdHashSet)
import qualified Data.HashSet.InsOrd   as InsOrdHashSet
import           Data.List             (intersect)
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Scientific       (Scientific, isInteger)
import           Data.String           (IsString (..))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Vector           (Vector)
import qualified Data.Vector           as Vector
import           GHC.Generics          (Generic)
import GHC.Stack
import           Network.HTTP.Media    hiding (Encoding)
import qualified Debug.Trace           as Debug

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

import           Network.Wai.Middleware.Helpers

jsonPrefix :: String -> Options
jsonPrefix prefix = defaultOptions
  { fieldLabelModifier      = modifier . drop 1
  , constructorTagModifier  = modifier
  , sumEncoding             = ObjectWithSingleField
  , omitNothingFields       = True
  }
  where
    modifier = lowerFirstUppers . drop (length prefix)

    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

-- import Data.OpenApi.Aeson.Compat        (deleteKey, hasKey, keyToText, lookupKey, objectToList)
-- import Data.OpenApi.Internal.Utils

-- $setup
-- >>> :seti -XDataKinds
-- >>> import Data.Aeson
-- >>> import Data.ByteString.Lazy.Char8 as BSL
-- >>> import Data.OpenApi.Internal.Utils

-- | A list of definitions that can be used in references.
type Definitions = InsOrdHashMap Text

-- | This is the root document object for the API specification.
data OpenApi = OpenApi
  { -- | Provides metadata about the API.
    -- The metadata can be used by the clients if needed.
    _openApiInfo :: Info

    -- | An array of Server Objects, which provide connectivity information
    -- to a target server. If the servers property is not provided, or is an empty array,
    -- the default value would be a 'Server' object with a url value of @/@.
  , _openApiServers :: [Server]

    -- | The available paths and operations for the API.
  , _openApiPaths :: InsOrdHashMap FilePath PathItem

    -- | An element to hold various schemas for the specification.
  , _openApiComponents :: Components

    -- | A declaration of which security mechanisms can be used across the API.
    -- The list of values includes alternative security requirement objects that can be used.
    -- Only one of the security requirement objects need to be satisfied to authorize a request.
    -- Individual operations can override this definition.
    -- To make security optional, an empty security requirement can be included in the array.
  , _openApiSecurity :: [SecurityRequirement]

    -- | A list of tags used by the specification with additional metadata.
    -- The order of the tags can be used to reflect on their order by the parsing tools.
    -- Not all tags that are used by the 'Operation' Object must be declared.
    -- The tags that are not declared MAY be organized randomly or based on the tools' logic.
    -- Each tag name in the list MUST be unique.
  , _openApiTags :: InsOrdHashSet Tag

    -- | Additional external documentation.
  , _openApiExternalDocs :: Maybe ExternalDocs
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The object provides metadata about the API.
-- The metadata MAY be used by the clients if needed,
-- and MAY be presented in editing or documentation generation tools for convenience.
data Info = Info
  { -- | The title of the API.
    _infoTitle :: Text

    -- | A short description of the API.
    -- [CommonMark syntax](https://spec.commonmark.org/) MAY be used for rich text representation.
  , _infoDescription :: Maybe Text

    -- | A URL to the Terms of Service for the API. MUST be in the format of a URL.
  , _infoTermsOfService :: Maybe Text

    -- | The contact information for the exposed API.
  , _infoContact :: Maybe Contact

    -- | The license information for the exposed API.
  , _infoLicense :: Maybe License

    -- | The version of the OpenAPI document (which is distinct from the
    -- OpenAPI Specification version or the API implementation version).
  , _infoVersion :: Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Contact information for the exposed API.
data Contact = Contact
  { -- | The identifying name of the contact person/organization.
    _contactName  :: Maybe Text

    -- | The URL pointing to the contact information.
  , _contactUrl   :: Maybe URL

    -- | The email address of the contact person/organization.
  , _contactEmail :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | License information for the exposed API.
data License = License
  { -- | The license name used for the API.
    _licenseName :: Text

    -- | A URL to the license used for the API.
  , _licenseUrl :: Maybe URL
  } deriving (Eq, Show, Generic, Data, Typeable)

instance IsString License where
  fromString s = License (fromString s) Nothing

-- | An object representing a Server.
data Server = Server
  { -- | A URL to the target host. This URL supports Server Variables and MAY be relative,
    -- to indicate that the host location is relative to the location where
    -- the OpenAPI document is being served. Variable substitutions will be made when
    -- a variable is named in @{brackets}@.
    _serverUrl :: Text

    -- | An optional string describing the host designated by the URL.
    -- [CommonMark syntax](https://spec.commonmark.org/) MAY be used for rich text representation.
  , _serverDescription :: Maybe Text

    -- | A map between a variable name and its value.
    -- The value is used for substitution in the server's URL template.
  , _serverVariables :: InsOrdHashMap Text ServerVariable
  } deriving (Eq, Show, Generic, Data, Typeable)

data ServerVariable = ServerVariable
  { -- | An enumeration of string values to be used if the substitution options
    -- are from a limited set. The array SHOULD NOT be empty.
    _serverVariableEnum :: Maybe (InsOrdHashSet Text) -- TODO NonEmpty

    -- | The default value to use for substitution, which SHALL be sent if an alternate value
    -- is not supplied. Note this behavior is different than the 'Schema\ Object's treatment
    -- of default values, because in those cases parameter values are optional.
    -- If the '_serverVariableEnum' is defined, the value SHOULD exist in the enum's values.
  , _serverVariableDefault :: Text

    -- | An optional description for the server variable.
    -- [CommonMark syntax](https://spec.commonmark.org/) MAY be used for rich text representation.
  , _serverVariableDescription :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

instance IsString Server where
  fromString s = Server (fromString s) Nothing mempty

-- | Holds a set of reusable objects for different aspects of the OAS.
-- All objects defined within the components object will have no effect on the API
-- unless they are explicitly referenced from properties outside the components object.
data Components = Components
  { _componentsSchemas :: Definitions Schema
  , _componentsResponses :: Definitions Response
  , _componentsParameters :: Definitions Param
  , _componentsExamples :: Definitions Example
  , _componentsRequestBodies :: Definitions RequestBody
  , _componentsHeaders :: Definitions Header
  , _componentsSecuritySchemes :: SecurityDefinitions
  , _componentsLinks :: Definitions Link
  , _componentsCallbacks :: Definitions Callback
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Describes the operations available on a single path.
-- A @'PathItem'@ may be empty, due to ACL constraints.
-- The path itself is still exposed to the documentation viewer
-- but they will not know which operations and parameters are available.
data PathItem = PathItem
  { -- | An optional, string summary, intended to apply to all operations in this path.
    _pathItemSummary :: Maybe Text

    -- | An optional, string description, intended to apply to all operations in this path.
    -- [CommonMark syntax](https://spec.commonmark.org/) MAY be used for rich text representation.
  , _pathItemDescription :: Maybe Text

    -- | A definition of a GET operation on this path.
  , _pathItemGet :: Maybe Operation

    -- | A definition of a PUT operation on this path.
  , _pathItemPut :: Maybe Operation

    -- | A definition of a POST operation on this path.
  , _pathItemPost :: Maybe Operation

    -- | A definition of a DELETE operation on this path.
  , _pathItemDelete :: Maybe Operation

    -- | A definition of a OPTIONS operation on this path.
  , _pathItemOptions :: Maybe Operation

    -- | A definition of a HEAD operation on this path.
  , _pathItemHead :: Maybe Operation

    -- | A definition of a PATCH operation on this path.
  , _pathItemPatch :: Maybe Operation

    -- | A definition of a TRACE operation on this path.
  , _pathItemTrace :: Maybe Operation

    -- | An alternative server array to service all operations in this path.
  , _pathItemServers :: [Server]

    -- | A list of parameters that are applicable for all the operations described under this path.
    -- These parameters can be overridden at the operation level, but cannot be removed there.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , _pathItemParameters :: [Referenced Param]
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Describes a single API operation on a path.
data Operation = Operation
  { -- | A list of tags for API documentation control.
    -- Tags can be used for logical grouping of operations by resources or any other qualifier.
    _operationTags :: InsOrdHashSet TagName

    -- | A short summary of what the operation does.
    -- For maximum readability in the swagger-ui, this field SHOULD be less than 120 characters.
  , _operationSummary :: Maybe Text

    -- | A verbose explanation of the operation behavior.
    -- [CommonMark syntax](https://spec.commonmark.org/) can be used for rich text representation.
  , _operationDescription :: Maybe Text

    -- | Additional external documentation for this operation.
  , _operationExternalDocs :: Maybe ExternalDocs

    -- | Unique string used to identify the operation.
    -- The id MUST be unique among all operations described in the API.
    -- The operationId value is **case-sensitive**.
    -- Tools and libraries MAY use the operationId to uniquely identify an operation, therefore,
    -- it is RECOMMENDED to follow common programming naming conventions.
  , _operationOperationId :: Maybe Text

    -- | A list of parameters that are applicable for this operation.
    -- If a parameter is already defined at the @'PathItem'@,
    -- the new definition will override it, but can never remove it.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , _operationParameters :: [Referenced Param]

    -- | The request body applicable for this operation.
    -- The requestBody is only supported in HTTP methods where the HTTP 1.1
    -- specification [RFC7231](https://tools.ietf.org/html/rfc7231#section-4.3.1)
    -- has explicitly defined semantics for request bodies.
    -- In other cases where the HTTP spec is vague, requestBody SHALL be ignored by consumers.
  , _operationRequestBody :: Maybe (Referenced RequestBody)

    -- | The list of possible responses as they are returned from executing this operation.
  , _operationResponses :: Responses

    -- | A map of possible out-of band callbacks related to the parent operation.
    -- The key is a unique identifier for the 'Callback' Object.
    -- Each value in the map is a 'Callback' Object that describes a request
    -- that may be initiated by the API provider and the expected responses.
  , _operationCallbacks :: InsOrdHashMap Text (Referenced Callback)

    -- | Declares this operation to be deprecated.
    -- Usage of the declared operation should be refrained.
    -- Default value is @False@.
  , _operationDeprecated :: Maybe Bool

    -- | A declaration of which security schemes are applied for this operation.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- This definition overrides any declared top-level security.
    -- To remove a top-level security declaration, @Just []@ can be used.
  , _operationSecurity :: [SecurityRequirement]

    -- | An alternative server array to service this operation.
    -- If an alternative server object is specified at the 'PathItem' Object or Root level,
    -- it will be overridden by this value.
  , _operationServers :: [Server]
  } deriving (Eq, Show, Generic, Data, Typeable)

-- This instance should be in @http-media@.
instance Data MediaType where
  gunfold k z c = case constrIndex c of
    1 -> k (k (k (z (\main sub params -> foldl (/:) (main // sub) (Map.toList params)))))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type MediaType."

  toConstr _ = mediaTypeConstr

  dataTypeOf _ = mediaTypeData

mediaTypeConstr :: Constr
mediaTypeConstr = mkConstr mediaTypeData "MediaType" [] Prefix
mediaTypeData :: DataType
mediaTypeData = mkDataType "MediaType" [mediaTypeConstr]

instance Hashable MediaType where
  hashWithSalt salt mt = salt `hashWithSalt` show mt

-- | Describes a single request body.
data RequestBody = RequestBody
  { -- | A brief description of the request body. This could contain examples of use.
    -- [CommonMark syntax](https://spec.commonmark.org/) MAY be used for rich text representation.
    _requestBodyDescription :: Maybe Text

    -- | The content of the request body.
    -- The key is a media type or media type range and the value describes it.
    -- For requests that match multiple keys, only the most specific key is applicable.
    -- e.g. @text/plain@ overrides @text/\*@
  , _requestBodyContent :: InsOrdHashMap MediaType MediaTypeObject

    -- | Determines if the request body is required in the request.
    -- Defaults to 'False'.
  , _requestBodyRequired :: Maybe Bool
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Each Media Type Object provides schema and examples for the media type identified by its key.
data MediaTypeObject = MediaTypeObject
  { _mediaTypeObjectSchema :: Maybe (Referenced Schema)

    -- | Example of the media type.
    -- The example object SHOULD be in the correct format as specified by the media type.
  , _mediaTypeObjectExample :: Maybe Value

    -- | Examples of the media type.
    -- Each example object SHOULD match the media type and specified schema if present.
  , _mediaTypeObjectExamples :: InsOrdHashMap Text (Referenced Example)

    -- | A map between a property name and its encoding information.
    -- The key, being the property name, MUST exist in the schema as a property.
    -- The encoding object SHALL only apply to 'RequestBody' objects when the media type
    -- is @multipart@ or @application/x-www-form-urlencoded@.
  , _mediaTypeObjectEncoding :: InsOrdHashMap Text Encoding
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | In order to support common ways of serializing simple parameters, a set of style values are defined.
data Style
  = StyleMatrix
    -- ^ Path-style parameters defined by [RFC6570](https://tools.ietf.org/html/rfc6570#section-3.2.7).
  | StyleLabel
    -- ^ Label style parameters defined by [RFC6570](https://tools.ietf.org/html/rfc6570#section-3.2.7).
  | StyleForm
    -- ^ Form style parameters defined by [RFC6570](https://tools.ietf.org/html/rfc6570#section-3.2.7).
    -- This option replaces @collectionFormat@ with a @csv@ (when @explode@ is false) or @multi@
    -- (when explode is true) value from OpenAPI 2.0.
  | StyleSimple
    -- ^ Simple style parameters defined by [RFC6570](https://tools.ietf.org/html/rfc6570#section-3.2.7).
    -- This option replaces @collectionFormat@ with a @csv@ value from OpenAPI 2.0.
  | StyleSpaceDelimited
    -- ^ Space separated array values.
    -- This option replaces @collectionFormat@ equal to @ssv@ from OpenAPI 2.0.
  | StylePipeDelimited
    -- ^ Pipe separated array values.
    -- This option replaces @collectionFormat@ equal to @pipes@ from OpenAPI 2.0.
  | StyleDeepObject
    -- ^ Provides a simple way of rendering nested objects using form parameters.
  deriving (Eq, Show, Generic, Data, Typeable)

data Encoding = Encoding
  { -- | The Content-Type for encoding a specific property.
    -- Default value depends on the property type: for @string@
    -- with format being @binary@ – @application/octet-stream@;
    -- for other primitive types – @text/plain@; for object - @application/json@;
    -- for array – the default is defined based on the inner type.
    -- The value can be a specific media type (e.g. @application/json@),
    -- a wildcard media type (e.g. @image/\*@), or a comma-separated list of the two types.
    _encodingContentType :: Maybe MediaType

    -- | A map allowing additional information to be provided as headers,
    -- for example @Content-Disposition@. @Content-Type@ is described separately
    -- and SHALL be ignored in this section.
    -- This property SHALL be ignored if the request body media type is not a @multipart@.
  , _encodingHeaders :: InsOrdHashMap Text (Referenced Header)

    -- | Describes how a specific property value will be serialized depending on its type.
    -- See 'Param' Object for details on the style property.
    -- The behavior follows the same values as query parameters, including default values.
    -- This property SHALL be ignored if the request body media type
    -- is not @application/x-www-form-urlencoded@.
  , _encodingStyle :: Maybe Style

    -- | When this is true, property values of type @array@ or @object@ generate
    -- separate parameters for each value of the array,
    -- or key-value-pair of the map.
    -- For other types of properties this property has no effect.
    -- When style is form, the default value is @true@. For all other styles,
    -- the default value is @false@. This property SHALL be ignored
    -- if the request body media type is not @application/x-www-form-urlencoded@.
  , _encodingExplode :: Maybe Bool

    -- | Determines whether the parameter value SHOULD allow reserved characters,
    -- as defined by [RFC3986](https://tools.ietf.org/html/rfc3986#section-2.2)
    -- @:/?#[]@!$&'()*+,;=@ to be included without percent-encoding.
    -- The default value is @false@. This property SHALL be ignored if the request body media type
    -- is not @application/x-www-form-urlencoded@.
  , _encodingAllowReserved :: Maybe Bool
  } deriving (Eq, Show, Generic, Data, Typeable)

newtype MimeList = MimeList { getMimeList :: [MediaType] }
  deriving (Eq, Show, Semigroup, Monoid, Typeable)

mimeListConstr :: Constr
mimeListConstr = mkConstr mimeListDataType "MimeList" ["getMimeList"] Prefix

mimeListDataType :: DataType
mimeListDataType = mkDataType "Data.OpenApi.MimeList" [mimeListConstr]

instance Data MimeList where
  gunfold k z c = case constrIndex c of
    1 -> k (z (MimeList . map fromString))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type MimeList."
  toConstr (MimeList _) = mimeListConstr
  dataTypeOf _ = mimeListDataType

-- | Describes a single operation parameter.
-- A unique parameter is defined by a combination of a name and location.
data Param = Param
  { -- | The name of the parameter.
    -- Parameter names are case sensitive.
    _paramName :: Text

    -- | A brief description of the parameter.
    -- This could contain examples of use.
    -- CommonMark syntax MAY be used for rich text representation.
  , _paramDescription :: Maybe Text

    -- | Determines whether this parameter is mandatory.
    -- If the parameter is in "path", this property is required and its value MUST be true.
    -- Otherwise, the property MAY be included and its default value is @False@.
  , _paramRequired :: Maybe Bool

    -- | Specifies that a parameter is deprecated and SHOULD be transitioned out of usage.
    -- Default value is @false@.
  , _paramDeprecated :: Maybe Bool

    -- | The location of the parameter.
  , _paramIn :: ParamLocation

    -- | Sets the ability to pass empty-valued parameters.
    -- This is valid only for 'ParamQuery' parameters and allows sending
    -- a parameter with an empty value. Default value is @false@.
  , _paramAllowEmptyValue :: Maybe Bool

    -- | Determines whether the parameter value SHOULD allow reserved characters,
    -- as defined by [RFC3986](https://tools.ietf.org/html/rfc3986#section-2.2)
    -- @:/?#[]@!$&'()*+,;=@ to be included without percent-encoding.
    -- This property only applies to parameters with an '_paramIn' value of 'ParamQuery'.
    -- The default value is 'False'.
  , _paramAllowReserved :: Maybe Bool

    -- | Parameter schema.
  , _paramSchema :: Maybe (Referenced Schema)

    -- | Describes how the parameter value will be serialized depending
    -- on the type of the parameter value. Default values (based on value of '_paramIn'):
    -- for 'ParamQuery' - 'StyleForm'; for 'ParamPath' - 'StyleSimple'; for 'ParamHeader' - 'StyleSimple';
    -- for 'ParamCookie' - 'StyleForm'.
  , _paramStyle :: Maybe Style

    -- | When this is true, parameter values of type @array@ or @object@
    -- generate separate parameters for each value of the array or key-value pair of the map.
    -- For other types of parameters this property has no effect.
    -- When style is @form@, the default value is true. For all other styles, the default value is false.
  , _paramExplode :: Maybe Bool

    -- | Example of the parameter's potential value.
    -- The example SHOULD match the specified schema and encoding properties if present.
    -- The '_paramExample' field is mutually exclusive of the '_paramExamples' field.
    -- Furthermore, if referencing a schema that contains an example, the example value
    -- SHALL override the example provided by the schema. To represent examples of media types
    -- that cannot naturally be represented in JSON or YAML, a string value can contain
    -- the example with escaping where necessary.
  , _paramExample :: Maybe Value

    -- | Examples of the parameter's potential value.
    -- Each example SHOULD contain a value in the correct format as specified
    -- in the parameter encoding. The '_paramExamples' field is mutually exclusive of the '_paramExample' field.
    -- Furthermore, if referencing a schema that contains an example,
    -- the examples value SHALL override the example provided by the schema.
  , _paramExamples :: InsOrdHashMap Text (Referenced Example)

    -- TODO
    -- _paramContent :: InsOrdHashMap MediaType MediaTypeObject
    -- should be singleton. mutually exclusive with _paramSchema.
  } deriving (Eq, Show, Generic, Data, Typeable)

data Example = Example
  { -- | Short description for the example.
    _exampleSummary :: Maybe Text

    -- | Long description for the example.
    -- CommonMark syntax MAY be used for rich text representation.
  , _exampleDescription :: Maybe Text

    -- | Embedded literal example.
    -- The '_exampleValue' field and '_exampleExternalValue' field are mutually exclusive.
    --
    -- To represent examples of media types that cannot naturally represented in JSON or YAML,
    -- use a string value to contain the example, escaping where necessary.
  , _exampleValue :: Maybe Value

    -- | A URL that points to the literal example.
    -- This provides the capability to reference examples that cannot easily be included
    -- in JSON or YAML documents. The '_exampleValue' field
    -- and '_exampleExternalValue' field are mutually exclusive.
  , _exampleExternalValue :: Maybe URL
  } deriving (Eq, Show, Generic, Typeable, Data)

data ExpressionOrValue
  = Expression Text
  | Value Value
  deriving (Eq, Show, Generic, Typeable, Data)

-- | The Link object represents a possible design-time link for a response.
-- The presence of a link does not guarantee the caller's ability to successfully invoke it,
-- rather it provides a known relationship and traversal mechanism between responses and other operations.
data Link = Link
  { -- | A relative or absolute URI reference to an OAS operation.
    -- This field is mutually exclusive of the '_linkOperationId' field,
    -- and MUST point to an 'Operation' Object. Relative '_linkOperationRef'
    -- values MAY be used to locate an existing 'Operation' Object in the OpenAPI definition.
    _linkOperationRef :: Maybe Text

    -- | The name of an /existing/, resolvable OAS operation, as defined with a unique
    -- '_operationOperationId'. This field is mutually exclusive of the '_linkOperationRef' field.
  , _linkOperationId :: Maybe Text

    -- | A map representing parameters to pass to an operation as specified with '_linkOperationId'
    -- or identified via '_linkOperationRef'. The key is the parameter name to be used, whereas
    -- the value can be a constant or an expression to be evaluated and passed to the linked operation.
    -- The parameter name can be qualified using the parameter location @[{in}.]{name}@
    -- for operations that use the same parameter name in different locations (e.g. path.id).
  , _linkParameters :: InsOrdHashMap Text ExpressionOrValue

    -- | A literal value or @{expression}@ to use as a request body when calling the target operation.
  , _linkRequestBody :: Maybe ExpressionOrValue

    -- | A description of the link.
  , _linkDescription :: Maybe Text

    -- | A server object to be used by the target operation.
  , _linkServer :: Maybe Server
  } deriving (Eq, Show, Generic, Typeable, Data)

-- | Items for @'OpenApiArray'@ schemas.
--
-- __Warning__: OpenAPI 3.0 does not support tuple arrays. However, OpenAPI 3.1 will, as
-- it will incorporate Json Schema mostly verbatim.
--
-- @'OpenApiItemsObject'@ should be used to specify homogenous array @'Schema'@s.
--
-- @'OpenApiItemsArray'@ should be used to specify tuple @'Schema'@s.
data OpenApiItems where
  OpenApiItemsObject    :: Referenced Schema   -> OpenApiItems
  OpenApiItemsArray     :: [Referenced Schema] -> OpenApiItems
  deriving (Eq, Show, Typeable, Data)

data OpenApiType where
  OpenApiString   :: OpenApiType
  OpenApiNumber   :: OpenApiType
  OpenApiInteger  :: OpenApiType
  OpenApiBoolean  :: OpenApiType
  OpenApiArray    :: OpenApiType
  OpenApiNull     :: OpenApiType
  OpenApiObject   :: OpenApiType
  deriving (Eq, Show, Typeable, Generic, Data)

data ParamLocation
  = -- | Parameters that are appended to the URL.
    -- For example, in @/items?id=###@, the query parameter is @id@.
    ParamQuery
    -- | Custom headers that are expected as part of the request.
  | ParamHeader
    -- | Used together with Path Templating, where the parameter value is actually part of the operation's URL.
    -- This does not include the host or base path of the API.
    -- For example, in @/items/{itemId}@, the path parameter is @itemId@.
  | ParamPath
    -- | Used to pass a specific cookie value to the API.
  | ParamCookie
  deriving (Eq, Show, Generic, Data, Typeable)

type Format = Text

type ParamName = Text

data Schema = Schema
  { _schemaTitle :: Maybe Text
  , _schemaDescription :: Maybe Text
  , _schemaRequired :: [ParamName]

  , _schemaNullable :: Maybe Bool
  , _schemaAllOf :: Maybe [Referenced Schema]
  , _schemaOneOf :: Maybe [Referenced Schema]
  , _schemaNot :: Maybe (Referenced Schema)
  , _schemaAnyOf :: Maybe [Referenced Schema]
  , _schemaProperties :: InsOrdHashMap Text (Referenced Schema)
  , _schemaAdditionalProperties :: Maybe AdditionalProperties

  , _schemaDiscriminator :: Maybe Discriminator
  , _schemaReadOnly :: Maybe Bool
  , _schemaWriteOnly :: Maybe Bool
  , _schemaXml :: Maybe Xml
  , _schemaExternalDocs :: Maybe ExternalDocs
  , _schemaExample :: Maybe Value
  , _schemaDeprecated :: Maybe Bool

  , _schemaMaxProperties :: Maybe Integer
  , _schemaMinProperties :: Maybe Integer

  , -- | Declares the value of the parameter that the server will use if none is provided,
    -- for example a @"count"@ to control the number of results per page might default to @100@
    -- if not supplied by the client in the request.
    -- (Note: "default" has no meaning for required parameters.)
    -- Unlike JSON Schema this value MUST conform to the defined type for this parameter.
    _schemaDefault :: Maybe Value

  , _schemaType :: Maybe OpenApiType
  , _schemaFormat :: Maybe Format
  , _schemaItems :: Maybe OpenApiItems
  , _schemaMaximum :: Maybe Scientific
  , _schemaExclusiveMaximum :: Maybe Bool
  , _schemaMinimum :: Maybe Scientific
  , _schemaExclusiveMinimum :: Maybe Bool
  , _schemaMaxLength :: Maybe Integer
  , _schemaMinLength :: Maybe Integer
  , _schemaPattern :: Maybe Pattern
  , _schemaMaxItems :: Maybe Integer
  , _schemaMinItems :: Maybe Integer
  , _schemaUniqueItems :: Maybe Bool
  , _schemaEnum :: Maybe [Value]
  , _schemaMultipleOf :: Maybe Scientific
  } deriving (Eq, Show, Generic, Data, Typeable)

oring :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
oring f (Just x) (Just y) = Just (f x y)
oring _ (Just x) Nothing = Just x
oring _ Nothing (Just y) = Just y
oring _ Nothing Nothing = Nothing

illegal :: (HasCallStack, Show a) => String -> Maybe a -> Maybe a -> Maybe a
illegal _ (Just x) Nothing = Just x
illegal _ Nothing (Just y) = Just y
illegal m (Just x) (Just y) = error $ "illegal to combine " <> m <> ": " <> show x <> ", " <> show y
illegal _ Nothing Nothing = Nothing

-- this is an intersection.
instance Semigroup Schema where
    sc1 <> sc2 = Schema
        { _schemaTitle = _schemaTitle sc1 <> _schemaTitle sc2
        , _schemaDescription = _schemaDescription sc1 <> _schemaDescription sc2
        , _schemaRequired = _schemaRequired sc1 <> _schemaRequired sc2

        , _schemaNullable = oring (||) (_schemaNullable sc1) (_schemaNullable sc2)
        , _schemaAllOf = _schemaAllOf sc1 <> _schemaAllOf sc2
        , _schemaOneOf = _schemaOneOf sc1 <> _schemaOneOf sc2
        , _schemaNot = _schemaNot sc1 <|> _schemaNot sc2
        , _schemaAnyOf = _schemaAnyOf sc1 <> _schemaAnyOf sc2
        , _schemaProperties = _schemaProperties sc1 <> _schemaProperties sc2
        , _schemaAdditionalProperties = oring (<>) (_schemaAdditionalProperties sc1) (_schemaAdditionalProperties sc2)

        , _schemaDiscriminator = illegal "discriminator" (_schemaDiscriminator sc1) (_schemaDiscriminator sc2)
        , _schemaReadOnly = illegal "readOnly" (_schemaReadOnly sc1) (_schemaReadOnly sc2)
        , _schemaWriteOnly = illegal "writeOnly" (_schemaWriteOnly sc1) (_schemaWriteOnly sc2)
        , _schemaXml = illegal "xml" (_schemaXml sc1) (_schemaXml sc2)
        , _schemaExternalDocs = illegal "ExternalDocs" (_schemaExternalDocs sc1) (_schemaExternalDocs sc2)
        , _schemaExample = illegal "Example" (_schemaExample sc1) (_schemaExample sc2)
        , _schemaDeprecated = illegal "Deprecated" (_schemaDeprecated sc1) (_schemaDeprecated sc2)

        , _schemaMaxProperties = oring min (_schemaMaxProperties sc1) (_schemaMaxProperties sc2)
        , _schemaMinProperties = oring max (_schemaMinProperties sc1) (_schemaMinProperties sc2)

        , -- | Declares the value of the parameter that the server will use if none is provided,
            -- for example a @"count"@ to control the number of results per page might default to @100@
            -- if not supplied by the client in the request.
            -- (Note: "default" has no meaning for required parameters.)
            -- Unlike JSON Schema this value MUST conform to the defined type for this parameter.
          _schemaDefault = illegal "Default" (_schemaDefault sc1) (_schemaDefault sc2)

        , _schemaType = illegal "Type" (_schemaType sc1) (_schemaType sc2)
        , _schemaFormat = _schemaFormat sc1 <> _schemaFormat sc2
        , _schemaItems = illegal "Items" (_schemaItems sc1) (_schemaItems sc2)
        , _schemaMaximum = oring min (_schemaMaximum sc1) (_schemaMaximum sc2)
        , _schemaExclusiveMaximum = oring (||) (_schemaExclusiveMaximum sc1) (_schemaExclusiveMaximum sc2)
        , _schemaMinimum = oring max (_schemaMinimum sc1) (_schemaMinimum sc2)
        , _schemaExclusiveMinimum = oring (||) (_schemaExclusiveMinimum sc1) (_schemaExclusiveMinimum sc2)
        , _schemaMaxLength = oring min (_schemaMaxLength sc1) (_schemaMaxLength sc2)
        , _schemaMinLength = oring min (_schemaMinLength sc1) (_schemaMinLength sc2)
        , _schemaPattern = _schemaPattern sc1 <> _schemaPattern sc2
        , _schemaMaxItems = oring min (_schemaMaxItems sc1) (_schemaMaxItems sc2)
        , _schemaMinItems = oring max (_schemaMinItems sc1) (_schemaMinItems sc2)
        , _schemaUniqueItems = illegal "UniqueItems" (_schemaUniqueItems sc1) (_schemaUniqueItems sc2)
        , _schemaEnum = oring intersect (_schemaEnum sc1) (_schemaEnum sc2)
        , _schemaMultipleOf = illegal "MultipleOf" (_schemaMultipleOf sc1) (_schemaMultipleOf sc2)
        }

instance Monoid Schema where
    mempty = Schema
        { _schemaTitle = Nothing
        , _schemaDescription = Nothing
        , _schemaRequired = mempty

        , _schemaNullable = Nothing
        , _schemaAllOf = Nothing
        , _schemaOneOf = Nothing
        , _schemaNot = Nothing
        , _schemaAnyOf = Nothing
        , _schemaProperties = mempty
        , _schemaAdditionalProperties = Nothing

        , _schemaDiscriminator = Nothing
        , _schemaReadOnly = Nothing
        , _schemaWriteOnly = Nothing
        , _schemaXml = Nothing
        , _schemaExternalDocs = Nothing
        , _schemaExample = Nothing
        , _schemaDeprecated = Nothing

        , _schemaMaxProperties = Nothing
        , _schemaMinProperties = Nothing

        , -- | Declares the value of the parameter that the server will use if none is provided,
            -- for example a @"count"@ to control the number of results per page might default to @100@
            -- if not supplied by the client in the request.
            -- (Note: "default" has no meaning for required parameters.)
            -- Unlike JSON Schema this value MUST conform to the defined type for this parameter.
          _schemaDefault = Nothing

        , _schemaType = Nothing
        , _schemaFormat = Nothing
        , _schemaItems = Nothing
        , _schemaMaximum = Nothing
        , _schemaExclusiveMaximum = Nothing
        , _schemaMinimum = Nothing
        , _schemaExclusiveMinimum = Nothing
        , _schemaMaxLength = Nothing
        , _schemaMinLength = Nothing
        , _schemaPattern = Nothing
        , _schemaMaxItems = Nothing
        , _schemaMinItems = Nothing
        , _schemaUniqueItems = Nothing
        , _schemaEnum = Nothing
        , _schemaMultipleOf = Nothing
        }

-- | Regex pattern for @string@ type.
type Pattern = Text

data Discriminator = Discriminator
  { -- | The name of the property in the payload that will hold the discriminator value.
    _discriminatorPropertyName :: Text

    -- | An object to hold mappings between payload values and schema names or references.
  , _discriminatorMapping :: InsOrdHashMap Text Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | A @'Schema'@ with an optional name.
-- This name can be used in references.
data NamedSchema = NamedSchema
  { _namedSchemaName :: Maybe Text
  , _namedSchemaSchema :: Schema
  } deriving (Eq, Show, Generic, Data, Typeable)

data Xml = Xml
  { -- | Replaces the name of the element/attribute used for the described schema property.
    -- When defined within the @'OpenApiItems'@ (items), it will affect the name of the individual XML elements within the list.
    -- When defined alongside type being array (outside the items),
    -- it will affect the wrapping element and only if wrapped is true.
    -- If wrapped is false, it will be ignored.
    _xmlName :: Maybe Text

    -- | The URL of the namespace definition.
    -- Value SHOULD be in the form of a URL.
  , _xmlNamespace :: Maybe Text

    -- | The prefix to be used for the name.
  , _xmlPrefix :: Maybe Text

    -- | Declares whether the property definition translates to an attribute instead of an element.
    -- Default value is @False@.
  , _xmlAttribute :: Maybe Bool

    -- | MAY be used only for an array definition.
    -- Signifies whether the array is wrapped
    -- (for example, @\<books\>\<book/\>\<book/\>\</books\>@)
    -- or unwrapped (@\<book/\>\<book/\>@).
    -- Default value is @False@.
    -- The definition takes effect only when defined alongside type being array (outside the items).
  , _xmlWrapped :: Maybe Bool
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | A container for the expected responses of an operation.
-- The container maps a HTTP response code to the expected response.
-- It is not expected from the documentation to necessarily cover all possible HTTP response codes,
-- since they may not be known in advance.
-- However, it is expected from the documentation to cover a successful operation response and any known errors.
data Responses = Responses
  { -- | The documentation of responses other than the ones declared for specific HTTP response codes.
    -- It can be used to cover undeclared responses.
   _responsesDefault :: Maybe (Referenced Response)

    -- | Any HTTP status code can be used as the property name (one property per HTTP status code).
    -- Describes the expected response for those HTTP status codes.
  , _responsesResponses :: InsOrdHashMap HttpStatusCode (Referenced Response)
  } deriving (Eq, Show, Generic, Data, Typeable)

type HttpStatusCode = Int

-- | Describes a single response from an API Operation.
data Response = Response
  { -- | A short description of the response.
    -- [CommonMark syntax](https://spec.commonmark.org/) can be used for rich text representation.
    _responseDescription :: Text

    -- | A map containing descriptions of potential response payloads.
    -- The key is a media type or media type range and the value describes it.
    -- For responses that match multiple keys, only the most specific key is applicable.
    -- e.g. @text/plain@ overrides @text/\*@.
  , _responseContent :: InsOrdHashMap MediaType MediaTypeObject

    -- | Maps a header name to its definition.
  , _responseHeaders :: InsOrdHashMap HeaderName (Referenced Header)

    -- | A map of operations links that can be followed from the response.
    -- The key of the map is a short name for the link, following the naming
    -- constraints of the names for 'Component' Objects.
  , _responseLinks :: InsOrdHashMap Text (Referenced Link)
  } deriving (Eq, Show, Generic, Data, Typeable)

instance IsString Response where
  fromString s = Response (fromString s) mempty mempty mempty

-- | A map of possible out-of band callbacks related to the parent operation.
-- Each value in the map is a 'PathItem' Object that describes a set of requests that
-- may be initiated by the API provider and the expected responses.
-- The key value used to identify the path item object is an expression, evaluated at runtime,
-- that identifies a URL to use for the callback operation.
newtype Callback = Callback (InsOrdHashMap Text PathItem)
  deriving (Eq, Show, Generic, Data, Typeable)

type HeaderName = Text

-- | Header fields have the same meaning as for 'Param'.
--
-- Style is always treated as 'StyleSimple', as it is the only value allowed for headers.
data Header = Header
  { -- | A short description of the header.
    _headerDescription :: Maybe HeaderName

  , _headerRequired :: Maybe Bool
  , _headerDeprecated :: Maybe Bool
  , _headerAllowEmptyValue :: Maybe Bool
  , _headerExplode :: Maybe Bool
  , _headerExample :: Maybe Value
  , _headerExamples :: InsOrdHashMap Text (Referenced Example)

  , _headerSchema :: Maybe (Referenced Schema)
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The location of the API key.
data ApiKeyLocation
  = ApiKeyQuery
  | ApiKeyHeader
  | ApiKeyCookie
  deriving (Eq, Show, Generic, Data, Typeable)

data ApiKeyParams = ApiKeyParams
  { -- | The name of the header or query parameter to be used.
    _apiKeyName :: Text

    -- | The location of the API key.
  , _apiKeyIn :: ApiKeyLocation
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The authorization URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type AuthorizationURL = Text

-- | The token URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type TokenURL = Text

newtype OAuth2ImplicitFlow
  = OAuth2ImplicitFlow {_oAuth2ImplicitFlowAuthorizationUrl :: AuthorizationURL}
  deriving (Eq, Show, Generic, Data, Typeable)

newtype OAuth2PasswordFlow
  = OAuth2PasswordFlow {_oAuth2PasswordFlowTokenUrl :: TokenURL}
  deriving (Eq, Show, Generic, Data, Typeable)

newtype OAuth2ClientCredentialsFlow
  = OAuth2ClientCredentialsFlow {_oAuth2ClientCredentialsFlowTokenUrl :: TokenURL}
  deriving (Eq, Show, Generic, Data, Typeable)

data OAuth2AuthorizationCodeFlow = OAuth2AuthorizationCodeFlow
  { _oAuth2AuthorizationCodeFlowAuthorizationUrl :: AuthorizationURL
  , _oAuth2AuthorizationCodeFlowTokenUrl :: TokenURL
  } deriving (Eq, Show, Generic, Data, Typeable)

data OAuth2Flow p = OAuth2Flow
  { _oAuth2Params :: p

    -- | The URL to be used for obtaining refresh tokens.
  , _oAath2RefreshUrl :: Maybe URL

    -- | The available scopes for the OAuth2 security scheme.
    -- A map between the scope name and a short description for it.
    -- The map MAY be empty.
  , _oAuth2Scopes :: InsOrdHashMap Text Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data OAuth2Flows = OAuth2Flows
  { -- | Configuration for the OAuth Implicit flow
    _oAuth2FlowsImplicit :: Maybe (OAuth2Flow OAuth2ImplicitFlow)

    -- | Configuration for the OAuth Resource Owner Password flow
  , _oAuth2FlowsPassword :: Maybe (OAuth2Flow OAuth2PasswordFlow)

    -- | Configuration for the OAuth Client Credentials flow
  , _oAuth2FlowsClientCredentials :: Maybe (OAuth2Flow OAuth2ClientCredentialsFlow)

    -- | Configuration for the OAuth Authorization Code flow
  , _oAuth2FlowsAuthorizationCode :: Maybe (OAuth2Flow OAuth2AuthorizationCodeFlow)
  } deriving (Eq, Show, Generic, Data, Typeable)

type BearerFormat = Text

data HttpSchemeType
  = HttpSchemeBearer (Maybe BearerFormat)
  | HttpSchemeBasic
  | HttpSchemeCustom Text
  deriving (Eq, Show, Generic, Data, Typeable)

-- |
--
-- >>> BSL.putStrLn $ encodePretty (SecuritySchemeHttp (HttpSchemeBearer Nothing))
-- {
--     "scheme": "bearer",
--     "type": "http"
-- }
--
-- >>> BSL.putStrLn $ encodePretty (SecuritySchemeHttp (HttpSchemeBearer (Just "jwt")))
-- {
--     "bearerFormat": "jwt",
--     "scheme": "bearer",
--     "type": "http"
-- }
--
-- >>> BSL.putStrLn $ encodePretty (SecuritySchemeHttp HttpSchemeBasic)
-- {
--     "scheme": "basic",
--     "type": "http"
-- }
--
-- >>> BSL.putStrLn $ encodePretty (SecuritySchemeHttp (HttpSchemeCustom "CANARY"))
-- {
--     "scheme": "CANARY",
--     "type": "http"
-- }
--
-- >>> BSL.putStrLn $ encodePretty (SecuritySchemeApiKey (ApiKeyParams "id" ApiKeyCookie))
-- {
--     "in": "cookie",
--     "name": "id",
--     "type": "apiKey"
-- }
--
data SecuritySchemeType
  = SecuritySchemeHttp HttpSchemeType
  | SecuritySchemeApiKey ApiKeyParams
  | SecuritySchemeOAuth2 OAuth2Flows
  | SecuritySchemeOpenIdConnect URL
  deriving (Eq, Show, Generic, Data, Typeable)

data SecurityScheme = SecurityScheme
  { -- | The type of the security scheme.
    _securitySchemeType :: SecuritySchemeType

    -- | A short description for security scheme.
  , _securitySchemeDescription :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

newtype SecurityDefinitions
  = SecurityDefinitions (Definitions SecurityScheme)
  deriving (Eq, Show, Generic, Data, Typeable)

-- | Lists the required security schemes to execute this operation.
-- The object can have multiple security schemes declared in it which are all required
-- (that is, there is a logical AND between the schemes).
newtype SecurityRequirement = SecurityRequirement
  { getSecurityRequirement :: InsOrdHashMap Text [Text]
  } deriving (Eq, Read, Show, Semigroup, Monoid, ToJSON, FromJSON, Data, Typeable)

-- | Tag name.
type TagName = Text

-- | Allows adding meta data to a single tag that is used by @Operation@.
-- It is not mandatory to have a @Tag@ per tag used there.
data Tag = Tag
  { -- | The name of the tag.
    _tagName :: TagName

    -- | A short description for the tag.
    -- [CommonMark syntax](https://spec.commonmark.org/) MAY be used for rich text representation.
  , _tagDescription :: Maybe Text

    -- | Additional external documentation for this tag.
  , _tagExternalDocs :: Maybe ExternalDocs
  } deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance Hashable Tag

instance IsString Tag where
  fromString s = Tag (fromString s) Nothing Nothing

-- | Allows referencing an external resource for extended documentation.
data ExternalDocs = ExternalDocs
  { -- | A short description of the target documentation.
    -- [CommonMark syntax](https://spec.commonmark.org/) MAY be used for rich text representation.
    _externalDocsDescription :: Maybe Text

    -- | The URL for the target documentation.
  , _externalDocsUrl :: URL
  } deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance Hashable ExternalDocs

-- | A simple object to allow referencing other definitions in the specification.
-- It can be used to reference parameters and responses that are defined at the top level for reuse.
newtype Reference = Reference { getReference :: Text }
  deriving (Eq, Show, Data, Typeable)

data Referenced a
  = Ref Reference
  | Inline a
  deriving (Eq, Show, Functor, Data, Typeable)

instance IsString a => IsString (Referenced a) where
  fromString = Inline . fromString

newtype URL = URL { getUrl :: Text } deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, Data, Typeable)

data AdditionalProperties
  = AdditionalPropertiesAllowed Bool
  | AdditionalPropertiesSchema (Referenced Schema)
  deriving (Eq, Show, Data, Typeable)

instance Semigroup AdditionalProperties where
    AdditionalPropertiesAllowed True <> AdditionalPropertiesAllowed True = AdditionalPropertiesAllowed True
    AdditionalPropertiesAllowed False <> AdditionalPropertiesAllowed True = error "illegal: AdditionalPropertiesAllowed False <> AdditionalPropertiesAllowed True"
    AdditionalPropertiesAllowed True <> AdditionalPropertiesAllowed False = error "illegal: AdditionalPropertiesAllowed True <> AdditionalPropertiesAllowed False"
    AdditionalPropertiesAllowed False <> AdditionalPropertiesAllowed False = AdditionalPropertiesAllowed False
    AdditionalPropertiesAllowed True <> AdditionalPropertiesSchema s = AdditionalPropertiesSchema s
    AdditionalPropertiesSchema s <> AdditionalPropertiesAllowed True = AdditionalPropertiesSchema s
    AdditionalPropertiesSchema _ <> AdditionalPropertiesSchema _ = error "illegal: AdditionalPropertiesSchema s1 <> AdditionalPropertiesSchema s2"
    AdditionalPropertiesAllowed False <> AdditionalPropertiesSchema _ = error "illegal: AdditionalPropertiesAllowed False <> AdditionalPropertiesSchema s"
    AdditionalPropertiesSchema _ <> AdditionalPropertiesAllowed False = error "illegal: AdditionalPropertiesSchema s <> AdditionalPropertiesAllowed False"

makeLensesWith swaggerFieldRules ''Responses
makeLensesWith swaggerFieldRules ''Schema
makeLensesWith swaggerFieldRules ''PathItem
makeLensesWith swaggerFieldRules ''Param
makeLensesWith swaggerFieldRules ''SecurityScheme --
makeFields ''OpenApi
makeFields ''Components
makeFields ''Server
makeFields ''RequestBody
makeFields ''MediaTypeObject
makeFields ''Info
makeFields ''Contact
makeFields ''License
makeFields ''Tag
makeFields ''Operation
makeFields ''Header
makeFields ''NamedSchema
makeFields ''Xml
makeFields ''Response
makeFields ''ApiKeyParams
makeFields ''OAuth2ImplicitFlow
makeFields ''OAuth2PasswordFlow
makeFields ''OAuth2ClientCredentialsFlow
makeFields ''OAuth2AuthorizationCodeFlow
makeFields ''OAuth2Flow
makeFields ''OAuth2Flows
makeFields ''ExternalDocs
makeFields ''Encoding
makeFields ''Example
makeFields ''Discriminator
makeFields ''Link

type instance Index Responses = HttpStatusCode
type instance Index Operation = HttpStatusCode

type instance IxValue Responses = Referenced Response
type instance IxValue Operation = Referenced Response

instance Ixed Responses where ix n = responses . ix n
instance At   Responses where at n = responses . at n

instance Ixed Operation where ix n = responses . ix n
instance At   Operation where at n = responses . at n

dereference :: Definitions a -> Referenced a -> a
dereference _ (Inline a)               = a
dereference defs (Ref (Reference ref)) = fromJust $ InsOrdHashMap.lookup ref defs

-- | Validate JSON @'Value'@ against Swagger @'Schema'@.
--
-- prop> validateJSON mempty (toSchema (Proxy :: Proxy Int)) (toJSON (x :: Int)) == []
--
-- /NOTE:/ @'validateJSON'@ does not perform string pattern validation.
-- See @'validateJSONWithPatternChecker'@.
validateJSON :: Definitions Schema -> Schema -> Value -> [ValidationError]
validateJSON = validateJSONWithPatternChecker (\_pattern _str -> True)

-- | Validate JSON @'Value'@ agains Swagger @'ToSchema'@ for a given value and pattern checker.
--
-- For validation without patterns see @'validateJSON'@.
validateJSONWithPatternChecker :: (Pattern -> Text -> Bool) -> Definitions Schema -> Schema -> Value -> [ValidationError]
validateJSONWithPatternChecker checker defs sch js =
  case runValidation (validateWithSchema js) cfg sch of
    Failed xs -> xs
    Passed _  -> mempty
  where
    cfg = defaultConfig
            { configPatternChecker = checker
            , configDefinitions = defs }

-- | Validation error message.
type ValidationError = String

-- | Validation result type.
data Result a
  = Failed [ValidationError]  -- ^ Validation failed with a list of error messages.
  | Passed a                  -- ^ Validation passed.
  deriving (Eq, Show, Functor)

instance Applicative Result where
  pure = Passed
  Passed f <*> Passed x = Passed (f x)
  Failed xs <*> Failed ys = Failed (xs <> ys)
  Failed xs <*> _ = Failed xs
  _ <*> Failed ys = Failed ys

instance Alternative Result where
  empty = Failed mempty
  Passed x <|> _ = Passed x
  _        <|> y = y

instance Monad Result where
  return = pure
  Passed x >>=  f = f x
  Failed xs >>= _ = Failed xs

-- | Validation configuration.
data Config = Config
  { -- | Pattern checker for @'_schemaPattern'@ validation.
    configPatternChecker :: Pattern -> Text -> Bool
    -- | Schema definitions in scope to resolve references.
  , configDefinitions    :: Definitions Schema
  }

-- | Default @'Config'@:
--
-- @
-- defaultConfig = 'Config'
--   { 'configPatternChecker' = \\_pattern _str -> True
--   , 'configDefinitions'    = mempty
--   }
-- @
defaultConfig :: Config
defaultConfig = Config
  { configPatternChecker = \_pattern _str -> True
  , configDefinitions    = mempty
  }

-- | Value validation.
newtype Validation s a = Validation { runValidation :: Config -> s -> Result a }
  deriving (Functor)

instance Applicative (Validation schema) where
  pure x = Validation (\_ _ -> pure x)
  Validation f <*> Validation x = Validation (\c s -> f c s <*> x c s)

instance Alternative (Validation schema) where
  empty = Validation (\_ _ -> empty)
  Validation x <|> Validation y = Validation (\c s -> x c s <|> y c s)

instance Profunctor Validation where
  dimap f g (Validation k) = Validation (\c s -> fmap g (k c (f s)))

instance Choice Validation where
  left'  (Validation g) = Validation (\c -> either (fmap Left . g c) (pure . Right))
  right' (Validation g) = Validation (\c -> either (pure . Left) (fmap Right . g c))

instance Monad (Validation s) where
  return = pure
  Validation x >>= f = Validation (\c s -> x c s >>= \y -> runValidation (f y) c s)
  (>>) = (*>)

withConfig :: (Config -> Validation s a) -> Validation s a
withConfig f = Validation (\c -> runValidation (f c) c)

withSchema :: (s -> Validation s a) -> Validation s a
withSchema f = Validation (\c s -> runValidation (f s) c s)

-- | Issue an error message.
invalid :: String -> Validation schema a
invalid msg = Validation (\_ _ -> Failed [msg])

-- | Validation passed.
valid :: Validation schema ()
valid = pure ()

-- | Validate schema's property given a lens into that property
-- and property checker.
checkMissing :: Validation s () -> Lens' s (Maybe a) -> (a -> Validation s ()) -> Validation s ()
checkMissing missing l g = withSchema $ \sch ->
  case sch ^. l of
    Nothing -> missing
    Just x  -> g x

-- | Validate schema's property given a lens into that property
-- and property checker.
-- If property is missing in schema, consider it valid.
check :: Lens' s (Maybe a) -> (a -> Validation s ()) -> Validation s ()
check = checkMissing valid

-- | Validate same value with different schema.
sub :: t -> Validation t a -> Validation s a
sub = lmap . const

-- | Validate same value with a part of the original schema.
sub_ :: Getting a s a -> Validation a r -> Validation s r
sub_ = lmap . view

-- | Validate value against a schema given schema reference and validation function.
withRef :: Reference -> (Schema -> Validation s a) -> Validation s a
withRef (Reference ref) f = withConfig $ \cfg ->
  case InsOrdHashMap.lookup ref (configDefinitions cfg) of
    Nothing -> invalid $ "unknown schema " ++ show ref
    Just s  -> f s

validateWithSchemaRef :: Referenced Schema -> Value -> Validation s ()
validateWithSchemaRef (Ref ref)  js = withRef ref $ \sch -> sub sch (validateWithSchema js)
validateWithSchemaRef (Inline s) js = sub s (validateWithSchema js)

-- | Validate JSON @'Value'@ with Swagger @'Schema'@.
validateWithSchema :: Value -> Validation Schema ()
validateWithSchema val = do
  validateSchemaType val
  validateEnum val

validateInteger :: Scientific -> Validation Schema ()
validateInteger n = do
  when (not (isInteger n)) $
    invalid ("not an integer")
  validateNumber n

validateNumber :: Scientific -> Validation Schema ()
validateNumber n = withConfig $ \_cfg -> withSchema $ \sch -> do
  let exMax = Just True == sch ^. exclusiveMaximum
      exMin = Just True == sch ^. exclusiveMinimum

  check maximum_ $ \m ->
    when (if exMax then (n >= m) else (n > m)) $
      invalid ("value " ++ show n ++ " exceeds maximum (should be " ++ (if exMax then "<" else "<=") ++ show m ++ ")")

  check minimum_ $ \m ->
    when (if exMin then (n <= m) else (n < m)) $
      invalid ("value " ++ show n ++ " falls below minimum (should be " ++ (if exMin then ">" else ">=") ++ show m ++ ")")

  check multipleOf $ \k ->
    when (not (isInteger (n / k))) $
      invalid ("expected a multiple of " ++ show k ++ " but got " ++ show n)

validateString :: Text -> Validation Schema ()
validateString s = do
  check maxLength $ \n ->
    when (len > fromInteger n) $
      invalid ("string is too long (length should be <=" ++ show n ++ ")")

  check minLength $ \n ->
    when (len < fromInteger n) $
      invalid ("string is too short (length should be >=" ++ show n ++ ")")

  check pattern $ \regex -> do
    withConfig $ \cfg -> do
      when (not (configPatternChecker cfg regex s)) $
        invalid ("string does not match pattern " ++ show regex)

  check format $ \f ->
    let
      maybeRegex = case f of
        "hostname" -> Just "^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9]))*$"
        "ipv4" -> Just "^((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\\.){3}(25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)$"
        "ipv6" -> Just "(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"
        _ -> Nothing
    in withConfig $ \cfg ->
      case maybeRegex of
        Nothing -> Debug.trace ("unknown format " <> Text.unpack f) (pure ())
        Just formatRegex ->
          when (not (configPatternChecker cfg formatRegex s)) $
            invalid ("string does not match format " <> Text.unpack f)

  where
    len = Text.length s

validateArray :: Vector Value -> Validation Schema ()
validateArray xs = do
  check maxItems $ \n ->
    when (len > fromInteger n) $
      invalid ("array exceeds maximum size (should be <=" ++ show n ++ ")")

  check minItems $ \n ->
    when (len < fromInteger n) $
      invalid ("array is too short (size should be >=" ++ show n ++ ")")

  check items $ \case
    OpenApiItemsObject itemSchema      -> traverse_ (validateWithSchemaRef itemSchema) xs
    OpenApiItemsArray itemSchemas -> do
      when (len /= length itemSchemas) $
        invalid ("array size is invalid (should be exactly " ++ show (length itemSchemas) ++ ")")
      sequenceA_ (zipWith validateWithSchemaRef itemSchemas (Vector.toList xs))

  check uniqueItems $ \unique ->
    when (unique && not allUnique) $
      invalid ("array is expected to contain unique items, but it does not")
  where
    len = Vector.length xs
    allUnique = len == InsOrdHashSet.size (InsOrdHashSet.fromList (Vector.toList xs))

validateObject ::
#if MIN_VERSION_aeson(2,0,0)
  KeyMap.KeyMap Value
#else
  HashMap Text Value
#endif
  -> Validation Schema ()
validateObject o = withSchema $ \sch ->
  case sch ^. discriminator of
    Just (Discriminator pname types) -> case fromJSON <$> lookupKey pname o of
      Just (JSON.Success pvalue) ->
        let ref = fromMaybe pvalue $ InsOrdHashMap.lookup pvalue types
        -- TODO ref may be name or reference
        in validateWithSchemaRef (Ref (Reference ref)) (Object o)
      Just (JSON.Error msg)   -> invalid ("failed to parse discriminator property " ++ show pname ++ ": " ++ show msg)
      Nothing            -> invalid ("discriminator property " ++ show pname ++ "is missing")
    Nothing -> do
      check maxProperties $ \n ->
        when (size > n) $
          invalid ("object size exceeds maximum (total number of properties should be <=" ++ show n ++ ")")

      check minProperties $ \n ->
        when (size < n) $
          invalid ("object size is too small (total number of properties should be >=" ++ show n ++ ")")

      validateRequired
      validateProps
  where
    size = fromIntegral (length o)

    validateRequired = withSchema $ \sch -> traverse_ validateReq (sch ^. required)
    validateReq n =
      when (not (hasKey n o)) $
        invalid ("property " ++ show n ++ " is required, but not found in " ++ show (encode o))

    validateProps = withSchema $ \sch -> do
      for_ (objectToList o) $ \(keyToText -> k, v) ->
        case v of
          Null | not (k `elem` (sch ^. required)) -> valid  -- null is fine for non-required property
          _ ->
            case InsOrdHashMap.lookup k (sch ^. properties) of
              Nothing -> checkMissing (unknownProperty k) additionalProperties $ validateAdditional k v
              Just s  -> validateWithSchemaRef s v

    validateAdditional _ _ (AdditionalPropertiesAllowed True) = valid
    validateAdditional k _ (AdditionalPropertiesAllowed False) = invalid $ "additionalProperties=false but extra property " <> show k <> " found"
    validateAdditional _ v (AdditionalPropertiesSchema s) = validateWithSchemaRef s v

    unknownProperty :: Text -> Validation s a
    unknownProperty pname = invalid $
      "property " <> show pname <> " is found in JSON value, but it is not mentioned in Swagger schema"

validateEnum :: Value -> Validation Schema ()
validateEnum val = do
  check enum_ $ \xs ->
    when (val `notElem` xs) $
      invalid ("expected one of " ++ show (encode xs) ++ " but got " ++ show val)

-- | Infer schema type based on used properties.
--
-- This is like 'inferParamSchemaTypes', but also works for objects:
--
-- >>> inferSchemaTypes <$> decode "{\"minProperties\": 1}"
-- Just [OpenApiObject]
inferSchemaTypes :: Schema -> [OpenApiType]
inferSchemaTypes sch = inferParamSchemaTypes sch ++
  [ OpenApiObject | any ($ sch)
       [ has (additionalProperties._Just)
       , has (maxProperties._Just)
       , has (minProperties._Just)
       , has (properties.folded)
       , has (required.folded) ] ]

-- | Infer schema type based on used properties.
--
-- >>> inferSchemaTypes <$> decode "{\"minLength\": 2}"
-- Just [OpenApiString]
--
-- >>> inferSchemaTypes <$> decode "{\"maxItems\": 0}"
-- Just [OpenApiArray]
--
-- From numeric properties 'OpenApiInteger' type is inferred.
-- If you want 'OpenApiNumber' instead, you must specify it explicitly.
--
-- >>> inferSchemaTypes <$> decode "{\"minimum\": 1}"
-- Just [OpenApiInteger]
inferParamSchemaTypes :: Schema -> [OpenApiType]
inferParamSchemaTypes sch = concat
  [ [ OpenApiArray | any ($ sch)
        [ has (items._Just)
        , has (maxItems._Just)
        , has (minItems._Just)
        , has (uniqueItems._Just) ] ]
  , [ OpenApiInteger | any ($ sch)
        [ has (exclusiveMaximum._Just)
        , has (exclusiveMinimum._Just)
        , has (maximum_._Just)
        , has (minimum_._Just)
        , has (multipleOf._Just) ] ]
  , [ OpenApiString | any ($ sch)
        [ has (maxLength._Just)
        , has (minLength._Just)
        , has (pattern._Just) ] ]
  ]

validateSchemaType :: Value -> Validation Schema ()
validateSchemaType val = withSchema $ \sch ->
  case sch of
    (view oneOf -> Just variants) -> do
      res <- forM variants $ \var ->
        (True <$ validateWithSchemaRef var val) <|> (return False)
      case length $ filter id res of
        0 -> invalid $ "Value not valid under any of 'oneOf' schemas: " ++ show val
        1 -> valid
        _ -> invalid $ "Value matches more than one of 'oneOf' schemas: " ++ show val
    (view anyOf -> Just variants) -> do
      res <- forM variants $ \var ->
        (True <$ validateWithSchemaRef var val) <|> (return False)
      case length $ filter id res of
        0 -> invalid $ "Value not valid under any of 'anyOf' schemas: " ++ show val
        _ -> valid
    (view allOf -> Just variants) -> do
      subSchemas <- forM variants $ \case
        Ref ref  -> withRef ref pure
        Inline s -> pure s

      sub (mconcat subSchemas) $ validateWithSchema val

    _ ->
      case (sch ^. type_, val) of
        (Just OpenApiNull,    Null)       -> valid
        (Just OpenApiBoolean, Bool _)     -> valid
        (Just OpenApiInteger, Number n)   -> validateInteger n
        (Just OpenApiNumber,  Number n)   -> validateNumber n
        (Just OpenApiString,  String s)   -> validateString s
        (Just OpenApiArray,   Array xs)   -> validateArray xs
        (Just OpenApiObject,  Object o)   -> validateObject o
        (Nothing, Null)                   -> valid
        (Nothing, Bool _)                 -> valid
        -- Number by default
        (Nothing, Number n)               -> validateNumber n
        (Nothing, String s)               -> validateString s
        (Nothing, Array xs)               -> validateArray xs
        (Nothing, Object o)               -> validateObject o
        bad -> invalid $ "expected JSON value of type " ++ showType bad

validateParamSchemaType :: Value -> Validation Schema ()
validateParamSchemaType val = withSchema $ \sch ->
  case (sch ^. type_, val) of
    (Just OpenApiBoolean, Bool _)     -> valid
    (Just OpenApiInteger, Number n)   -> validateInteger n
    (Just OpenApiNumber,  Number n)   -> validateNumber n
    (Just OpenApiString,  String s)   -> validateString s
    (Just OpenApiArray,   Array xs)   -> validateArray xs
    (Nothing, Bool _)                 -> valid
    -- Number by default
    (Nothing, Number n)               -> validateNumber n
    (Nothing, String s)               -> validateString s
    (Nothing, Array xs)               -> validateArray xs
    bad -> invalid $ "expected JSON value of type " ++ showType bad

showType :: (Maybe OpenApiType, Value) -> String
showType (Just ty, _)        = show ty
showType (Nothing, Null)     = "OpenApiNull"
showType (Nothing, Bool _)   = "OpenApiBoolean"
showType (Nothing, Number _) = "OpenApiNumber"
showType (Nothing, String _) = "OpenApiString"
showType (Nothing, Array _)  = "OpenApiArray"
showType (Nothing, Object _) = "OpenApiObject"

-- =======================================================================
-- Simple Generic-based FromJSON instances
-- =======================================================================

instance FromJSON Style where
  parseJSON = genericParseJSON (jsonPrefix "Style")

instance FromJSON OpenApiType where
  parseJSON = genericParseJSON (jsonPrefix "Swagger")

instance FromJSON ParamLocation where
  parseJSON = genericParseJSON (jsonPrefix "Param")

instance FromJSON Info where
  parseJSON = genericParseJSON (jsonPrefix "Info")

instance FromJSON Contact where
  parseJSON = genericParseJSON (jsonPrefix "Contact")

instance FromJSON License where
  parseJSON = genericParseJSON (jsonPrefix "License")

instance FromJSON ServerVariable where
  parseJSON = genericParseJSON (jsonPrefix "ServerVariable")

instance FromJSON ApiKeyLocation where
  parseJSON = genericParseJSON (jsonPrefix "ApiKey")

instance FromJSON ApiKeyParams where
  parseJSON = genericParseJSON (jsonPrefix "apiKey")

instance FromJSON Tag where
  parseJSON = genericParseJSON (jsonPrefix "Tag")

instance FromJSON ExternalDocs where
  parseJSON = genericParseJSON (jsonPrefix "ExternalDocs")

instance FromJSON Discriminator where
  parseJSON = genericParseJSON (jsonPrefix "Discriminator")

instance FromJSON OAuth2ImplicitFlow where
  parseJSON = genericParseJSON (jsonPrefix "OAuth2ImplicitFlow")

instance FromJSON OAuth2PasswordFlow where
  parseJSON = genericParseJSON (jsonPrefix "OAuth2PasswordFlow")

instance FromJSON OAuth2ClientCredentialsFlow where
  parseJSON = genericParseJSON (jsonPrefix "OAuth2ClientCredentialsFlow")

instance FromJSON OAuth2AuthorizationCodeFlow where
  parseJSON = genericParseJSON (jsonPrefix "OAuth2AuthorizationCodeFlow")

-- =======================================================================
-- Manual ToJSON instances
-- =======================================================================

instance ToJSON MediaType where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

instance ToJSONKey MediaType where
  toJSONKey = JSON.toJSONKeyText (Text.pack . show)

-- =======================================================================
-- Manual FromJSON instances
-- =======================================================================

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \str ->
    maybe (fail $ "Invalid media type literal " <> Text.unpack str) pure $ parseAccept $ encodeUtf8 str

instance FromJSONKey MediaType where
  fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

instance (Eq p, FromJSON p) => FromJSON (OAuth2Flow p) where
  parseJSON = pure undefined

instance FromJSON OAuth2Flows where
  parseJSON = pure undefined

instance FromJSON SecuritySchemeType where
  parseJSON js@(Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "http"   -> do
          scheme <-  o .: "scheme"
          SecuritySchemeHttp <$> case scheme of
              "bearer" -> HttpSchemeBearer <$> (o .:! "bearerFormat")
              "basic" -> pure HttpSchemeBasic
              t -> pure $ HttpSchemeCustom t
      "apiKey" -> SecuritySchemeApiKey <$> parseJSON js
      "oauth2" -> SecuritySchemeOAuth2 <$> (o .: "flows")
      "openIdConnect" -> SecuritySchemeOpenIdConnect <$> (o .: "openIdConnectUrl")
      _ -> empty
  parseJSON _ = empty

instance FromJSON OpenApiItems where
  parseJSON js@(Object obj)
      | null obj  = pure $ OpenApiItemsArray [] -- Nullary schema.
      | otherwise = OpenApiItemsObject <$> parseJSON js
  parseJSON js@(Array _)  = OpenApiItemsArray  <$> parseJSON js
  parseJSON _ = empty

instance FromJSON MimeList where
  parseJSON js = MimeList . map fromString <$> parseJSON js

instance FromJSON Responses where
  parseJSON (Object o) = Responses
    <$> o .:? "default"
    <*> parseJSON (Object (deleteKey "default" o))
  parseJSON _ = empty

instance FromJSON SecurityDefinitions where
  parseJSON js = SecurityDefinitions <$> parseJSON js

instance FromJSON OpenApi where
    parseJSON = withObject "OpenApi" $ \o -> OpenApi
        <$> o .: "info"
        <*> o .:? "servers" .!= mempty
        <*> o .: "paths"
        <*> o .: "components"
        <*> o .:? "security" .!= mempty
        <*> o .:? "tags" .!= mempty
        <*> o .:? "externalDocs"

instance FromJSON Operation where
    parseJSON = withObject "Operation" $ \o -> Operation
        <$> o .:? "tags" .!= mempty
        <*> o .:? "summary"
        <*> o .:? "description"
        <*> o .:? "externalDocs"
        <*> o .:? "id"
        <*> o .:? "parameters" .!= mempty
        <*> o .:? "requestBody"
        <*> o .: "responses"
        <*> o .:? "callbacks" .!= mempty
        <*> o .:? "deprecated"
        <*> o .:? "security" .!= mempty
        <*> o .:? "servers" .!= mempty

instance FromJSON Schema where
    parseJSON = withObject "Schema" $ \o -> Schema
        <$> o .:? "title"
        <*> o .:? "description"
        <*> o .:? "required" .!= mempty
        <*> o .:? "nullable"
        <*> o .:? "allOf"
        <*> o .:? "oneOf"
        <*> o .:? "not"
        <*> o .:? "anyOf"
        <*> o .:? "properties" .!= mempty
        <*> o .:? "additionalProperties"

        <*> o .:? "discriminator"
        <*> o .:? "readOnly"
        <*> o .:? "writeOnly"
        <*> o .:? "xml"
        <*> o .:? "externalDocs"
        <*> o .:? "example"
        <*> o .:? "deprecated"

        <*> o .:? "maxProperties"
        <*> o .:? "minProperties"

        <*> o .:? "default"

        <*> o .:? "type"
        <*> o .:? "format"
        <*> o .:? "items"
        <*> o .:? "maximum"
        <*> o .:? "exclusiveMaximum"
        <*> o .:? "minimum"
        <*> o .:? "exclusiveMinimum"
        <*> o .:? "maxLength"
        <*> o .:? "minLength"
        <*> o .:? "pattern"
        <*> o .:? "maxItems"
        <*> o .:? "minItems"
        <*> o .:? "uniqueItems"
        <*> o .:? "enum"
        <*> o .:? "multipleOf"

instance FromJSON MediaTypeObject where
    parseJSON = withObject "MediaTypeObject" $ \o -> MediaTypeObject
        <$> o .:? "schema"
        <*> o .:? "example"
        <*> o .:? "examples" .!= mempty
        <*> o .:? "encoding" .!= mempty

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> Response
        <$> o .: "description"
        <*> o .:? "content" .!= mempty
        <*> o .:? "headers" .!= mempty
        <*> o .:? "links" .!= mempty

instance FromJSON PathItem where
    parseJSON = withObject "PathItem" $ \o -> PathItem
        <$> o .:? "summary"
        <*> o .:? "description"
        <*> o .:? "get"
        <*> o .:? "put"
        <*> o .:? "post"
        <*> o .:? "delete"
        <*> o .:? "options"
        <*> o .:? "head"
        <*> o .:? "patch"
        <*> o .:? "trace"
        <*> o .:? "servers" .!= mempty
        <*> o .:? "parameters" .!= mempty

instance FromJSON Param where
    parseJSON = withObject "Param" $ \o -> Param
        <$> o .: "name"
        <*> o .:? "description"
        <*> o .:? "required"
        <*> o .:? "deprecated"
        <*> o .: "in"
        <*> o .:? "allowEmptyValue"
        <*> o .:? "allowReserved"
        <*> o .:? "schema"
        <*> o .:? "style"
        <*> o .:? "explode"
        <*> o .:? "example"
        <*> o .:? "examples" .!= mempty

instance FromJSON Components where
    parseJSON = withObject "Components" $ \o -> Components
        <$> o .:? "schemas" .!= mempty
        <*> o .:? "responses" .!= mempty
        <*> o .:? "parameters" .!= mempty
        <*> o .:? "examples" .!= mempty
        <*> o .:? "requestBodies" .!= mempty
        <*> o .:? "headers" .!= mempty
        <*> o .:? "securitySchemes" .!= SecurityDefinitions mempty
        <*> o .:? "links" .!= mempty
        <*> o .:? "callbacks" .!= mempty

fold <$> sequence
    [ deriveFromJSON defaultOptions { fieldLabelModifier = over _head toLower . drop (length ("_server" :: String)) } ''Server
    , deriveFromJSON defaultOptions { fieldLabelModifier = over _head toLower . drop (length ("_securityScheme" :: String)) } ''SecurityScheme
    , deriveFromJSON defaultOptions { fieldLabelModifier = over _head toLower . drop (length ("_header" :: String)) } ''Header
    , deriveFromJSON defaultOptions { fieldLabelModifier = over _head toLower . drop (length ("_example" :: String)) } ''Example
    , deriveFromJSON defaultOptions { fieldLabelModifier = over _head toLower . drop (length ("_requestBody" :: String)) } ''RequestBody
    , deriveFromJSON defaultOptions { fieldLabelModifier = over _head toLower . drop (length ("_link" :: String)) } ''Link
    , deriveFromJSON defaultOptions { fieldLabelModifier = over _head toLower . drop (length ("_encoding" :: String)) } ''Encoding
    ]

instance FromJSON Reference where
  parseJSON (Object o) = Reference <$> o .: "$ref"
  parseJSON _ = empty

referencedParseJSON :: FromJSON a => Text -> Value -> JSON.Parser (Referenced a)
referencedParseJSON prefix js@(Object o) = do
  ms <- o .:? "$ref"
  case ms of
    Nothing -> Inline <$> parseJSON js
    Just s  -> Ref <$> parseRef s
  where
    parseRef s = do
      case Text.stripPrefix prefix s of
        Nothing     -> fail $ "expected $ref of the form \"" <> Text.unpack prefix <> "*\", but got " <> show s
        Just suffix -> pure (Reference suffix)
referencedParseJSON _ _ = fail "referenceParseJSON: not an object"

instance FromJSON (Referenced Schema)   where parseJSON = referencedParseJSON "#/components/schemas/"
instance FromJSON (Referenced Param)    where parseJSON = referencedParseJSON "#/components/parameters/"
instance FromJSON (Referenced Response) where parseJSON = referencedParseJSON "#/components/responses/"
instance FromJSON (Referenced RequestBody) where parseJSON = referencedParseJSON "#/components/requestBodies/"
instance FromJSON (Referenced Example)  where parseJSON = referencedParseJSON "#/components/examples/"
instance FromJSON (Referenced Header)   where parseJSON = referencedParseJSON "#/components/headers/"
instance FromJSON (Referenced Link)     where parseJSON = referencedParseJSON "#/components/links/"
instance FromJSON (Referenced Callback) where parseJSON = referencedParseJSON "#/components/callbacks/"

instance FromJSON Xml where
  parseJSON = genericParseJSON (jsonPrefix "xml")

instance FromJSON AdditionalProperties where
  parseJSON (Bool b) = pure $ AdditionalPropertiesAllowed b
  parseJSON js = AdditionalPropertiesSchema <$> parseJSON js

-- | All strings are parsed as expressions
instance FromJSON ExpressionOrValue where
  parseJSON (String expr) = pure $ Expression expr
  parseJSON v = pure $ Value v

instance FromJSON Callback where
  parseJSON = fmap Callback . parseJSON