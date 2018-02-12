module Data.Protobuf.Parser( module Data.Protobuf.ProtoSpec
                           , identifier
                           , protoSpec
                           , protoType) where

import           Data.Protobuf.Parser.Internal (identifier, protoSpec,
                                                protoType)
import qualified Data.Protobuf.ProtoSpec

-- Shim module so that the relevant parts of the internal implementation
-- are exposed from this module without the irrelevant, internal parts.
-- This allows the internal module to be imported to the test suite to
-- test the individual components directly, as well as letting an expert
-- user write some implementation based on the internals if they need
-- an unforeseen use case.
