{- This file was auto-generated from bench/Test3.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, MultiParamTypeClasses, FlexibleContexts,
  FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Bench.Test3 where
import qualified Data.ProtoLens.Reexport.Prelude as Prelude
import qualified Data.ProtoLens.Reexport.Data.Int as Data.Int
import qualified Data.ProtoLens.Reexport.Data.Word as Data.Word
import qualified Data.ProtoLens.Reexport.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Reexport.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Reexport.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Reexport.Data.Default.Class
       as Data.Default.Class
import qualified Data.ProtoLens.Reexport.Data.Text as Data.Text
import qualified Data.ProtoLens.Reexport.Data.Map as Data.Map
import qualified Data.ProtoLens.Reexport.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Reexport.Lens.Labels as Lens.Labels

data Test3 = Test3{_Test3'test1 :: !Data.Word.Word32,
                   _Test3'completed :: !Prelude.Bool}
           deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.Word.Word32, b ~ Data.Word.Word32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "test1" f Test3 Test3 a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _Test3'test1
              (\ x__ y__ -> x__{_Test3'test1 = y__})

instance (a ~ Prelude.Bool, b ~ Prelude.Bool, Prelude.Functor f) =>
         Lens.Labels.HasLens "completed" f Test3 Test3 a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _Test3'completed
              (\ x__ y__ -> x__{_Test3'completed = y__})

instance Data.Default.Class.Default Test3 where
        def
          = Test3{_Test3'test1 = Data.ProtoLens.fieldDefault,
                  _Test3'completed = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message Test3 where
        descriptor
          = let test1__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "test1"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional test1)
                      :: Data.ProtoLens.FieldDescriptor Test3
                completed__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "completed"
                      (Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional completed)
                      :: Data.ProtoLens.FieldDescriptor Test3
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 5, test1__field_descriptor),
                    (Data.ProtoLens.Tag 21, completed__field_descriptor)])
                (Data.Map.fromList
                   [("test1", test1__field_descriptor),
                    ("completed", completed__field_descriptor)])

data Test3'Extra = Test3'Extra{_Test3'Extra'blah ::
                               !Data.Text.Text}
                 deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "blah" f Test3'Extra Test3'Extra a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _Test3'Extra'blah
              (\ x__ y__ -> x__{_Test3'Extra'blah = y__})

instance Data.Default.Class.Default Test3'Extra where
        def = Test3'Extra{_Test3'Extra'blah = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message Test3'Extra where
        descriptor
          = let blah__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "blah"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional blah)
                      :: Data.ProtoLens.FieldDescriptor Test3'Extra
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, blah__field_descriptor)])
                (Data.Map.fromList [("blah", blah__field_descriptor)])

data Test3'Result = Test3'Result{_Test3'Result'data' ::
                                 !Data.Word.Word32}
                  deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.Word.Word32, b ~ Data.Word.Word32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "data'" f Test3'Result Test3'Result a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _Test3'Result'data'
              (\ x__ y__ -> x__{_Test3'Result'data' = y__})

instance Data.Default.Class.Default Test3'Result where
        def
          = Test3'Result{_Test3'Result'data' = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message Test3'Result where
        descriptor
          = let data'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "data"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional data')
                      :: Data.ProtoLens.FieldDescriptor Test3'Result
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, data'__field_descriptor)])
                (Data.Map.fromList [("data", data'__field_descriptor)])

blah ::
     forall f s t a b . (Lens.Labels.HasLens "blah" f s t a b) =>
       Lens.Family2.LensLike f s t a b
blah
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "blah")

completed ::
          forall f s t a b . (Lens.Labels.HasLens "completed" f s t a b) =>
            Lens.Family2.LensLike f s t a b
completed
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "completed")

data' ::
      forall f s t a b . (Lens.Labels.HasLens "data'" f s t a b) =>
        Lens.Family2.LensLike f s t a b
data'
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "data'")

test1 ::
      forall f s t a b . (Lens.Labels.HasLens "test1" f s t a b) =>
        Lens.Family2.LensLike f s t a b
test1
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "test1")