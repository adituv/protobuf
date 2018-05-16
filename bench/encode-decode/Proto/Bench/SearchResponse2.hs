{- This file was auto-generated from bench/SearchResponse2.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, MultiParamTypeClasses, FlexibleContexts,
  FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Bench.SearchResponse2 where
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

data SearchResponse2 = SearchResponse2{_SearchResponse2'results ::
                                       ![SearchResponse2'Result]}
                     deriving (Prelude.Show, Prelude.Eq)

instance (a ~ [SearchResponse2'Result],
          b ~ [SearchResponse2'Result], Prelude.Functor f) =>
         Lens.Labels.HasLens "results" f SearchResponse2 SearchResponse2 a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse2'results
              (\ x__ y__ -> x__{_SearchResponse2'results = y__})

instance Data.Default.Class.Default SearchResponse2 where
        def = SearchResponse2{_SearchResponse2'results = []}

instance Data.ProtoLens.Message SearchResponse2 where
        descriptor
          = let results__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "results"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor SearchResponse2'Result)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked results)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse2
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, results__field_descriptor)])
                (Data.Map.fromList [("results", results__field_descriptor)])

data SearchResponse2'Result = SearchResponse2'Result{_SearchResponse2'Result'url
                                                     :: !Data.Text.Text,
                                                     _SearchResponse2'Result'title ::
                                                     !Data.Text.Text,
                                                     _SearchResponse2'Result'snippets ::
                                                     ![Data.Text.Text]}
                            deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "url" f SearchResponse2'Result
           SearchResponse2'Result
           a
           b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse2'Result'url
              (\ x__ y__ -> x__{_SearchResponse2'Result'url = y__})

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "title" f SearchResponse2'Result
           SearchResponse2'Result
           a
           b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse2'Result'title
              (\ x__ y__ -> x__{_SearchResponse2'Result'title = y__})

instance (a ~ [Data.Text.Text], b ~ [Data.Text.Text],
          Prelude.Functor f) =>
         Lens.Labels.HasLens "snippets" f SearchResponse2'Result
           SearchResponse2'Result
           a
           b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse2'Result'snippets
              (\ x__ y__ -> x__{_SearchResponse2'Result'snippets = y__})

instance Data.Default.Class.Default SearchResponse2'Result where
        def
          = SearchResponse2'Result{_SearchResponse2'Result'url =
                                     Data.ProtoLens.fieldDefault,
                                   _SearchResponse2'Result'title = Data.ProtoLens.fieldDefault,
                                   _SearchResponse2'Result'snippets = []}

instance Data.ProtoLens.Message SearchResponse2'Result where
        descriptor
          = let url__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "url"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional url)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse2'Result
                title__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "title"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional title)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse2'Result
                snippets__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "snippets"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked snippets)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse2'Result
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, url__field_descriptor),
                    (Data.ProtoLens.Tag 2, title__field_descriptor),
                    (Data.ProtoLens.Tag 3, snippets__field_descriptor)])
                (Data.Map.fromList
                   [("url", url__field_descriptor),
                    ("title", title__field_descriptor),
                    ("snippets", snippets__field_descriptor)])

results ::
        forall f s t a b . (Lens.Labels.HasLens "results" f s t a b) =>
          Lens.Family2.LensLike f s t a b
results
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "results")

snippets ::
         forall f s t a b . (Lens.Labels.HasLens "snippets" f s t a b) =>
           Lens.Family2.LensLike f s t a b
snippets
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "snippets")

title ::
      forall f s t a b . (Lens.Labels.HasLens "title" f s t a b) =>
        Lens.Family2.LensLike f s t a b
title
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "title")

url ::
    forall f s t a b . (Lens.Labels.HasLens "url" f s t a b) =>
      Lens.Family2.LensLike f s t a b
url
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "url")