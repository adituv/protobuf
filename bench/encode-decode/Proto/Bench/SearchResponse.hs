{- This file was auto-generated from bench/SearchResponse.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, MultiParamTypeClasses, FlexibleContexts,
  FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Bench.SearchResponse where
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

data SearchResponse = SearchResponse{_SearchResponse'query ::
                                     !Data.Text.Text,
                                     _SearchResponse'pageNumber :: !Data.Int.Int32,
                                     _SearchResponse'resultPerPage :: !Data.Int.Int32,
                                     _SearchResponse'results :: ![Data.Text.Text]}
                    deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "query" f SearchResponse SearchResponse a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse'query
              (\ x__ y__ -> x__{_SearchResponse'query = y__})

instance (a ~ Data.Int.Int32, b ~ Data.Int.Int32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "pageNumber" f SearchResponse SearchResponse a
           b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse'pageNumber
              (\ x__ y__ -> x__{_SearchResponse'pageNumber = y__})

instance (a ~ Data.Int.Int32, b ~ Data.Int.Int32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "resultPerPage" f SearchResponse SearchResponse
           a
           b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse'resultPerPage
              (\ x__ y__ -> x__{_SearchResponse'resultPerPage = y__})

instance (a ~ [Data.Text.Text], b ~ [Data.Text.Text],
          Prelude.Functor f) =>
         Lens.Labels.HasLens "results" f SearchResponse SearchResponse a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _SearchResponse'results
              (\ x__ y__ -> x__{_SearchResponse'results = y__})

instance Data.Default.Class.Default SearchResponse where
        def
          = SearchResponse{_SearchResponse'query =
                             Data.ProtoLens.fieldDefault,
                           _SearchResponse'pageNumber = Data.ProtoLens.fieldDefault,
                           _SearchResponse'resultPerPage = Data.ProtoLens.fieldDefault,
                           _SearchResponse'results = []}

instance Data.ProtoLens.Message SearchResponse where
        descriptor
          = let query__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "query"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional query)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse
                pageNumber__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "page_number"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional pageNumber)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse
                resultPerPage__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "result_per_page"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional resultPerPage)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse
                results__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "results"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked results)
                      :: Data.ProtoLens.FieldDescriptor SearchResponse
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, query__field_descriptor),
                    (Data.ProtoLens.Tag 2, pageNumber__field_descriptor),
                    (Data.ProtoLens.Tag 3, resultPerPage__field_descriptor),
                    (Data.ProtoLens.Tag 5, results__field_descriptor)])
                (Data.Map.fromList
                   [("query", query__field_descriptor),
                    ("page_number", pageNumber__field_descriptor),
                    ("result_per_page", resultPerPage__field_descriptor),
                    ("results", results__field_descriptor)])

pageNumber ::
           forall f s t a b . (Lens.Labels.HasLens "pageNumber" f s t a b) =>
             Lens.Family2.LensLike f s t a b
pageNumber
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "pageNumber")

query ::
      forall f s t a b . (Lens.Labels.HasLens "query" f s t a b) =>
        Lens.Family2.LensLike f s t a b
query
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "query")

resultPerPage ::
              forall f s t a b .
                (Lens.Labels.HasLens "resultPerPage" f s t a b) =>
                Lens.Family2.LensLike f s t a b
resultPerPage
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "resultPerPage")

results ::
        forall f s t a b . (Lens.Labels.HasLens "results" f s t a b) =>
          Lens.Family2.LensLike f s t a b
results
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "results")