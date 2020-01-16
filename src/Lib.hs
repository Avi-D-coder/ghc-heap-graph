{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( writeSvg,
    dotHeapGraph,
  )
where

import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.Functor
import qualified Data.IntMap as M
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Internal
import Data.Tuple
import Dot
import GHC.HeapView
import System.Process

writeSvg :: String -> a -> IO ()
writeSvg name a = do
  let name' = filter isAlphaNum name
  hg <- buildHeapGraph 1000 name (asBox a)
  encodeToFile (name' <> ".dot") $ dotHeapGraph hg
  callProcess "dot" [name' <> ".dot", "-Tsvg", "-o " <> name' <> ".svg"]

dotHeapGraph :: HeapGraph String -> DotGraph
dotHeapGraph (HeapGraph m) =
  DotGraph Strict Directed Nothing
    $ concatMap from
    $ M.toList m
  where
    from (hi, hge) =
      [ StatementNode $
          NodeStatement
            (NodeId (Id . T.pack $ show hi) Nothing)
            [Attribute "label" $ Id $ T.pack label]
      ]
        <> edges
      where
        (label, edges) = ano hi hge
    id' :: Show a => a -> Id
    id' = Id . T.pack . show
    mkEdge a b = StatementEdge (EdgeStatement (ListTwo (edgeNode a) (edgeNode b) []) [])
    mkEdgeL a (b, l) =
      StatementEdge
        ( EdgeStatement
            (ListTwo (edgeNode a) (edgeNode b) [])
            [Attribute "label" l]
        )
    edgeNode a = EdgeNode $ NodeId (id' a) Nothing
    ano :: HeapGraphIndex -> HeapGraphEntry String -> (String, [Statement])
    ano a (HeapGraphEntry b c@ConstrClosure {..} _ s) =
      ( name <> ": " <> show dataArgs <> "\n",
        map (mkEdge a) $ catMaybes ptrArgs
      )
    ano a (HeapGraphEntry b c@FunClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes ptrArgs
      )
    ano a (HeapGraphEntry b c@ThunkClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes ptrArgs
      )
    ano a (HeapGraphEntry b c@SelectorClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes [selectee]
      )
    ano a (HeapGraphEntry b c@PAPClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes $ fun : payload
      )
    ano a (HeapGraphEntry b c@APClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes $ fun : payload
      )
    ano a (HeapGraphEntry b c@APStackClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes $ fun : payload
      )
    ano a (HeapGraphEntry b c@IndClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes [indirectee]
      )
    ano a (HeapGraphEntry b c@BCOClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes [instrs, literals, bcoptrs]
      )
    ano a (HeapGraphEntry b c@BlackholeClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes [indirectee]
      )
    ano a (HeapGraphEntry b c@ArrWordsClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
    ano a (HeapGraphEntry b c@MutArrClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes mccPayload
      )
    ano a (HeapGraphEntry b c@MVarClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdgeL a) $
          filter
            (isJust . fst)
            [ (queueHead, "queueHead"),
              (queueTail, "queueTail"),
              (value, "value")
            ]
      )
    ano a (HeapGraphEntry b c@MutVarClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes [var]
      )
    ano a (HeapGraphEntry b c@BlockingQueueClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdgeL a) $
          filter
            (isJust . fst)
            [ (blackHole, "blackHole"),
              (link, "link"),
              (owner, "owner"),
              (queue, "queue")
            ]
      )
    ano a (HeapGraphEntry b c@IntClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
    ano a (HeapGraphEntry b c@WordClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
    ano a (HeapGraphEntry b c@Int64Closure {..} _ s) =
      (s <> ppClosure (\_ _ -> "") 0 c, [])
    ano a (HeapGraphEntry b c@Word64Closure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
    ano a (HeapGraphEntry b c@AddrClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
    ano a (HeapGraphEntry b c@FloatClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
    ano a (HeapGraphEntry b c@DoubleClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
    ano a (HeapGraphEntry b c@OtherClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        map (mkEdge a) $ catMaybes hvalues
      )
    ano a (HeapGraphEntry b c@UnsupportedClosure {..} _ s) =
      ( s <> ppClosure (\_ _ -> "") 0 c,
        []
      )
