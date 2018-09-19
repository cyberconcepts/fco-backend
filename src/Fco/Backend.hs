{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend where

import BasicPrelude
import Control.Exception (bracket)
import Data.Char (isDigit)
import Data.List (lookup)
import qualified Data.Text as T
import Data.IntMap (elems, fromList)

import Fco.Backend.Database (
                Connection,
                connect, disconnect, 
                getNamespaces,
                addNode, getNode, queryNode,
                addText, getText, queryText,
                addTriple, queryTriple)
import qualified Fco.Backend.Database as DB
import Fco.Backend.Types (
                DBSettings, Environment,
                NamespaceId, 
                NodeId, TextId, Node (..),
                TripleId, Triple (..),
                Object (..), 
                QueryCrit (..), TripleQuery (..),
                dbSettings, envDB, envNamespaces)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..), NodeName)
import qualified Fco.Core.Types as CT


-- high-level query functions

query :: Environment -> CT.Query -> IO [CT.Triple]
query env query = 
    withConnection (envDB env) $ \conn ->
        fromCoreQuery env query >>=
        queryTriples conn >>=
        mapM (toCoreTriple env) . elems

queryTxt :: Environment -> Text -> IO [Text]
queryTxt env txt = do
    withConnection (envDB env) $ \conn ->
        parseQuery env txt >>= 
        queryTriples conn >>= 
        mapM (showTriple env) . elems


-- convert (nodes and) triples to a readable representation

showTriple :: Environment -> Triple -> IO Text
showTriple env triple =
    toCoreTriple env triple >>= return . CS.showTriple


-- parse triples and queries using backend store

parseTriple :: Environment -> Text -> IO TripleId
parseTriple env txt = do
    let ct = CP.parseTriple (Namespace "") txt
    Triple s p o <- fromCoreTriple env ct
    withConnection (envDB env) $ \conn ->
        getOrCreateTriple conn s p o    


parseQuery :: Environment -> Text -> IO TripleQuery
parseQuery env txt =
    let qt = CP.parseQuery (Namespace "") txt
    in fromCoreQuery env qt


-- conversion to / from Fco.Core representations

toCoreNode :: Environment -> NodeId -> IO CT.Node
toCoreNode env nodeId = 
    withConnection (envDB env) $ \conn -> do
        Node nsId name <- getNode conn nodeId
        return $ CT.Node (getNamespace env nsId) name

toText :: Environment -> TextId -> IO Text
toText env txtId = 
    withConnection (envDB env) $ \conn -> getText conn txtId

toCoreObject :: Environment -> Object -> IO CT.Object
toCoreObject env (Object 1 nodeId) =
    toCoreNode env nodeId >>= return . CT.NodeRef
toCoreObject env (Object 2 txtId) = 
    toText env txtId >>= return . CT.TextVal
toCoreObject env (Object 3 i) = return $ CT.IntVal i

toCoreTriple :: Environment -> Triple -> IO CT.Triple
toCoreTriple env (Triple subject predicate object) = do
    s <- toCoreNode env subject 
    p <- toCoreNode env predicate
    o <- toCoreObject env object
    return $ CT.Triple s p o


fromCoreNode :: Environment -> CT.Node -> IO NodeId
fromCoreNode env (CT.Node (Namespace iri prefix) name) =
    withConnection (envDB env) $ \conn ->
        getOrCreateNode conn (findNameSpaceByPrefix env prefix) name

fromText :: Environment -> Text -> IO TextId
fromText env txt =
    withConnection (envDB env) $ \conn ->
        getOrCreateText conn txt

fromCoreObject :: Environment -> CT.Object -> IO Object
fromCoreObject env (CT.NodeRef node) = 
    fromCoreNode env node >>= return . (Object 1)
fromCoreObject env (CT.TextVal txt) =
    fromText env txt >>= return . (Object 2)
fromCoreObject env (CT.IntVal i) = return $ (Object 3 i)

fromCoreTriple :: Environment -> CT.Triple -> IO Triple
fromCoreTriple env (CT.Triple cs cp co) = do
    s <- fromCoreNode env cs
    p <- fromCoreNode env cp
    o <- fromCoreObject env co
    return $ Triple s p o


fromCoreQuery :: Environment -> CT.Query -> IO TripleQuery
fromCoreQuery env (CT.Query cs cp co) = do
    s <- fromCoreQuCrit env cs fromCoreNode
    p <- fromCoreQuCrit env cp fromCoreNode
    o <- fromCoreQuCrit env co fromCoreObject
    return $ TripleQuery s p o

fromCoreQuCrit :: Environment -> CT.QuCrit a -> (Environment -> a -> IO b) 
        -> IO (QueryCrit b)
fromCoreQuCrit env qc conv =
    case qc of 
        CT.Ignore -> return Ignore
        CT.IsEqual x -> conv env x >>= (return . IsEqual)


-- helper functions

getNamespace :: Environment -> NamespaceId -> Namespace
getNamespace env nsId = 
    case lookup nsId (envNamespaces env) of
        Just ns -> ns
        Nothing -> error $ "Namespace " ++ (show nsId) ++ " not found!"

findNameSpaceByPrefix :: Environment -> Text -> NamespaceId
findNameSpaceByPrefix env prefix = 
    case find checkPrefix (envNamespaces env) of
        Just (id, Namespace iri pf) -> id
        Nothing -> error $ "Namespace " ++ (show prefix) ++ " not found!"
    where checkPrefix (id, Namespace iri pf) = pf == prefix

setupEnv :: Environment -> IO Environment
setupEnv env = 
    withConnection (envDB env) $ \conn -> do
        nss <- getNamespaces conn
        return $ env { envNamespaces = nss }


-- lower-level database-related stuff

fcoConnect :: DBSettings -> IO Connection
fcoConnect settings = connect settings

withConnection :: DBSettings -> (Connection -> IO c) -> IO c
withConnection settings = bracket (fcoConnect settings) disconnect


getOrCreateNode :: Connection -> NamespaceId -> Text -> IO NodeId
getOrCreateNode conn nsId name = do
    result <- queryNode conn nsId name
    case result of
      Nothing -> addNode conn (Node nsId name)
      Just id -> return id

getOrCreateText :: Connection -> Text -> IO TextId
getOrCreateText conn txt = do
    result <- queryText conn txt
    case result of
      Nothing -> addText conn txt
      Just id -> return id

getOrCreateTriple :: Connection -> NodeId -> NodeId -> Object -> 
          IO TripleId
getOrCreateTriple conn subject predicate object = do
    result <- queryTriple conn subject predicate object
    case result of
      Nothing -> addTriple conn (Triple subject predicate object)
      Just id -> return id

queryTriples :: Connection -> TripleQuery -> IO (IntMap Triple)
queryTriples conn query = do
  triples <- DB.queryTriples conn query
  return $ fromList triples

-- load bootstrap definitions (obsolete):

-- triple env "fco:datatype rdf:type rdf:Class"
-- triple env "fco:int rdf:type fco:datatype"
-- triple env "fco:string rdf:type fco:datatype"
-- triple env "rdf:type rdf:type rdf:Property"
-- triple env "rdf:Property rdf:type rdf:Class"
