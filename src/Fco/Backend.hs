{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend where

import BasicPrelude
import Control.Exception (bracket)
import Data.Char (isDigit)
import Data.List (lookup)
import qualified Data.Text as T
import Data.IntMap (fromList)

import Fco.Backend.Database (
                Connection,
                connect, disconnect, 
                getNamespaces,
                addNode, getNode, queryNode,
                addTriple, queryTriple)
import Fco.Backend.Database as DB
import Fco.Backend.Types (
                DBSettings, Environment,
                NamespaceId, 
                NodeId, Node (..),
                TripleId, Triple (..),
                Object (..), 
                QueryCrit (..), TripleQuery (..),
                dbSettings, envDB, envNamespaces)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..), NodeName)
import qualified Fco.Core.Types as CT


-- convert nodes and triples to core representation

toCoreNode :: Environment -> NodeId -> IO CT.Node
toCoreNode env nodeId = 
    withConnection (envDB env) $ \conn -> do
        Node nsId name <- getNode conn nodeId
        return $ CT.Node (getNamespace env nsId) name

toCoreObject :: Environment -> Object -> IO CT.Object
toCoreObject env (NodeRef nodeId) =
    toCoreNode env nodeId >>= return . CT.NodeRef
toCoreObject env (TextVal txt) = return $ CT.TextVal txt
toCoreObject env (IntVal i) = return $ CT.IntVal i

toCoreTriple :: Environment -> Triple -> IO CT.Triple
toCoreTriple env (Triple subject predicate object) = do
    s <- toCoreNode env subject 
    p <- toCoreNode env predicate
    o <- toCoreObject env object
    return $ CT.Triple s p o

showTriple :: Environment -> Triple -> IO Text
showTriple env triple =
    toCoreTriple env triple >>= return . CS.showTriple


-- parse nodes and triples using backend store

fromCoreNode :: Environment -> CT.Node -> IO NodeId
fromCoreNode env (CT.Node (Namespace iri prefix) name) =
    withConnection (envDB env) $ \conn ->
        getOrCreateNode conn (findNameSpaceByPrefix env prefix) name

parseNode :: Environment -> Text -> IO NodeId
parseNode env txt = do
    let (ns, rname) = T.breakOn ":" txt
        nsId = findNameSpaceByPrefix env ns
    withConnection (envDB env) $ \conn ->
        getOrCreateNode conn nsId $ T.tail rname

fromCoreObject :: Environment -> CT.Object -> IO Object
fromCoreObject env (CT.NodeRef node) = 
    fromCoreNode env node >>= return . NodeRef
fromCoreObject env (CT.IntVal i) = return $ IntVal i
fromCoreObject env (CT.TextVal txt) = return $ TextVal txt

fromCoreTriple :: Environment -> CT.Triple -> IO Triple
fromCoreTriple env (CT.Triple cs cp co) = do
    s <- fromCoreNode env cs
    p <- fromCoreNode env cp
    o <- fromCoreObject env co
    return $ Triple s p o

parseTriple :: Environment -> Text -> IO TripleId
parseTriple env txt = do
    let ct = CP.parseTriple (Namespace "") txt
    Triple s p o <- fromCoreTriple env ct
    withConnection (envDB env) $ \conn ->
        getOrCreateTriple conn s p o    


parseQuery :: Environment -> Text -> IO TripleQuery
parseQuery env txt = do
    let (st, pt, ot) = splitTripleString txt
    s <- case st of
        "?" -> return Ignore
        _ -> do 
                sx <- parseNode env st
                return $ IsEqual sx
    return $ TripleQuery s Ignore Ignore


-- helper functions

splitTripleString :: Text -> (Text, Text, Text)
splitTripleString txt = 
    let stripSpace = snd . T.span (== ' ')
        txt1 = stripSpace txt
        (st, r1) = T.breakOn " " txt1
        r2 = stripSpace r1
        (pt, r3) = T.breakOn " " r2
        ot = stripSpace r3
    in (st, pt, ot)


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
