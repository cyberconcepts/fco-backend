{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend where

import BasicPrelude
import Control.Exception (bracket)
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
                Namespace (..), NamespaceId, 
                NodeId, Node (..),
                TripleId, Triple (..),
                Object (..), 
                QueryCrit (..), TripleQuery (..),
                dbSettings, envDB, envNamespaces)
import Fco.Core.Types (
                NodeName)


-- format nodes and triples for display

showNode :: Environment -> NodeId -> IO Text
showNode env nodeId = 
    withConnection (envDB env) $ \conn -> do
        Node nsId name <- getNode conn nodeId
        return $ (getNamespacePrefix env nsId) ++ ":" ++ name

showObject :: Environment -> Object -> IO Text
showObject env (NodeRef id) = showNode env id
showObject env (TextVal txt) = return $ "\"" ++ txt ++ "\""

showTriple :: Environment -> Triple -> IO Text
showTriple env (Triple subject predicate object) = do
    s <- showNode env subject 
    p <- showNode env predicate
    o <- showObject env object
    return $ unwords [s, p, o]


-- parse nodes and triples using backend store

parseNode :: Environment -> Text -> IO NodeId
parseNode env txt = do
    let (ns, rname) = T.breakOn ":" txt
        nsId = findNameSpaceByPrefix env ns
    withConnection (envDB env) $ \conn ->
        getOrCreateNode conn nsId $ T.tail rname

--parseTriple :: Environment -> Text -> IO Triple
parseTriple :: Environment -> Text -> IO TripleId
parseTriple env txt = do
    let (st, pt, ot) = splitTripleString txt
    s <- parseNode env st
    p <- parseNode env pt
    o <- parseNode env ot
    --return $ Triple s p (NodeRef o)
    withConnection (envDB env) $ \conn ->
        getOrCreateTriple conn s p (NodeRef o)


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


getNamespacePrefix :: Environment -> NamespaceId -> Text
getNamespacePrefix env nsId = 
    case lookup nsId (envNamespaces env) of
        Just (Namespace iri prefix) -> prefix
        Nothing -> ""

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
