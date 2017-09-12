{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.JSString.Text
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text.Lazy.Encoding as LT
import GHC.Generics
import JavaScript.Web.XMLHttpRequest
import Miso hiding (defaultOptions)
import Miso.String (MisoString, pack)
import qualified Miso.String as MS
import Text.RawString.QQ

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

owner = "dplusic"

githubToken = "d5936d1a4c17fa964751fc53f1e9f29f9e136866"

-- | Model
data Model = Model
  { info :: Maybe [GithubPullRequest]
  } deriving (Eq, Show)

-- | Action
data Action
  = FetchGitHub
  | SetGitHub [GithubPullRequest]
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = startApp App {model = Model Nothing, initialAction = FetchGitHub, ..}
  where
    update = updateModel
    events = defaultEvents
    subs = []
    view = viewModel

{-# ANN updateModel "HLint: ignore Redundant do" #-}

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel FetchGitHub m = m <# do SetGitHub <$> getPullRequests
updateModel (SetGitHub pullRequests) m = noEff m {info = Just pullRequests}

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} = view
  where
    view =
      div_
        [class_ $ pack "container has-text-centered", style_ $ M.fromList [(pack "margin-top", pack "100px")]]
        [ h1_ [class_ $ pack "title is-spaced"] [text $ pack owner]
        , h2_ [class_ $ pack "subtitle"] [text $ pack "Contributed Repositories"]
        , case info of
            Nothing -> div_ [] [text $ pack "Fetching..."]
            Just pullRequests ->
              table_
                [class_ $ pack "table is-striped is-fullwidth"]
                [ tbody_ [] $
                  map
                    (\ContributeRepository {..} ->
                       tr_
                         []
                         [ td_ [] [text $ MS.take 10 lastMergedAt]
                         , td_ [style_ $ M.fromList [(pack "font-size", pack "1.1rem")]] [a_ [href_ repoUrl] [text name]]
                         , td_
                             []
                             [ a_
                                 [href_ $ repoUrl `MS.append` pack "/pulls?q=is%3Amerged%20author%3A" `MS.append` pack owner]
                                 [text $ countToWord "pull request" totalPrs]
                             ]
                         , td_ [] [a_ [href_ $ repoUrl `MS.append` pack "/commits?author=" `MS.append` pack owner] [text $ countToWord "commit" totalCommits]]
                         ]) $
                  toContributedRepositories pullRequests
                ]
        ]
      where
        countToWord unitWord count = pack $ show count ++ " " ++ pluralizeOrNot unitWord count
        pluralizeOrNot word 1 = word
        pluralizeOrNot word _ = word ++ "s"

toContributedRepositories =
  sortBy (\x y -> (lastMergedAt y `compare` lastMergedAt x) `mappend` (totalPrs y `compare` totalPrs x)) .
  map
    (foldr1
       (\x acc ->
          ContributeRepository
          { name = name acc
          , repoUrl = repoUrl acc
          , totalCommits = totalCommits acc + totalCommits x
          , totalPrs = totalPrs acc + 1
          , lastMergedAt = max (lastMergedAt acc) (lastMergedAt x)
          })) .
  groupBy (\x y -> name x == name y) . sortBy (\x y -> name x `compare` name y) . map toContributedRepository

data ContributedRepository = ContributeRepository
  { name :: MisoString
  , repoUrl :: MisoString
  , totalCommits :: Int
  , totalPrs :: Int
  , lastMergedAt :: MisoString
  }

toContributedRepository GithubPullRequest {..} =
  ContributeRepository {name = nameWithOwner repository, repoUrl = url repository, totalCommits = totalCount commits, totalPrs = 1, lastMergedAt = mergedAt}

data GithubResponse = GithubResponse
  { getData :: GithubData
  } deriving (Show, Eq, Generic)

githubResponsefieldLabelModifier "getData" = "data"

instance FromJSON GithubResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = githubResponsefieldLabelModifier}

data GithubData = GithubData
  { viewer :: GithubUser
  } deriving (Show, Eq, Generic)

instance FromJSON GithubData

data GithubUser = GithubUser
  { pullRequests :: GithubPullRequestConnection
  } deriving (Show, Eq, Generic)

instance FromJSON GithubUser

data GithubPullRequestConnection = GithubPullRequestConnection
  { nodes :: [GithubPullRequest]
  } deriving (Show, Eq, Generic)

instance FromJSON GithubPullRequestConnection

data GithubPullRequest = GithubPullRequest
  { commits :: GithubPullRequestCommitConnection
  , mergedAt :: MisoString
  , repository :: GithubRepository
  } deriving (Show, Eq, Generic)

instance FromJSON GithubPullRequest

data GithubPullRequestCommitConnection = GithubPullRequestCommitConnection
  { totalCount :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON GithubPullRequestCommitConnection

data GithubRepository = GithubRepository
  { nameWithOwner :: MisoString
  , url :: MisoString
  } deriving (Show, Eq, Generic)

instance FromJSON GithubRepository

getPullRequests :: IO [GithubPullRequest]
getPullRequests = do
  response <-
    requestGitHubGraphQL
      [r|
{
  viewer {
    pullRequests(first: 100, states: MERGED) {
      nodes {
        repository {
          nameWithOwner
          url
        }
        commits {
          totalCount
        }
        mergedAt
        url
      }
    }
  }
}
    |]
  return $ nodes . pullRequests . viewer . getData $ response

newtype Query = Query
  { query :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Query

requestGitHubGraphQL :: String -> IO GithubResponse
requestGitHubGraphQL query = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String GithubResponse of
    Left s -> error s
    Right j -> pure j
  where
    req =
      Request
      { reqMethod = POST
      , reqURI = pack "https://api.github.com/graphql"
      , reqLogin = Nothing
      , reqHeaders = [(pack "Authorization", pack $ "bearer " ++ githubToken)]
      , reqWithCredentials = False
      , reqData = StringData $ lazyTextToJSString . LT.decodeUtf8 . encode $ Query {query = query}
      }
