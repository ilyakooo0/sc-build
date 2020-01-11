module Control.GithubCloner
  ( GithubAccessToken (..),
    GithubUserName (..),
    GithubCloner (..),
  )
where

newtype GithubAccessToken = GithubAccessToken String

newtype GithubUserName = GithubUserName String

class GithubCloner m where

  getGithubAccessToken :: m GithubAccessToken

  getGithubUserName :: m GithubUserName
