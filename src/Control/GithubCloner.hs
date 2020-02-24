module Control.GithubCloner
  ( GithubCloner (..),
    InstallationToken (..),
  )
where

newtype InstallationToken = InstallationToken String

class GithubCloner m where
  getGithubInstallationToken :: m InstallationToken
