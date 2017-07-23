{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.Common.Syntax where

import Data.Text

--------------------------------------------------------------------------------
--                               AST MAXIMUM                                  --
--------------------------------------------------------------------------------

data Expr
  = Nat_ Integer
  | Var_ Text
  | TyNat_
  | KStar_
  | KHash_
  | Arrow_ Expr Expr
  | Binder_ Text Expr Expr
  | Ann_ Expr Expr
  | App_ Expr Expr
  deriving (Show,Eq)

class CommonEncode a where
   from :: Expr -> Maybe a
   to :: a -> Expr

--------------------------------------------------------------------------------
--                                 Symbols                                    --
--------------------------------------------------------------------------------

existsChar,forallChar :: Char
existsChar = '\8707'
forallChar = '\8704'

greek :: [Char]
greek =
  [ alphaLower, alphaUpper
  , betaLower, betaUpper
  , gammaLower, gammaUpper
  , deltaLower, deltaUpper
  , epsilonLower, epsilonUpper
  , zetaLower, zetaUpper
  , etaLower, etaUpper
  , thetaLower, thetaUpper
  , iotaLower, iotaUpper
  , kappaLower, kappaUpper
  , lambdaLower, lambdaUpper
  , muLower, muUpper
  , nuLower, nuUpper
  , xiLower, xiUpper
  , omicronLower, omicronUpper
  , piLower, piUpper
  , rhoLower, rhoUpper
  , sigmaLower, sigmaUpper
  , tauLower, tauUpper
  , upsilonLower, upsilonUpper
  , phiLower, phiUpper
  , chiLower, chiUpper
  , psiLower, psiUpper
  , omegaLower, omegaUpper
  ]

alphaLower, alphaUpper :: Char
alphaLower = '\945'
alphaUpper = '\913'

betaLower, betaUpper :: Char
betaLower = '\946'
betaUpper = '\914'

gammaLower, gammaUpper :: Char
gammaLower = '\947'
gammaUpper = '\915'

deltaLower, deltaUpper :: Char
deltaLower = '\948'
deltaUpper = '\916'

epsilonLower, epsilonUpper :: Char
epsilonLower = '\949'
epsilonUpper = '\917'

zetaLower, zetaUpper :: Char
zetaLower = '\950'
zetaUpper = '\918'

etaLower, etaUpper :: Char
etaLower = '\951'
etaUpper = '\919'

thetaLower, thetaUpper :: Char
thetaLower = '\952'
thetaUpper = '\920'

iotaLower, iotaUpper :: Char
iotaLower = '\953'
iotaUpper = '\921'

kappaLower, kappaUpper :: Char
kappaLower = '\954'
kappaUpper = '\922'

lambdaLower, lambdaUpper :: Char
lambdaLower = '\955'
lambdaUpper = '\923'

muLower, muUpper :: Char
muLower = '\956'
muUpper = '\924'

nuLower, nuUpper :: Char
nuLower = '\957'
nuUpper = '\925'

xiLower, xiUpper :: Char
xiLower = '\958'
xiUpper = '\926'

omicronLower, omicronUpper :: Char
omicronLower = '\959'
omicronUpper = '\927'

piLower, piUpper :: Char
piLower = '\960'
piUpper = '\928'

rhoLower, rhoUpper :: Char
rhoLower = '\961'
rhoUpper = '\929'

sigmaLower, sigmaUpper :: Char
sigmaLower = '\962'
sigmaUpper = '\931'

tauLower, tauUpper :: Char
tauLower = '\963'
tauUpper = '\932'

upsilonLower, upsilonUpper :: Char
upsilonLower = '\964'
upsilonUpper = '\933'

phiLower, phiUpper :: Char
phiLower = '\965'
phiUpper = '\934'

chiLower, chiUpper :: Char
chiLower = '\966'
chiUpper = '\935'

psiLower, psiUpper :: Char
psiLower = '\967'
psiUpper = '\936'

omegaLower, omegaUpper :: Char
omegaLower = '\968'
omegaUpper = '\937'
