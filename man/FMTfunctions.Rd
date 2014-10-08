\name{FMTfunctions}
\alias{beltob}
\alias{beltom}
\alias{beltopl}
\alias{beltoq}
\alias{btobel}
\alias{btom}
\alias{btopl}
\alias{btoq}
\alias{btov}
\alias{mtob}
\alias{mtobel}
\alias{mtobetp}
\alias{mtonm}
\alias{mtopl}
\alias{mtoq}
\alias{mtov}
\alias{mtow}
\alias{pltob}
\alias{pltobel}
\alias{pltom}
\alias{pltoq}
\alias{qtom}
\alias{qtow}
\alias{vtob}
\alias{vtom}
\alias{wtom}
\alias{wtoq}
\title{Fast Mobius Transform}
\description{Use the Fast Mobius Transformation to convert  one measure  to another one} 
\usage{
beltob(InputVec)
beltom(InputVec)
beltopl(InputVec)
beltoq(InputVec)
btobel (InputVec)
btom(InputVec)
btopl (InputVec)
btoq (InputVec)
btov(InputVec)
mtob (InputVec)
mtobel(InputVec)
mtobetp(InputVec)
mtonm(InputVec)
mtopl(InputVec)
mtoq (InputVec)
mtov (InputVec)
mtow (InputVec)
pltob(InputVec)
pltobel(InputVec)
pltom(InputVec)
pltoq (InputVec)
qtom (InputVec)
qtow(InputVec)
vtob(InputVec)
vtom (InputVec)
wtom (InputVec)
wtoq(InputVec)
}

\arguments{
  \item{InputVec}{the measure to transform, e.g., mass, bel function, plausibility function, etc.}}
\value{The associated converted new measure}
\examples{
	Mass=RandomMass(nbFocalElement=3, ThetaSize=3, nbMass=4, Type=1)
	mass=mtobel(Mass)
	qvec=mtoq(mass)
	mass=qtom(qvec)
}

