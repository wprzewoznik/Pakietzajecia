% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obl_emis.R
\name{obl_emis}
\alias{obl_emis}
\title{Funkcja obliczajaca wskaznik emisji la danych na podstawie podanych,
zaimportowanych parametrow}
\usage{
obl_emis(
  dane = input,
  kategoria = "Passenger Cars",
  paliwo = "Petrol",
  euro = "Euro 5",
  technologia = "GDI",
  mode = "",
  substancja = c("EC", "CO")
)
}
\arguments{
\item{dane}{char}

\item{kategoria}{char}

\item{paliwo}{char}

\item{euro}{char}

\item{technologia}{char}

\item{mode}{char}

\item{substancja}{char}
}
\value{
double
}
\description{
Funkcja obliczajaca wskaznik emisji la danych na podstawie podanych,
zaimportowanych parametrow
}
\examples{
#obl_emis() -> tmp
#tmp \%>\% glimpse()

}
