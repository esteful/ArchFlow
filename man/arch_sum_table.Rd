% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arch_sum_table.R
\name{arch_sum_table}
\alias{arch_sum_table}
\title{Summary Table.}
\usage{
arch_sum_table(df_comp.group, signif = FALSE, decimals = 2, sig_digits = 3)
}
\arguments{
\item{df_comp.group}{A dataframe with the compositional data of a given compositional group}

\item{signif}{Include sifnificant numbers FALSE other TRUE}

\item{decimals}{How many decimals to include}

\item{sig_digits}{How many significant numbers to include}
}
\value{
A dataframe with following summary chemical compositions of a compositional group: number of individuals, means, SD and RSD .
}
\description{
Summary table including the following compositional data of a compositional groups: number of individuals, means, SD and RSD.
}
