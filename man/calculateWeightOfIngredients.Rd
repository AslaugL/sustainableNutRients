% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateWeightOfIngredients.R
\name{calculateWeightOfIngredients}
\alias{calculateWeightOfIngredients}
\title{calculateWeightOfIngredients}
\usage{
calculateWeightOfIngredients(df, weight_in = "hektograms")
}
\arguments{
\item{df}{Output from findFoodInDatabase "volume_weight".}

\item{weight_in}{Unit the weight should be calculated to. Three options "grams", "hektograms" or "kilograms".}
}
\value{
A dataframe with the weight of ingredients in the chosen weight unit.
}
\description{
Calculate weight of ingredients after using findFoodInDatabase "volume_weight".
}
\details{
Calculate weight of ingredients after using findFoodInDatabase "volume_weight".
}
