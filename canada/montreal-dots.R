library(cancensus)
library(tidyverse)
options(cancensus.api_key = "CensusMapper_6293253518cbfcfc86b62e83f056f92f")

## Need: "CA16"

# Montreal: CMA 24462

View(list_census_vectors("CA16"))

# 