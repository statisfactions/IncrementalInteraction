

library(palmerpenguins)
library(tidyverse)
peng_mm = select(na.omit(penguins), ends_with("mm"))
peng_mm %>% mutate(bb = bill_length_mm*bill_depth_mm) %>% cor
peng_mm %>% mutate(across(everything(), scale)) %>% mutate(bb = bill_length_mm*bill_depth_mm) %>% cor
