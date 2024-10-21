## Choe, Yoshida, & Cole 2022

<a href="https://escholarship.org/uc/item/5d84n2x7"><img src="https://img.shields.io/badge/DOI-10.5070/G601136-brightgreen"/></a>

Choe, J., Yoshida, M., & Cole, J. (2022). The role of prosodic focus in the reanalysis of garden path sentences: Depth of semantic processing impedes the revision of an erroneous local analysis. Glossa Psycholinguistics, 1(1). http://dx.doi.org/10.5070/G601136

This repo only contains data and script for the auditory comprehension experiment. Experiment materials including audio stimuli can be found at: https://osf.io/u6dq5/

---

`CYC_2022.arrow` contains the cleaned data for the auditory comprehension experiment, added after publication for pedagogical use.[^1]

```r
library(arrow)
read_feather("https://raw.githubusercontent.com/yjunechoe/Semantic-Persistence/refs/heads/master/CYC_2022.arrow")
```

[^1]: Diagnosing convergence warnings: https://gist.github.com/yjunechoe/37e6c1300b96f7c466807cc538efbd65 
